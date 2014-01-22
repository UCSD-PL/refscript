{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Nano.Typecheck.Typecheck (verifyFile, typeCheck, patchTypeAnnots) where 

import           Control.Exception                  (throw)
import           Control.Applicative                ((<$>))
import           Control.Monad                

import qualified Data.HashSet                       as HS 
import qualified Data.HashMap.Strict                as M 
import qualified Data.Traversable                   as T
import qualified Data.Either                        as E
import           Data.Monoid
import qualified Data.Foldable                      as FL
import qualified Data.List                          as L
import           Data.Maybe                         (catMaybes, isJust, fromJust, fromMaybe, listToMaybe, maybeToList)
import           Data.Generics                   

import           Text.PrettyPrint.HughesPJ          (text, render, vcat, ($+$), (<+>))
import           Text.Printf                        (printf)

import           Language.Nano.CmdLine              (getOpts)
import           Language.Nano.Errors
import           Language.Nano.Types
import           Language.Nano.Annots
import           Language.Nano.Env
import           Language.Nano.Misc
import           Language.Nano.Typecheck.Compare
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Parse 
import           Language.Nano.Typecheck.TCMonad
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Typecheck.Lookup
import           Language.Nano.Typecheck.Unfold
import           Language.Nano.Typecheck.Unify
import           Language.Nano.SSA.SSA

import           Language.Fixpoint.Errors
import qualified Language.Fixpoint.Types            as F
import           Language.Fixpoint.Misc             as FM 
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Parser.Type  (SourceSpan (..))
import           Debug.Trace                        hiding (traceShow)

import qualified System.Console.CmdArgs.Verbosity as V


--------------------------------------------------------------------------------
-- | Top-level Verifier 

--------------------------------------------------------------------------------
verifyFile :: FilePath -> IO (UAnnSol a, F.FixResult Error)
--------------------------------------------------------------------------------
verifyFile f = do 
  p <- parseNanoFromFile f
  case p of 
    Left err -> return (NoAnn, F.Unsafe [err]) 
    Right nano -> 
      do  V.whenLoud $ donePhase FM.Loud "Parse"
          V.whenLoud $ putStrLn . render . pp $ nano
          case ssaTransform' nano of 
            Left err -> return (NoAnn, F.Unsafe [err])
            Right p  -> 
              do 
                let nanoSsa = patchTypeAnnots p
                V.whenLoud  $ donePhase FM.Loud "SSA Transform"
                V.whenLoud  $ putStrLn . render . pp $ p
                verb       <- V.getVerbosity
                let annp    = execute verb nanoSsa $ tcNano p
                r          <- either unsafe safe annp 
                V.whenLoud  $ donePhase FM.Loud "Typechecking"
                return      $ (NoAnn, r)

unsafe errs = do putStrLn "\n\n\nErrors Found!\n\n" 
                 forM_ errs (putStrLn . ppshow) 
                 return $ F.Unsafe errs

safe (_, Nano {code = Src fs})
  = do V.whenLoud $ printAllAnns fs 
       nfc       <- noFailCasts <$> getOpts
       return     $ F.Safe `mappend` failCasts nfc fs 


-- | Inline type annotations
patchTypeAnnots :: NanoSSAR r -> NanoTSSAR r
patchTypeAnnots p@(Nano {code = Src fs, tAnns = m}) = 
    p {code = Src $ (patchAnn <$>) <$> fs}
  where
    patchAnn (Ann l bs) = Ann l $ (TAnnot <$> (maybeToList $ M.lookup l mm)) ++ bs
    mm = M.fromList (mapFst getAnnotation <$> envToList m)


-- | Cast manipulation

failCasts True  _  = F.Safe
failCasts False fs = applyNonNull F.Safe F.Unsafe $ concatMap castErrors $ getCasts fs 

getCasts         :: (Data r, Typeable r) => [Statement (AnnType r)] -> [(AnnType r)]
getCasts stmts   = everything (++) ([] `mkQ` f) stmts
  where 
    f            :: Expression (AnnType r) -> [(AnnType r)]
    f (Cast a _) = [a]
    f _          = [] 

-- castErrors :: (F.Reftable r) => AnnType r -> [Error] 
castErrors (Ann l facts) = downErrors ++ deadErrors
  where 
    downErrors           = [errorDownCast l t | TCast _ (DCST t) <- facts]
    deadErrors           = [errorDeadCast l   | TCast _ (DC _)   <- facts]

printAllAnns fs 
  = do putStrLn "********************** ALL ANNOTATIONS **********************"
       forM_ fs $ T.mapM printAnn 
    where 
       printAnn (Ann _ []) = return () 
       printAnn (Ann l fs) = putStrLn $ printf "At %s: %s" (ppshow l) (ppshow fs)


-------------------------------------------------------------------------------------------
typeCheck :: (Data r, Ord r, PP r, F.Reftable r, Substitutable r (Fact r), Free (Fact r)) 
          => V.Verbosity -> (NanoSSAR r) -> Either [Error] (NanoTypeR r)
-------------------------------------------------------------------------------------------
typeCheck verb pgm = fmap snd (execute verb pgm $ tcNano pgm)

-------------------------------------------------------------------------------
-- | TypeCheck Nano Program ---------------------------------------------------
-------------------------------------------------------------------------------
-- tcNano :: (Data r, Ord r, PP r, F.Reftable r, Substitutable r (Fact r), Free (Fact r)) 
--            => NanoSSAR r -> TCM r (AnnInfo r, NanoTypeR r)
-------------------------------------------------------------------------------
tcNano p@(Nano {code = Src fs}) 
  = do checkTypeDefs p
       (fs', _) <- tcInScope γ $ tcStmts γ fs
       m        <- concatMaps <$> getAllAnns
       θ        <- getSubst
       let p'    = p {code = (patchAnn m . apply θ) <$> Src fs'}
       whenLoud  $ (traceCodePP p' m θ)
       return (m, p')
    where
       γ         = initEnv p

patchAnn m (Ann l fs) = Ann l $ sortNub $ fs'' ++ fs' ++ fs 
  where
    fs'               = [f | f@(TypInst _ _) <- M.lookupDefault [] l m]
    fs''              = [f | f@(Overload (Just _)) <- M.lookupDefault [] l m]

initEnv pgm           = TCE (specs pgm) (sigs pgm) (tAnns pgm) emptyContext
traceCodePP p m s     = trace (render $ {- codePP p m s -} pp p) $ return ()
      
codePP (Nano {code = Src src}) anns sub 
  =   text "*************************** CODE ****************************************"
  $+$ pp src
  $+$ text "*************************** SUBSTITUTIONS *******************************"
  $+$ pp sub
  $+$ text "*************************** ANNOTATIONS **********************************"
  $+$ vcat (pp <$> {- annotCasts -} M.toList anns )
  $+$ text "*************************************************************************"

annotCasts anns = [ (l, f) | (l, fs) <- M.toList anns, f@(TCast _ _) <- fs]

-------------------------------------------------------------------------------
checkTypeDefs :: (Data r, Typeable r, F.Reftable r) => Nano (AnnSSA r) (RType r) -> TCM r ()
-------------------------------------------------------------------------------
checkTypeDefs pgm = reportAll $ grep
  where 
    ds        = sigs pgm 
    ts        = defs pgm
    reportAll = mapM_ report
    report t  = tcError $ errorUnboundType (srcPos t) t

    -- There should be no undefined type constructors
    grep :: [Id SourceSpan]        = everything (++) ([] `mkQ` g) ds
    g (TDef i) | not $ envMem i ts = [i]
    g _                            = [ ]
  
    -- TODO: Also add check for free top-level type variables, i.e. make sure 
    -- all type variables used are bound. Use something like:
    -- @everythingWithContext@



-------------------------------------------------------------------------------
-- | Typecheck Environment ----------------------------------------------------
-------------------------------------------------------------------------------

--   We define this alias as the "output" type for typechecking any entity
--   that can create or affect binders (e.g. @VarDecl@ or @Statement@)
--   @Nothing@ means if we definitely hit a "return" 
--   @Just γ'@ means environment extended with statement binders


data TCEnv r  = TCE { tce_env  :: Env (RType r)
                    , tce_spec :: Env (RType r) 
                    , tce_anns :: Env (RType r)
                    , tce_ctx  :: !IContext 
                    }

type TCEnvO r = Maybe (TCEnv r)

-- type TCEnv  r = Maybe (Env (RType r))
instance (PP r, F.Reftable r) => Substitutable r (TCEnv r) where 
  apply θ (TCE m sp an c) = TCE (apply θ m) (apply θ sp) (apply θ an) c 

instance (PP r, F.Reftable r) => PP (TCEnv r) where
  pp = ppTCEnv

ppTCEnv (TCE env spc an ctx) 
  =   text "******************** Environment ************************"
  $+$ pp env
  $+$ text "******************** Specifications *********************"
  $+$ pp spc 
  $+$ text "******************** Annotations ************************"
  $+$ pp an
  $+$ text "******************** Call Context ***********************"
  $+$ pp ctx


tcEnvPushSite i γ            = γ { tce_ctx = pushContext i    $ tce_ctx γ }

-- Since we assume the raw types should be the same among the various SSA 
-- variants, we should only be adding bindings for the non-SSA version of the 
-- variable, to be able to retrieve it correctly later on.
tcEnvAdds                   :: (IsLocated a, F.Reftable r) => [(Id a, RType r)] -> TCEnv r -> TCEnv r
tcEnvAdds     x γ            = γ { tce_env = envAdds (mapFst stripSSAId <$> x)        $ tce_env γ }

tcEnvAddReturn x t γ         = γ { tce_env = envAddReturn x t $ tce_env γ }
tcEnvMem x                   = envMem (stripSSAId x)      . tce_env 
tcEnvFindTy x                = envFindTy (stripSSAId x)   . tce_env
tcEnvFindReturn              = envFindReturn              . tce_env
tcEnvFindSpec x              = envFindTy (stripSSAId x)   . tce_anns
tcEnvFindTyOrDie l x         = fromMaybe ugh . tcEnvFindTy (stripSSAId x)  where ugh = die $ errorUnboundId (ann l) x

-------------------------------------------------------------------------------
-- | TypeCheck Scoped Block in Environment ------------------------------------
-------------------------------------------------------------------------------

tcInScope γ act = accumAnn annCheck act
  where
    annCheck    = catMaybes . map (validInst γ) . M.toList

-------------------------------------------------------------------------------
-- | TypeCheck Function -------------------------------------------------------
-------------------------------------------------------------------------------



-------------------------------------------------------------------------------
tcFun :: (Ord r, F.Reftable r, PP r) =>
  TCEnv r -> Statement (AnnSSA r) -> TCM r (Statement (AnnSSA r), Maybe (TCEnv r))
-------------------------------------------------------------------------------
tcFun γ (FunctionStmt l f xs body)
  = case tcEnvFindTy f γ of
      Nothing -> die $ errorMissingSpec (srcPos l) f
      Just ft -> do body' <- foldM (tcFun1 γ l f xs) body =<< tcFunTys l f xs ft
                    return   (FunctionStmt l f xs body', Just γ) 
tcFun _  s = die $ bug (srcPos s) $ "Calling tcFun not on FunctionStatement"

-------------------------------------------------------------------------------
tcFun1 ::
  (Ord r, F.Reftable r, PP r, IsLocated a1, IsLocated t1, IsLocated a, CallSite t) =>
  TCEnv r -> a -> t1 -> [Id a1] -> [Statement (AnnSSA r)] -> 
  (t, ([TVar], [RType r], RType r)) -> TCM r [Statement (AnnSSA r)]
-------------------------------------------------------------------------------
tcFun1 γ l f xs body (i, (αs,ts,t)) = tcInScope γ' $ tcFunBody γ' l f body t
  where 
    γ'                              = envAddFun f i αs xs ts t γ 

tcFunBody γ l f body t = liftM2 (,) (tcStmts γ body) getTDefs >>= ret
  where
    ret ((_, Just _), d) | not (isSubType d t tVoid) = tcError $ errorMissingReturn (srcPos l)
    ret ((b, _     ), _) | otherwise                 = return b

-------------------------------------------------------------------------------
envAddFun :: (F.Reftable r, IsLocated c, IsLocated a, CallSite b) =>
  a -> b -> [TVar] -> [Id c] -> [RType r] -> RType r -> TCEnv r -> TCEnv r
-------------------------------------------------------------------------------
envAddFun f i αs xs ts t = tcEnvAdds tyBinds 
                         . tcEnvAdds (varBinds xs ts) 
                         . tcEnvAddReturn f t
                         . tcEnvPushSite i 
  where  
    tyBinds                = [(tVarId α, tVar α) | α <- αs]
    varBinds               = zip
    
validInst γ (l, ts)
  = case [β | β <-  HS.toList $ free ts, not ((tVarId β) `tcEnvMem` γ)] of
      [] -> Nothing
      βs -> Just $ errorFreeTyVar l βs

-- | Strings ahead: HACK Alert
tVarId (TV a l) = Id l $ "TVAR$$" ++ F.symbolString a   

--------------------------------------------------------------------------------
tcSeq :: (TCEnv r -> a -> TCM r (b, TCEnvO r)) -> TCEnv r -> [a] -> TCM r ([b], TCEnvO r)
--------------------------------------------------------------------------------
tcSeq f             = go []
  where
    go acc γ []     = return (reverse acc, Just γ)
    go acc γ (x:xs) = do (y, γo) <- f γ x
                         case γo of
                           Nothing -> return (reverse (y:acc), Nothing) 
                           Just γ' -> go (y:acc) γ' xs

-- tcSeq f             = foldM step . Just 
--   where 
--     step Nothing _  = return Nothing
--     step (Just γ) x = f γ x

--------------------------------------------------------------------------------
tcStmts :: (Ord r, PP r, F.Reftable r, Substitutable r (Fact r), Free (Fact r)) =>
            TCEnv r -> [Statement (AnnSSA r)] -> TCM r ([Statement (AnnSSA r)], TCEnvO r)
--------------------------------------------------------------------------------
tcStmts γ stmts 
  = do γ' <- addStatementFunBinds γ stmts
       tcSeq tcStmt γ' stmts

addStatementFunBinds γ stmts 
  = do fts   <- forM fns $ \f -> (f,) <$> getDefType f
       return $ tcEnvAdds fts γ
    where
       fs  = concatMap getFunctionStatements stmts
       fns = [f | FunctionStmt _ f _ _ <- fs]

-------------------------------------------------------------------------------
tcStmt  :: (Ord r, PP r, F.Reftable r, Substitutable r (Fact r), Free (Fact r)) =>
            TCEnv r -> Statement (AnnSSA r) -> TCM r (Statement (AnnSSA r), TCEnvO r)
-------------------------------------------------------------------------------
-- skip
tcStmt γ s@(EmptyStmt _) 
  = return (s, Just γ)

-- x = e
tcStmt γ (ExprStmt l1 (AssignExpr l2 OpAssign (LVar lx x) e))   
  = do (e', g) <- tcAsgn γ (Id lx x) e
       return   (ExprStmt l1 (AssignExpr l2 OpAssign (LVar lx x) e'), g)

-- e1.fld = e2
tcStmt γ (ExprStmt l (AssignExpr l2 OpAssign (LDot l1 e1 fld) e2))
  = do (e1', tfld) <- tcPropRead getProp γ l e1 fld
       (e2', t2)   <- tcExpr γ $ e2                    
       e2''        <- castM  ξ e2' t2 tfld
       return         (ExprStmt l (AssignExpr l2 OpAssign (LDot l1 e1' fld) e2''), Just γ)
    where
       ξ            = tce_ctx γ      

-- The type of @e2@ should be a a subtype of the type of the @f@ field of @e1@. 
-- e3.x = e2
-- The type of @e2@ should be assignable (a subtype of) the type of @e3.x@.
-- RJ: TODO FIELD tcStmt γ (ExprStmt l1 (AssignExpr l2 OpAssign (LDot l3 e3 x) e2))
-- RJ: TODO FIELD    = do  θ  <- getTDefs
-- RJ: TODO FIELD          t2 <- tcExpr' γ e2 
-- RJ: TODO FIELD          t3 <- tcExpr' γ e3
-- RJ: TODO FIELD          tx <- safeGetProp (tce_ctx γ) x t3
-- RJ: TODO FIELD          case tx of 
-- RJ: TODO FIELD            -- NOTE: Atm assignment to non existing binding has no effect!
-- RJ: TODO FIELD            TApp TUndef _ _ -> return $ Just γ
-- RJ: TODO FIELD            _ ->  if isSubType θ t2 tx 
-- RJ: TODO FIELD                    then return  $ Just γ
-- RJ: TODO FIELD                    else tcError $ errorTypeAssign (srcPos l2) t2 tx


-- e3[i] = e2
-- RJ: TODO ARRAY tcStmt γ (ExprStmt _ (AssignExpr l2 OpAssign (LBracket _ e3 (IntLit _ _)) e2))
-- RJ: TODO ARRAY   = do  t2 <- tcExpr' γ e2 
-- RJ: TODO ARRAY         _ {- t3 -}  <- tcExpr' γ e3
-- RJ: TODO ARRAY         ti <- throw $ bug (srcPos l2) "UNIMPLEMENTED: tc: e[e] = e" --safeGetIdx i t3
-- RJ: TODO ARRAY         θ  <- unifyTypeM (srcPos l2) "DotRef" e2 t2 ti 
-- RJ: TODO ARRAY         -- Once we've figured out what the type of the array should be,
-- RJ: TODO ARRAY         -- update the output environment.
-- RJ: TODO ARRAY         setSubst θ
-- RJ: TODO ARRAY         return $ Just $ apply θ γ 


-- e
tcStmt γ (ExprStmt l e)   
  = do (e', _) <- tcExpr γ e 
       return (ExprStmt l e', Just γ) 

-- s1;s2;...;sn
tcStmt γ (BlockStmt l stmts) 
  = do (stmts', z) <- tcStmts γ stmts
       return (BlockStmt l stmts', z)

-- if b { s1 }
tcStmt γ (IfSingleStmt l b s)
  = tcStmt γ (IfStmt l b s (EmptyStmt l))

-- if b { s1 } else { s2 }
tcStmt γ (IfStmt l e s1 s2)
  = do (e', t)   <- tcExpr γ e 
       unifyTypeM (srcPos l) "If condition" e t tBool
       (s1', γ1) <- tcStmt γ s1
       (s2', γ2) <- tcStmt γ s2
       z         <- envJoin l γ γ1 γ2
       return       (IfStmt l e' s1' s2', z)

-- while c { b } ; exit environment is entry as may skip. SSA adds phi-asgn prior to while.
tcStmt γ (WhileStmt l c b) 
  = do (c', t)   <- tcExpr γ c
       unifyTypeM (srcPos l) "While condition" c t tBool
       (b', _)   <- tcStmt γ' b
       return       (WhileStmt l c' b', Just γ)  
    where 
       xts'       = [(mkNextId x, tcEnvFindTyOrDie l x γ) | x <- phiVarsAnnot l]
       γ'         = tcEnvAdds xts' γ

-- var x1 [ = e1 ]; ... ; var xn [= en];
tcStmt γ (VarDeclStmt l ds)
  = do (ds', z) <- tcSeq tcVarDecl γ ds
       return      (VarDeclStmt l ds', z)

-- return e 
tcStmt γ (ReturnStmt l eo) 
  = do  (eo', t)    <- case eo of 
                         Nothing -> return (Nothing, tVoid)
                         Just e  -> mapFst Just <$>  tcExpr γ e
        let rt       = tcEnvFindReturn γ 
        θ           <- unifyTypeM (srcPos l) "Return" eo t rt
        -- Apply the substitution
        let (rt',t') = mapPair (apply θ) (rt,t)
        -- Subtype the arguments against the formals and cast using subtyping result
        eo''        <- case eo' of
                        Nothing -> return Nothing
                        Just e' -> Just <$> castM (tce_ctx γ) e' t' rt'
        return (ReturnStmt l eo'', Nothing)

tcStmt γ s@(FunctionStmt _ _ _ _)
  = tcFun γ s

-- OTHER (Not handled)
tcStmt _ s 
  = convertError "tcStmt" s


---------------------------------------------------------------------------------------
tcVarDecl :: (Ord r, PP r, F.Reftable r) 
          => TCEnv r -> VarDecl (AnnSSA r) -> TCM r (VarDecl (AnnSSA r), TCEnvO r)
---------------------------------------------------------------------------------------
tcVarDecl γ v@(VarDecl l x (Just e)) 
  = do (e', g) <- tcAsgn γ x e
       return (VarDecl l x (Just e'), g)

tcVarDecl γ v@(VarDecl _ _ Nothing)  
  = return   (v, Just γ)

varDeclAnnot v = listToMaybe [ t | TAnnot t <- ann_fact $ getAnnotation v]

-------------------------------------------------------------------------------
tcAsgn :: (PP r, Ord r, F.Reftable r) => 
  TCEnv r -> Id (AnnSSA r) -> ExprSSAR r -> TCM r (ExprSSAR r, TCEnvO r)
-------------------------------------------------------------------------------
tcAsgn γ x e
  = do (e' , t) <- tcExprT γ e rhsT
       return      (e', Just $ tcEnvAdds [(x, t)] γ)
    where
       rhsT      = maybe (tcEnvFindTy x γ) Just (tcEnvFindSpec x γ)


-------------------------------------------------------------------------------
tcExprT :: (Ord r, PP r, F.Reftable r)
       => TCEnv r -> ExprSSAR r -> Maybe (RType r) -> TCM r (ExprSSAR r, RType r)
-------------------------------------------------------------------------------
tcExprT γ e to 
  = do (e', t)    <- tcExpr γ e
       (e'', te)  <- case to of
                       Nothing -> return (e', t)
                       Just ta -> (,ta) <$> castM (tce_ctx γ) e t ta
                    {-Just ta -> checkAnnotation "tcExprT" e t ta-}
       return     (e'', te)

----------------------------------------------------------------------------------------------
tcExpr :: (Ord r, PP r, F.Reftable r) => TCEnv r -> ExprSSAR r -> TCM r (ExprSSAR r, RType r)
----------------------------------------------------------------------------------------------
tcExpr _ e@(IntLit _ _)
  = return (e, tInt)

tcExpr _ e@(BoolLit _ _)
  = return (e, tBool)

tcExpr _ e@(StringLit _ _)
  = return (e, tString)

tcExpr _ e@(NullLit _)
  = return (e, tNull)

tcExpr γ e@(VarRef l x)
  = return (e, tcEnvFindTyOrDie l x γ) 
  
  -- case tcEnvFindTy x γ of 
  --     -- Nothing -> die $ errorUnboundId (ann l) x
  --     Nothing -> die $ errorUnboundIdEnv (ann l) x (tce_env γ)
  --     Just z  -> return $ (e, z)

tcExpr γ e@(PrefixExpr _ _ _)
  = tcCall γ e 

tcExpr γ e@(InfixExpr _ _ _ _)
  = tcCall γ e 

tcExpr γ e@(CallExpr _ _ _)
  = tcCall γ e 

tcExpr γ e@(ArrayLit _ _)
  = tcCall γ e 

tcExpr γ (ObjectLit l bs) 
  = do let (ps, es)  = unzip bs
       ets          <- mapM (tcExpr γ) es
       let (es', ts) = unzip ets
       let bts       = zipWith B (F.symbol <$> ps) ts
       return (ObjectLit l (zip ps es'), TObj bts fTop)

tcExpr γ (Cast l@(Ann loc fs) e)
  = do (e', t) <- tcExpr γ e
       case e' of
         Cast (Ann _ fs') e'' -> return (Cast (Ann loc (fs ++ fs')) e'', t)
         _                    -> return (Cast l e', t)

-- e.f
tcExpr γ e1@(DotRef l e fld) 
  = do (e', t) <- tcPropRead getProp γ l e (unId fld)
       return     (DotRef l e' fld, t)
        
-- e["f"]
tcExpr γ (BracketRef l e fld@(StringLit _ s)) 
  = do (e', t) <- tcPropRead getProp γ l e s
       return     (BracketRef l e' fld, t)
 
-- e1[e2]
tcExpr γ e@(BracketRef _ _ _) 
  = tcCall γ e

-- e1[e2] = e3
tcExpr γ e@(AssignExpr _ OpAssign (LBracket _ _ _) _)
  = tcCall γ e


tcExpr _ e 
  = convertError "tcExpr" e

---------------------------------------------------------------------------------------
tcCall :: (Ord r, F.Reftable r, PP r) => TCEnv r -> ExprSSAR r -> TCM r (ExprSSAR r, RType r)
---------------------------------------------------------------------------------------

-- | `o e`
tcCall γ ex@(PrefixExpr l o e)        
  = do z                      <- tcCallMatch γ l o [e] (prefixOpTy o $ tce_env γ) 
       case z of
         Just ([e'], t)       -> return (PrefixExpr l o e', t)
         Nothing              -> deadCast (srcPos l) γ ex 

-- | `e1 o e2`
tcCall γ ex@(InfixExpr l o e1 e2)        
  = do z                      <- tcCallMatch γ l o [e1, e2] (infixOpTy o $ tce_env γ) 
       case z of
         Just ([e1', e2'], t) -> return (InfixExpr l o e1' e2', t)
         Nothing              -> deadCast (srcPos l) γ ex 
         
-- | `e(e1,...,en)`
tcCall γ ex@(CallExpr l e es)
  = do (e', ft0)              <- tcExpr γ e
       z                      <- tcCallMatch γ l e es ft0
       case z of
         Just (es', t)        -> return (CallExpr l e' es', t)
         Nothing              -> deadCast (srcPos l) γ ex

-- | `e1[e2]`
tcCall γ ex@(BracketRef l e1 e2)
  = do z                      <- tcCallMatch γ l BIBracketRef [e1, e2] $ builtinOpTy l BIBracketRef $ tce_env γ 
       case z of
         Just ([e1', e2'], t) -> return (BracketRef l e1' e2', t)
         Nothing              -> tcError $ errorPropRead (srcPos l) e1 e2 
                                 -- deadCast (srcPos l) γ ex 
   

-- | `e1[e2] = e3`
tcCall γ ex@(AssignExpr l OpAssign (LBracket l1 e1 e2) e3)
  = do z                           <- tcCallMatch γ l BIBracketAssign [e1,e2,e3] $ builtinOpTy l BIBracketAssign $ tce_env γ
       case z of
         Just ([e1', e2', e3'], t) -> return (AssignExpr l OpAssign (LBracket l1 e1' e2') e3', t)
         Nothing                   -> tcError $ errorBracketAssign (srcPos l) ex 


tcCall γ ex@(ArrayLit l es) 
  = do z                           <- tcCallMatch γ l BIArrayLit es $ arrayLitTy l (length es) $ tce_env γ
       case z of
         Just (es', t)             -> return (ArrayLit l es', t)
         Nothing                   -> tcError $ errorArrayLit (srcPos l) ex


tcCall γ e
  = die $ bug (srcPos e) $ "tcCall: cannot handle" ++ ppshow e        


---------------------------------------------------------------------------------------
tcCallMatch γ l fn es ft0
  = do -- Typecheck arguments
       (es', ts)     <- unzip <$> mapM (tcExpr γ) es
       case calleeType l ts ft0 of 
        -- Try to match it with a non-generic type
        Just t -> call es' ts t
        -- If this fails try to instantiate possible generic types in the
        -- function signature.
        Nothing ->
          do  mType <- resolveOverload γ l fn es' ts ft0
              addAnn (srcPos l) (Overload mType)
              maybe (return Nothing) (call es' ts) mType
    where
      call es' ts t = fmap Just $ tcCallCase γ l fn es' ts t


---------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------
resolveOverload γ l fn es ts ft =
  shd <$> filterM (\t -> valid <$> tcCallCaseTry γ l fn es ts t) eqLenSigs
  where
    valid (Right (Su m) )   | not (M.null m) 
                            = True
    valid _                 = False
    shd []                  = Nothing
    shd xs                  = Just $ head xs
    isRight (Right _)       = True
    isRight (Left _ )       = False
    eqLenSigs               = mkFun <$> L.filter (eqLen es . snd3) sigs
    sigs                    = catMaybes (bkFun <$> bkAnd ft)

eqLen xs ys       = length xs == length ys 

sameLengthArgs :: [t] -> RType r -> Bool
sameLengthArgs args f = FL.or (bkFun f >>= \(_,bs,_) -> return $ eqLen args bs)
      

tcCallCaseTry γ l fn es' ts ft
  = do let ξ          = tce_ctx γ
       -- Generate fresh type parameters
       (_,ibs,ot)    <- instantiate l ξ fn ft
       let its        = b_type <$> ibs
       θ             <- getSubst 
       defs          <- getTDefs
       --HACK - erase latest annotation on l
       remAnn         $ (srcPos l)
       return         $ unifys (ann l) defs θ ts its

tcCallCase γ l fn es' ts ft 
  = do let ξ          = tce_ctx γ
       -- Generate fresh type parameters
       (_,ibs,ot)    <- instantiate l ξ fn ft
       let its        = b_type <$> ibs
       -- Unify with formal parameter types
       θ             <- unifyTypesM (srcPos l) "tcCall" ts its
       -- Apply substitution
       let (ts',its') = mapPair (apply θ) (ts, its)
       -- Subtype the arguments against the formals and up/down cast if needed 
       es''          <- zipWith3M (castM ξ) es' ts' its'
       return           (es'', apply θ ot)

instantiate l ξ fn ft 
  = do let (αs, t) = bkAll ft
       t'         <- freshTyArgs (srcPos l) ξ αs t 
       maybe err return $ bkFun t'
    where
       err = tcError   $ errorNonFunction (ann l) fn ft


deadCast l γ e 
  = do t'    <- freshTyArgs (srcPos l) ξ [α] t 
       e'    <- addDeadCast ξ e t'
       return   (e', t') 
    where 
      (α, t)  = undefType l γ
      ξ       = tce_ctx γ

undefType l γ 
  = case bkAll $ ut of
      ([α], t) -> (α, t)
      _        -> die $ bug (srcPos l) $ "Malformed type --" ++ ppshow ut ++ "-- for BIUndefined in prelude.js"
    where 
      ut       = builtinOpTy l BIUndefined $ tce_env γ

-- ----------------------------------------------------------------------------------
-- tcArrayLit :: (Ord r, PP r, F.Reftable r) =>
--   TCEnv r -> ExprSSAR r -> Maybe (RType r) -> TCM r (ExprSSAR r, RType r)
-- ----------------------------------------------------------------------------------
-- tcArrayLit γ (Just t@(TArr ta _)) (ArrayLit l es) = do 
--     let tao       = Just ta
--     ets          <- mapM (\e -> tcExprT γ e tao) es
--     let (es', ts) = unzip ets
--     checkElts ta es' ts
--     return (ArrayLit l es', t)
--   where
--     checkElts = zipWithM_ . (checkAnnotation "tcArrayLit")
-- 
-- tcArrayLit _ Nothing (ArrayLit l _)  = 
--   die $ bug (srcPos l) "Array literals need type annotations at the moment to typecheck in TC."
-- 
-- tcArrayLit _ (Just _) (ArrayLit l _) = 
--   die $ bug (srcPos l) "Type annotation for array literal needs to be of Array type."
-- 
-- tcArrayLit _ _ e = 
--   die $ bug (srcPos $ getAnnotation e) "BUG: Only support tcArray for array literals with type annotation"
             
----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
tcPropRead getter γ l e fld
  = do (e', te)   <- tcExpr γ e
       tdefs      <- getTDefs 
       case getter l (tce_env γ) tdefs fld te of
         Nothing        -> tcError $  errorPropRead (srcPos l) e fld
         Just (te', tf) -> (, tf) <$> castM (tce_ctx γ) e' te te'

----------------------------------------------------------------------------------
envJoin :: (Ord r, F.Reftable r, PP r) =>
  (AnnSSA r) -> TCEnv r -> TCEnvO r -> TCEnvO r -> TCM r (TCEnvO r)
----------------------------------------------------------------------------------
envJoin _ _ Nothing x           = return x
envJoin _ _ x Nothing           = return x
envJoin l γ (Just γ1) (Just γ2) = envJoin' l γ γ1 γ2 

envJoin' l γ γ1 γ2
  = do let xs = phiVarsAnnot l -- concat [x | PhiVar x <- ann_fact l]
       ts    <- mapM (getPhiType l γ1 γ2) xs
       -- NOTE: Instantiation on arrays could have happened in the branches and
       -- then lost if the variables are no Phi. So replay the application of
       -- the instantiations on γ
       θ     <- getSubst
       return $ Just $ tcEnvAdds (zip xs ts) (apply θ γ)
  

----------------------------------------------------------------------------------
getPhiType ::  (Ord r, F.Reftable r, PP r) => 
  Annot b SourceSpan -> TCEnv r -> TCEnv r -> Id SourceSpan-> TCM r (RType r)
----------------------------------------------------------------------------------
getPhiType l γ1 γ2 x =
  case (tcEnvFindTy x γ1, tcEnvFindTy x γ2) of
    (Just t1, Just t2) -> do  env <- getTDefs
                              return $ fst4 $ compareTs env t1 t2
    (_      , _      ) -> if forceCheck x γ1 && forceCheck x γ2 
                            then tcError $ bug loc "Oh no, the HashMap GREMLIN is back...1"
                            else tcError $ bugUnboundPhiVar loc x
                          where loc = srcPos $ ann l

forceCheck x γ 
  = elem x $ fst <$> envToList (tce_env γ)

