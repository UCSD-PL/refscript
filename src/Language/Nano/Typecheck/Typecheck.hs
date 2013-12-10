{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Nano.Typecheck.Typecheck (verifyFile, typeCheck) where 

import           Control.Exception                  (throw)
import           Control.Applicative                ((<$>))
import           Control.Monad                

import qualified Data.HashSet                       as HS 
import qualified Data.HashMap.Strict                as M 
import qualified Data.Traversable                   as T
import           Data.Monoid
import           Data.Maybe                         (catMaybes, isJust, fromJust, fromMaybe, listToMaybe)
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
import           Language.Nano.SSA.SSA

import           Language.Fixpoint.Errors
import qualified Language.Fixpoint.Types            as F
import           Language.Fixpoint.Misc             as FM 
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Parser        (SourceSpan (..))
import           Debug.Trace                        hiding (traceShow)

import qualified System.Console.CmdArgs.Verbosity as V


--------------------------------------------------------------------------------
-- | Top-level Verifier 

--------------------------------------------------------------------------------
verifyFile :: FilePath -> IO (UAnnSol a, F.FixResult Error)
--------------------------------------------------------------------------------
verifyFile f = do 
  nano    <- parseNanoFromFile f
  V.whenLoud $ donePhase FM.Loud "Parse"
  V.whenLoud $ putStrLn . render . pp $ nano
  let nanoSsa = ssaTransform nano
  V.whenLoud $ donePhase FM.Loud "SSA Transform"
  V.whenLoud $ putStrLn . render . pp $ nanoSsa
  verb      <- V.getVerbosity
  let annp   = execute verb nanoSsa $ tcNano nanoSsa
  r         <- either unsafe safe annp 
  V.whenLoud $ donePhase FM.Loud "Typechecking"
  return (NoAnn, r)

unsafe errs = do putStrLn "\n\n\nErrors Found!\n\n" 
                 forM_ errs (putStrLn . ppshow) 
                 return $ F.Unsafe errs

safe (_, Nano {code = Src fs})
  = do V.whenLoud $ forM_ fs $ T.mapM printAnn
       nfc       <- noFailCasts <$> getOpts
       return     $ F.Safe `mappend` failCasts nfc fs 


failCasts True  _  = F.Safe
failCasts False fs = applyNonNull F.Safe F.Unsafe $ concatMap castErrors $ getCasts fs 

getCasts       :: (Data r, Typeable r) => [Statement (AnnType r)] -> [(AnnType r)]
getCasts stmts = everything (++) ([] `mkQ` f) stmts
  where 
    f :: Expression (AnnType r) -> [(AnnType r)]
    f (Cast a _) = [a]
    f _          = [] 

-- castErrors :: (F.Reftable r) => AnnType r -> [Error] 
castErrors (Ann l facts) = downErrors ++ deadErrors
  where 
    downErrors           = [errorDownCast l t | TCast _ (DCST t) <- facts]
    deadErrors           = [errorDeadCast l   | TCast _ (DC _)   <- facts]

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
tcNano :: (Data r, Ord r, PP r, F.Reftable r, Substitutable r (Fact r), Free (Fact r)) 
           => NanoSSAR r -> TCM r (AnnInfo r, NanoTypeR r)
-------------------------------------------------------------------------------
tcNano p@(Nano {code = Src fs}) 
  = do checkTypeDefs p
       (fs', _) <- tcStmts (initEnv p) fs
       m        <- concatMaps <$> getAllAnns
       θ        <- getSubst
       let p'    = p {code = (patchAnn m . apply θ) <$> Src fs'}
       whenLoud  $ (traceCodePP p' m θ)
       return (m, p')

patchAnn m (Ann l fs) = Ann l $ sortNub $ fs' ++ fs 
  where
    fs'               = [f | f@(TypInst _ _) <- M.lookupDefault [] l m]



initEnv pgm           = TCE (specs pgm) emptyContext
traceCodePP p m s     = trace (render $ codePP p m s) $ return ()
      
codePP (Nano {code = Src src}) anns sub 
  =   text "******************************** CODE ***********************************"
  $+$ pp src
  $+$ text "*************************** SUBSTITUTIONS *******************************"
  $+$ pp sub
  $+$ text "******************************** CASTS **********************************"
  $+$ vcat (pp <$> annotCasts anns )
  --  $+$ vcat ((\(e,t) -> (pp $ ann $ getAnnotation e) <+> pp (e,t)) <$> cst)
  $+$ text "*************************************************************************"

annotCasts anns = [ (l, f) | (l, fs) <- M.toList anns, f@(TCast _ _) <- fs]

-------------------------------------------------------------------------------
checkTypeDefs :: (Data r, Typeable r, F.Reftable r) => Nano (AnnSSA r) (RType r) -> TCM r ()
-------------------------------------------------------------------------------
checkTypeDefs pgm = reportAll $ grep
  where 
    ds        = defs pgm 
    ts        = tDefs pgm
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


data TCEnv r  = TCE { tce_env :: Env (RType r), tce_ctx :: !IContext }

type TCEnvO r = Maybe (TCEnv r)

-- type TCEnv  r = Maybe (Env (RType r))
instance (PP r, F.Reftable r) => Substitutable r (TCEnv r) where 
  apply θ (TCE m c) = TCE (apply θ m) c

tcEnvPushSite i (TCE m c)    = TCE m (pushContext i c)
tcEnvAdds x      (TCE m c)   = TCE (envAdds x m) c
tcEnvAddReturn x t (TCE m c) = TCE (envAddReturn x t m) c
tcEnvMem x                   = envMem x      . tce_env 
tcEnvFindTy x                = envFindTy x   . tce_env
tcEnvFindReturn              = envFindReturn . tce_env
-- tcEnvAdds l x TCEmpty   = die $ bug l $ "Cannot add to TCEmpty"
-- apply θ TCEmpty   = TCEmpty

-------------------------------------------------------------------------------
-- | TypeCheck Function -------------------------------------------------------
-------------------------------------------------------------------------------

-- tcFun    :: (F.Reftable r) => Env (RType r) -> FunctionStatement (AnnSSA r) -> TCM r (TCEnv r)
tcFun γ (FunctionStmt l f xs body)
  = do ft    <- getDefType f
       let γ' = tcEnvAdds [(f, ft)] γ
       body' <- foldM (tcFun1 γ' l f xs) body $ funTys l f xs ft
       return   (FunctionStmt l f xs body', Just γ') 

tcFun _  s = die $ bug (srcPos s) $ "Calling tcFun not on FunctionStatement"

tcFun1 γ l f xs body (i, (αs,ts,t)) = accumAnn annCheck $ tcFunBody γ' l f body t
  where 
    γ'                              = envAddFun l f i αs xs ts t γ 
    annCheck                        = catMaybes . map (validInst γ') . M.toList

tcFunBody γ l f body t
  = do (body', q)     <- tcStmts γ body
       when (isJust q) $ void $ unifyTypeM (srcPos l) "Missing return" f tVoid t
       return body'

envAddFun _ f i αs xs ts t = tcEnvAdds tyBinds 
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
tcStmts = tcSeq tcStmt

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
  = do (e', _) <- tcExpr' γ e 
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
  = do (e', t)   <- tcExpr' γ e 
       unifyTypeM (srcPos l) "If condition" e t tBool
       (s1', γ1) <- tcStmt γ s1
       (s2', γ2) <- tcStmt γ s2
       z         <- envJoin l γ γ1 γ2
       return       (IfStmt l e' s1' s2', z)

tcStmt γ (WhileStmt l c b) = do
    let phis   = [φ | LoopPhiVar φs <- ann_fact l, φ <- φs]
    let phiTs  = fromJust <$> (`tcEnvFindTy` γ) <$> (fst3 <$> phis)
    let γ'     = tcEnvAdds (zip (snd3 <$> phis) phiTs) γ
    (c', t)   <- tcExpr' γ' c
    unifyTypeM (srcPos l) "While condition" c t tBool
    (b', γ'') <- tcStmt γ' b
    return       (WhileStmt l c' b', γ'')

-- var x1 [ = e1 ]; ... ; var xn [= en];
tcStmt γ (VarDeclStmt l ds)
  = do (ds', z) <- tcSeq tcVarDecl γ ds
       return      (VarDeclStmt l ds', z)

-- return e 
tcStmt γ (ReturnStmt l eo) 
  = do  (eo', t)    <- case eo of 
                         Nothing -> return (Nothing, tVoid)
                         Just e  -> mapFst Just <$>  tcExpr' γ e
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
tcVarDecl γ v@(VarDecl l x (Just e)) = do
    (e', t) <- tcExprT γ e (varDeclAnnot v)
    return   (VarDecl l x (Just e'), Just $ tcEnvAdds [(x, t)] γ)

tcVarDecl γ v@(VarDecl _ _ Nothing)  
  = return   (v, Just γ)

varDeclAnnot v = listToMaybe [ t | TAnnot t <- ann_fact $ getAnnotation v]

------------------------------------------------------------------------------------
tcAsgn :: (PP r, Ord r, F.Reftable r) => 
  TCEnv r -> Id (AnnSSA r) -> ExprSSAR r -> TCM r (ExprSSAR r, TCEnvO r)
------------------------------------------------------------------------------------
tcAsgn γ x e = do 
  (e', t) <- tcExpr γ e 
  return     (e', Just $ tcEnvAdds [(x, t)] γ)

-------------------------------------------------------------------------------
tcExprT :: (Ord r, PP r, F.Reftable r)
       => TCEnv r -> ExprSSAR r -> Maybe (RType r) -> TCM r (ExprSSAR r, RType r)
-------------------------------------------------------------------------------
tcExprT γ e@(ArrayLit _ _) ct = tcArray γ ct e
tcExprT γ e         (Just ta) = do (e', t) <- tcExpr γ e 
                                   checkAnnotation "tcExprAnnot" ta e t 
                                   return (e', ta)
tcExprT γ e           Nothing = tcExpr γ e

-- UGH. STATE!!!! NO!!!!
tcExpr' γ e                   = {- setExpr (Just e) >> -} tcExpr γ e

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
  = case tcEnvFindTy x γ of 
      Nothing -> logError (errorUnboundIdEnv (ann l) x (tce_env γ)) (e, tErr)
      Just z  -> return $ (e, z)

tcExpr γ e@(PrefixExpr _ _ _)
  = tcCall γ e 

tcExpr γ e@(InfixExpr _ _ _ _)
  = tcCall γ e 

tcExpr γ e@(CallExpr _ _ _)
  = tcCall γ e 

tcExpr γ (ObjectLit l bs) 
  = do let (ps, es)  = unzip bs
       ets          <- mapM (tcExpr' γ) es
       let (es', ts) = unzip ets
       let bts       = zipWith B (F.symbol <$> ps) ts -- <$> mapM (tcExpr' γ) es
       return (ObjectLit l (zip ps es'), TObj bts F.top)

tcExpr γ (Cast l@(Ann loc fs) e)
  = do (e', t) <- tcExpr γ e
       case e' of
         Cast (Ann _ fs') e'' -> return (Cast (Ann loc (fs ++ fs')) e'', t)
         _                    -> return (Cast l e', t)


-- x.f =def= x["f"]
-- RJ: TODO FIELD tcExpr γ (DotRef _ e s) = tcExpr' γ e >>= safeGetProp (tce_ctx γ) (unId s) 

-- x["f"]
-- RJ: TODO FIELD tcExpr γ (BracketRef _ e (StringLit _ s)) = tcExpr' γ e >>= safeGetProp (tce_ctx γ) s

-- x[i] 
-- RJ: TODO FIELD tcExpr γ (BracketRef _ e (IntLit _ _)) = tcExpr' γ e >>= indexType (tce_ctx γ) 

-- x[e]
-- RJ: TODO FIELD tcExpr γ e@(BracketRef l e1 e2) = do
-- RJ: TODO FIELD     t1 <- tcExpr' γ e1
-- RJ: TODO FIELD     t2 <- tcExpr' γ e2
-- RJ: TODO FIELD     case t1 of 
-- RJ: TODO FIELD       -- NOTE: Only support dynamic access of array with index of integer type.
-- RJ: TODO FIELD       TArr t _  -> unifyTypeM (srcPos l) "BracketRef" e t2 tInt >> return t
-- RJ: TODO FIELD       t         -> errorstar $ "Unimplemented: BracketRef of non-array expression of type " ++ ppshow t

tcExpr _ e 
  = convertError "tcExpr" e


---------------------------------------------------------------------------------------
tcCall :: (Ord r, F.Reftable r, PP r) => TCEnv r -> ExprSSAR r -> TCM r (ExprSSAR r, RType r)
---------------------------------------------------------------------------------------

-- tcCall γ (PrefixExpr l o e)
--   = do ([e'], z) <- tcCall γ l o [e] (prefixOpTy o $ tce_env γ)
--        return (PrefixExpr l o e', z)
-- 
-- tcCall γ (InfixExpr l o e1 e2)        
--   = do ([e1', e2'], z) <- tcCall γ l o [e1, e2] (infixOpTy o $ tce_env γ)
--        return (InfixExpr l o e1' e2', z)
--
-- tcCall γ (CallExpr l e es)
--   = do (e', z)   <- tcExpr' γ e 
--        (es', z') <- tcCall γ l e es z
--        return       (CallExpr l e' es', z')

tcCall γ ex@(PrefixExpr l o e)        
  = do z                      <- tcCallMatch γ l o [e] (prefixOpTy o $ tce_env γ) 
       case z of
         Just ([e'], t)       -> return (PrefixExpr l o e', t)
         Nothing              -> deadCast (srcPos l) γ ex 

tcCall γ ex@(InfixExpr l o e1 e2)        
  = do z                      <- tcCallMatch γ l o [e1, e2] (infixOpTy o $ tce_env γ) 
       case z of
         Just ([e1', e2'], t) -> return (InfixExpr l o e1' e2', t)
         Nothing              -> deadCast (srcPos l) γ ex 

tcCall γ ex@(CallExpr l e es)
  = do (e', ft0)              <- tcExpr' γ e
       z                      <- tcCallMatch γ l e es ft0
       case z of
         Just (es', t)        -> return (CallExpr l e' es', t)
         Nothing              -> deadCast (srcPos l) γ ex

tcCall γ e
  = die $ bug (srcPos e) $ "tcCall: cannot handle" ++ ppshow e        

tcCallMatch γ l fn es ft0
  = do -- Typecheck arguments
       (es', ts)     <- unzip <$> mapM (tcExpr' γ) es
       -- Extract callee type (if intersection: match with args)
       maybe (return Nothing) (fmap Just . tcCallCase γ l fn es' ts) $ calleeType l ts ft0

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
       err = die   $ errorNonFunction (ann l) fn ft


deadCast l γ e 
  = do t'    <- freshTyArgs (srcPos l) ξ [α] t 
       e'    <- addDeadCast ξ e t'
       return   (e', t') 
    where 
      (α, t)  = undefType l γ
      ξ       = tce_ctx γ

undefType l γ  = case bkAll $ builtinOpTy l Undefined $ tce_env γ of
                   ([α], t) -> (α, t)
                   _        -> die $ bug (srcPos l) "Malformed type for Undefined in prelude.js"

----------------------------------------------------------------------------------
tcArray :: (Ord r, PP r, F.Reftable r) =>
  TCEnv r -> Maybe (RType r) -> ExprSSAR r -> TCM r (ExprSSAR r, RType r)
----------------------------------------------------------------------------------
tcArray γ (Just t@(TArr ta _)) (ArrayLit l es) = do 
    let tao       = Just ta
    ets          <- mapM (\e -> tcExprT γ e tao) es
    let (es', ts) = unzip ets
    checkElts ta es' ts
    return (ArrayLit l es', t)
  where
    checkElts = zipWithM_ . (checkAnnotation "tcArray")

tcArray _ Nothing (ArrayLit l _)  = 
  die $ bug (srcPos l) "Array literals need type annotations at the moment to typecheck in TC."

tcArray _ (Just _) (ArrayLit l _) = 
  die $ bug (srcPos l) "Type annotation for array literal needs to be of Array type."

tcArray _ _ e = 
  die $ bug (srcPos $ getAnnotation e) "BUG: Only support tcArray for array literals with type annotation"
              
----------------------------------------------------------------------------------
envJoin :: (Ord r, F.Reftable r, PP r) =>
  (AnnSSA r) -> TCEnv r -> TCEnvO r -> TCEnvO r -> TCM r (TCEnvO r)
----------------------------------------------------------------------------------
envJoin _ _ Nothing x           = return x
envJoin _ _ x Nothing           = return x
envJoin l γ (Just γ1) (Just γ2) = envJoin' l γ γ1 γ2 

envJoin' l γ γ1 γ2
  = do let xs = concat [x | PhiVar x <- ann_fact l]
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

