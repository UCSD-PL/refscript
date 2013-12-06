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
--verifyFile f = tc =<< parseNanoFromFile f
--  where 
--   tc pgm    = either unsafe safe . execute pgm . tcNano . ssaTransform $ pgm 

-- | Debug mode
verifyFile f = do 
  nano    <- parseNanoFromFile f
  V.whenLoud $ donePhase FM.Loud "Parse"
  putStrLn . render . pp $ nano
  let nanoSsa = ssaTransform nano
  V.whenLoud $ donePhase FM.Loud "SSA Transform"
  V.whenLoud $ putStrLn . render . pp $ nanoSsa
  verb    <- V.getVerbosity
  let p =  execute verb nanoSsa $ tcAndPatch nanoSsa
  TC { noFailCasts = nfc } <- getOpts
  r <- either unsafe (\q -> safe q >>= return . (`mappend` failCasts nfc q)) p
  V.whenLoud $ donePhase FM.Loud "Typechecking"
  return (NoAnn, r)

unsafe errs = do putStrLn "\n\n\nErrors Found!\n\n" 
                 forM_ errs (putStrLn . ppshow) 
                 return $ F.Unsafe errs

safe (Nano {code = Src fs})
  = do V.whenLoud $ forM_ fs $ T.mapM printAnn
       return F.Safe 


-------------------------------------------------------------------------------
typeCheck ::
  (Data r, Ord r, PP r, F.Reftable r, Substitutable r (Fact r), Free (Fact r)) =>
  V.Verbosity -> (NanoSSAR r) -> Either [Error] (NanoTypeR r)
-------------------------------------------------------------------------------
typeCheck verb pgm = execute verb pgm $ tcAndPatch pgm

-------------------------------------------------------------------------------
failCasts :: (Data r, Typeable r) => 
              Bool -> Nano (AnnSSA r) (RType r) -> F.FixResult Error 
-------------------------------------------------------------------------------
failCasts False (Nano {code = Src fs}) | not $ null csts = F.Unsafe csts
                                       | otherwise       = F.Safe
  where csts = allCasts fs
failCasts True   _                                       = F.Safe                                            
    

-------------------------------------------------------------------------------
allCasts :: (Data r, Typeable r) => [FunctionStatement (AnnSSA r)] -> [Error]
-------------------------------------------------------------------------------
allCasts fs =  everything (++) ([] `mkQ` f) $ fs
  where f (DownCast l t)  = [errorDownCast l t]
        f (DeadCast l _)  = [errorDeadCast l  ]
        -- UpCasts are safe
        f _               = [ ]


printAnn (Ann l fs) = when (not $ null fs) $ putStrLn 
    $ printf "At %s: %s" (ppshow l) (ppshow fs)

-------------------------------------------------------------------------------
-- | TypeCheck Nano Program ---------------------------------------------------
-------------------------------------------------------------------------------
-- | The first argument true to tranform casted expressions e to Cast(e,T)
-------------------------------------------------------------------------------
tcAndPatch :: (Data r, Ord r, PP r, F.Reftable r, Substitutable r (Fact r), Free (Fact r)) =>
  Nano (AnnSSA r) (RType r) -> TCM r (Nano (AnnType r) (RType r))
-------------------------------------------------------------------------------
tcAndPatch p = 
  do  checkTypeDefs p
      p1 <- tcNano p 
      p2 <- patchPgmM p1
      s  <- getSubst
      c  <- getCasts
      whenQuiet' (return p2) (return $ trace (codePP p2 s c) p2)
      -- return p1
  where 
    codePP (Nano {code = Src src}) sub cst = render $
          text "********************** CODE **********************"
      $+$ pp src
      $+$ text "***************** SUBSTITUTIONS ******************"
      $+$ pp sub
      $+$ text "******************** CASTS ***********************"
      $+$ vcat ((\(e,t) -> (pp $ ann $ getAnnotation e) <+> pp (e,t)) <$> cst)
      $+$ text "**************************************************"



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
    grep :: [Id SourceSpan] = everything (++) ([] `mkQ` g) ds
    g (TDef i) | not $ envMem i ts = [i]
    g _                            = [ ]
  
    -- TODO: Also add check for free top-level type variables, i.e. make sure 
    -- all type variables used are bound. Use something like:
    -- @everythingWithContext@


-------------------------------------------------------------------------------
tcNano :: (Ord r, PP r, F.Reftable r, Substitutable r (Fact r), Free (Fact r)) =>
  Nano (AnnSSA r) (RType r) -> TCM r (Nano (AnnType r) (RType r))
-------------------------------------------------------------------------------
tcNano p@(Nano {code = Src fs})
  = do m     <- tcNano' p 
       return $ (trace "") $ p {code = Src $ (patchAnn m <$>) <$> fs}

-------------------------------------------------------------------------------
-- tcNano' :: Nano (AnnSSA r) (RType r) -> TCM r UAnnInfo  
-------------------------------------------------------------------------------
tcNano' pgm@(Nano {code = Src fs}) 
  = do tcStmts (initEnv pgm) fs
       M.unions <$> getAllAnns

initEnv pgm = TCE (specs pgm) emptyContext

-- patchAnn              :: UAnnInfo -> (AnnSSA r) -> (AnnType r)
patchAnn m (Ann l fs) = Ann l $ sortNub $ (M.lookupDefault [] l m) ++ fs

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
       forM (funTys l f xs ft) $ tcFun1 γ' l f xs body
       return $ Just γ' 

tcFun _  s = die $ bug (srcPos s) $ "Calling tcFun not on FunctionStatement"

tcFun1 γ l f xs body (i, (αs,ts,t)) = getAnns $ tcFunBody γ' l f body t
  where 
    γ'                              = envAddFun l f i αs xs ts t γ 
    getAnns                         = accumAnn (catMaybes . map (validInst γ') . M.toList) 


tcFunBody γ l f body t
  = do q              <- tcStmts γ body
       when (isJust q) $ void $ unifyTypeM (srcPos l) "Missing return" f tVoid t


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
tcSeq :: (TCEnv r -> a -> TCM r (TCEnvO r)) -> TCEnv r -> [a] -> TCM r (TCEnvO r)
--------------------------------------------------------------------------------

tcSeq f             = foldM step . Just 
  where 
    step Nothing _  = return Nothing
    step (Just γ) x = f γ x

--------------------------------------------------------------------------------
tcStmts :: (Ord r, PP r, F.Reftable r, Substitutable r (Fact r), Free (Fact r)) =>
            TCEnv r -> [Statement (AnnSSA r)] -> TCM r (TCEnvO r)
--------------------------------------------------------------------------------
tcStmts = tcSeq tcStmt

-------------------------------------------------------------------------------
tcStmt  :: (Ord r, PP r, F.Reftable r, Substitutable r (Fact r), Free (Fact r)) =>
            TCEnv r -> Statement (AnnSSA r) -> TCM r (TCEnvO r)
-------------------------------------------------------------------------------
-- skip
tcStmt' γ (EmptyStmt _) 
  = return $ Just γ

-- x = e
tcStmt' γ (ExprStmt _ (AssignExpr _ OpAssign (LVar lx x) e))   
  = tcAsgn γ (Id lx x) e

-- e3.x = e2
-- The type of @e2@ should be assignable (a subtype of) the type of @e3.x@.
tcStmt' γ (ExprStmt _ (AssignExpr l2 OpAssign (LDot _ e3 x) e2))
  = do  θ  <- getTDefs
        t2 <- tcExpr' γ e2 
        t3 <- tcExpr' γ e3
        tx <- safeGetProp (tce_ctx γ) x t3
        case tx of 
          -- NOTE: Atm assignment to non existing binding has no effect!
          TApp TUndef _ _ -> return $ Just γ
          _ ->  if isSubType θ t2 tx 
                  then return  $ Just γ
                  else tcError $ errorTypeAssign (srcPos l2) t2 tx

-- e3[i] = e2
tcStmt' γ (ExprStmt _ (AssignExpr l2 OpAssign (LBracket _ e3 (IntLit _ _)) e2))
  = do  t2 <- tcExpr' γ e2 
        _ {- t3 -}  <- tcExpr' γ e3
        ti <- throw $ bug (srcPos l2) "UNIMPLEMENTED: tc: e[e] = e" --safeGetIdx i t3
        θ  <- unifyTypeM (srcPos l2) "DotRef" e2 t2 ti 
        -- Once we've figured out what the type of the array should be,
        -- update the output environment.
        setSubst θ
        return $ Just $ apply θ γ 


-- e
tcStmt' γ (ExprStmt _ e)   
  = tcExpr' γ e >> return (Just γ) 

-- s1;s2;...;sn
tcStmt' γ (BlockStmt _ stmts) 
  = tcStmts γ stmts 

-- if b { s1 }
tcStmt' γ (IfSingleStmt l b s)
  = tcStmt' γ (IfStmt l b s (EmptyStmt l))

-- if b { s1 } else { s2 }
tcStmt' γ (IfStmt l e s1 s2)
  = do  t <- tcExpr' γ e 
    -- Doing check for boolean for the conditional for now
    -- TODO: Will have to support truthy/falsy later.
        unifyTypeM (srcPos l) "If condition" e t tBool
        γ1      <- tcStmt' γ s1
        γ2      <- tcStmt' γ s2
        envJoin l γ γ1 γ2


tcStmt' γ (WhileStmt l c b) = do
    let phis = [φ | LoopPhiVar φs <- ann_fact l, φ <- φs]
    let phiTs = fromJust <$> (`tcEnvFindTy` γ) <$> (fst3 <$> phis)
    let γ' =  tcEnvAdds (zip (snd3 <$> phis) phiTs) γ
    t   <- tcExpr' γ' c
    unifyTypeM (srcPos l) "While condition" c t tBool
    tcStmt' γ' b


-- var x1 [ = e1 ]; ... ; var xn [= en];
tcStmt' γ (VarDeclStmt _ ds)
  = tcSeq tcVarDecl γ ds

-- return e 
tcStmt' γ (ReturnStmt l eo) 
  = do  t           <- maybe (return tVoid) (tcExpr' γ) eo
        let rt       = tcEnvFindReturn γ 
        θ           <- unifyTypeM (srcPos l) "Return" eo t rt
        -- Apply the substitution
        let (rt',t') = mapPair (apply θ) (rt,t)
        -- Subtype the arguments against the formals and cast if 
        -- necessary based on the direction of the subtyping outcome
        maybeM_ (\e -> castM (tce_ctx γ) e t' rt') eo
        return Nothing

tcStmt' γ s@(FunctionStmt _ _ _ _)
  = tcFun γ s

-- OTHER (Not handled)
tcStmt' _ s 
  = convertError "tcStmt" s

tcStmt γ s = tcStmt' γ s

-------------------------------------------------------------------------------
tcVarDecl :: (Ord r, PP r, F.Reftable r) => TCEnv r -> VarDecl (AnnSSA r) -> TCM r (TCEnvO r)
-------------------------------------------------------------------------------
tcVarDecl γ v@(VarDecl _ x (Just e)) = do
    t <- tcExpr γ (listToMaybe [ t | TAnnot t <- ann_fact $ getAnnotation v]) e
    return $ Just $ tcEnvAdds [(x, t)] γ

tcVarDecl γ (VarDecl _ _ Nothing)  
  = return $ Just γ

------------------------------------------------------------------------------------
tcAsgn :: (PP r, Ord r, F.Reftable r) => 
  TCEnv r -> Id (AnnSSA r) -> Expression (AnnSSA r) -> TCM r (TCEnvO r)
------------------------------------------------------------------------------------
tcAsgn γ x e = tcExpr' γ e >>= \t -> return $ Just $ tcEnvAdds [(x, t)] γ


-- XXX: At the moment only arrays take annotations into account !!!
-------------------------------------------------------------------------------
tcExpr :: (Ord r, PP r, F.Reftable r)
       => TCEnv r                -- Typing environment
       -> Maybe (RType r)        -- Contextual type RJ: meaning what? 
       -> Expression (AnnSSA r)  -- Current expression
       -> TCM r (RType r)        -- Return type
-------------------------------------------------------------------------------
tcExpr γ ct e@(ArrayLit _ _) = tcArray γ ct e
tcExpr γ (Just ta) e         = tcExpr' γ e >>= checkAnnotation "tcExprAnnot" ta e >> return ta
tcExpr γ Nothing e           = tcExpr' γ e

tcExpr' γ e                  = setExpr (Just e) >> tcExpr'' γ e

----------------------------------------------------------------------------------------------
tcExpr'' :: (Ord r, PP r, F.Reftable r) => TCEnv r -> Expression (AnnSSA r) -> TCM r (RType r)
----------------------------------------------------------------------------------------------
tcExpr'' _ (IntLit _ _)
  = return tInt

tcExpr'' _ (BoolLit _ _)
  = return tBool

tcExpr'' _ (StringLit _ _)
  = return tString

tcExpr'' _ (NullLit _)
  = return tNull

tcExpr'' γ (VarRef l x)
  = case tcEnvFindTy x γ of 
      Nothing -> logError (errorUnboundIdEnv (ann l) x (tce_env γ)) tErr
      Just z  -> return $ tracePP ("tcExpr'' VarRef x = " ++ ppshow x) z

tcExpr'' γ (PrefixExpr l o e)
  = tcCall γ l o [e] (prefixOpTy o $ tce_env γ)

tcExpr'' γ (InfixExpr l o e1 e2)        
  = tcCall γ l o [e1, e2] (infixOpTy o $ tce_env γ)

tcExpr'' γ (CallExpr l e es)
  = tcExpr' γ e >>= tcCall γ l e es

tcExpr'' γ (ObjectLit _ ps) 
  = tcObject γ ps

-- x.f =def= x["f"]
tcExpr'' γ (DotRef _ e s) = tcExpr' γ e >>= safeGetProp (tce_ctx γ) (unId s) 

-- x["f"]
tcExpr'' γ (BracketRef _ e (StringLit _ s)) = tcExpr' γ e >>= safeGetProp (tce_ctx γ) s

-- x[i] 
tcExpr'' γ (BracketRef _ e (IntLit _ _)) = tcExpr' γ e >>= indexType (tce_ctx γ) 

-- x[e]
tcExpr'' γ e@(BracketRef l e1 e2) = do
    t1 <- tcExpr' γ e1
    t2 <- tcExpr' γ e2
    case t1 of 
      -- NOTE: Only support dynamic access of array with index of integer type.
      TArr t _  -> unifyTypeM (srcPos l) "BracketRef" e t2 tInt >> return t
      t         -> errorstar $ "Unimplemented: BracketRef of " ++ 
                               "non-array expression of type " ++ 
                               ppshow t

tcExpr'' _ e 
  = convertError "tcExpr" e



----------------------------------------------------------------------------------
tcCall :: (Ord r, F.Reftable r, PP r, PP fn) => 
  TCEnv r -> AnnSSA r -> fn -> [Expression (AnnSSA r)]-> RType r -> TCM r (RType r)
----------------------------------------------------------------------------------

tcCall γ l fn es ft0
  = do -- Typecheck arguments
       ts            <- mapM (tcExpr' γ) es
       -- Extract callee type (if intersection: match with args)
       let ft         = calleeType l ts ft0
       (_,ibs,ot)    <- instantiate l fn ft
       let its        = b_type <$> ibs
       -- Unify with formal parameter types
       θ             <- unifyTypesM (srcPos l) "tcCall" ts its
       -- Apply substitution
       let (ts',its') = mapPair (apply θ) (ts, its)
       -- Subtype the arguments against the formals and up/down cast if needed 
       castsM (tce_ctx γ) es ts' its'
       return         $ apply θ ot

instantiate l fn ft 
  = do let (αs, t) = bkAll ft 
       t'         <-  {- tracePP "new Ty Args" <$> -} freshTyArgs (srcPos l) (αs, t) 
       maybe err return $ tracePP (printf "instantiate/bkFun fn = %s ft = %s t' = %s " (ppshow fn) (ppshow ft) (ppshow t')) $ bkFun t'
    where
       err = die   $ errorNonFunction (ann l) fn ft

----------------------------------------------------------------------------------
tcObject ::  (Ord r, F.Reftable r, PP r) => 
  TCEnv r -> [(Prop (AnnSSA r), Expression (AnnSSA r))] -> TCM r (RType r)
----------------------------------------------------------------------------------
tcObject γ bs 
  = do 
      let (ps, es) = unzip bs
      bts <- zipWith B (map F.symbol ps) <$> mapM (tcExpr' γ) es
      return $ TObj bts F.top


----------------------------------------------------------------------------------
tcArray :: (Ord r, PP r, F.Reftable r) =>
  TCEnv r -> Maybe (RType r) -> Expression (AnnSSA r) -> TCM r (RType r)
----------------------------------------------------------------------------------
tcArray γ (Just t@(TArr ta _)) (ArrayLit _ es) = do 
    ts <- mapM (tcExpr γ $ Just ta) es
    checkElts ta es ts
    return (tracePP "Type being propagated for array literal: " t)
  where
    checkElts = zipWithM_ . (checkAnnotation "tcArray")

tcArray _ Nothing (ArrayLit _ _)  = 
  errorstar $ "Array literals need type annotations at " ++
              "the moment to typecheck in TC."
tcArray _ (Just _) (ArrayLit _ _) = 
  errorstar $ "Type annotation for array literal needs to be " ++ 
              "of Array type."
tcArray _ _ _ = 
  errorstar $ "BUG: Only support tcArray for array literals " ++ 
              "with type annotation"

-- XXX: Infering this very precise will make it very 
-- hard to do comparison with TArr later. 
--   case es of 
--     [] -> freshTArray l
--     _  -> mapM (tcExpr γ) es >>= return . mkObj
--   where 
--     mkObj ts = {- tracePP (ppshow es) $ -} TObj (bs ts) F.top
--     bs ts    = zipWith B (F.symbol . show <$> [0..]) ts ++ [len]
--     len      = B (F.symbol "length") tInt
--     l  = getAnnotation e


              
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

