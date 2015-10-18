{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Language.Rsc.Typecheck.Checker (verifyFile, typeCheck) where

import           Control.Applicative                (pure, (<$>), (<*>))
import           Control.Arrow                      (first, (***))
import           Control.Exception                  (throw)
import           Control.Monad
import qualified Data.Foldable                      as FD
import           Data.Function                      (on)
import           Data.Generics
import qualified Data.IntMap.Strict                 as I
import           Data.List                          (any, find, nub, sortBy)
import qualified Data.Map.Strict                    as M
import           Data.Maybe                         (catMaybes, fromMaybe, maybeToList)
import           Data.Monoid                        (mappend, mempty)
import qualified Data.Traversable                   as T
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc             as FM
import qualified Language.Fixpoint.Types            as F
import           Language.Rsc.Annotations
import           Language.Rsc.AST
import           Language.Rsc.ClassHierarchy
import           Language.Rsc.CmdLine               (Config, noFailCasts)
import           Language.Rsc.Core.EitherIO
import           Language.Rsc.Core.Env
import           Language.Rsc.Environment
import           Language.Rsc.Errors
import           Language.Rsc.Locations
import           Language.Rsc.Lookup
import           Language.Rsc.Misc                  (dup, nths, zipWith3M, (&))
import           Language.Rsc.Names
import           Language.Rsc.Parser
import           Language.Rsc.Pretty
import           Language.Rsc.Program
import           Language.Rsc.SSA.SSA
import           Language.Rsc.SystemUtils
import           Language.Rsc.Traversals
import           Language.Rsc.Typecheck.Environment
import           Language.Rsc.Typecheck.Subst
import           Language.Rsc.Typecheck.TCMonad
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Typecheck.Unify
import           Language.Rsc.Types
import           Language.Rsc.TypeUtilities
import           Language.Rsc.Visitor

import           Debug.Trace                        hiding (traceShow)

import qualified System.Console.CmdArgs.Verbosity   as V

--------------------------------------------------------------------------------
-- | Top-level Verifier
--------------------------------------------------------------------------------
verifyFile :: Config -> [FilePath] -> IO (UAnnSol a, FError)
--------------------------------------------------------------------------------
verifyFile cfg fs
  = runEitherIO (do p   <- EitherIO   $ parseRscFromFiles fs
                    cha <- liftEither $ mkCHA p
                    ssa <- EitherIO   $ ssaTransform p cha
                    tc  <- EitherIO   $ typeCheck cfg ssa cha
                    return tc)
  >>= either unsafe (pure . safe cfg)


unsafe errs = do putStrLn "\n\n\nErrors Found!\n\n"
                 return $ (NoAnn, errs)

safe cfg (Rsc {code = Src fs}) = (NoAnn, failCasts (noFailCasts cfg) fs)
    where
        failCasts True  _   = F.Safe
        failCasts False f   = applyNonNull F.Safe F.Unsafe
                            $ concatMap castErrors
                            $ casts f

        casts              :: Data r => [Statement (AnnTc r)] -> [AnnTc r]
        casts stmts         = everything (++) ([] `mkQ` f) stmts
          where
            f              :: Expression (AnnTc r) -> [(AnnTc r)]
            f (Cast_ a _)   = [a]
            f _             = []

--------------------------------------------------------------------------------
castErrors :: Unif r => AnnTc r -> [Error]
--------------------------------------------------------------------------------
castErrors (FA _ l facts) = downErrors
  where
    downErrors = [errorDownCast l t1 t2 | TCast _ (CDn t1 t2) <- facts]


--------------------------------------------------------------------------------
typeCheck :: Unif r => Config -> TcRsc r -> ClassHierarchy r -> IO (Either FError (TcRsc r))
--------------------------------------------------------------------------------
typeCheck cfg pgm cha
  = do  v <- V.getVerbosity
        pure $ execute cfg v pgm $ tcRsc pgm cha


--------------------------------------------------------------------------------
-- | TypeCheck program
--------------------------------------------------------------------------------
tcRsc :: Unif r => TcRsc r -> ClassHierarchy r -> TCM r (TcRsc r)
--------------------------------------------------------------------------------
tcRsc p@(Rsc {code = Src fs}) cha
  = do  _       <- checkTypes cha
        (fs',_) <- tcStmts γ fs
        fs''    <- patch fs'
        ast_cnt <- getAstCount
        return   $ p { code = Src fs'', maxId = ast_cnt }
  where
    γ = initGlobalEnv p cha


-- | Patch annotation on the AST
--------------------------------------------------------------------------------
patch :: Unif r => [Statement (AnnTc r)] -> TCM r [Statement (AnnTc r)]
--------------------------------------------------------------------------------
patch fs
  = do  (m, θ) <- (,) <$> getAnns <*> getSubst
        return $ map (fmap (pa m) . apply θ) fs
  where
    pa m (FA i l f)   = FA i l $ f ++ filter vld (I.findWithDefault [] i m)
    vld TypInst{}     = True
    vld Overload{}    = True
    vld EltOverload{} = True
    vld PhiVarTy{}    = True
    vld PhiVarTC{}    = True
    vld PhiVar{}      = True
    vld TCast{}       = True
    vld _             = False

-- accumNodeIds :: Unif r => Statement (AnnTc r) -> [(Int, [Fact r])]
-- accumNodeIds = FD.foldl (\b (FA i _ f) -> (i, f): b) []


--------------------------------------------------------------------------------
-- | TypeCheck Function
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
tcFun :: Unif r => TCEnv r -> Statement (AnnTc r)
                 -> TCM r (Statement (AnnTc r), Maybe (TCEnv r))
--------------------------------------------------------------------------------
tcFun γ (FunctionStmt l f xs Nothing)
  = return $ (FunctionStmt l f xs Nothing, Just γ)
tcFun γ (FunctionStmt l f xs (Just body))
  | Just ft   <- tcEnvFindTy f γ
  = do  ts    <- tcFunTys l f xs ft
        body' <- foldM (tcCallable γ l f xs) body ts
        return $ (FunctionStmt l f xs (Just body'), Just γ)
  | otherwise
  = die $ errorMissingSpec (srcPos l) f

tcFun _  s = die $ bug (srcPos s) $ "Calling tcFun not on FunctionStatement"

-- | `tcCallable g l f xs fty body` checks @body@ under the environment @g@
-- (which needs to be set beforehand), with input arguments @xs@ and overload
-- signature @fty@ (which includes the context number).
--------------------------------------------------------------------------------
tcCallable :: (Unif r, IsLocated l, PP l)
           => TCEnv r -> AnnTc r -> l
           -> [Id (AnnTc r)]
           -> [Statement (AnnTc r)]
           -> IOverloadSig r
           -> TCM r [Statement (AnnTc r)]
--------------------------------------------------------------------------------
tcCallable γ l f xs body fty
  = do  let γ' = initCallableEnv l γ f fty xs body
        body' <- addReturnStmt l γ' body
        tcFunBody γ' l body'

addReturnStmt l γ body
  | isTVoid (tcEnvFindReturn γ)
  = (body ++) . single <$> (`ReturnStmt` Nothing) <$> freshenAnn l
  | otherwise
  = return body

tcFunBody γ l body
  = tcStmts γ body >>= \case
      (b, Just _) |  tcEnvFindReturn γ /= tVoid
                  -> fatal (errorMissingReturn (srcPos l)) b
      (b, _     ) |  otherwise
                  -> return b

--------------------------------------------------------------------------------
tcSeq :: (TCEnv r -> a -> TCM r (a, TCEnvO r)) -> TCEnv r -> [a] -> TCM r ([a], TCEnvO r)
--------------------------------------------------------------------------------
tcSeq f             = go []
  where
    go acc γ []     = return (reverse acc, Just γ)
    go acc γ (x:xs) = do (y, γo) <- f γ x
                         case γo of
                           Nothing -> return (reverse acc ++ y:xs, Nothing)
                           Just γ' -> go (y:acc) γ' xs

--------------------------------------------------------------------------------
tcStmts :: Unif r =>
  TCEnv r -> [Statement (AnnTc r)] -> TCM r ([Statement (AnnTc r)], TCEnvO r)
--------------------------------------------------------------------------------
tcStmts = tcSeq tcStmt

--------------------------------------------------------------------------------
tcStmt  :: Unif r =>
  TCEnv r -> Statement (AnnTc r) -> TCM r (Statement (AnnTc r), TCEnvO r)
--------------------------------------------------------------------------------
-- skip
tcStmt γ s@(EmptyStmt _)
  = return (s, Just γ)

-- interface Foo;
-- this definitions will be hoisted
tcStmt γ s@(InterfaceStmt _ _)
  = return (s, Just γ)

-- x = e
tcStmt γ (ExprStmt l (AssignExpr l1 OpAssign (LVar lx x) e))
  = do (e', g) <- tcAsgn l γ (Id lx x) e
       return $ (ExprStmt l (AssignExpr l1 OpAssign (LVar lx x) e'), g)

-- e1.f = e2
--
--  We use runFail to enable contextual typing of e2.
--
tcStmt γ (ExprStmt l (AssignExpr l2 OpAssign (LDot l1 e1 f) e2))
  = do  z               <- runFailM ( tcExpr γ e1l Nothing )
        case z of
          Right (_,te1) -> tcSetProp (fmap snd $ getProp γ FieldAccess f te1)
          Left _        -> tcSetProp Nothing
  where
    e1l  = fmap (\a -> a { fFact = BypassUnique : fFact a }) e1
    tcSetProp rhsCtx = do
      ([e1',e2'],_) <- tcNormalCallWCtx γ l BISetProp [(e1l, Nothing), (e2, rhsCtx)] (setPropTy f)
      return (ExprStmt l (AssignExpr l2 OpAssign (LDot l1 e1' f) e2'), Just γ)

-- e
tcStmt γ (ExprStmt l e)
  = do (e', to) <- tcExprW γ e
       return (ExprStmt l e', const γ <$> to)

-- s1;s2;...;sn
tcStmt γ (BlockStmt l stmts)
  = do (stmts', g) <- tcStmts γ stmts
       return (BlockStmt l stmts', g)

-- if b { s1 }
tcStmt γ (IfSingleStmt l b s)
  = tcStmt γ (IfStmt l b s (EmptyStmt l))

-- if b { s1 } else { s2 }
tcStmt γ (IfStmt l e s1 s2)
  = do opTy         <- safeEnvFindTy l γ (builtinOpId BITruthy)
       ([e'], z)    <- tcNormalCallW γ l BITruthy [e] opTy
       case z of
         Just _  ->
            do  (s1', γ1) <- tcStmt γ s1
                (s2', γ2) <- tcStmt γ s2
                z         <- envJoin l γ γ1 γ2
                case z of
                  Just (γ3',s1s,s2s) ->
                    do  l1 <- freshenAnn l
                        l2 <- freshenAnn l
                        return $ (IfStmt l e' (postfixStmt l1 s1s s1')
                                              (postfixStmt l2 s2s s2')
                                 , Just γ3')
                  Nothing ->
                    return (IfStmt l e' s1' s2', Nothing)
         _       -> return (IfStmt l e' s1 s2, Nothing)

-- while c { b }
tcStmt γ (WhileStmt l c b)
  = tcExprW γ c >>= \case
      (c', Nothing) -> return (WhileStmt l c' b, Nothing)
      (c', Just t)  ->
        do unifyTypeM (srcPos l) γ t tBool
           phiTys   <- mapM (safeEnvFindTy l γ) phis
           (b', γl) <- tcStmt (tcEnvAdds (zip xs (VI Local WriteLocal Initialized <$> phiTys)) γ) b
           tcWA γ dummyExpr (envLoopJoin l γ γl) >>= \case
             Left e     -> return $ (ExprStmt  l e    , γl)
             Right γout -> return $ (WhileStmt l c' b', γout)
  where
    dummyExpr     = StringLit l "DUMMY_LOOP_REPLACEMENT"
    xs            = [ mkNextId x | x <- phis ]
    phis          = [ x | x <- phiVarsAnnot l ]

-- var x1 [ = e1 ]; ... ; var xn [= en];
tcStmt γ (VarDeclStmt l ds)
  = do (ds', z) <- tcSeq tcVarDecl γ ds
       return      (VarDeclStmt l ds', z)

-- return e
tcStmt γ (ReturnStmt l eo)
  = tcRetW γ l eo

-- throw e
tcStmt γ (ThrowStmt l e)
  = do  (e', _) <- tcExprW γ e
        return   $ (ThrowStmt l e', Nothing)


tcStmt γ s@(FunctionStmt _ _ _ _)
  = tcFun γ s

-- | class A<S...> [extends B<T...>] [implements I,J,...] { ... }
tcStmt γ (ClassStmt l x ce)
  = do  d  <- resolveTypeM l γ rn
        ms <- mapM (tcClassElt γ d) ce
        return $ (ClassStmt l x ms, Just γ)
  where
    rn = QN (envPath γ) (F.symbol x)

-- | module M { ... }
tcStmt γ (ModuleStmt l n body)
  = (ModuleStmt l n *** return (Just γ)) <$>  tcStmts (initModuleEnv γ n body) body

-- | enum M { ... }
tcStmt γ (EnumStmt l n body)
  = return (EnumStmt l n body, Just γ)

-- OTHER (Not handled)
tcStmt _ s
  = convertError "tcStmt" s


--------------------------------------------------------------------------------
tcVarDecl :: Unif r => TCEnv r -> VarDecl (AnnTc r) -> TCM r (VarDecl (AnnTc r), TCEnvO r)
--------------------------------------------------------------------------------
-- PV: Some ugly special casing for Function expressions
tcVarDecl γ (VarDecl l x (Just e@FuncExpr{}))
  = do  (e', _) <- tcExpr γ e (v_type <$> envFindTy x (tce_names γ))
        return (VarDecl l x (Just e'), Just γ)

tcVarDecl γ v@(VarDecl l x (Just e))
  = case envFindTy x (tce_names γ) of
      -- | Local
      Nothing ->
        do  (e', to) <- tcExprW γ e
            return $ (VarDecl l x (Just e'), tcEnvAddo γ x $ VI Local WriteLocal Initialized <$> to)

      Just (VI loc WriteLocal _ t) ->
        do  ([e'], Just t') <- tcNormalCallWCtx γ l "VarDecl-WL" [(e, Just t)] (idTy t)
            return $ (VarDecl l x $ Just e', Just $ tcEnvAdd x (VI loc WriteLocal Initialized t') γ)

      -- | Global
      Just (VI _ WriteGlobal _ t) ->
        -- PV: the global variable should be in scope already,
        --     since it is being hoisted to the beginning of the
        --     scope.
        first (VarDecl l x . Just) <$> tcAsgn l γ x e

      -- | ReadOnly
      Just (VI loc RdOnly _ t) ->
        do  ([e'], Just t') <- tcNormalCallWCtx γ l "VarDecl-RO" [(e, Just t)] (idTy t)
            return $ (VarDecl l x $ Just e', Just $ tcEnvAdd x (VI loc RdOnly Initialized t') γ)

      c -> fatal (unimplemented l "tcVarDecl" ("case: " ++ ppshow c)) (v, Just γ)

tcVarDecl γ v@(VarDecl _ x Nothing)
  = case envFindTy x (tce_names γ) of
      Just (VI loc Ambient _ t) ->
          return $ (v, Just $ tcEnvAdds [(x, VI loc Ambient Initialized t)] γ)
      _ -> error "TC-tcVarDecl: this shouldn't happen"


--------------------------------------------------------------------------------
tcClassElt :: Unif r => TCEnv r -> TypeDecl r -> ClassElt (AnnTc r) -> TCM r (ClassElt (AnnTc r))
--------------------------------------------------------------------------------
-- | Constructor
tcClassElt γ (TD sig@(TS _ (BGen nm bs) _) ms) (Constructor l xs body)
  = do  its    <- tcFunTys l ctor xs ctorTy
        body'  <- foldM (tcCallable γ' l ctor xs) body its
        return  $ Constructor l xs body'
  where
    γ'     = γ
           & initClassInstanceEnv sig
           & initClassCtorEnv sig
           & tcEnvAdd cExit viExit
           -- TODO: TEST THIS
    ctor   = builtinOpId BICtor

    thisT = TRef (Gen nm (map btVar bs)) fTop
    cExit = builtinOpId BICtorExit
    viExit = VI Local Ambient Initialized $ mkFun (bs, xts, ret)
    ret   = thisT
    xts   = sortBy c_sym [ B x t' | (x, FI _ _ t) <- F.toListSEnv (tm_prop ms)
                                  , let t' = t ] -- unqualifyThis g0 thisT t ]
    out   = [ f | (f, FI _ m _) <- F.toListSEnv (tm_prop ms), isImm m ]
    v_sym = F.symbol $ F.vv Nothing
    c_sym = on compare b_sym
    ctorTy = fromMaybe (die $ unsupportedNonSingleConsTy (srcPos l)) (tm_ctor ms)

-- | Static field
tcClassElt γ (TD sig ms) c@(MemberVarDecl l True x (Just e))
  | Just (FI _ _ t) <- F.lookupSEnv (F.symbol x) $ tm_sprop ms
  = do  ([e'],_) <- tcNormalCall γ l "field init" [(e, Just t)] $ mkInitFldTy t
        return $ MemberVarDecl l True x $ Just e'
  | otherwise
  = fatal (errorClassEltAnnot (srcPos l) sig x) c

tcClassElt _ _ c@(MemberVarDecl l True x Nothing)
  = fatal (unsupportedStaticNoInit (srcPos l) x) c

-- | Instance variable
tcClassElt _ _ m@(MemberVarDecl _ False _ Nothing)
  = return m
tcClassElt _ _ (MemberVarDecl l False x _)
  = die $ bugClassInstVarInit (srcPos l) x

-- | Static method: The classes type parameters should not be included in the
-- environment
tcClassElt γ (TD sig ms) c@(MemberMethDecl l True x xs body)
  | Just (MI _ mts) <- F.lookupSEnv (F.symbol x) $ tm_smeth ms
  = do  let t = mkAnd $ map snd mts
        its <- tcFunTys l x xs t
        body' <- foldM (tcCallable γ l x xs) body its
        return $ MemberMethDecl l True x xs body'
  | otherwise
  = fatal (errorClassEltAnnot (srcPos l) (sig) x) c

-- | Instance method
--
--   TODO: check method mutability
--   TODO: The method's mutability should influence the type of tThis that is used
--         as a binder to this.
--   TODO: Also might not need 'tce_this'
--
tcClassElt γ (TD sig ms) c@(MemberMethDecl l False x xs bd)
  | Just (MI _ mts) <- F.lookupSEnv (F.symbol x) (tm_meth ms)
  = do  let (ms, ts)  = unzip mts
        its          <- tcFunTys l x xs $ mkAnd ts
        let mts'      = zip ms its
        bd'          <- foldM (\b (m,t) -> tcCallable (mkEnv m) l x xs b t) bd mts'
        return        $ MemberMethDecl l False x xs bd'
  | otherwise
  = fatal (errorClassEltAnnot (srcPos l) (sig) x) c
  where
    mkEnv m     = γ
                & initClassInstanceEnv sig
                & initClassMethEnv m sig
                & tcEnvAdds [eThis]
    TS _ bgen _ = sig
    BGen nm bs  = bgen
    tThis       = TRef (Gen nm (map btVar bs)) fTop
    idThis      = Id l "this"
    eThis       = (idThis, VI Local RdOnly Initialized tThis)



--------------------------------------------------------------------------------
tcAsgn :: Unif r
       => AnnTc r -> TCEnv r -> Id (AnnTc r) -> ExprSSAR r -> TCM r (ExprSSAR r, TCEnvO r)
--------------------------------------------------------------------------------
tcAsgn l γ x e
  | Just (VI _ a _ t) <- tcEnvFindTyForAsgn x γ
  = do  eitherET  <- tcWrap (tcExprT l "assign" γ e t)
        (e', to)  <- tcEW γ e eitherET
        return     $ (e', tcEnvAddo γ x $ VI Local a Initialized <$> to)
  | otherwise
  = do (e', to)   <- tcExprW γ e
       return      $ (e', tcEnvAddo γ x $ VI Local WriteLocal Initialized <$> to)

tcExprT l fn γ e t
  = do ([e'], _) <- tcNormalCall γ l fn [(e, Just t)] $ idTy t
       return (e', t)

tcEnvAddo _ _ Nothing  = Nothing
tcEnvAddo γ x (Just t) = Just $ tcEnvAdds [(x, t)] γ

--------------------------------------------------------------------------------
-- tcExprW  :: Unif r => TCEnv r -> ExprSSAR r ->                    TCM r (ExprSSAR r, Maybe (RType r))
tcExprWD :: Unif r => TCEnv r -> ExprSSAR r -> Maybe (RType r) -> TCM r (ExprSSAR r, RType r)
--------------------------------------------------------------------------------
tcExprW γ e
  = do  t <- tcWrap (tcExpr γ e Nothing)
        tcEW γ e t

tcExprWD γ e t = (tcWrap $ tcExpr γ e t) >>= tcEW γ e >>= \case
                   (e, Just t) -> return (e, t)
                   (e, _     ) -> return (e, tNull)

tcNormalCallW γ l o es t
  = (tcWrap $ tcNormalCall γ l o (es `zip` nths) t) >>= \case
      Right (es', t') -> return (es', Just t')
      Left err -> (,Nothing) <$> mapM (deadcastM (tce_ctx γ) err) es

tcNormalCallWCtx γ l o es t
  = (tcWrap $ tcNormalCall γ l o es t) >>= \case
      Right (es', t') -> return (es', Just t')
      Left err -> (,Nothing) <$> mapM (deadcastM (tce_ctx γ) err) (fst <$> es)

tcRetW γ l (Just e)
  | VarRef lv x <- e
  , Just t <- tcEnvFindTy x γ
  , isUMRef t
  = tcRetW (newEnv t) l (Just (CallExpr l (VarRef l fn) [e'])) >>= \case
      -- e' won't be cast
      (ReturnStmt _ (Just (CallExpr _ _ [e''])), eo)
          -> return (ReturnStmt l (Just e''),eo)
      _   -> die $ errorUqMutSubtyping (srcPos l) e t retTy

  | otherwise
  = (tcWrap $ tcNormalCall γ l "return" [(e, Just retTy)] (returnTy retTy True)) >>= \case
       Right ([e'], _) -> (,Nothing) . ReturnStmt l . Just <$> return e'
       Left err        -> (,Nothing) . ReturnStmt l . Just <$> deadcastM (tce_ctx γ) err e
  where
    retTy     = tcEnvFindReturn γ

    newEnv t  = tcEnvAdd fn (VI Local Ambient Initialized $ finalizeTy t) γ
    fn        = Id l "__finalize__"
    e'        = fmap (\a -> a { fFact = BypassUnique : fFact a }) e

tcRetW γ l Nothing
  = do (_, _) <- tcNormalCall γ l "return" [] $ returnTy (tcEnvFindReturn γ) False
       return  $ (ReturnStmt l Nothing, Nothing)

--------------------------------------------------------------------------------
-- tcEW :: Unif r => TCEnv r -> ExprSSAR r -> Either Error ((ExprSSAR r), b)
--                -> TCM r ((ExprSSAR r), Maybe b)
--------------------------------------------------------------------------------
tcEW _ _ (Right (e', t')) = return $ (e', Just t')
tcEW γ e (Left err)       = do  e'    <- deadcastM (tce_ctx γ) err e
                                return $ (e', Nothing)

-- Execute a and if it fails wrap e in a deadcast
tcWA γ e a = tcWrap a >>= \x -> case x of Right r -> return $ Right r
                                          Left  l -> Left <$> deadcastM (tce_ctx γ) l e

--------------------------------------------------------------------------------
tcExpr :: Unif r => TCEnv r -> ExprSSAR r -> Maybe (RType r) -> TCM r (ExprSSAR r, RType r)
--------------------------------------------------------------------------------
tcExpr _ e@(IntLit _ _) _
  = return (e, tNum)

tcExpr _ e@(HexLit _ _) _
  = return (e, tBV32)

tcExpr _ e@(BoolLit _ _) _
  = return (e, tBool)

tcExpr _ e@(StringLit _ _) _
  = return (e, tString)

tcExpr _ e@(NullLit _) _
  = return (e, tNull)

tcExpr γ e@(ThisRef l) _
  = case tcEnvFindTy (F.symbol "this") γ of
      Just t  -> return (e, t)
      Nothing -> fatal (errorUnboundId (fSrc l) "this") (e, tBot)

tcExpr γ e@(VarRef l x) _
  | F.symbol x == F.symbol "undefined"
  = return (e, tUndef)

  | Just t <- to, not $ null [ () | BypassUnique <- fFact l ]
  = return (e,t)

  | Just t <- to, isCastId x  -- Avoid TC added casts
  = return (e,t)

  | Just t <- to, isUMRef t
  = fatal (errorAssignsFields (fSrc l) x t) (e, tBot)

  | Just t <- to
  = return (e,t)

  | otherwise
  = fatal (errorUnboundId (fSrc l) x) (VarRef l x, tBot)
  where
    to = tcEnvFindTy x γ

-- | e ? e1 : e2
--
--   Conditional expresion is transformed to a call to a function with
--   signature:
--
--     forall C . (c: C, _t: T, x: T, y: T) => T
--
--   The arguments are:
--
--    * The condition expression
--    * A phony expression with:
--      - The contextual type `t` if there exists one
--      - Top, otherwise
--    * The first conditional expression
--    * The second conditional expression
--
tcExpr γ (CondExpr l e e1 e2) to
  = do  opTy    <- mkTy to <$> safeEnvFindTy l γ (builtinOpId BICondExpr)
        (sv,v)  <- dup F.symbol (VarRef l) <$> freshCastId l
        let γ'   = tcEnvAdd sv (VI Local WriteLocal Initialized tt) γ
        ([e',_,e1',e2'], t') <- tcNormalCall γ' l BICondExpr (args v) opTy
        return (CondExpr l e' e1' e2', t')
  where
    args v   = [(e,Nothing), (v, Nothing),(e1,to),(e2,to)]
    tt       = fromMaybe tTop to
    mkTy Nothing (TAll cv (TAll tv (TFun [B c_ tc, B t_ _   , B x_ xt, B y_ yt] o r))) =
                  TAll cv (TAll tv (TFun [B c_ tc, B t_ tTop, B x_ xt, B y_ yt] o r))
    mkTy _ t = t

tcExpr γ e@(PrefixExpr _ _ _) _
  = tcCall γ e

tcExpr γ e@(InfixExpr _ _ _ _) _
  = tcCall γ e

-- | f(e)
tcExpr γ e@(CallExpr _ _ _) _
  = tcCall γ e

-- | [e1,..,en]
tcExpr γ e@(ArrayLit _ _) _
  = tcCall γ e

-- | { f: e }
tcExpr γ e@(ObjectLit _ _) _
  = tcCall γ e

-- | <T>e
tcExpr γ ex@(Cast l e) _
  | [t] <- [ ct | UserCast ct <- fFact l ]
  = mapFst (Cast l) <$> tcCast l γ e t
  | otherwise
  = die $  bugNoCasts (srcPos l) ex

-- | Subtyping induced cast
--
--   If the existing cast has been added by another context (i.e. e' == Cast_  ...)
--   then typecheck the contents of the cast and return the type of the internal
--   expression.
--
--   If it's from the same context then the internal expression has been
--   typechecked, so just return the inferred type.
--
tcExpr γ (Cast_ l e) to
  = case [ t_ | TCast ξ t_ <- fFact l, tce_ctx γ == ξ ] of
      [ ] -> do (e', t)    <- tcExpr γ e to
                case e' of
                  Cast_ {} -> die $ bugNestedCasts (srcPos l) e
                  _        -> (,t) . (`Cast_` e') <$> freshenAnn l
      [c] -> return (Cast_ l e, castType c)
      _   -> die $ bugNestedCasts (srcPos l) e

-- | e.f
tcExpr γ e@(DotRef _ _ _) _
  = tcCall γ e

-- -- | e1["s"]
-- tcExpr γ (BracketRef l1 e1 (StringLit l2 s)) to
--   = tcExpr γ (DotRef l1 e1 (Id l2 s)) to
--
-- -- | e1[1]
-- tcExpr γ (BracketRef l1 e1 (IntLit l2 i)) to
--   = tcExpr γ (DotRef l1 e1 (Id l2 $ show i)) to
--
-- | e1[e2]
tcExpr γ e@(BracketRef _ _ _) _
  = tcCall γ e

-- | e1[e2] = e3
tcExpr γ e@(AssignExpr _ OpAssign (LBracket _ _ _) _) _
  = tcCall γ e

-- | new C(e, ...)
tcExpr γ e@(NewExpr _ _ _) _
  = tcCall γ e

-- | super
tcExpr γ e@(SuperRef l) _
  = case tcEnvFindTy (builtinOpId BISuper) γ of
      Just t  -> return (e,t)
      Nothing -> fatal (errorSuper (fSrc l)) (e, tBot)

-- | function (x,..) {  }
tcExpr γ e@(FuncExpr l fo xs body) tCtxO
  | Just ft   <- funTy
  =  do ts    <- tcFunTys l f xs ft
        body' <- foldM (tcCallable γ l f xs) body ts
        return $ (FuncExpr l fo xs body', ft)
  | otherwise
  = fatal (errorNoFuncAnn $ srcPos l) (e, tBot)
  where
    funTy | [ft] <- [ t | SigAnn _ t <- fFact l ] = Just ft
          | Just ft <- tCtxO = Just ft
          | otherwise        = Nothing
    f     = maybe (F.symbol "<anonymous>") F.symbol fo

tcExpr _ e _
  = convertError "tcExpr" e

-- | @tcCast@ emulating a simplified version of a function call
--------------------------------------------------------------------------------
tcCast :: Unif r => AnnTc r -> TCEnv r -> ExprSSAR r -> RType r -> TCM r (ExprSSAR r, RType r)
--------------------------------------------------------------------------------
tcCast l γ e tc
  = do  cid         <- freshCastId l
        ([e'],t')   <- tcNormalCall γ l "user-cast" [(e, Just tc)] (castTy tc)
        return       $ (e', t')

--------------------------------------------------------------------------------
tcCall :: Unif r => TCEnv r -> ExprSSAR r -> TCM r (ExprSSAR r, RType r)
--------------------------------------------------------------------------------

-- | `o e`
tcCall γ c@(PrefixExpr l o e)
  = do opTy <- safeEnvFindTy l γ (prefixOpId o)
       z    <- tcNormalCall γ l o [(e, Nothing)] opTy
       case z of
         ([e'], t) -> return (PrefixExpr l o e', t)
         _ -> fatal (impossible (srcPos l) "tcCall PrefixExpr") (c, tBot)

-- | `e1 o e2`
tcCall γ c@(InfixExpr l o@OpInstanceof e1 e2)
  = do (e2',t) <- tcExpr γ e2 Nothing
       case t of
         TClass (BGen (QN _ x) _)  ->
            do  opTy <- safeEnvFindTy l γ (infixOpId o)
                ([e1',_], t) <- let args = [e1, StringLit l2 (F.symbolString x)] `zip` nths in
                                tcNormalCall γ l o args opTy
                      -- TODO: add qualified name
                return (InfixExpr l o e1' e2', t)
         _  -> fatal (unimplemented (srcPos l) "tcCall-instanceof" $ ppshow e2) (c,tBot)
  where
    l2 = getAnnotation e2

tcCall γ c@(InfixExpr l o e1 e2)
  = do opTy <- safeEnvFindTy l γ (infixOpId o)
       z    <- tcNormalCall γ l o ([e1,e2] `zip` nths) opTy
       case z of
         ([e1', e2'], t) -> return (InfixExpr l o e1' e2', t)
         _ -> fatal (impossible (srcPos l) "tcCall InfixExpr") (c, tBot)

-- | `e1[e2]` Special case for Enumeration, and object literals with numeric
-- or string arguments.
--
tcCall γ e@(BracketRef l e1 e2)
  = runFailM (tcExpr γ e1 Nothing) >>= \case
      -- Enumeration
      Right (_, t) | isEnumType (envCHA γ) t -> fatal (unimplemented (srcPos l) msg e) (e, tBot)
      -- Default
      _ -> safeEnvFindTy l γ (builtinOpId BIBracketRef) >>= call
  where
    msg     = "Support for dynamic access of enumerations"
    call ty = tcNormalCall γ l BIBracketRef ([e1,e2] `zip` nths) ty >>= \case
          ([e1', e2'], t) -> return (BracketRef l e1' e2', t)
          _ -> fatal (impossible (srcPos l) "tcCall BracketRef") (e, tBot)

-- | `e1[e2] = e3`
tcCall γ e@(AssignExpr l OpAssign (LBracket l1 e1 e2) e3)
  = do opTy <- safeEnvFindTy l γ (builtinOpId BIBracketAssign)
       z <- tcNormalCall γ l BIBracketAssign ([e1,e2,e3] `zip` nths) opTy
       case z of
         ([e1', e2', e3'], t) -> return (AssignExpr l OpAssign (LBracket l1 e1' e2') e3', t)
         _ -> fatal (impossible (srcPos l) "tcCall AssignExpr") (e, tBot)

-- | `[e1,...,en]`
tcCall γ (ArrayLit l es)
  = do opTy <- arrayLitTy (length es) <$> safeEnvFindTy l γ (builtinOpId BIArrayLit)
       (es', t) <- tcNormalCall γ l BIArrayLit (es `zip` nths) opTy
       return $ (ArrayLit l es', t)

-- | `{ f1:t1,...,fn:tn }`
tcCall γ (ObjectLit l (unzip -> (ps, es)))
  = do (es', t) <- tcNormalCall γ l "ObjectLit" (es `zip` nths) (objLitTy l ps)
       return $ (ObjectLit l (zip ps es'), t)

-- | `new e(e1,...,en)`
tcCall γ c@(NewExpr l e es)
  = do (e',t) <- tcExpr γ e Nothing
       case extractCtor γ t of
         Just ct ->
            do (es', t) <- tcNormalCall γ l "new" (es `zip` nths) ct
               return $ (NewExpr l e' es', t)
         _ -> fatal (errorConstrMissing (srcPos l) t) (c, tBot)

-- | e.f
tcCall γ ef@(DotRef l e f)
  = runFailM (tcExpr γ e Nothing) >>= \case
      Right (_, te) ->
          case getProp γ FieldAccess f te of
            Just (tObj, tField) ->
                do  funTy <- mkDotRefFunTy l γ f tObj tField
                    (e':_, t') <- tcNormalCall γ l ef [(e, Nothing)] funTy
                    return (DotRef l e' f, t')
            Nothing -> tcError $ errorMissingFld (srcPos l) f te
      Left err -> fatal err (ef, tBot)

-- | `super(e1,...,en)`
--
--   XXX: there shouldn't be any `super(..)` calls after SSA ...
--
tcCall _ (CallExpr _ (SuperRef _)  _)
  = error "BUG: super(..) calls should have been eliminated"

-- | `e.m(es)`
--
tcCall γ ex@(CallExpr l em@(DotRef l1 e f) es)
  | isVariadicCall f
  = fatal (unimplemented l "Variadic" ex) (ex, tBot)
  | otherwise
  = runFailM (tcExpr γ e Nothing) >>= \case
      Right (_, t) | Just (t',tF)   <- getProp γ FieldAccess f t          -- Function field
                   , isTFun tF ->
                      do  (e', _ )  <- tcExprT l1 (ppshow em) γ e t'
                          (es',to)  <- tcNormalCall γ l ex (es `zip` nths) tF
                          return     $ (CallExpr l (DotRef l1 e' f) es', to)

                   | Just (t', tF)  <- getProp γ MethodAccess f t ->     -- Invoking a method
                      do  (e', _ )  <- tcExprT l1 (ppshow em) γ e t'
                          (es', to) <- tcNormalCall γ l  ex (es `zip` nths) tF
                          return     $ (CallExpr l (DotRef l1 e' f) es', to)

                   | otherwise ->
                      fatal (errorCallNotFound (srcPos l1) e f) (ex, tBot)

      Left err ->
            fatal err (ex,tBot)
  where
    isVariadicCall f = F.symbol f == F.symbol "call"

-- | `e(es)`
tcCall γ (CallExpr l e es)
  = do (e', ft0) <- tcExpr γ e Nothing
       (es', t)  <- tcNormalCall γ l e (es `zip` nths) ft0
       return $ (CallExpr l e' es', t)

tcCall _ e = fatal (unimplemented (srcPos e) "tcCall" e) (e, tBot)

--------------------------------------------------------------------------------
-- | @tcNormalCall@ resolves overloads and returns cast-wrapped versions of the arguments.
--------------------------------------------------------------------------------
tcNormalCall :: (Unif r, PP a) => TCEnv r -> AnnTc r -> a -> [(ExprSSAR r, Maybe (RType r))]
             -> RType r -> TCM r ([ExprSSAR r], RType r)
--------------------------------------------------------------------------------
tcNormalCall γ l fn etos ft0
  = do ets <- T.mapM (uncurry $ tcExprWD γ) etos
       z <- resolveOverload γ l fn ets ft0
       case z of
         Just (i, θ, ft) ->
             do addAnn (fId l) (Overload (tce_ctx γ) i)
                addSubst l θ
                tcWrap (tcCallCase γ l fn ets ft) >>= \case
                  Right ets' -> return ets'
                  Left  err -> (,tNull) <$> T.mapM (deadcastM (tce_ctx γ) err . fst) ets
         Nothing -> tcError $ uncurry (errorCallNotSup (srcPos l) fn ft0) (unzip ets)


-- | `resolveOverload γ l fn es ts ft`
--
-- When resolving an overload there are two prossible cases:
--
-- * There is only a single signature available: then return just this
--   signature regardless of subtyping constraints
--
-- * There are more than one signature available: return all that pass the
--   subtype check (this is what tcCallCaseTry does).
--
--------------------------------------------------------------------------------
resolveOverload :: (Unif r, PP a)
                => TCEnv r
                -> AnnTc r
                -> a
                -> [(ExprSSAR r, RType r)]
                -> RType r
                -> TCM r (Maybe (IntCallSite, RSubst r, RType r))
--------------------------------------------------------------------------------
-- | A function signature is compatible if:
--
--   * The supplied parameters have the same number of 'normal' arguments, i.e.
--     not including the 'self' argument.
--
--   * If the function requires a 'self' argument, the parameters provide one.
--
resolveOverload γ l fn es ft
  | [(i,t)] <- validOverloads
  = Just . (i,,t) <$> getSubst
  |otherwise
  = tcCallCaseTry γ l fn (snd <$> es) validOverloads
  where
    validOverloads = [ (i, mkFun s) | (i, s@(_, bs, _)) <- extractCall γ ft
                                    , length bs == length es ]


--------------------------------------------------------------------------------
-- | A successful pairing of formal to actual parameters will return `Just θ`,
-- where @θ@ is the corresponding substitution. If the types are not acceptable
-- this will return `Nothing`. In this case successful means:
--
--  * Unifying without errors.
--
--  * Passing the subtyping test.
--
-- The monad state is completely reversed after this function returns, thanks
-- to `runMaybeM`. We don't need to reverse the action of `instantiateFTy`.
--------------------------------------------------------------------------------
tcCallCaseTry :: (Unif r, PP a)
  => TCEnv r -> AnnTc r -> a -> [RType r] -> [(IntCallSite, RType r)]
  -> TCM r (Maybe (IntCallSite, RSubst r, RType r))
--------------------------------------------------------------------------------
tcCallCaseTry _ _ _ _ []
  = return Nothing

tcCallCaseTry γ l fn ts ((i,ft):fts)
  = runFailM (do
      (βs, its1, _)  <- instantiateFTy l (tce_ctx γ) fn ft
      ts1            <- zipWithM (instantiateTy l $ tce_ctx γ) [1..] ts
      θ              <- unifyTypesM (srcPos l) γ ts1 its1
      let (ts2, its2) = apply θ (ts1, its1)
      let (ts, cs)    = unzip [(t, c) | (t, BTV _ _ (Just c)) <- zip ts2 βs]
      if not (and (zipWith (isSubtype γ) ts cs))
        then
          return Nothing
        else
          if and (zipWith (isSubtype γ) ts2 its2)
            then return $ Just θ
            else return Nothing
    )
    >>= \case
          Right (Just θ') -> return $ Just (i, θ', ft)
          _               -> tcCallCaseTry γ l fn ts fts

--------------------------------------------------------------------------------
tcCallCase :: (PP a, Unif r)
  => TCEnv r -> AnnTc r -> a -> [(ExprSSAR r, RType r)] -> RType r -> TCM r ([ExprSSAR r], RType r)
--------------------------------------------------------------------------------
tcCallCase γ@(tce_ctx -> ξ) l fn (unzip -> (es, ts)) ft
  = do  (βs, rhs, ot)   <- instantiateFTy l ξ fn ft
        lhs             <- zipWithM (instantiateTy l ξ) [1..] ts
        θ               <- unifyTypesM (srcPos l) γ lhs rhs
        let θβ           = fromList [ (TV s l, t) | BTV s l (Just t) <- βs ]
        let θ'           = θ `mappend` θβ
        let (lhs', rhs') = apply θ' $ (lhs, rhs)
        es'             <- zipWith3M (castM γ) es lhs' rhs'
        return           $ (es', apply θ ot)

--------------------------------------------------------------------------------
instantiateTy :: Unif r => AnnTc r -> IContext -> Int -> RType r -> TCM r (RType r)
--------------------------------------------------------------------------------
instantiateTy l ξ i (bkAll -> (bs, t)) = snd <$> freshTyArgs l i ξ (btvToTV <$> bs) t

--------------------------------------------------------------------------------
instantiateFTy :: (Unif r, PP a) => AnnTc r -> IContext -> a -> RType r -> TCM r ([BTVar r], [RType r], RType r)
--------------------------------------------------------------------------------
instantiateFTy l ξ fn ft@(bkAll -> (bs, t))
  = do  (βs, ft')   <- freshTyArgs l 0 ξ αs t
        (_, ts, t') <- maybe err return (bkFunNoBinds ft')
        return       $ (mkBs βs, ts, t')
    where
      αs      = [ TV α l     | BTV α l _ <- bs ]
      ls      = [ l          | BTV _ l _ <- bs ]
      cs      = [ c          | BTV _ _ c <- bs ]
      mkBs βs = [ BTV β' l c | (TV β' l, c) <- zip βs cs ]
      err     = tcError $ errorNonFunction (srcPos l) fn ft

--------------------------------------------------------------------------------
-- envJoin :: Unif r => AnnTc r -> TCEnv r -> TCEnvO r -> TCEnvO r -> TCM r (TCEnvO r)
--------------------------------------------------------------------------------
envJoin _ _ Nothing x           = return $ fmap (,[],[]) x
envJoin _ _ x Nothing           = return $ fmap (,[],[]) x
envJoin l γ (Just γ1) (Just γ2) =
  do  xts     <- catMaybes <$> mapM phiType xs
      r       <- foldM (envJoinStep l γ1 γ2 next) (γ,[],[]) xts
      return   $ Just r
  where
    phiType x = fmap (x,) <$> getPhiType l γ1 γ2 x
    xs        = concat [ xs | PhiVar xs  <- fFact l ] -- The PHI vars as reported by SSA - no casted vars
    next      = concat [ vs | PhiPost vs <- fFact l ] -- These need to be added to each branch

envJoinStep l γ1 γ2 next (γ, st01, st02) xt =
  case xt of
    (v, ta@(VI _ WriteLocal _ t)) ->
      case find ((v ==) . snd3) next of
        Just (_,va, vb) -> do
          st1     <- mkVdStmt l γ1 va vb t          -- FIRST BRANCH
          st2     <- mkVdStmt l γ2 va vb t          -- SECOND BRANCH
          θ       <- getSubst
          addAnn (fId l) $ PhiVarTC vb
          return   $ (tcEnvAdd vb ta $ γ { tce_names = apply θ (tce_names γ) }, st1:st01, st2:st02)
        Nothing -> error $ "Couldn't find " ++ ppshow v ++ " in " ++ ppshow next
    (v, ta) ->
          do  addAnn (fId l) $ PhiVarTC v
              return   $ (tcEnvAdd v ta $ γ { tce_names = tce_names γ }, [], [])

mkVdStmt l γ va vb t1 =
  do  t0           <- safeEnvFindTy l γ va
      rhs          <- VarRef <$> freshenAnn l <*> return va
      θ            <- unifyTypeM (srcPos l) γ t0 t1
      setSubst θ
      let (t0',t1') = apply θ (t0,t1)
      c1s          <- castM γ rhs t0' t1'
      vd1          <- mkVds vb c1s
      VarDeclStmt <$> freshenAnn l <*> return [vd1]
  where
    mkVds v e = VarDecl <$> freshenAnn l <*> return v <*> return (Just e)


--------------------------------------------------------------------------------
envLoopJoin :: Unif r => AnnTc r -> TCEnv r -> TCEnvO r -> TCM r (TCEnvO r)
--------------------------------------------------------------------------------
envLoopJoin _ γ Nothing   = return $ Just γ
envLoopJoin l γ (Just γl) =
  do  xts      <- toXts <$> mapM (getLoopNextPhiType l γ γl) xs
      _        <- mapM_ mkPhiAnn $ (\(x, VI _ _ _ t) -> (x, t)) <$> xts
      Just . tcEnvAdds xts . (`substNames` γ) <$> getSubst
  where
      xs             = phiVarsAnnot l
      substNames θ γ = γ { tce_names = apply θ (tce_names γ) }
      toXts ts       = [ (x,t) | (x, Just t) <- zip xs ts ]
      mkPhiAnn       = addAnn (fId l) . PhiVarTy

--
-- Using @tcEnvFindTyForAsgn@ here as the initialization status is
-- recorded in the initialization part of the output.
--
--------------------------------------------------------------------------------
getPhiType :: Unif r => AnnTc r -> TCEnv r -> TCEnv r -> Var r -> TCM r (Maybe (EnvEntry r))
--------------------------------------------------------------------------------
getPhiType l γ1 γ2 x =
  case (tcEnvFindTyForAsgn x γ1, tcEnvFindTyForAsgn x γ2) of
    (Just (VI l1 a1 i1 t1), Just (VI _ _ i2 t2)) ->
        do  θ     <- getSubst
            t     <- unifyPhiTypes l γ1 x t1 t2 θ
            return $ Just $ VI l1 a1 (i1 `mappend` i2) t
    (_, _) -> return Nothing
    -- bindings that are not in both environments are discarded

--------------------------------------------------------------------------------
getLoopNextPhiType :: Unif r => AnnTc r -> TCEnv r -> TCEnv r -> Var r -> TCM r (Maybe (EnvEntry r))
--------------------------------------------------------------------------------
getLoopNextPhiType l γ γl x =
  case (tcEnvFindTyForAsgn x γ, tcEnvFindTyForAsgn (mkNextId x) γl) of
    (Just (VI l1 a1 i1 t1), Just (VI _ _ i2 t2)) ->
        do  θ <- getSubst
            t <- unifyPhiTypes l γ x t1 t2 θ
            return $ Just $ VI l1 a1 (i1 `mappend` i2) t
    _ -> return Nothing
    -- bindings that are not in both environments are discarded

-- | `unifyPhiTypes`
--
--   * Special casing the situation where one the types in undefined.
--------------------------------------------------------------------------------
unifyPhiTypes :: Unif r => AnnTc r -> TCEnv r -> Var r
                       -> RType r -> RType r -> RSubst r -> TCM r (RType r)
--------------------------------------------------------------------------------
unifyPhiTypes l γ x t1 t2 θ
  | Left _ <- substE
  = fatal (errorEnvJoinUnif (srcPos l) x t1 t2) t1

  | Right θ' <- substE
  , on (==) (apply θ') t1 t2
  = setSubst θ' >> return (apply θ' t1)

  | Right θ' <- substE
  = do  setSubst θ'
        let t1' = apply θ' t1
            t2' = apply θ' t2
            to  = go t1' t2'
        case to of
          Right t -> return t
          Left  e -> tcError e

  | otherwise
  = fatal (errorEnvJoin (srcPos l) x (toType t1) (toType t2)) t1
  where
    go a b  | isTUndef a   = Right $ orUndef b
            | isTUndef b   = Right $ orUndef a
            | isTNull a    = Right $ orNull b
            | isTNull b    = Right $ orNull a
            | maybeNull a  = orNull  <$> go (cleanNull a) b
            | maybeNull b  = orNull  <$> go a (cleanNull b)
            | maybeUndef a = orUndef <$> go (cleanUndef a) b
            | maybeUndef b = orUndef <$> go a (cleanUndef b)
            | a `equiv` b  = Right a
            | otherwise    = Left $ errorEnvJoin (srcPos l) x (toType t1) (toType t2)

    substE      = unifys (srcPos l) γ θ [t1] [t2]
    clean       = cleanNull . cleanUndef
    cleanNull   = mkUnion . filter (not . isTNull)  . bkUnion
    cleanUndef  = mkUnion . filter (not . isTUndef) . bkUnion
    equiv a b   = isSubtype γ a b && isSubtype γ b a
    maybeNull   = any isTNull . bkUnion
    maybeUndef  = any isTNull . bkUnion

--------------------------------------------------------------------------------
postfixStmt :: a -> [Statement a] -> Statement a -> Statement a
--------------------------------------------------------------------------------
postfixStmt _ [] s = s
postfixStmt l ss s = BlockStmt l $ expandBlock $ [s] ++ ss

--------------------------------------------------------------------------------
expandBlock :: [Statement t] -> [Statement t]
--------------------------------------------------------------------------------
expandBlock = concatMap f
  where
    f (BlockStmt _ ss) = ss
    f s                = [s ]

-- Local Variables:
-- flycheck-disabled-checkers: (haskell-liquid)
-- End:
