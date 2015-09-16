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

module Language.Rsc.Typecheck.Checker (verifyFile, typeCheck) where

import           Control.Applicative                (pure, (<$>), (<*>))
import           Control.Arrow                      (first, (***))
import           Control.Exception                  (throw)
import           Control.Monad
import qualified Data.Foldable                      as FD
import           Data.Function                      (on)
import           Data.Generics
import qualified Data.IntMap.Strict                 as I
import           Data.List                          (find, nub, sortBy)
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
        return   $ p { code  = Src fs'', maxId = ast_cnt }
  where
    γ = initGlobalEnv p cha


-- | Patch annotation on the AST
--------------------------------------------------------------------------------
patch :: Unif r => [Statement (AnnTc r)] -> TCM r [Statement (AnnTc r)]
--------------------------------------------------------------------------------
patch fs
  = do  (m, θ) <- (,) <$> getAnns <*> getSubst
        -- patch code with annotations gathered in `m`
        return $ map ((fmap (pa m)) . (apply θ)) fs
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
tcStmt γ (ExprStmt l1 (AssignExpr l2 OpAssign (LVar lx x) e))
  = do (e', g) <- tcAsgn l1 γ (Id lx x) e
       return $ (ExprStmt l1 (AssignExpr l2 OpAssign (LVar lx x) e'), g)

-- e1.f = e2
tcStmt γ (ExprStmt l (AssignExpr l2 OpAssign (LDot l1 e1 f) e2))
  = do  z               <- runFailM ( tcExpr γ e1l Nothing )
        case z of
          Right (_,te1) -> tcSetProp (fmap snd $ getProp γ FieldAccess f te1)
          Left _        -> tcSetProp Nothing
  where
    e1l  = fmap (\a -> a { fFact = BypassUnique : fFact a }) e1
    opTy = setPropTy (F.symbol f)
    tcSetProp rhsCtx = do
      ([e1',e2'],_) <- tcNormalCallWCtx γ l BISetProp [(e1l, Nothing), (e2, rhsCtx)] opTy
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
  = (,Nothing) . ThrowStmt l . fst <$> tcExprW γ e


tcStmt γ s@(FunctionStmt _ _ _ _)
  = tcFun γ s

-- | class A<S...> [extends B<T...>] [implements I,J,...] { ... }
tcStmt γ (ClassStmt l x e is ce)
  = do  d  <- resolveTypeM l γ rn
        ms <- mapM (tcClassElt γ d) ce
        return $ (ClassStmt l x e is ms, Just γ)
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
tcVarDecl γ v@(VarDecl l x (Just e))
  = case envFindTy x (tce_names γ) of
      -- | Local
      Nothing ->
        do  (e', to) <- tcExprW γ e
            return $ (VarDecl l x (Just e'), tcEnvAddo γ x $ VI Local WriteLocal Initialized <$> to)

      Just (VI loc WriteLocal _ t) ->
        do  ([e'], Just t') <- tcNormalCallWCtx γ l "VarDecl-WL" [(e, Just t)] (localTy t)
            return $ (VarDecl l x $ Just e', Just $ tcEnvAdd x (VI loc WriteLocal Initialized t') γ)

      -- | Global
      Just (VI _ WriteGlobal _ t) ->
        -- PV: the global variable should be in scope already,
        --     since it is being hoisted to the beginning of the
        --     scope.
        first (VarDecl l x . Just) <$> tcAsgn l γ x e

      -- | Ambient
      Just (VI loc Ambient _ t) ->
        do  ([e'], Just t') <- tcNormalCallWCtx γ l "VarDecl-RO" [(e, Just t)] (localTy t)
            return $ (VarDecl l x $ Just e', Just $ tcEnvAdd x (VI loc Ambient Initialized t') γ)

      c -> fatal (unimplemented l "tcVarDecl" ("case: " ++ ppshow c)) (v, Just γ)

tcVarDecl γ v@(VarDecl _ x Nothing)
  = case envFindTy x (tce_names γ) of
      Just (VI loc Ambient _ t) ->
          return $ (v, Just $ tcEnvAdds [(x, VI loc Ambient Initialized t)] γ)
      _ ->  -- The rest should have fallen under the 'undefined' initialization case
          error "TC-tcVarDecl: this shouldn't happen"


--------------------------------------------------------------------------------
tcClassElt :: Unif r => TCEnv r -> TypeDecl r -> ClassElt (AnnTc r) -> TCM r (ClassElt (AnnTc r))
--------------------------------------------------------------------------------
-- | Constructor
tcClassElt γ (TD sig@(TS _ (BGen _ bs) _) ms) (Constructor l xs body)
  = do  its    <- tcFunTys l ctor xs ctorTy
        body'  <- foldM (tcCallable γ' l ctor xs) body its
        return  $ Constructor l xs body'
  where
    γ'     = γ
           & initClassInstanceEnv sig
           & initClassCtorEnv sig
           -- TODO: TEST THIS
    ctor   = builtinOpId BICtor
    ctorTy | Just t <- tm_ctor ms = mkAll bs t
           | otherwise = die $ unsupportedNonSingleConsTy (srcPos l)

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
  | Just (MI _ _ t) <- F.lookupSEnv (F.symbol x) $ tm_smeth ms
  = do  its <- tcFunTys l x xs t
        body' <- foldM (tcCallable γ l x xs) body its
        return $ MemberMethDecl l True x xs body'
  | otherwise
  = fatal (errorClassEltAnnot (srcPos l) (sig) x) c

-- | Instance method
--
-- TODO: check method mutability
--
tcClassElt γ0 (TD sig ms) c@(MemberMethDecl l False x xs bd)
  | Just (MI _ m t) <- F.lookupSEnv (F.symbol x) (tm_meth ms)
  = do  let γ = γ0
              & initClassInstanceEnv sig    -- Adds class bounded type params
              & initClassMethEnv m sig      -- Adds method's mutability and type for 'this'
        its <- tcFunTys l x xs t
        bd' <- foldM (tcCallable γ l x xs) bd its
        return $ MemberMethDecl l False x xs bd'
  | otherwise
  = fatal (errorClassEltAnnot (srcPos l) (sig) x) c


--------------------------------------------------------------------------------
tcAsgn :: Unif r
       => AnnTc r -> TCEnv r -> Id (AnnTc r) -> ExprSSAR r -> TCM r (ExprSSAR r, TCEnvO r)
--------------------------------------------------------------------------------
tcAsgn l γ x e
  = do (e', to) <-  tcExprTW γ e tRHS
       return $ (e', tcEnvAddo γ x $ VI Local asgn init <$> to)
    where
       (tRHS, asgn, init) = case tcEnvFindTyForAsgn x γ of
                              Just (VI _ a _ t) -> (Just t, a, Initialized)
                              Nothing           -> (Nothing, WriteLocal, Initialized)

tcEnvAddo _ _ Nothing  = Nothing
tcEnvAddo γ x (Just t) = Just $ tcEnvAdds [(x, t)] γ


--------------------------------------------------------------------------------
tcExprTW :: Unif r => TCEnv r -> ExprSSAR r -> Maybe (RType r) -> TCM r (ExprSSAR r, Maybe (RType r))
tcExprW  :: Unif r => TCEnv r -> ExprSSAR r ->                    TCM r (ExprSSAR r, Maybe (RType r))
tcExprWD :: Unif r => TCEnv r -> ExprSSAR r -> Maybe (RType r) -> TCM r (ExprSSAR r, RType r)
--------------------------------------------------------------------------------
tcExprTW γ e Nothing = tcExprW γ e
tcExprTW γ e (Just t) = (tcWrap $ tcExprT γ e t) >>= tcEW γ e

tcExprW γ e = (tcWrap $ tcExpr γ e Nothing) >>= tcEW γ e

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
tcEW :: Unif r => TCEnv r -> ExprSSAR r -> Either Error ((ExprSSAR r), b)
               -> TCM r ((ExprSSAR r), Maybe b)
--------------------------------------------------------------------------------
tcEW _ _ (Right (e', t')) = return $  (e', Just t')
tcEW γ e (Left err)       = (, Nothing) <$> deadcastM (tce_ctx γ) err e

-- Execute a and if it fails wrap e in a deadcast
tcWA γ e a = tcWrap a >>= \x -> case x of Right r -> return $ Right r
                                          Left  l -> Left <$> deadcastM (tce_ctx γ) l e

--------------------------------------------------------------------------------
tcExprT :: Unif r => TCEnv r -> ExprSSAR r -> RType r -> TCM r (ExprSSAR r, RType r)
--------------------------------------------------------------------------------
tcExprT γ e t
  = do ([e'], _) <- tcNormalCall γ (getAnnotation e) "tcExprT" [(e, Nothing)] ty
       return (e', t)
  where
    ty = TFun [B (F.symbol "x") t] tVoid fTop

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

  | Just t <- to, disallowed t
  = fatal (errorAssignsFields (fSrc l) x t) (e, tBot)

  | Just t <- to
  = return (e,t)

  | otherwise
  = fatal (errorUnboundId (fSrc l) x) (VarRef l x, tBot)
  where
    to = tcEnvFindTy x γ
    disallowed (TRef (Gen _ (m:_)) _) = isUM m
    disallowed _ = False

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

-- | < t > e
tcExpr γ ex@(Cast l e) _
  = case [ ct | UserCast ct <- fFact l ] of
      [t] -> mapFst (Cast l) <$> tcCast γ l e t
      _   -> die $  bugNoCasts (srcPos l) ex

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
tcCast :: Unif r => TCEnv r -> AnnTc r -> ExprSSAR r -> RType r -> TCM r (ExprSSAR r, RType r)
--------------------------------------------------------------------------------
tcCast γ l e tc
  = do  opTy        <- safeEnvFindTy l γ (builtinOpId BICastExpr)
        cid         <- freshCastId l
        let γ'       = tcEnvAdd (F.symbol cid) (VI Local WriteLocal Initialized tc) γ
        ([_,e'],t') <- tcNormalCall γ' l "user-cast" [(VarRef l cid, Nothing),(e, Just tc)] opTy
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
tcCall γ (ObjectLit l bs)
  = do (es', t) <- tcNormalCall γ l "ObjectLit" (es `zip` nths) (objLitTy l ps)
       return $ (ObjectLit l (zip ps es'), t)
  where
    (ps,es) = unzip bs

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
  = runFailM (tcExpr γ e Nothing) >>= go
  where
    -- Variadic call
    go (Right (_, t))
      | isVariadicCall f, v:vs <- es
      = do (e', _) <- tcExpr γ e Nothing
           (v':vs', t') <- tcNormalCall γ l em ((v:vs) `zip` nths) t
           return $ (CallExpr l (DotRef l1 e' f) (v':vs'), t')

    -- Accessing and calling a function field
    go (Right (_, t))
      | Just (o,ft) <- getProp γ FieldAccess f t, isTFun ft
      = do e' <- castM γ e t o
           (es',t') <- tcNormalCall γ l ex (args es) ft
           return $ (CallExpr l (DotRef l1 e' f) es', t')

    -- Invoking a method
    go (Right (_, t))
      | Just (o,ft) <- getProp γ MethodAccess f t, isTFun ft
      = do e' <- castM γ e t o
           (e'':es', t') <- tcNormalCall γ l  ex ((e':es) `zip` nths) ft
           return $ (CallExpr l (DotRef l1 e'' f) es', t')

    go (Right (_,_))
      = tcError $ errorCallNotFound (srcPos l) e f

    go (Left err)
      = fatal err (ex,tBot)

    isVariadicCall f = F.symbol f == F.symbol "call"
    args = (`zip` nths)

-- | `e(es)`
tcCall γ (CallExpr l e es)
  = do (e', ft0) <- tcExpr γ e Nothing
       (es', t)  <- tcNormalCall γ l e (es `zip` nths) ft0
       return $ (CallExpr l e' es', t)

tcCall _ e = fatal (unimplemented (srcPos e) "tcCall" e) (e, tBot)

--------------------------------------------------------------------------------
-- | @tcNormalCall@ resolves overloads and returns cast-wrapped versions of the arguments.
--------------------------------------------------------------------------------
tcNormalCall :: (Unif r, PP a)
             => TCEnv r
             -> AnnTc r
             -> a
             -> [(ExprSSAR r, Maybe (RType r))]
             -> RType r
             -> TCM r ([ExprSSAR r], RType r)
--------------------------------------------------------------------------------
tcNormalCall γ l fn etos ft0
  -- = do ets <- ltracePP l ("tcNormalCall " ++ ppshow fn) <$> T.mapM (uncurry $ tcExprWD γ) etos
  = do ets <- T.mapM (uncurry $ tcExprWD γ) etos
       z <- resolveOverload γ l fn ets ft0
       case z of
         Just (θ, ft) ->
             do addAnn (fId l) (Overload (tce_ctx γ) ft)
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
                -> TCM r (Maybe (RSubst r, RType r))
--------------------------------------------------------------------------------
-- | A function signature is compatible if:
--
--   * The supplied parameters have the same number of 'normal' arguments, i.e.
--     not including the 'self' argument.
--
--   * If the function requires a 'self' argument, the parameters provide one.
--
resolveOverload γ l fn es ft
  = case [ mkFun (vs, bs, τ) | (vs, bs, τ) <- catMaybes $ bkFun <$> extractCall γ ft
                             , length bs == length es ] of
                             -- , and $ zipWith (\b e -> matchTypes (b_type b) (snd e)) bs es ] of
      [t]    -> Just . (,t) <$> getSubst
      fts    -> tcCallCaseTry γ l fn (snd <$> es) fts
  where
    -- -- A deep check on the number of arguments
    -- matchTypes (TFun as _ _) (TFun bs _ _)
    --   | length as == length bs = and $ zipWith (on matchTypes b_type) as bs
    --   | otherwise              = False
    -- matchTypes _  _            = True


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
  => TCEnv r -> AnnTc r -> a -> [RType r] -> [RType r] -> TCM r (Maybe (RSubst r, RType r))
--------------------------------------------------------------------------------
tcCallCaseTry _ _ _ _ [] = return Nothing
tcCallCaseTry γ l fn ts (ft:fts)
  = runFailM (do  (_,its1,_) <- instantiateFTy l (tce_ctx γ) fn ft
                  ts1 <- zipWithM (instantiateTy l $ tce_ctx γ) [1..] ts
                  θ0 <- unifyTypesM (srcPos l) γ ts1 its1
                  if and $ zipWith (isSubtype γ) (apply θ0 ts1) (apply θ0 its1)
                    then return $ Just θ0
                    else return Nothing)
  >>= \case
        Right (Just θ) -> return $ Just (θ, ft)
        _              -> tcCallCaseTry γ l fn ts fts

--------------------------------------------------------------------------------
tcCallCase :: (PP a, Unif r)
           => TCEnv r
           -> AnnTc r
           -> a
           -> [(ExprSSAR r, RType r)]
           -> RType r
           -> TCM r ([ExprSSAR r], RType r)
--------------------------------------------------------------------------------
tcCallCase γ l fn ets ft
  = do let ξ            = tce_ctx γ
       (_,its1,ot)     <- instantiateFTy l ξ fn ft
       ts1             <- zipWithM (instantiateTy l $ tce_ctx γ) [1..] ts
       -- let (ts2, its2)  = balance ts1 its1
       θ               <- unifyTypesM (srcPos l) γ ts1 its1
       let (ts3,its3)   = mapPair (apply θ) (ts1, its1)
       es'             <- zipWith3M (castM γ) es ts3 its3
       return           $ (es', apply θ ot)
  where
    (es, ts)            = unzip ets

--
-- TODO: Add check on polymorphic bounds
--
--------------------------------------------------------------------------------
instantiateTy :: Unif r => AnnTc r -> IContext -> Int -> RType r -> TCM r (RType r)
--------------------------------------------------------------------------------
instantiateTy l ξ i t = freshTyArgs l i ξ (btvToTV <$> bs) t'
  where
    (bs, t') = bkAll t

--------------------------------------------------------------------------------
instantiateFTy :: (Unif r, PP a)
               => AnnTc r
               -> IContext
               -> a
               -> RType r
               -> TCM r ([BTVar r], [RType r], RType r)
--------------------------------------------------------------------------------
instantiateFTy l ξ fn ft
  = freshTyArgs l 0 ξ αs t >>= maybe err return . bkFunNoBinds
    where
      (bs, t) = bkAll ft
      αs = btvToTV <$> bs
      err = tcError $ errorNonFunction (srcPos l) fn ft

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

  | otherwise
  = fatal (errorEnvJoin (srcPos l) x (toType t1) (toType t2)) t1

    -- Right θ' | isTUndef t1 -> setSubst θ' >> return (apply θ' $ orUndef t2)
    --          | isTUndef t2 -> setSubst θ' >> return (apply θ' $ orUndef t1)
    --          | isTNull  t1 -> setSubst θ' >> return (apply θ' $ orNull  t2)
    --          | isTNull  t2 -> setSubst θ' >> return (apply θ' $ orNull  t1)
    --          | on (==) (apply θ') t1 t2   -> setSubst θ' >> return (apply θ' t1)
    --          | on (==) (apply θ') t1' t2' -> setSubst θ'
    --                                       >> return (apply θ' $ fillNullOrUndef t1 t2 t1)
    --          | otherwise   -> tcError $ errorEnvJoin (srcPos l) x (toType t1) (toType t2)
  where
    substE                  = unifys (srcPos l) γ θ [t1] [t2]
    -- (t1', t2')              = mapPair (mkUnion . clear . bkUnion) (t1, t2)
    -- fillNullOrUndef t1 t2 t | any isMaybeNull  [t1,t2] = fillUndef t1 t2 $ orNull t
    --                         | otherwise                = t
    -- fillUndef t1 t2 t       | any isMaybeUndef [t1,t2] = orUndef t
    --                         | otherwise                = t
    -- isMaybeUndef            = any isUndef . bkUnion
    -- isMaybeNull             = any isNull  . bkUnion
    -- clear ts                = [ t | t <- ts, not (isNull t) && not (isUndef t) ]


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
