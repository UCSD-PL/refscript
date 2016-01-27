{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Language.Rsc.Typecheck.Checker (verifyFile, typeCheck) where

import           Control.Applicative                (pure, (<$>), (<*>))
import           Control.Arrow                      (first, (***))
import           Control.Monad
import           Data.Function                      (on)
import           Data.Generics
import qualified Data.IntMap.Strict                 as I
import           Data.List                          (find, sortBy)
import           Data.Maybe                         (catMaybes, fromMaybe)
import qualified Data.Traversable                   as T
import           Language.Fixpoint.Misc             as FM
import qualified Language.Fixpoint.Types            as F
import           Language.Fixpoint.Types.Errors
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
import           Language.Rsc.Misc                  (dup, nths, single, zipWith3M, (&))
import           Language.Rsc.Names
import           Language.Rsc.Parser
import           Language.Rsc.Pretty
import           Language.Rsc.Program
import           Language.Rsc.SSA.SSA
import           Language.Rsc.Symbols
import           Language.Rsc.SystemUtils
import           Language.Rsc.Typecheck.Environment
import           Language.Rsc.Typecheck.Sub
import           Language.Rsc.Typecheck.Subst
import           Language.Rsc.Typecheck.TCMonad
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Typecheck.Unify
import           Language.Rsc.Types
import           Language.Rsc.TypeUtilities

-- import           Debug.Trace                        hiding (traceShow)

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
    downErrors = [ errorDownCast l t | TypeCast _ t <- facts]

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
        return $ map (\f -> apply θ $ fmap (pa m) f) fs
  where
    pa m (FA i l f)   = FA i l $ f ++ filter vld (I.findWithDefault [] i m)
    vld TypInst{}     = True
    vld Overload{}    = True
    vld EltOverload{} = True
    vld PhiVarTy{}    = True
    vld PhiVarTC{}    = True
    vld PhiVar{}      = True
    vld TypeCast{}    = True
    vld DeadCast{}    = True
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
  = do  γ'    <- pure (initCallableEnv l γ f fty xs body)
        body' <- addReturnStmt l γ' body
        tcFunBody γ' l body'

addReturnStmt l γ body
  | isTVoid (tcEnvFindReturn γ)
  = (body ++) . single <$> (`ReturnStmt` Nothing) <$> freshenAnn l
  | otherwise
  = return body

tcFunBody γ l body
  = do  z <- tcStmts γ body
        case z of
          (b, Just _) | rt /= tVoid -> fatal er b
          (b, _     ) | otherwise   -> return b
  where
    er = errorMissingReturn (srcPos l)
    rt = tcEnvFindReturn γ

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
tcStmt γ@(envCHA -> c) (ExprStmt l (AssignExpr l2 OpAssign (LDot l1 e1 f) e2))
  = do  (e1'', te1) <- tcExpr γ e1' Nothing
        case (getMutability c te1, e1) of
          (Just m, _        )
            | isSubtype l γ m tMU || isUQ m ->
              case getProp l γ f te1 of
                Left e        -> tcError e
                Right (map snd -> fs) ->
                    do  (e2', _) <- foldM (tcSetPropMut γ l f te1) (e2, tBot) fs
                        return (mkAsgnExp e1'' e2', Just γ)

          (Just m, ThisRef _)
            | isSubtype l γ m tAF ->
              case getProp l γ f te1 of
                Left e        -> tcError e
                Right (map snd -> fs) ->
                    do  (e2', _) <- foldM (tcSetPropMut γ l f te1) (e2, tBot) fs
                        return (mkAsgnExp e1'' e2', Just γ)

          _ ->
              case getProp l γ f te1 of
                Left e        -> tcError e
                Right (map snd -> fs) ->
                    do  (e2', _) <- foldM (tcSetPropImm γ l f te1) (e2, tBot) fs
                        return (mkAsgnExp e1'' e2', Just γ)
  where
    e1' = fmap (\a -> a { fFact = BypassUnique : fFact a }) e1
    mkAsgnExp e1_ e2_ = ExprStmt l (AssignExpr l2 OpAssign (LDot l1 e1_ f) e2_)

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
                z1         <- envJoin l γ γ1 γ2
                case z1 of
                  Just (γ3',s1s,s2s) ->
                    do  l1 <- freshenAnn l
                        l2 <- freshenAnn l
                        return (IfStmt l e' (postfixStmt l1 s1s s1')
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
          do  _ <- unifyTypeM (srcPos l) γ t tBool
              phiTys   <- mapM (safeEnvFindTy l γ) phis
              (b', γl) <- tcStmt (tcEnvAdds (zip xs (SI Local WriteLocal Initialized <$> phiTys)) γ) b
              tcWA γ dummyExpr (envLoopJoin l γ γl) >>= \case
                Left e     -> return (ExprStmt  l e    , γl)
                Right γout -> return (WhileStmt l c' b', γout)
  where
    dummyExpr     = StringLit l "DUMMY_LOOP_REPLACEMENT"
    xs            = [ mkNextId x | x <- phis ]
    phis          = phiVarsAnnot l

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
        return     (ThrowStmt l e', Nothing)


tcStmt γ s@FunctionStmt{}
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
      -- Local (no type annotation)
      Nothing ->
        do  (e', to) <- tcExprW γ e
            return $ (VarDecl l x (Just e'), tcEnvAddo γ x $ SI Local WriteLocal Initialized <$> to)

      -- Local (with type annotation)
      Just (SI lc WriteLocal _ t) ->
        do  (e', t') <- tcExprT l "VarDecl" γ e t
            return $ (VarDecl l x $ Just e', Just $ tcEnvAdd x (SI lc WriteLocal Initialized t') γ)

      -- Global
      Just (SI _ WriteGlobal _ _) ->
        -- PV: the global variable should be in scope already,
        --     since it is being hoisted to the beginning of the
        --     scope.
        first (VarDecl l x . Just) <$> tcAsgn l γ x e

      -- ReadOnly
      Just (SI lc RdOnly _ t) ->
        do  ([e'], Just t') <- tcNormalCallWCtx γ l "VarDecl-RO" [(e, Just t)] (idTy t)
            return $ (VarDecl l x $ Just e', Just $ tcEnvAdd x (SI lc RdOnly Initialized t') γ)

      c -> fatal (unimplemented l "tcVarDecl" ("case: " ++ ppshow c)) (v, Just γ)

tcVarDecl γ v@(VarDecl l x Nothing)
  = case envFindTy x (tce_names γ) of
      Just (SI lc Ambient _ t) ->
          return $ (v, Just $ tcEnvAdds [(x, SI lc Ambient Initialized t)] γ)
      _ -> fatal (bug l "TC-tcVarDecl: this shouldn't happen") (v, Just γ)


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
    viExit = SI Local Ambient Initialized $ mkFun (bs, xts, ret)
    ret   = thisT
    xts   = sortBy c_sym [ B x t' | (x, FI _ _ t) <- F.toListSEnv (i_mems ms)
                                  , let t' = t ] -- unqualifyThis g0 thisT t ]
    -- out   = [ f | (f, FI _ Final _) <- F.toListSEnv (i_mems ms) ]
    -- v_sym = F.symbol $ F.vv Nothing
    c_sym = on compare b_sym
    ctorTy = fromMaybe (die $ unsupportedNonSingleConsTy (srcPos l)) (tm_ctor ms)

-- | Static field
tcClassElt γ (TD sig ms) c@(MemberVarDecl l True x (Just e))
  | Just (FI _ _ t) <- F.lookupSEnv (F.symbol x) $ i_mems ms
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
  | Just (MI _ mts) <- F.lookupSEnv (F.symbol x) $ s_mems ms
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
  | Just (MI _ mts) <- F.lookupSEnv (F.symbol x) (i_mems ms)
  = do  let (ms', ts) = unzip mts
        its          <- tcFunTys l x xs $ mkAnd ts
        let mts'      = zip ms' its
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
    eThis       = (idThis, SI Local RdOnly Initialized tThis)



--------------------------------------------------------------------------------
tcAsgn :: Unif r
       => AnnTc r -> TCEnv r -> Id (AnnTc r) -> ExprSSAR r -> TCM r (ExprSSAR r, TCEnvO r)
--------------------------------------------------------------------------------
tcAsgn l γ x e
  | Just (SI _ a _ t) <- tcEnvFindTyForAsgn x γ
  = do  eitherET  <- tcWrap (tcExprT l "assign" γ e t)
        (e', to)  <- tcEW γ e eitherET
        return     $ (e', tcEnvAddo γ x $ SI Local a Initialized <$> to)
  | otherwise
  = do (e', to)   <- tcExprW γ e
       return      $ (e', tcEnvAddo γ x $ SI Local WriteLocal Initialized <$> to)


tcSetPropMut γ l f t0 (e, t') (FI _ a t)
  | a == Final
  = fatal (errorFinalField l f t0) (e, t')
  | otherwise
  = tcExprT l BISetProp γ e t

tcSetPropImm γ l f t0 (e, t') (FI _ a t)
  | a == Final
  = fatal (errorImmutableRefAsgn l f t0) (e, t')
  | otherwise
  = tcExprT l BISetProp γ e t


-- | `tcExprT l fn γ e t` checks expression @e@ under environment @γ@
--   enforcing a type @t@.
--------------------------------------------------------------------------------
tcExprT :: (Unif r , PP f)
        => AnnTc r -> f -> TCEnv r -> ExprSSAR r -> RType r -> TCM r (ExprSSAR r, RType r)
--------------------------------------------------------------------------------
tcExprT l fn γ e t
  = do ([e'], _) <- tcNormalCall γ l fn [(e, Just t)] (idTy t)
       return (e', t)

tcEnvAddo _ _ Nothing  = Nothing
tcEnvAddo γ x (Just t) = Just (tcEnvAdds [(x, t)] γ)

--------------------------------------------------------------------------------
-- tcExprW  :: Unif r => TCEnv r -> ExprSSAR r ->                    TCM r (ExprSSAR r, Maybe (RType r))
--------------------------------------------------------------------------------
tcExprW γ e
  = do  t <- tcWrap (tcExpr γ e Nothing)
        tcEW γ e t

-- | `tcExprWD γ e t` checks expression @e@ under environment @γ@ (with an
--   optional contextual type @t@ potentially wrapping it in a cast.
--------------------------------------------------------------------------------
tcExprWD :: Unif r => TCEnv r -> ExprSSAR r -> Maybe (RType r) -> TCM r (ExprSSAR r, RType r)
--------------------------------------------------------------------------------
tcExprWD γ e t
  = do  eet <- tcWrap (tcExpr γ e t)
        et  <- tcEW γ e eet
        case et of
          (e', Just t) -> return (e', t)
          (e', _     ) -> return (e', tNull)

tcNormalCallW γ l o es t
  = (tcWrap $ tcNormalCall γ l o (es `zip` nths) t) >>= \case
      Right (es', t') -> return (es', Just t')
      Left e -> (,Nothing) <$> mapM (deadcastM (tce_ctx γ) [e]) es

tcNormalCallWCtx γ l o es t
  = (tcWrap $ tcNormalCall γ l o es t) >>= \case
      Right (es', t') -> return (es', Just t')
      Left err -> (,Nothing) <$> mapM (deadcastM (tce_ctx γ) [err]) (fst <$> es)

tcRetW γ l (Just e)
  | VarRef lv x <- e, Just t <- tcEnvFindTy x γ, isUMRef t

  = do  re  <- pure $ Just $ CallExpr l (VarRef l fn) [e']
        tw  <- tcRetW (newEnv t) l re
        case tw of
          -- e' won't be cast
          (ReturnStmt _ (Just (CallExpr _ _ [e''])), eo)
              -> return (ReturnStmt l (Just e''), eo)

          _   -> die $ errorUqMutSubtyping (srcPos l) e t rt

  | otherwise
  = do  c     <- tcWrap (tcNormalCall γ l "return" [(e, Just rt)] ft)
        case c of
          Right ([e'], _) -> return (ReturnStmt l (Just e'), Nothing)
          Left err        -> do de <- deadcastM (tce_ctx γ) [err] e
                                return (ReturnStmt l (Just de), Nothing)
  where
    rt        = tcEnvFindReturn γ
    ft        = returnTy rt True

    newEnv t  = tcEnvAdd fn (SI Local Ambient Initialized $ finalizeTy t) γ
    fn        = Id l "__finalize__"
    e'        = fmap (\a -> a { fFact = BypassUnique : fFact a }) e

tcRetW γ l Nothing
  = do (_, _) <- tcNormalCall γ l "return" [] $ returnTy (tcEnvFindReturn γ) False
       return  $ (ReturnStmt l Nothing, Nothing)

--------------------------------------------------------------------------------
-- tcEW :: Unif r => TCEnv r -> ExprSSAR r -> Either Error ((ExprSSAR r), b)
--                -> TCM r ((ExprSSAR r), Maybe b)
--------------------------------------------------------------------------------
tcEW _ _ (Right (e', t')) = return (e', Just t')
tcEW γ e (Left er)        = (,Nothing) <$> deadcastM (tce_ctx γ) [er] e

-- Execute a and if it fails wrap e in a deadcast
tcWA γ e a = tcWrap a >>= \x -> case x of Right r -> return $ Right r
                                          Left  l -> Left <$> deadcastM (tce_ctx γ) [l] e

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
  -- | `undefined`
  | F.symbol x == F.symbol "undefined"
  = return (e, tUndef)

  -- | `arguments`
  | F.symbol x == F.symbol "arguments"
  = tcExpr γ (VarRef l (Id (getAnnotation x) ("arguments_" ++ show (envFnId γ)))) to

  -- | Unique reference exception
  | Just t <- to, not $ null [ () | BypassUnique <- fFact l ]
  = return (e,t)

  -- | Ignore the `cast` variable
  | Just t <- to, isCastId x
  = return (e,t)

  -- | Regural bound variable
  | Just t <- to
  = return (e,t)

  | otherwise
  = fatal (errorUnboundId (fSrc l) x) (VarRef l x, tBot)
  where
    to = tcEnvFindTy x γ

tcExpr γ ex@(CondExpr l e e1 e2) (Just t)
  = do opTy         <- safeEnvFindTy l γ (builtinOpId BITruthy)
       ([e'], z)    <- tcNormalCallW γ l BITruthy [e] opTy
       case z of
         Just _  ->
            do  (e1', t1) <- tcExprWD γ e1 (Just t)
                (e2', t2) <- tcExprWD γ e2 (Just t)
                e1''      <- castM γ e1 t1 t
                e2''      <- castM γ e2 t2 t
                return     $ (CondExpr l e' e1'' e2'', t)
         _  -> error "TODO: error tcExpr condExpr"

tcExpr γ e@(CondExpr l _ _ _) Nothing
  = tcError $ unimpCondExpCtxType l e

tcExpr γ e@(PrefixExpr _ _ _) s
  = tcCall γ e s

tcExpr γ e@(InfixExpr _ _ _ _) s
  = tcCall γ e s

-- | f(e)
tcExpr γ e@(CallExpr _ _ _) s
  = tcCall γ e s

-- | [e1,..,en]
tcExpr γ e@(ArrayLit l es) to
  = arrayLitTy l γ e to (length es) >>= \case
      Left ee    -> fatal ee (e, tBot)
      Right opTy -> first (ArrayLit l) <$> tcNormalCall γ l BIArrayLit (es `zip` nths) opTy

-- | { f1: e1, ..., fn: tn }
tcExpr γ ex@(ObjectLit l pes) to
  = do  (pes', tys) <- unzip <$> mapM tce ets
        return (ObjectLit l pes', TObj tIM (typeMembersFromList tys) fTop)
  where
    ctxtys      | Just t <- to
                = i_mems $ typeMembersOfType (envCHA γ) t
                | otherwise
                = mempty
    ets         = [ (p, e, F.lookupSEnv (F.symbol p) ctxtys) | (p, e) <- pes ]

    tce (p,e,x) | Just (FI o a t)   <- x
                = (***) (p,) ((F.symbol p,) . (FI o a))       <$> tcExpr γ e (Just t)
                | otherwise
                -- Default optionality and assignability for field
                = (***) (p,) ((F.symbol p,) . (FI Req Final)) <$> tcExpr γ e Nothing

-- | <T>e
tcExpr γ ex@(Cast l e) _
  | [t] <- [ ct | UserCast ct <- fFact l ]
  = first (Cast l) <$> tcCast l γ e t
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
  = case [ t_ | TypeCast ξ t_ <- fFact l, tce_ctx γ == ξ ] of
      [ ] -> do (e', t)    <- tcExpr γ e to
                case e' of
                  Cast_ {} -> die (bugNestedCasts (srcPos l) e)
                  _        -> (,t) . (`Cast_` e') <$> freshenAnn l

      [t] -> pure (Cast_ l e, ofType t)

      _   -> die $ bugNestedCasts (srcPos l) e

-- | e.f
tcExpr γ e@(DotRef _ _ _) s
  = tcCall γ e s

-- | e1[e2]
tcExpr γ e@(BracketRef _ _ _) s
  = tcCall γ e s

-- | e1[e2] = e3
tcExpr γ e@(AssignExpr _ OpAssign (LBracket _ _ _) _) s
  = tcCall γ e s

-- | new C(e, ...)
tcExpr γ e@(NewExpr _ _ _) s
  = tcCall γ e s

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
  = do  ([e'],t')   <- tcNormalCall γ l "user-cast" [(e, Just tc)] (castTy tc)
        return       $ (e', t')

--------------------------------------------------------------------------------
tcCall :: Unif r => TCEnv r -> ExprSSAR r -> Maybe (RType r) -> TCM r (ExprSSAR r, RType r)
--------------------------------------------------------------------------------

-- | `o e`
tcCall γ c@(PrefixExpr l o e) _
  = do opTy <- safeEnvFindTy l γ (prefixOpId o)
       z    <- tcNormalCall γ l o [(e, Nothing)] opTy
       case z of
         ([e'], t) -> return (PrefixExpr l o e', t)
         _ -> fatal (impossible (srcPos l) "tcCall PrefixExpr") (c, tBot)

-- | `e1 o e2`
tcCall γ c@(InfixExpr l o@OpInstanceof e1 e2) _
  = do (e2',t) <- tcExpr γ e2 Nothing
       case t of
         TClass (BGen (QN _ x) _)  ->
            do  opTy <- safeEnvFindTy l γ (infixOpId o)
                ([e1',_], t') <- let args = [e1, StringLit l2 (F.symbolSafeString x)] `zip` nths in
                                 tcNormalCall γ l o args opTy
                      -- TODO: add qualified name
                return (InfixExpr l o e1' e2', t')
         _  -> fatal (unimplemented (srcPos l) "tcCall-instanceof" $ ppshow e2) (c,tBot)
  where
    l2 = getAnnotation e2

tcCall γ c@(InfixExpr l o e1 e2) _
  = do opTy <- safeEnvFindTy l γ (infixOpId o)
       z    <- tcNormalCall γ l o ([e1,e2] `zip` nths) opTy
       case z of
         ([e1', e2'], t) -> return (InfixExpr l o e1' e2', t)
         _ -> fatal (impossible (srcPos l) "tcCall InfixExpr") (c, tBot)

-- | `e1[e2]` Special case for Enumeration, and object literals with numeric
-- or string arguments.
--
tcCall γ e@(BracketRef l e1 e2) _
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
tcCall γ e@(AssignExpr l OpAssign (LBracket l1 e1 e2) e3) _
  = do opTy <- safeEnvFindTy l γ (builtinOpId BIBracketAssign)
       z <- tcNormalCall γ l BIBracketAssign ([e1,e2,e3] `zip` nths) opTy
       case z of
         ([e1', e2', e3'], t) -> return (AssignExpr l OpAssign (LBracket l1 e1' e2') e3', t)
         _ -> fatal (impossible (srcPos l) "tcCall AssignExpr") (e, tBot)

-- | `new e(e1,...,en)`
tcCall γ c@(NewExpr l e es) _
  = do (e',t) <- tcExpr γ e Nothing
       case extractCtor γ t of
         Just ct ->
            do (es', t') <- tcNormalCall γ l "new" (es `zip` nths) ct
               return (NewExpr l e' es', t')
         _ -> fatal (errorConstrMissing (srcPos l) t) (c, tBot)

-- tcCall γ c@(NewExpr l e es) Nothing
--   = tcError $ errorNewExprCtxType l c

-- | e.f
--
tcCall γ ef@(DotRef l e f) _
  = runFailM (tcExpr γ e Nothing) >>= checkAccess
  where
    checkAccess (Right (_, tRcvr))
      | isArrayLen tRcvr = checkArrayLength
      | otherwise        = checkProp (getProp l γ f tRcvr)
    checkAccess (Left er) = fatal er (ef, tBot)

    -- `array.length`
    checkArrayLength
      = do  l1  <- freshenAnn l
            l2  <- freshenAnn l
            i   <- pure $ Id l2 "__getLength"
            tcExpr γ (CallExpr l1 (DotRef l e i) []) Nothing

    -- Normal property access
    checkProp (Left er)   = tcError er
    checkProp (Right tfs) = adjustOpt tfs . fst <$> tcExprT l ef γ e (rcvrTy tfs)

    -- Add `or undef` in case of an optional field access
    adjustOpt tfs e_
      | isOptional tfs
      = (DotRef l e_ f, tOr $ tUndef : typesOf tfs)
      | otherwise
      = (DotRef l e_ f, tOr $          typesOf tfs)

    isArrayLen t = isArrayType t && F.symbol f == F.symbol "length"

    isArrayType (TRef (Gen x _) _) = F.symbol x == F.symbol "Array"
    isArrayType (TOr _ _) = True
    isArrayType _ = False

    isOptional tfs = Opt `elem` [ o | (_, FI o _ _) <- tfs ]
    typesOf    tfs = [ t | (_, FI _ _ t) <- tfs]
    rcvrTy         = tOr . map fst

-- | `super(e1,...,en)`
--
--   XXX: there shouldn't be any `super(..)` calls after SSA ...
--
tcCall _ (CallExpr _ (SuperRef _)  _) _
  = error "BUG: super(..) calls should have been eliminated"

-- | `e.m(es)`
--
tcCall γ ex@(CallExpr l em@(DotRef l1 e f) es) _
  | isVariadicCall f = tcError (unimplemented l "Variadic" ex)
  | otherwise        = checkNonVariadic
  where
    -- Variadic check
    isVariadicCall f_ = F.symbol f_ == F.symbol "call"

    -- Non-variadic
    checkNonVariadic = runFailM (tcExpr γ e Nothing)
                   >>= checkWithRcvr

    -- Check receiver
    checkWithRcvr (Right (_, te)) = checkEltCall (getProp l γ f te) te
    checkWithRcvr (Left er      ) = fatal er (ex, tBot)

    checkEltCall (Right (unzip -> (ts, fs))) te
      = do  (e', _  )       <- tcExprT l1 em γ e (tOr ts)
            (es', t')       <- checkWithMut (getMutability (envCHA γ) te) te fs
            return           $ (CallExpr l (DotRef l1 e' f) es', t')
    checkEltCall (Left er) _ = tcError er

    checkWithMut (Just mR) te [m] = call te mR m
    checkWithMut (Just _ ) _  _   = error "TODO checkWithMut add error here"
    checkWithMut Nothing   te _   = fatal (bugGetMutability l te) (es, tBot)

    call tR _ (FI Req _ ft) = tcNormalCall γ l em (es `zip` nths) ft
    call tR _ (FI _   _ _ ) = fatal (errorCallOptional l f tR) (es, tBot)

    call tR mR (MI Req mts)
      = case [ t | (m, t) <- mts, isSubtype l γ mR m ] of
        [] -> fatal (errorMethMutIncomp l em mts mR) (es, tBot)
        ts -> tcNormalCall γ l em (es `zip` nths) (mkAnd ts)

    call tR _ _ = fatal (errorCallOptional l f tR) (es, tBot)

-- | `e(es)`
tcCall γ (CallExpr l e es) _
  = do (e', ft0) <- tcExpr γ e Nothing
       (es', t)  <- tcNormalCall γ l e (es `zip` nths) ft0
       return $ (CallExpr l e' es', t)

tcCall _ e _
  = fatal (unimplemented (srcPos e) "tcCall" e) (e, tBot)

--------------------------------------------------------------------------------
-- | @tcNormalCall@ resolves overloads and returns cast-wrapped versions of the arguments.
--------------------------------------------------------------------------------
tcNormalCall :: (Unif r, PP a)
             => TCEnv r -> AnnTc r -> a -> [(ExprSSAR r, Maybe (RType r))]
             -> RType r -> TCM r ([ExprSSAR r], RType r)
--------------------------------------------------------------------------------
tcNormalCall γ l fn etos ft0
  = do ets <- T.mapM (uncurry $ tcExprWD γ) etos
       z   <- resolveOverload γ l fn ets ft0
       case z of
         Just (i, θ, ft) ->
             do addAnn (fId l) (Overload (tce_ctx γ) i)
                addSubst l θ
                (es, ts) <- pure (unzip ets)
                tcWrap (tcCallCase γ l fn es ts ft) >>= \case
                  Right ets' -> return ets'
                  Left  er  -> (,tNull) <$> T.mapM (deadcastM (tce_ctx γ) [er] . fst) ets
         Nothing -> tcError $ uncurry (errorCallNotSup (srcPos l) fn ft0) (unzip ets)


--------------------------------------------------------------------------------
-- | `resolveOverload γ l fn es ts ft`
--
--   When resolving an overload there are two prossible cases:
--
--   1. There is only a single signature available: then return just this
--      signature regardless of subtyping constraints
--
--   2. There are more than one signature available: return all that pass the
--      subtype check (this is what tcCallCaseTry does).
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
resolveOverload γ l fn es ft =
  case validOverloads of
    [(i,t)] -> Just . (i,,t) <$> getSubst
    _       -> tcCallCaseTry γ l fn (snd <$> es) validOverloads
  where
    validOverloads = [ (i, mkFun s) | (i, s@(_, bs, _)) <- extractCall γ ft
                                    , length bs == length es ]

--------------------------------------------------------------------------------
-- | A successful pairing of formal to actual parameters will return `Just θ`,
--   where @θ@ is the corresponding substitution. If the types are not acceptable
--   this will return `Nothing`. In this case successful means:
--
--   1. Unifying without errors.
--
--   2. Passing the subtyping test.
--
--   The monad state is completely reversed after this function returns, thanks
--   to `runMaybeM`. We don't need to reverse the action of `instantiateFTy`.
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
      (ts2, its2)    <- pure $ apply θ (ts1, its1)
      (ts', cs)      <- pure $ unzip [(t, c) | (t, BTV _ _ (Just c)) <- zip ts2 βs]
      case not (and (zipWith (isSubtype l γ) ts' cs)) of
        True -> return Nothing
        _    -> case and (zipWith (isSubtype l γ) ts2 its2) of
                  True -> return $ Just θ
                  _    -> return Nothing
    )
    >>= \case
          Right (Just θ') -> return $ Just (i, θ', ft)
          _               -> tcCallCaseTry γ l fn ts fts

--------------------------------------------------------------------------------
tcCallCase :: (PP a, Unif r)
  => TCEnv r -> AnnTc r -> a -> [ExprSSAR r] -> [RType r] -> RType r -> TCM r ([ExprSSAR r], RType r)
--------------------------------------------------------------------------------
tcCallCase γ@(tce_ctx -> ξ) l fn es ts ft
  = do  (βs, rhs, ot)   <- instantiateFTy l ξ fn ft
        lhs             <- zipWithM (instantiateTy l ξ) [1..] ts
        θ               <- unifyTypesM (srcPos l) γ lhs rhs
        θβ              <- pure $ fromList [ (TV s l_, t) | BTV s l_ (Just t) <- βs ]
        θ'              <- pure $ θ `mappend` θβ
        (lhs', rhs')    <- pure $ apply θ' $ (lhs, rhs)
        es'             <- zipWith3M (castM γ) es lhs' rhs'
        return           $ (es', apply θ ot)

--------------------------------------------------------------------------------
instantiateTy :: Unif r => AnnTc r -> IContext -> Int -> RType r -> TCM r (RType r)
--------------------------------------------------------------------------------
instantiateTy l ξ i (bkAll -> (bs, t))
  = do  (fbs, t)  <- freshTyArgs l i ξ bs t
        -- TODO: need to take `fbs` into account, i.e. add the
        --       constraint to the environment
        return     $ t

--------------------------------------------------------------------------------
instantiateFTy :: (Unif r, PP a) => AnnTc r -> IContext -> a -> RType r
                                 -> TCM r ([BTVar r], [RType r], RType r)
--------------------------------------------------------------------------------
instantiateFTy l ξ fn ft@(bkAll -> (bs, t))
  = do  (fbs, ft')      <- freshTyArgs l 0 ξ bs t
        (_, ts, t')     <- maybe er return (bkFunNoBinds ft')
        return           $ (fbs, ts, t')
    where
      er      = tcError $ errorNonFunction (srcPos l) fn ft

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
    xs        = concat [ xs_ | PhiVar xs_ <- fFact l ] -- The PHI vars as reported by SSA - no casted vars
    next      = concat [ vs  | PhiPost vs <- fFact l ] -- These need to be added to each branch

envJoinStep l γ1 γ2 next (γ, st01, st02) xt =
  case xt of
    (v, ta@(SI _ WriteLocal _ t)) ->
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
      _        <- mapM_ mkPhiAnn $ (\(x, SI _ _ _ t) -> (x, t)) <$> xts
      Just . tcEnvAdds xts . (`substNames` γ) <$> getSubst
  where
      xs              = phiVarsAnnot l
      substNames θ γ_ = γ_ { tce_names = apply θ (tce_names γ) }
      toXts ts        = [ (x,t) | (x, Just t) <- zip xs ts ]
      mkPhiAnn        = addAnn (fId l) . PhiVarTy

--
-- Using @tcEnvFindTyForAsgn@ here as the initialization status is
-- recorded in the initialization part of the output.
--
--------------------------------------------------------------------------------
getPhiType :: Unif r => AnnTc r -> TCEnv r -> TCEnv r -> Var r -> TCM r (Maybe (SymInfo r))
--------------------------------------------------------------------------------
getPhiType l γ1 γ2 x =
  case (tcEnvFindTyForAsgn x γ1, tcEnvFindTyForAsgn x γ2) of
    (Just (SI l1 a1 i1 t1), Just (SI _ _ i2 t2)) ->
        do  θ     <- getSubst
            t     <- unifyPhiTypes l γ1 x t1 t2 θ
            return $ Just $ SI l1 a1 (i1 `mappend` i2) t
    (_, _) -> return Nothing
    -- bindings that are not in both environments are discarded

--------------------------------------------------------------------------------
getLoopNextPhiType :: Unif r => AnnTc r -> TCEnv r -> TCEnv r -> Var r -> TCM r (Maybe (SymInfo r))
--------------------------------------------------------------------------------
getLoopNextPhiType l γ γl x =
  case (tcEnvFindTyForAsgn x γ, tcEnvFindTyForAsgn (mkNextId x) γl) of
    (Just (SI l1 a1 i1 t1), Just (SI _ _ i2 t2)) ->
        do  θ <- getSubst
            t <- unifyPhiTypes l γ x t1 t2 θ
            return $ Just $ SI l1 a1 (i1 `mappend` i2) t
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
            | maybeNull a  = orNull  <$> go (stripNull a) b
            | maybeNull b  = orNull  <$> go a (stripNull b)
            | maybeUndef a = orUndef <$> go (stripUndefined a) b
            | maybeUndef b = orUndef <$> go a (stripUndefined b)
            | a `equiv` b  = Right a
            | otherwise    = Left $ errorEnvJoin (srcPos l) x (toType t1) (toType t2)

    substE      = unifys (srcPos l) γ θ [t1] [t2]
    equiv a b   = isSubtype l γ a b && isSubtype l γ b a
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
