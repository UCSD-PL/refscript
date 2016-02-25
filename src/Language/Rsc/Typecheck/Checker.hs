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

import           Control.Arrow                      (first, second, (***))
import           Control.Monad
import           Data.Default
import           Data.Function                      (on)
import           Data.Generics
import qualified Data.IntMap.Strict                 as I
import           Data.List                          (partition, sortBy)
import           Data.Maybe                         (fromMaybe)
import qualified Data.Traversable                   as T
import           Language.Fixpoint.Misc             as FM
import qualified Language.Fixpoint.Types            as F
import           Language.Fixpoint.Types.Errors
import           Language.Rsc.Annotations
import           Language.Rsc.AST
import           Language.Rsc.ClassHierarchy
import           Language.Rsc.CmdLine               (Config, noFailCasts)
import           Language.Rsc.Core.EitherIO
import           Language.Rsc.Environment
import           Language.Rsc.Errors
import           Language.Rsc.Locations
import           Language.Rsc.Lookup
import           Language.Rsc.Misc                  (nths, single, zipWith3M)
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
import           Language.Rsc.Visitor
import           Text.PrettyPrint.HughesPJ          (($+$))

-- import           Debug.Trace                        hiding (traceShow)

import qualified System.Console.CmdArgs.Verbosity   as V

--------------------------------------------------------------------------------
-- | Top-level Verifier
--------------------------------------------------------------------------------
verifyFile :: Config -> [FilePath] -> IO (UAnnSol a, FError)
--------------------------------------------------------------------------------
verifyFile cfg fs
  = runEitherIO (do p   <- parseRscFromFiles fs
                    cha <- liftEither (mkCHA p)
                    ssa <- ssaTransform p cha
                    tc  <- typeCheck cfg ssa cha
                    return tc)
  >>= either unsafe (pure . safe cfg)


unsafe errs = do putStrLn "\n\n\nErrors Found!\n\n"
                 return $ (NoAnn, errs)

safe cfg (Rsc {code = Src fs}) = (NoAnn, failCasts (noFailCasts cfg) fs)
    where
        failCasts True  _ = F.Safe
        failCasts False f = applyNonNull F.Safe F.Unsafe
                          $ concatMap castErrors
                          $ casts f

        casts :: Data r => [Statement (AnnTc r)] -> [AnnTc r]
        casts = foldStmts castVis () []
          where
            castVis = defaultVisitor { accExpr = aE }
            aE _ (Cast_ a _) = [a]
            aE _ _ = [ ]

--------------------------------------------------------------------------------
castErrors :: Unif r => AnnTc r -> [Error]
--------------------------------------------------------------------------------
castErrors (FA _ l facts) = downErrors
  where
    downErrors = [ errorDownCast l t | TypeCast _ t <- facts]

--------------------------------------------------------------------------------
typeCheck
  :: Unif r => Config -> TcRsc r -> ClassHierarchy r -> EitherIO FError (TcRsc r)
--------------------------------------------------------------------------------
typeCheck cfg pgm cha = EitherIO $ do
    startPhase Loud "TC"
    v <- V.getVerbosity
    return $ execute cfg v pgm (tcRsc pgm cha)

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
    pa m (FA i l f) = FA i l $ f ++ filter vld (I.findWithDefault [] i m)
    vld TypInst{}     = True
    vld Overload{}    = True
    vld EltOverload{} = True
    vld PhiVar{}      = True
    vld PhiLoopTC{}   = True
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
-- | skip
tcStmt γ s@(EmptyStmt _)
  = return (s, Just γ)

-- | interface Foo; (this definitions will be hoisted)
tcStmt γ s@(InterfaceStmt _ _)
  = return (s, Just γ)

-- | x = e
tcStmt γ s@(ExprStmt l (AssignExpr l1 OpAssign (LVar lx x) e))
  = tcWrap check >>= tcSW γ s
  where
    check =
      case info of
        Just (SI _ _ WriteGlobal Uninitialized t) -> do
            (e', _) <- tcExprT γ e t
            γ'      <- pure (tcEnvAdd (SI xSym Local WriteGlobal Initialized t) γ)
            return     (mkStmt e', Just γ')

        Just (SI _ _ WriteGlobal _ t) -> do
            (e', _) <- tcExprT γ e t
            return     (mkStmt e', Just γ)

        Just (SI _ _ a _ _) -> error $ "Could this happen- x = e ?? " ++ ppshow a

        Nothing -> do
            (e', t) <- tcExpr γ e Nothing
            γ'      <- pure (tcEnvAdd (SI xSym Local WriteLocal Initialized t) γ)
            return     (mkStmt e', Just γ')

    mkStmt  = ExprStmt l . AssignExpr l1 OpAssign (LVar lx x)
    xSym    = F.symbol x
    info    = tcEnvFindTyForAsgn x γ


-- | e1.f = e2
tcStmt γ s@(ExprStmt l (AssignExpr l2 OpAssign (LDot l1 e1 f) e2))
  = tcWrap check >>= tcSW γ s
  where
    check = do
      ue1       <- pure (enableUnique e1)
      (e1', t1) <- tcExpr γ ue1 Nothing
      m1o       <- pure (getMutability (envCHA γ) t1)
      m1fo      <- pure (getFieldMutability (envCHA γ) t1 f)
      case (m1o, m1fo) of
        (Just m1, Just m1f)
          | isSubtype γ m1f tMU || isSubtypeWithUq γ m1 tUQ ->
            case getProp l γ F.dummySymbol f t1 of
              Left e -> tcError e
              Right (unzip -> (ts, fs)) ->
                  do  e1''      <- castM γ e1' True t1 (tOr ts)
                      -- This is a consumable position --> UQ is allowed
                      (e2' , _) <- tcScan (tcExprT γ) e2 (map f_ty fs)
                      return (ExprStmt l (AssignExpr l2 OpAssign (LDot l1 e1'' f) e2'), Just γ)

          | otherwise -> tcError (errorImmutableRefAsgn l f e1 t1)

        (Nothing, _) -> tcError (errorExtractMut l t1 e1)
        (_, Nothing) -> tcError (errorExtractFldMut l f t1)

-- | e
tcStmt γ (ExprStmt l e)
  = do (e', to) <- tcExprW γ e
       return (ExprStmt l e', const γ <$> to)

-- | s1;s2;...;sn
tcStmt γ (BlockStmt l stmts)
  = do (stmts', g) <- tcStmts γ stmts
       return (BlockStmt l stmts', g)

-- | if b { s1 }
tcStmt γ (IfSingleStmt l b s)
  = tcStmt γ (IfStmt l b s (EmptyStmt l))

-- | if b { s1 } else { s2 }
tcStmt γ s@(IfStmt l e s1 s2)
  = do opTy         <- safeEnvFindTy l γ (builtinOpId BITruthy)
       ([e'], z)    <- tcNormalCallW γ l (builtinOpId BITruthy) [e] opTy
       case z of
         Just _  ->
            do  z1 <- tcStmt γ s1
                z2 <- tcStmt γ s2
                tcWrap (check e' z1 z2) >>= tcSW γ s

         Nothing -> return (IfStmt l e' s1 s2, Nothing)
  where
    check e' (s1', γ1) (s2', γ2) = do
        z <- envJoin l γ γ1 γ2
        return (IfStmt l e' s1' s2', z)

-- | while c { b }
tcStmt γ (WhileStmt l c b)
  = tcExprW γ c >>= \case
      (c', Nothing) -> return (WhileStmt l c' b, Nothing)
      (c', Just t)  -> do
          _         <- unifyTypeM (srcPos l) γ t tBool  -- meh ..
          (b', γ')  <- tcStmt γ b
          γ'        <- envLoopJoin l γ γ'
          return       (WhileStmt l c' b', Just γ')

-- | var x1 [ = e1 ]; ... ; var xn [= en];
tcStmt γ (VarDeclStmt l ds)
  = do (ds', z) <- tcSeq tcVarDecl γ ds
       return      (VarDeclStmt l ds', z)

-- | return e
tcStmt γ (ReturnStmt l (Just e)) =
    do  (e', _)   <- tcWrap check >>= tcEW γ e
        return       (ReturnStmt l (Just e'), Nothing)
  where
    rt    = tcEnvFindReturn γ
    check = do
        (e', t)   <- tcExpr γ (enableUnique e) (Just rt)
        θ         <- unifyTypeM (srcPos l) γ t rt
        (t', rt') <- pure (apply θ t, apply θ rt)
        e''       <- castM γ e' True t' rt'
        return       (e'', rt')

tcStmt _ (ReturnStmt l Nothing)
  = return (ReturnStmt l Nothing, Nothing)

-- | throw e
tcStmt γ (ThrowStmt l e)
  = do  (e', _) <- tcExprW γ e
        return     (ThrowStmt l e', Nothing)

-- | function (xs) { s }
tcStmt γ s@FunctionStmt{}
  = tcFun γ s

-- | class A<S...> [extends B<T...>] [implements I,J,...] { ... }
tcStmt γ (ClassStmt l x ce)
  = do  d  <- resolveTypeM l γ rn
        msS <- mapM (tcStaticClassElt γS d) ceS
        msI <- mapM (tcInstanceClassElt γI d) ceI
        return $ (ClassStmt l x (msS ++ msI), Just γ)
  where
    γS         = γ

    γI         = γ
    rn         = QN (envPath γ) (F.symbol x)
    (ceS, ceI) = partition isStaticClassElt ce

-- | module M { ... }
tcStmt γ (ModuleStmt l n body)
  = (ModuleStmt l n *** return (Just γ)) <$>  tcStmts (initModuleEnv γ n body) body

-- | enum M { ... }
tcStmt γ (EnumStmt l n body)
  = return (EnumStmt l n body, Just $ tcEnvAdd si γ)
  where
    si    = SI nSym exprt RdOnly init tEnum
    tEnum = TRef (Gen name []) fTop
    path  = envPath γ
    name  = QN path nSym
    nSym  = F.symbol n
    exprt = Exported      -- TODO: do this check
    init  = Initialized   -- TODO: do this check

-- OTHER (Not handled)
tcStmt _ s
  = convertError "tcStmt" s


--------------------------------------------------------------------------------
tcVarDecl :: Unif r => TCEnv r -> VarDecl (AnnTc r) -> TCM r (VarDecl (AnnTc r), TCEnvO r)
--------------------------------------------------------------------------------
tcVarDecl γ v@(VarDecl l x (Just e)) =
  case varDeclSymbol v of
    Left err -> tcError err

    Right Nothing ->
      -- Local (no type annotation)
        do  (e', to) <- tcExprW γ e
            sio      <- pure (SI (F.symbol x) Local WriteLocal Initialized <$> to)
            return (VarDecl l x (Just e'), tcEnvAddo γ x sio)

      -- Local (with type annotation)
    Right (Just (SI y lc WriteLocal _ t)) ->
        do  (e', t') <- tcExprT γ e t
            return ( VarDecl l x $ Just e'
                   , Just $ tcEnvAdd (SI y lc WriteLocal Initialized t') γ)

      -- Global
    Right (Just s@(SI _ _ WriteGlobal _ t)) ->
        -- PV: the global variable should be in scope already,
        --     since it is being hoisted to the beginning of the
        --     scope.
        do  (e', _)  <- tcExprT γ e t
            return    $ (VarDecl l x (Just e'), Just $ tcEnvAdd s γ)

      -- ReadOnly
    Right (Just (SI y lc RdOnly _ t)) ->
        do  ([e'], Just t') <- tcNormalCallWCtx γ l (builtinOpId BIExprT) [(e, Just t)] (idTy t)
            return ( VarDecl l x $ Just e'
                   , Just $ tcEnvAdd (SI y lc RdOnly Initialized t') γ)

    c -> fatal (unimplemented l "tcVarDecl" ("case: " ++ ppshow c)) (v, Just γ)

tcVarDecl γ v@(VarDecl l x Nothing) =
  case varDeclSymbol v of
    Left err -> tcError err

    Right (Just (SI y lc Ambient _ t)) ->
          return $ (v, Just $ tcEnvAdds [(x, SI y lc Ambient Initialized t)] γ)

    Right _ -> fatal (bug l "TC-tcVarDecl: this shouldn't happen") (v, Just γ)

--------------------------------------------------------------------------------
tcStaticClassElt
  :: Unif r
  => TCEnv r -> TypeDecl r -> ClassElt (AnnTc r) -> TCM r (ClassElt (AnnTc r))
--------------------------------------------------------------------------------

-- | Static method: The classes type parameters should not be included in env
--
tcStaticClassElt γ (TD sig _ ms) c@(MemberMethDecl l True x xs body) =
  case F.lookupSEnv (F.symbol x) (s_mems ms) of
    Just FI{} ->
        tcError $ bugStaticField l x sig
    Just (MI _ _ mts) -> do
        let t = tAnd $ map snd mts
        its <- tcFunTys l x xs t
        body' <- foldM (tcCallable γ l x xs) body its
        return $ MemberMethDecl l True x xs body'
    Nothing ->
        fatal (errorClassEltAnnot (srcPos l) (sig) x) c

-- | Static field
--
tcStaticClassElt γ (TD sig _ ms) c@(MemberVarDecl l True x (Just e)) =
  case F.lookupSEnv (F.symbol x) (s_mems ms) of
    Just MI{} ->
        tcError $ bugStaticField l x sig
    Just (FI _ _ _ t) -> do
        ([e'],_) <- tcNormalCall γ l (builtinOpId BIFieldInit) [(e, Just t)] (mkInitFldTy t)
        return $ MemberVarDecl l True x $ Just e'
    Nothing ->
        fatal (errorClassEltAnnot (srcPos l) sig x) c

tcStaticClassElt _ _ c@(MemberVarDecl l True x Nothing)
  = fatal (unsupportedStaticNoInit (srcPos l) x) c

tcStaticClassElt _ _ c = error (show $ pp "tcStaticClassElt - not a static element: " $+$ pp c)



--------------------------------------------------------------------------------
tcInstanceClassElt
  :: Unif r
  => TCEnv r -> TypeDecl r -> ClassElt (AnnTc r) -> TCM r (ClassElt (AnnTc r))
--------------------------------------------------------------------------------
-- | Constructor

tcInstanceClassElt γ typeDecl (Constructor l xs body) = do
    its    <- tcFunTys l ctor xs ctorTy
    body'  <- foldM (tcCallable γ' l ctor xs) body its
    return  $ Constructor l xs body'
  where
    TD sig@(TS _ (BGen nm bs) _) _ ms = typeDecl
    γ'     = tcEnvAdd viExit (initClassCtorEnv sig γ)
    ctor   = builtinOpId BICtor

    thisT   = TRef (Gen nm (map btVar bs)) fTop
    cExit   = builtinOpId BICtorExit
    viExit  = SI (F.symbol cExit) Local Ambient Initialized $ mkFun (bs, xts, ret)
    ret     = thisT

    xts      = case expandType def (envCHA γ) thisT of
                Just (TObj _ ms _ ) ->
                    sortBy c_sym [ B x Req t | (x, FI _ _ _ t) <- F.toListSEnv (i_mems ms) ]
                _ -> []

    c_sym = on compare (show . b_sym)     -- XXX: Symbolic compare is on Symbol ID
    ctorTy = fromMaybe (die $ unsupportedNonSingleConsTy (srcPos l)) (tm_ctor ms)


-- | Instance variable
--
tcInstanceClassElt _ _ m@(MemberVarDecl _ False _ Nothing)
  = return m
tcInstanceClassElt _ _ (MemberVarDecl l False x _)
  = die $ bugClassInstVarInit (srcPos l) x

-- | Instance method
--
--   TODO: check method mutability
--   TODO: The method's mutability should influence the type of tThis that is used
--         as a binder to this.
--   TODO: Also might not need 'tce_this'
--
tcInstanceClassElt γ (TD sig _ ms) c@(MemberMethDecl l False x xs bd)
  | Just (MI _ _ mts) <- F.lookupSEnv (F.symbol x) (i_mems ms)
  = do  let (ms', ts) = unzip mts
        its          <- tcFunTys l x xs $ tAnd ts
        let mts'      = zip ms' its
        bd'          <- foldM (\b (m,t) -> tcCallable (mkEnv m) l x xs b t) bd mts'
        return        $ MemberMethDecl l False x xs bd'
  | otherwise
  = fatal (errorClassEltAnnot (srcPos l) (sig) x) c
  where
    mkEnv m     = initClassMethEnv m sig γ

tcInstanceClassElt _ _ c = error (show $ pp "tcInstanceClassElt - not an instance element: " $+$ pp c)


-- | `tcExprT l fn γ e t` checks expression `e`
--
--     * under environment `γ`,
--
--     * against a type `t`.
--
--   Expressions that are ONLY contextually typed, do not need to be checked
--   against type `t`.
--------------------------------------------------------------------------------
tcExprT :: Unif r => TCEnv r -> ExprSSAR r -> RType r -> TCM r (ExprSSAR r, RType r)
--------------------------------------------------------------------------------
tcExprT γ e t
  | onlyCtxTyped e
  = tcExprWD γ e (Just t)
  | otherwise
  = do  ([e'], _) <- tcNormalCall γ l (builtinOpId BIExprT) [(e, Just t)] (idTy t)
        return (e', t)
  where
    l = getAnnotation e

tcEnvAddo _ _ Nothing  = Nothing
tcEnvAddo γ x (Just t) = Just (tcEnvAdds [(x, t)] γ)

--------------------------------------------------------------------------------
tcExprW  :: Unif r => TCEnv r -> ExprSSAR r -> TCM r (ExprSSAR r, Maybe (RType r))
--------------------------------------------------------------------------------
tcExprW γ e = tcWrap (tcExpr γ e Nothing) >>= tcEW γ e


-- | `tcExprWD γ e t` checks expression @e@ under environment @γ@.
--    (with an optional contextual type @t@ potentially wrapping it in a cast)
--
--------------------------------------------------------------------------------
tcExprWD :: Unif r
  => TCEnv r -> ExprSSAR r -> Maybe (RType r) -> TCM r (ExprSSAR r, RType r)
--------------------------------------------------------------------------------
tcExprWD γ e t
  = do  eet <- tcWrap (tcExpr γ e t)
        et  <- tcEW γ e eet
        case et of
          (e', Just t) -> return (e', t)
          (e', _     ) -> return (e', tBot)


-- | Wraps `tcNormalCall` with a cast-wrapping check of an error.
--
--------------------------------------------------------------------------------
tcNormalCallW
  :: Unif r => TCEnv r -> AnnTc r -> Identifier
            -> [ExprSSAR r] -> RType r
            -> TCM r ([ExprSSAR r], Maybe (RType r))
--------------------------------------------------------------------------------
tcNormalCallW γ l o es t
  = tcWrap (tcNormalCall γ l o (es `zip` nths) t) >>= \case
      Right (es', t') -> return (es', Just t')
      Left e -> (,Nothing) <$> mapM (deadcastM "tcNormalCallW" γ [e]) es

--------------------------------------------------------------------------------
tcNormalCallWCtx
  :: Unif r => TCEnv r -> AnnTc r -> Identifier
            -> [(ExprSSAR r, Maybe (RType r))] -> RType r
            -> TCM r ([ExprSSAR r], Maybe (RType r))
--------------------------------------------------------------------------------
tcNormalCallWCtx γ l o es t
  = tcWrap (tcNormalCall γ l o es t) >>= \case
      Right (es', t') -> return (es', Just t')
      Left err -> (,Nothing) <$> mapM (deadcastM "tcNormalCallWCtx" γ [err]) (fst <$> es)

-- | Like `tcNormalCallW`, but return _|_ in case of failure
--
--------------------------------------------------------------------------------
tcNormalCallWD
  :: Unif r => TCEnv r -> AnnTc r -> Identifier -> [ExprSSAR r]
            -> RType r -> TCM r ([ExprSSAR r], RType r)
--------------------------------------------------------------------------------
tcNormalCallWD γ l o es t =
    tcNormalCallW γ l o es t >>= \case
      (es', Just t) -> return (es', t)
      (es', _     ) -> return (es', tBot)


-- | Wrap expressions and statement in deadcasts
--
--   Both return empty environments in case of a failure. This causes the
--   remainder of the basic block to not b checked at all. If this is not the
--   prefered behavior, change by returning `Just γ`, where `γ` the input
--   environment.
--
--------------------------------------------------------------------------------
tcEW :: Unif r => TCEnv r -> ExprSSAR r -> Either Error (ExprSSAR r, b)
               -> TCM r ((ExprSSAR r), Maybe b)
--------------------------------------------------------------------------------
tcEW _ _ (Right (e', t')) = return (e', Just t')
tcEW γ e (Left er)        = (,Nothing) <$> deadcastM "tcEW" γ [er] e

--------------------------------------------------------------------------------
tcSW :: Unif r => TCEnv r -> Statement (AnnSSA r)
               -> Either Error (Statement (AnnSSA r), Maybe b)
               -> TCM r (Statement (AnnSSA r), Maybe b)
--------------------------------------------------------------------------------
tcSW _ _ (Right (s, b)) = return (s, b)
tcSW γ s (Left  e)      = do
    l1    <- freshenAnn l
    l2    <- freshenAnn l
    dc    <- deadcastM "tcSW" γ [e] (NullLit l1)
    return   (ExprStmt l2 dc, Nothing)
  where l = getAnnotation s


--------------------------------------------------------------------------------
tcExpr :: Unif r
       => TCEnv r -> ExprSSAR r -> Maybe (RType r) -> TCM r (ExprSSAR r, RType r)
--------------------------------------------------------------------------------
tcExpr _ e@(IntLit _ _) _
  = return (e, tNum)

tcExpr _ e@(NumLit _ _) _
  = return (e, tReal)

tcExpr _ e@(HexLit _ _) _
  = return (e, tBV32)

tcExpr _ e@(BoolLit _ _) _
  = return (e, tBool)

tcExpr _ e@(StringLit _ _) _
  = return (e, tString)

tcExpr _ e@(NullLit _) _
  = return (e, tNull)

tcExpr γ e@(ThisRef l) _
  = case tcEnvFindTy thisSym γ of
      Just t  -> return (e, t)
      Nothing -> fatal (errorUnboundId (fSrc l) "this") (e, tBot)

tcExpr γ e@(VarRef l x) _
  -- | `undefined`
  | F.symbol x == F.symbol "undefined"
  = return (e, tUndef)

  -- | Ignore the `cast` variable
  | Just t <- to, isCastId x
  = return (e,t)

  -- | If `x` is a unique reference and it this does not appear to be
  --   place where unique references can appear, then flag an error.
  | Just t <- to
  , Just m <- getMutability (envCHA γ) t
  , isSubtypeWithUq γ m tUQ
  , not (isUniqueEnabled e)
  = tcError (errorUniqueRef l (Just e))

  -- | Regural bound variable
  | Just t <- to
  = return (e,t)

  | otherwise
  = tcError (errorUnboundId (fSrc l) x)
  where
    to = tcEnvFindTy x γ

tcExpr γ (CondExpr l e e1 e2) (Just t)
  = do  opTy         <- safeEnvFindTy l γ (builtinOpId BITruthy)
        ([e'], z)    <- tcNormalCallW γ l (builtinOpId BITruthy) [e] opTy
        case z of
          Just _  -> do
              (e1', _) <- tcWrap (tcExprT γ e1 t) >>= tcEW γ e1
              (e2', _) <- tcWrap (tcExprT γ e2 t) >>= tcEW γ e2
              return      (CondExpr l e' e1' e2', t)

          Nothing -> error "TODO: error tcExpr condExpr"

tcExpr _ e@(CondExpr l _ _ _) Nothing
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
      Right opTy -> first (ArrayLit l) <$> tcNormalCallWD γ l (builtinOpId BIArrayLit) es opTy

-- | { f1: e1, ..., fn: tn }
tcExpr γ (ObjectLit l pes) to
  = do  (es', ts) <- unzip <$> T.mapM (uncurry (tcExprWD γ)) ets
        t         <- pure (TObj tUQ (tmsFromList (zipWith toFI ps ts)) fTop)
        return       $ (ObjectLit l (zip ps es'), t)
  where
    (ps , _)       = unzip pes
    toFI p t       = FI (F.symbol p) Req tUQ t
    ets            = map (\(p,e) -> (e, pTy p)) pes
    pTy p          | Just f@FI{} <- lkup p = Just (f_ty f)
                   | otherwise             = Nothing
    lkup p         = F.lookupSEnv (F.symbol p) ctxTys
    ctxTys         = maybe mempty (i_mems . typeMembersOfType (envCHA γ)) to

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
  = case tCasts ++ dCasts of
      [ ] -> do (e', t)    <- tcExpr γ e to
                case e' of
                  Cast_ {} -> die (bugNestedCasts (srcPos l) e)
                  _        -> (,t) . (`Cast_` e') <$> freshenAnn l

      [Right t] -> pure (Cast_ l e, ofType t)

      [Left _ ] -> pure (Cast_ l e, tBot)

      _         -> die $ bugNestedCasts (srcPos l) e

  where
    tCasts = [ Right t_ | TypeCast ξ t_ <- fFact l, tce_ctx γ == ξ ]
    dCasts = [ Left  es | DeadCast ξ es <- fFact l, tce_ctx γ == ξ ]

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
  = do  ([e'],t') <- tcNormalCall γ l (builtinOpId BICastExpr) [(e, Just tc)] (castTy tc)
        return       (e', t')

--------------------------------------------------------------------------------
tcCall :: Unif r => TCEnv r -> ExprSSAR r -> Maybe (RType r) -> TCM r (ExprSSAR r, RType r)
--------------------------------------------------------------------------------

-- | `o e`
tcCall γ c@(PrefixExpr l o e) _
  = do opTy <- safeEnvFindTy l γ (prefixOpId o)
       z    <- tcNormalCallWD γ l (prefixOpId o) [e] opTy
       case z of
         ([e'], t) -> return (PrefixExpr l o e', t)
         _         -> fatal (impossible l "tcCall PrefixExpr") (c, tBot)

-- | `e1 o e2`
tcCall γ c@(InfixExpr l o@OpInstanceof e1 e2) _
  = do (e2',t) <- tcExpr γ e2 Nothing
       case t of
         TClass (BGen (QN _ x) _)  ->
            do  opTy <- safeEnvFindTy l γ (infixOpId o)
                ([e1',_], t') <- let args = [e1, StringLit l2 (F.symbolSafeString x)] in
                                 tcNormalCallWD γ l (infixOpId o) args opTy
                      -- TODO: add qualified name
                return (InfixExpr l o e1' e2', t')
         _  -> fatal (unimplemented l "tcCall-instanceof" $ ppshow e2) (c,tBot)
  where
    l2 = getAnnotation e2

tcCall γ c@(InfixExpr l o e1 e2) _
  = do opTy <- safeEnvFindTy l γ (infixOpId o)
       z    <- tcNormalCallWD γ l (infixOpId o) [e1, e2'] opTy
       case z of
         ([e1', e2'], t) -> return (InfixExpr l o e1' e2', t)
         _ -> fatal (impossible (srcPos l) "tcCall InfixExpr") (c, tBot)
  where
    e2' | o `elem` [OpIn] = enableUnique e2
        | otherwise          = e2

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
    call ty = tcNormalCallWD γ l (builtinOpId BIBracketRef) [e1, e2] ty >>= \case
          ([e1', e2'], t) -> return (BracketRef l e1' e2', t)
          _ -> fatal (impossible (srcPos l) "tcCall BracketRef") (e, tBot)

-- | `e1[e2] = e3`
tcCall γ e@(AssignExpr l OpAssign (LBracket l1 e1 e2) e3) _
  = do opTy <- safeEnvFindTy l γ (builtinOpId BIBracketAssign)
       z <- tcNormalCallWD γ l (builtinOpId BIBracketAssign) [e1,e2,e3] opTy
       case z of
         ([e1', e2', e3'], t) -> return (AssignExpr l OpAssign (LBracket l1 e1' e2') e3', t)
         _ -> fatal (impossible (srcPos l) "tcCall AssignExpr") (e, tBot)

-- | `new e(e1,...,en)`
tcCall γ c@(NewExpr l e es) s
  = do (e',t) <- tcExpr γ e Nothing
       case extractCtor γ t of
         Just ct -> do
            (es', tNew) <- tcNormalCallWD γ l (builtinOpId BICtor) es ct
            tNew'       <- pure (adjustCtxMut tNew s)
            return (NewExpr l e' es', tNew')
         Nothing ->
            fatal (errorConstrMissing (srcPos l) t) (c, tBot)

-- | e.f
--
tcCall γ ef@(DotRef l e0 f) _
  = runFailM (tcExpr γ ue Nothing) >>= checkAccess
  where
    ue = enableUnique e0
    checkAccess (Right (_, tRcvr))
      | isArrayLen tRcvr = checkArrayLength
      | otherwise        = checkProp (getProp l γ F.dummySymbol f tRcvr)
    checkAccess (Left er) = fatal er (ef, tBot)

    -- `array.length`
    checkArrayLength
      = do  l1  <- freshenAnn l
            l2  <- freshenAnn l
            i   <- pure $ Id l2 "__getLength"
            tcExpr γ (CallExpr l1 (DotRef l ue i) []) Nothing

    -- Normal property access
    checkProp (Left er)   = tcError er
    checkProp (Right tfs) = adjustOpt tfs . fst <$> tcExprT γ ue (rcvrTy tfs)

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

    isOptional tfs = Opt `elem` [ o | (_, FI _ o _ _) <- tfs ]
    typesOf    tfs = [ t | (_, FI _ _ _ t) <- tfs]
    rcvrTy         = tOr . map fst

-- | `super(e1,...,en)`
--
--   XXX: there shouldn't be any `super(..)` calls after SSA ...
--
tcCall _ (CallExpr _ (SuperRef _)  _) _
  = error "BUG: super(..) calls should have been eliminated"

-- | `e.m(es)`
--
tcCall γ (CallExpr l em@(DotRef l1 e0 f) es) _
  -- | isVariadicCall f = tcError (unimplemented l "Variadic" ex)
  | otherwise        = checkNonVariadic
  where
    ue  = enableUnique e0
    -- -- Variadic check
    -- isVariadicCall f_ = F.symbol f_ == F.symbol "call"

    -- Non-variadic
    checkNonVariadic =
      runFailM (tcExpr γ ue Nothing) >>= \case
        Right (_, te) -> checkMemberAccess te
        Left e        -> tcError e

    -- Check all corresponding type members `tms`
    checkMemberAccess te =
      case getProp l γ F.dummySymbol f te of
        Right tms@(map fst -> ts) -> do
            (e', _   ) <- tcExprT γ ue (tOr ts)
            (es', ts') <- foldM stepTypeMemM (es, []) tms
            return        (CallExpr l (DotRef l1 e' f) es', tOr ts')

        Left e -> tcError e

    stepTypeMemM (es_, ts) (tR, m) = second (:ts) <$> checkTypeMember es_ tR m

    -- Check a single type member
    checkTypeMember es_ _  (FI _ Req _ ft) = tcNormalCallWD γ l biID es_ ft
    checkTypeMember _   _  (FI f _   _ _ ) = tcError $ errorOptFunUnsup l f ue
    checkTypeMember es_ tR (MI _ Req mts)  =
      case getMutability (envCHA γ) tR of
        Just mR ->
            case [ t | (m, t) <- mts, isSubtypeWithUq γ mR m ] of
              [] -> tcError $ errorMethMutIncomp l em mts mR
              ts -> tcNormalCallWD γ l biID es_ (tAnd ts)

        Nothing -> tcError $ errorNoMutAvailable l ue tR

    checkTypeMember _ _ (MI _ Opt _)  =
      error "TODO: Add error message at checkTypeMember MI Opt"

    biID = builtinOpId BIDotRefCallExpr

-- | `e(es)`
tcCall γ ex@(CallExpr l e es) _ = do
    (e', ft0) <- tcExpr γ e Nothing
    tcWrap (tcNormalCall γ l (builtinOpId BICallExpr) (es `zip` nths) ft0) >>= \case
      Right (es', t) -> return (CallExpr l e' es', t)
      Left e         -> (,tBot) <$> deadcastM "tcCall-CallExpr" γ [e] ex

       -- (es', t)  <- tcNormalCallWD γ l (builtinOpId BICallExpr) es ft0
       -- return       (CallExpr l e' es', t)

tcCall _ e _
  = fatal (unimplemented (srcPos e) "tcCall" e) (e, tBot)

--------------------------------------------------------------------------------
-- | @tcNormalCall@ resolves overloads and returns cast-wrapped versions of
--   the arguments if subtyping fails.
--
--   May throw exceptions.
--
--------------------------------------------------------------------------------
tcNormalCall :: Unif r
             => TCEnv r -> AnnTc r -> Identifier
             -> [(ExprSSAR r, Maybe (RType r))]
             -> RType r -> TCM r ([ExprSSAR r], RType r)
--------------------------------------------------------------------------------
tcNormalCall γ l fn etos ft0
  = do ets <- T.mapM (uncurry (tcExpr γ)) etos
       z   <- resolveOverload γ l fn ets ft0
       case z of
         Just (i, θ, ft) ->
             do addAnn l (Overload (tce_ctx γ) (F.symbol fn) i)
                addSubst l θ
                (es, ts) <- pure (unzip ets)
                -- tcCallCase γ l fn es ts ft

-- XXX: Why all this ??
                tcWrap (tcCallCase γ l fn es ts ft) >>= \case
                  Right ets' -> return ets'
                  Left  er   -> do
                      es' <- T.mapM (deadcastM "tcNormalCall" γ [er] . fst) ets
                      return (es', tNull)

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
    validOverloads = [ (i, mkFun s) | (i, s@(_, bs, _)) <- overloads γ ft
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
  = runFailM go >>= \case Right (Just θ') -> return $ Just (i, θ', ft)
                          _               -> tcCallCaseTry γ l fn ts fts
  where
    go = do
      (βs, its1, _)  <- instantiateFTy l (tce_ctx γ) fn ft
      ts1            <- zipWithM (instantiateTy l $ tce_ctx γ) [1..] ts
      θ              <- unifyTypesM (srcPos l) γ ts1 its1

      let (ts2, its2) = apply θ (ts1, its1)

      --  1. Pick the type variables (V) that have a bound (B).
      --  2. Find the binding that the unification braught to them (T := V θ).
      let (appVs, cs) = unzip [(apply θ (btVar bv), c) | bv@(BTV _ _ (Just c)) <- βs]

      --  3. Use that to check against the bound B.
      --  4. Also check the argument types.
      if appVs <:: cs && ts2 <:: its2 then
          return $ Just θ
      else
          return Nothing

    (<::) ts1 ts2 = and (zipWith (isSubtypeWithUq γ) ts1 ts2)

--------------------------------------------------------------------------------
tcCallCase
  :: (PP a, Unif r)
  => TCEnv r -> AnnTc r -> a -> [ExprSSAR r]
  -> [RType r] -> RType r -> TCM r ([ExprSSAR r], RType r)
--------------------------------------------------------------------------------
tcCallCase γ@(tce_ctx -> ξ) l fn es ts ft
  = do  (βs, rhs, ot)   <- instantiateFTy l ξ fn ft
        lhs             <- zipWithM (instantiateTy l ξ) [1..] ts
        θ               <- unifyTypesM (srcPos l) γ lhs rhs

        let (vs, appVs, cs)  = unzip3 [ (s, apply θ (btVar bv), c) | bv@(BTV s l (Just c)) <- βs
                                                            , not (unassigned (TV s l) θ) ]

        unless (appVs <:: cs) (tcError (errorBoundsInvalid l vs appVs cs))

        let (lhs', rhs') = apply θ (lhs, rhs)

        es'             <- zipWith3M (castMC γ) es lhs' rhs'
        return           $ (es', apply θ ot)

  where
    (<::) ts1 ts2 = and (zipWith (isSubtypeWithUq γ) ts1 ts2)

--------------------------------------------------------------------------------
instantiateTy :: Unif r => AnnTc r -> IContext -> Int -> RType r -> TCM r (RType r)
--------------------------------------------------------------------------------
instantiateTy l ξ i (bkAll -> (bs, t))
  = do  (_, t)  <- freshTyArgs l i ξ bs t
        -- TODO: need to take the first arg into account, i.e. add the
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

-- | envJoin
--
--   XXX: May throw `tcError`: wrap appropriately
--
--------------------------------------------------------------------------------
envJoin :: Unif r
  => AnnTc r -> TCEnv r -> TCEnvO r -> TCEnvO r -> TCM r (Maybe (TCEnv r))
--------------------------------------------------------------------------------
envJoin _ _ Nothing x           = return x
envJoin _ _ x Nothing           = return x
envJoin l γ (Just γ1) (Just γ2) = do
    s1s   <- mapM (safeEnvFindSI l γ1) xs    -- SI entry at the end of the 1st branch
    s2s   <- mapM (safeEnvFindSI l γ2) xs    -- SI entry at the end of the 2nd branch
    return $ Just $ foldl (\γ' (s1, s2) -> tcEnvAdd (siJoin γ1 s1 s2) γ') γ (zip s1s s2s)
  where
    xs        = [ x | PhiVar x <- fFact l ]

siJoin γ s1 s2
    | isSubtype γ (v_type s1) (v_type s2) = s2
    | isSubtype γ (v_type s2) (v_type s1) = s1
    | otherwise = s1 { v_type = tOr [v_type s1, v_type s2] }

--------------------------------------------------------------------------------
envLoopJoin :: Unif r => AnnTc r -> TCEnv r -> TCEnvO r -> TCM r (TCEnv r)
--------------------------------------------------------------------------------
envLoopJoin _ γ Nothing   = return γ

--  XXX: use the "next" name for the Φ-var in the propagated env γ_
--
envLoopJoin l γ (Just γ') = foldM (\γ_ (x, x') -> do
      s    <- safeEnvFindSI l γ  x
      s'   <- safeEnvFindSI l γ' x'

      -- [ T2 <: T1 ==> T1 ]
      --
      -- Lift uniqueness restriction since we are not allowing the loop SSA var
      -- to escape (any more than the original variable).
      --
      if isSubtypeWithUq γ_ (v_type s') (v_type s) then do
          addAnn l (PhiLoopTC (x, x', toType (v_type s)))
          return   (tcEnvAdd (s { v_name = v_name s' }) γ_)

      -- This would require a fixpoint computation, so it's not allowed
      else
          tcError (errorLoopWiden l x x' (v_type s) (v_type s'))
    ) γ (zip xs xs')

  where
    (xs, xs') = unzip [ x | PhiLoop x <- fFact l ]


--------------------------------------------------------------------------------
tcScan :: (a -> b -> TCM r (a, b)) -> a -> [b] -> TCM r (a, [b])
--------------------------------------------------------------------------------
tcScan f e ts = second reverse <$> foldM step (e, []) ts
  where
    step (a, bs) b = second (:bs) <$> f a b


-- Local Variables:
-- flycheck-disabled-checkers: (haskell-liquid)
-- End:
