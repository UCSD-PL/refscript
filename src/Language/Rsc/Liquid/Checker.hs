{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE TupleSections             #-}

-- | Top Level for Refinement Type checker
module Language.Rsc.Liquid.Checker (verifyFile) where

import           Control.Applicative             (pure, (<$>))
import           Control.Monad
import           Data.Function                   (on)
import qualified Data.HashMap.Strict             as HM
import           Data.List                       (sortBy)
import           Data.Maybe                      (catMaybes, fromMaybe)
import           Data.Monoid                     (mempty)
import qualified Data.Text                       as T
import qualified Data.Traversable                as T
import qualified Language.Fixpoint.Config        as C
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Interface     (solve)
import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Types         as F
import qualified Language.Fixpoint.Visitor       as V
import           Language.Rsc.Annots
import           Language.Rsc.AST
import           Language.Rsc.ClassHierarchy
import           Language.Rsc.CmdLine            (Config)
import           Language.Rsc.Core.EitherIO
import           Language.Rsc.Core.Env
import           Language.Rsc.Environment
import           Language.Rsc.Errors
import           Language.Rsc.Liquid.CGMonad
import           Language.Rsc.Liquid.Constraint
import           Language.Rsc.Liquid.Environment
import           Language.Rsc.Liquid.Types
import           Language.Rsc.Locations
import           Language.Rsc.Lookup
import           Language.Rsc.Misc
import           Language.Rsc.Names
import           Language.Rsc.Options
import           Language.Rsc.Parser
import           Language.Rsc.Pretty
import           Language.Rsc.Program
import           Language.Rsc.SSA.SSA
import qualified Language.Rsc.SystemUtils        as A
import           Language.Rsc.Traversals
import           Language.Rsc.Typecheck.Checker  (typeCheck)
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
import           System.Console.CmdArgs.Default

import           Debug.Trace                     hiding (traceShow)

--------------------------------------------------------------------------------
verifyFile :: Config -> FilePath -> [FilePath] -> IO (A.UAnnSol RefType, FError)
--------------------------------------------------------------------------------
verifyFile cfg f fs = fmap (either (A.NoAnn,) id) $ runEitherIO $
  do  p   <- EitherIO   $ parseRscFromFiles fs
      _   <- liftIO     $ donePhase Loud "Parse Files"
      cha <- liftEither $ mkCHA p
      _   <- liftIO     $ donePhase Loud "SSA Transform"
      ssa <- EitherIO   $ ssaTransform p cha
      _   <- liftIO     $ donePhase Loud "Typecheck"
      tc  <- EitherIO   $ typeCheck cfg ssa cha
      _   <- liftIO     $ donePhase Loud "Generate Constraints"
      cgi <- pure       $ generateConstraints cfg tc cha
      res <- liftIO     $ solveConstraints tc f cgi
      return res

-- | solveConstraints: Call solve with `ueqAllSorts` enabled.
--------------------------------------------------------------------------------
solveConstraints :: RefScript
                 -> FilePath
                 -> CGInfo
                 -> IO (A.UAnnSol RefType, F.FixResult Error)
--------------------------------------------------------------------------------
solveConstraints p f cgi
  = do (r, s)  <- solve fpConf $ cgi_finfo cgi
       let r'   = fmap (ci_info . F.sinfo) r
       let anns = cgi_annot cgi
       let sol  = applySolution s
       return (A.SomeAnn anns sol, r')
  where
    real        = RealOption `elem` pOptions p
    fpConf      = def { C.real        = real
                      , C.ueqAllSorts = C.UAS True
                      , C.srcFile     = f
                      }

--------------------------------------------------------------------------------
applySolution :: F.FixSolution -> A.UAnnInfo RefType -> A.UAnnInfo RefType
--------------------------------------------------------------------------------
applySolution  = fmap . fmap . tx
  where
    tx         = F.mapPredReft . txPred
    txPred s   = F.simplify . V.mapKVars (appSol s)
    appSol s k = Just $ HM.lookupDefault F.PTop k s

    -- tx   s (F.Reft (x, ra)) = F.Reft (x, txRa s ra)
    -- txRa s                  = F.Refa . F.simplify . V.mapKVars (appSol s) . F.raPred
    -- appSol _ ra@(F.RConc _) = ra
    -- appSol s (F.RKvar k su) = F.RConc $ F.subst su $ HM.lookupDefault F.PTop k s

--------------------------------------------------------------------------------
generateConstraints :: Config -> RefScript -> ClassHierarchy F.Reft -> CGInfo
--------------------------------------------------------------------------------
generateConstraints cfg pgm = getCGInfo cfg pgm . consRsc pgm

--------------------------------------------------------------------------------
consRsc :: RefScript -> ClassHierarchy F.Reft -> CGM ()
--------------------------------------------------------------------------------
consRsc p@(Rsc {code = Src fs}) cha
  = do  g   <- initGlobalEnv p cha
        consStmts g fs
        return ()


--------------------------------------------------------------------------------
-- | Initialize environment
--------------------------------------------------------------------------------

-- TODO: 1. Split the freshening part
--       2. checkSyms?
--------------------------------------------------------------------------------
initGlobalEnv :: RefScript -> ClassHierarchy F.Reft -> CGM CGEnv
--------------------------------------------------------------------------------
initGlobalEnv pgm@(Rsc { code = Src ss }) cha = freshenCGEnvM g
  where
    g     = CGE nms bnds ctx pth cha fenv grd cst mut thisT
    nms   = mkVarEnv (accumVars ss)    -- modules ?
    bnds  = mempty
    ctx   = emptyContext
    pth   = mkAbsPath []
    fenv  = F.emptySEnv
    grd   = []
    cst   = consts pgm
    mut   = Nothing
    thisT = Nothing

-- TODO: CheckSyms
--------------------------------------------------------------------------------
initModuleEnv :: (F.Symbolic n, PP n) => CGEnv -> n -> [Statement AnnLq] -> CGM CGEnv
--------------------------------------------------------------------------------
initModuleEnv g n s = freshenCGEnvM g'
  where
    g'    = CGE nms bnds ctx pth cha fenv grd cst mut thisT
    nms   = mkVarEnv (accumVars s)
    bnds  = envBounds g
    ctx   = cge_ctx g
    pth   = extendAbsPath (cge_path g) n
    cha   = cge_cha g
    fenv  = cge_fenv g
    grd   = cge_guards g
    cst   = cge_consts g
    mut   = Nothing
    thisT = Nothing

-- | `initCallableEnv l f i xs (αs, ts, t) g`
--
--    * Pushes a new context @i@
--    * Adds return type @t@
--    * Adds binders for the type variables @αs@
--    * Adds binders for the arguments @ts@
--    * Adds binder for the 'arguments' variable
--
--------------------------------------------------------------------------------
initCallableEnv :: (PP x, IsLocated x, IsLocated l)
                => l -> CGEnv -> x
                -> IOverloadSig F.Reft
                -> [Id (AnnR F.Reft)]
                -> [Statement (AnnR F.Reft)]
                -> CGM CGEnv
--------------------------------------------------------------------------------
initCallableEnv l g f fty xs s
  = do  g1 <- cgEnvAdds ("init-func-" ++ ppshow f ++ "-0") params g0
        g2 <- cgEnvAdds ("init-func-" ++ ppshow f ++ "-1") arg g1
        -- XXX: Isn't this done at CGMonad envAddGoup?
        -- let _  = concatMap (uncurry $ checkSyms "init" g2 []) xts
        freshenCGEnvM g2
  where
    g0     = CGE nms bnds ctx pth cha fenv grd cst mut thisT
             -- No FP binding for these
    nms    = toFgn (envNames g)
           & envUnion (mkVarEnv (accumVars s))
           & envAdds tyBs
           & envAddReturn f (VI Local ReturnVar Initialized t)
    bnds   = envAdds [(v,tv) | BTV v _ (Just tv) <- bs] $ cge_bounds g
    ctx    = pushContext i (cge_ctx g)
    pth    = cge_path g
    cha    = cge_cha g
    fenv   = cge_fenv g
    grd    = []
    cst    = cge_consts g
    mut    = cge_mut g
    thisT  = cge_this g

    tyBs   = [(Loc (srcPos l) α, VI Local Ambient Initialized $ tVar α) | α <- αs]
    params = [(x, VI Local WriteLocal Initialized t) | (x, t) <- safeZip "initCallableEnv" xs ts ]
    arg    = single (argId $ srcPos l, mkArgTy l ts)
    ts     = map b_type xts
    αs     = map btvToTV bs
    (i, (bs,xts,t)) = fty


--------------------------------------------------------------------------------
-- | Constraint generation
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
consFun :: CGEnv -> Statement AnnLq -> CGM ()
--------------------------------------------------------------------------------
consFun g (FunctionStmt l f xs Nothing)
  = return ()
consFun g (FunctionStmt l f xs (Just body))
  = case envFindTy f (cge_names g) of
      Just (VI _ _ _ t) -> cgFunTys l f xs t >>= mapM_ (consCallable l g f xs body)
      Nothing   -> cgError $ errorMissingSpec (srcPos l) f

consFun _ s
  = die $ bug (srcPos s) "consFun called not with FunctionStmt"

-- | @consCallable@ checks a function body against a *one* of multiple
--   conjuncts of an overloaded (intersection) type signature.
--   Assume: len ts' = len xs

consCallable l g f xs body ift
  = initCallableEnv l g f ift xs body >>= (`consStmts` body)

--------------------------------------------------------------------------------
consStmts :: CGEnv -> [Statement AnnLq]  -> CGM (Maybe CGEnv)
--------------------------------------------------------------------------------
consStmts = consFold consStmt

--------------------------------------------------------------------------------
consStmt :: CGEnv -> Statement AnnLq -> CGM (Maybe CGEnv)
--------------------------------------------------------------------------------

-- | @consStmt g s@ returns the environment extended with binders that are
--   due to the execution of statement s. @Nothing@ is returned if the
--   statement has (definitely) hit a `return` along the way.

-- skip
consStmt g (EmptyStmt _)
  = return $ Just g

-- x = e
consStmt g (ExprStmt l (AssignExpr _ OpAssign (LVar lx x) e))
  = consAsgn l g (Id lx x) e

-- e1.f = e2
consStmt g (ExprStmt l (AssignExpr _ OpAssign (LDot _ e1 f) e2))
  = mseq (consExpr g e1 Nothing) $ \(x1,g') -> do
      t         <- cgSafeEnvFindTyM x1 g'
      let rhsCtx = fmap snd $ getProp g' FieldAccess f t
          opTy   = setPropTy (F.symbol f) -- <$> cgSafeEnvFindTyM (builtinOpId BISetProp) g'
      fmap snd <$> consCall g' l BISetProp [(vr x1, Nothing),(e2, rhsCtx)] opTy
  where
      vr         = VarRef $ getAnnotation e1

-- e
consStmt g (ExprStmt _ e)
  = fmap snd <$> consExpr g e Nothing

-- s1;s2;...;sn
consStmt g (BlockStmt _ stmts)
  = consStmts g stmts

-- if b { s1 }
consStmt g (IfSingleStmt l b s)
  = consStmt g (IfStmt l b s (EmptyStmt l))

--  1. Use @envAddGuard True@ and @envAddGuard False@ to add the binder
--     from the condition expression @e@ into @g@ to obtain the @CGEnv@
--     for the "then" and "else" statements @s1@ and @s2 respectively.
--  2. Recursively constrain @s1@ and @s2@ under the respective environments.
--  3. Combine the resulting environments with @envJoin@

-- if e { s1 } else { s2 }
consStmt g (IfStmt l e s1 s2) =
  mseq ((cgSafeEnvFindTyM (builtinOpId BITruthy) g)
        >>= consCall g l "truthy" [(e, Nothing)]) $ \(xe,ge) -> do
    g1' <- (`consStmt` s1) $ envAddGuard xe True ge
    g2' <- (`consStmt` s2) $ envAddGuard xe False ge
    envJoin l g g1' g2'

-- while e { s }
consStmt g (WhileStmt l e s)
  = consWhile g l e s

-- var x1 [ = e1 ]; ... ; var xn [= en];
consStmt g (VarDeclStmt _ ds)
  = consFold consVarDecl g ds

-- return
consStmt g (ReturnStmt l Nothing)
  = do _ <- subType l (errorLiquid' l) g tVoid (cgEnvFindReturn g)
       return Nothing

-- return e
consStmt g (ReturnStmt l (Just e@(VarRef lv x)))
  | Just t <- cgEnvFindTy x g, needsCall t
  = do  g' <- cgEnvAdds "Return" [(fn, VI Local Ambient Initialized $ finalizeTy t)] g
        consStmt g' (ReturnStmt l (Just (CallExpr l (VarRef lv fn) [e])))
  | otherwise
  = do  _ <- consCall g l "return" [(e, Just retTy)] $ returnTy retTy True
        return Nothing
  where
    retTy = cgEnvFindReturn g
    fn    = Id l "__final)))ize__"
    needsCall t | TRef (Gen _ (m:_)) _ <- t = isUM m | otherwise = False

consStmt g (ReturnStmt l (Just e))
  = do  _ <- consCall g l "return" [(e, Just retTy)] $ returnTy retTy True
        return Nothing
  where
    retTy = cgEnvFindReturn g

-- throw e
consStmt g (ThrowStmt _ e)
  = consExpr g e Nothing >> return Nothing

-- function f(x1...xn){ s }
consStmt g s@(FunctionStmt _ _ _ _)
  = consFun g s >> return (Just g)

--
-- class A<V extends S> [extends B<T>] {..}
--
--  * Add the type vars V in the environment
--
--  * Compute type for "this" and add that to the env as well.
--
consStmt g (ClassStmt l x _ _ ce)
  = do  d <- resolveTypeM l g nm
        mapM_ (consClassElt g d) ce
        return $ Just g
  where
    nm = QN (cge_path g) (F.symbol x)

consStmt g (InterfaceStmt _ _)
  = return $ Just g

consStmt g (EnumStmt _ _ _)
  = return $ Just g

consStmt g (ModuleStmt _ n body)
  = initModuleEnv g n body >>= (`consStmts` body) >> return (Just g)

-- OTHER (Not handled)
consStmt _ s
  = errorstar $ "consStmt: not handled " ++ ppshow s


--------------------------------------------------------------------------------
consVarDecl :: CGEnv -> VarDecl AnnLq -> CGM (Maybe CGEnv)
--------------------------------------------------------------------------------
consVarDecl g v@(VarDecl l x (Just e))
  = case envFindTy x (cge_names g) of
      -- | Local
      Nothing ->
        mseq (consExpr g e Nothing) $ \(y,gy) -> do
          t       <- cgSafeEnvFindTyM y gy
          Just   <$> cgEnvAdds "consVarDecl" [(x, VI Local WriteLocal Initialized t)] gy

      Just (VI loc  WriteLocal _ t) ->
        mseq (consCall g l "consVarDecl" [(e, Just t)] $ localTy t) $ \(y,gy) -> do
          declT   <- cgSafeEnvFindTyM y gy
          Just   <$> cgEnvAdds "consVarDecl" [(x, VI loc WriteLocal Initialized declT)] gy

      -- | Global
      Just (VI loc WriteGlobal _ t) ->
        mseq (consExpr g e $ Just t) $ \(y, gy) -> do
          ty      <- cgSafeEnvFindTyM y gy
          fta     <- freshenType WriteGlobal gy l t
          _       <- subType l (errorLiquid' l) gy ty  fta
          _       <- subType l (errorLiquid' l) gy fta t
          Just   <$> cgEnvAdds "consVarDecl" [(x, VI loc WriteGlobal Initialized fta)] gy

      -- | ReadOnly
      Just (VI loc Ambient _ t) ->
        mseq (consCall g l "consVarDecl" [(e, Just t)] $ localTy t) $ \(y,gy) -> do
          declT   <- cgSafeEnvFindTyM y gy
          Just   <$> cgEnvAdds "consVarDecl" [(x, VI loc Ambient Initialized declT)] gy

      _ -> cgError $ errorVarDeclAnnot (srcPos l) x

consVarDecl g v@(VarDecl _ x Nothing)
  = case envFindTy x (cge_names g) of
      Just (VI loc Ambient _ t) ->
          Just <$> cgEnvAdds "consVarDecl" [(x, VI loc Ambient Initialized t)] g
      _ ->   -- The rest should have fallen under the 'undefined' initialization case
          error "LQ: consVarDecl this shouldn't happen"

--------------------------------------------------------------------------------
consExprT :: CGEnv -> Expression AnnLq -> Maybe RefType
          -> CGM (Maybe (Id AnnLq, CGEnv))
--------------------------------------------------------------------------------
consExprT g e Nothing  = consExpr g e Nothing
consExprT g e (Just t) = consCall  g (getAnnotation e) "consExprT" [(e, Nothing)]
                       $ TFun [B (F.symbol "x") t] tVoid fTop

-- --------------------------------------------------------------------------------
-- consClassElts :: IsLocated l => l -> CGEnv -> TypeDecl F.Reft -> [ClassElt AnnLq] -> CGM ()
-- --------------------------------------------------------------------------------
-- consClassElts l g d@(ID nm _ vs _ es) cs
--   = do  -- g0   <- cgEnvAdds "consClassElts-2" thisInfo g -- XXX: Add these at specific elements only
--         -- g1   <- cgEnvAdds "consClassElts-1" fs g  -- Add imm fields into scope beforehand
--         mapM_ (consClassElt g d) cs
--   where
--     -- fs = [ (ql f, VI ro ii t) | ((_,InstanceMember), (FieldSig f _ m t)) <- M.toList es
--     --                           , isImmutable m ]
--     -- TODO: Make location more precise
--     --
--     -- PV: qualified name here used as key -> ok
--     ql        = mkQualSym $ F.symbol thisId
--     ro        = ReadOnly
--     ii        = Initialized
--     this_t    = TRef nm (tVar <$> vs) fTop
--     thisInfo  = [(this, VI ReadOnly Initialized this_t)]
--     this      = Loc (srcPos l) thisId

--------------------------------------------------------------------------------
consClassElt :: CGEnv -> TypeDecl F.Reft -> ClassElt AnnLq -> CGM ()
--------------------------------------------------------------------------------
consClassElt g0 (TD sig@(TS _ (BGen nm bs) _) _) (Constructor l xs body)
  = do  let g = g0
              & initClassInstanceEnv sig
              & initClassCtorEnv sig
        g'   <- cgEnvAdds "ctor" exitP g
        ts   <- cgFunTys l ctor xs ctorT
        mapM_ (consCallable l g' ctor xs body) ts
  where
    thisT = TRef (Gen nm (map btVar bs)) fTop
    ms    = typeMembersOfType (envCHA g0) thisT
    ctor  = builtinOpId BICtor
    cExit = builtinOpId BICtorExit
    -- substOffsetThis exitTy ???
    exitP = [(cExit, VI Local Ambient Initialized $ mkFun (bs, xts, ret))]
    ret   = thisT `strengthen` F.reft (F.vv Nothing) (F.pAnd $ bnd <$> out)
    xts   = sortBy c_sym [ B x t' | (x, FI _ _ t) <- F.toListSEnv (tm_prop ms)
                                  , let t' = unqualifyThis g0 thisT t ]
    out   = [ f | (f, FI _ m _) <- F.toListSEnv (tm_prop ms), isImm m ]
    bnd f = F.PAtom F.Eq (mkOffset v_sym $ F.symbolString f) (F.eVar f)
    v_sym = F.symbol $ F.vv Nothing
    c_sym = on compare b_sym
    ctorT | Just t <- tm_ctor ms
          = mkAll bs t -- XXX $ remThisBinding t
          | otherwise = die $ unsupportedNonSingleConsTy (srcPos l)

-- | Static field
--
consClassElt g (TD sig ms) (MemberVarDecl l True x (Just e))
  | Just (FI _ _ t) <- F.lookupSEnv (F.symbol x) $ tm_sprop ms
  = void $ consCall g l "field init" [(e, Just t)] (mkInitFldTy t)
  | otherwise
  = cgError $ errorClassEltAnnot (srcPos l) (sigTRef sig) x

consClassElt _ _ (MemberVarDecl l True x Nothing)
  = cgError $ unsupportedStaticNoInit (srcPos l) x

-- | Instance variable (checked at constructor check)
--
consClassElt _ _ (MemberVarDecl _ False _ Nothing)
  = return ()

consClassElt _ _ (MemberVarDecl l False x _)
  = die $ bugClassInstVarInit (srcPos l) x

-- | Static method
--
consClassElt g (TD sig ms) (MemberMethDecl l True x xs body)
  | Just (MI _ _ t) <- F.lookupSEnv (F.symbol x) $ tm_smeth ms
  = do  its <- cgFunTys l x xs t
        mapM_ (consCallable l g x xs body) its
  | otherwise
  = cgError  $ errorClassEltAnnot (srcPos l) (sigTRef sig) x

-- | Instance method
--
consClassElt g (TD sig ms) (MemberMethDecl l False x xs body)
  | Just (MI _ m t) <- F.lookupSEnv (F.symbol x) (tm_meth ms)
  = do  let g0 = g
               & initClassInstanceEnv sig
               & initClassMethEnv m sig
        ft    <- cgFunTys l x xs t
        mapM_ (consCallable l g0 x xs body) ft

  | otherwise
  = cgError $ errorClassEltAnnot (srcPos l) (sigTRef sig) x


--------------------------------------------------------------------------------
consAsgn :: AnnLq -> CGEnv -> Id AnnLq -> Expression AnnLq -> CGM (Maybe CGEnv)
--------------------------------------------------------------------------------
consAsgn l g x e =
  case envFindTyWithAsgn x g of
    -- This is the first time we initialize this variable
    Just (VI loc WriteGlobal Uninitialized t) ->
      do  t' <- freshenType WriteGlobal g l t
          mseq (consExprT g e $ Just t') $ \(_, g') -> do
            g'' <- cgEnvAdds "consAsgn-0" [(x, VI loc WriteGlobal Initialized t')] g'
            return $ Just g''

    Just (VI _ WriteGlobal _ t) -> mseq (consExprT g e $ Just t) $ \(_, g') ->
                                     return $ Just g'
    Just (VI loc a i t) -> mseq (consExprT g e $ Just t) $ \(x', g') -> do
                             t      <- cgSafeEnvFindTyM x' g'
                             Just  <$> cgEnvAdds "consAsgn-1" [(x, VI loc a i t)] g'
    Nothing -> mseq (consExprT g e Nothing) $ \(x', g') -> do
                 t      <- cgSafeEnvFindTyM x' g'
                 Just  <$> cgEnvAdds "consAsgn-1" [(x, VI Local WriteLocal Initialized t)] g'


-- | @consExpr g e@ returns a pair (g', x') where x' is a fresh,
-- temporary (A-Normalized) variable holding the value of `e`,
-- g' is g extended with a binding for x' (and other temps required for `e`)
--------------------------------------------------------------------------------
consExpr :: CGEnv -> Expression AnnLq -> Maybe RefType -> CGM (Maybe (Id AnnLq, CGEnv))
--------------------------------------------------------------------------------
consExpr g (Cast_ l e) tCtx =
  case envGetContextCast g l of
    CDead e' t' -> consDeadCode g l e' t'
    _           -> consExpr g e tCtx

--     CNo         -> mseq (consExpr g e Nothing) $ return . Just
--     CUp t t'    -> mseq (consExpr g e Nothing) $ \(x,g') -> Just <$> consUpCast   g' l x t t'
--     CDn t t'    -> mseq (consExpr g e Nothing) $ \(x,g') -> Just <$> consDownCast g' l x t t'

-- | < t > e
consExpr g ex@(Cast l e) _
  | [t] <- [ ct | UserCast ct <- fFact l ]
  = consCall g l "Cast" [(e, Just t)] (mkCastFunTy t)
  | otherwise
  = die $ bugNoCasts (srcPos l) ex

consExpr g (IntLit l i) _
  = Just <$> cgEnvAddFresh "8" l (VI Local WriteLocal Initialized $ tNum `eSingleton` i) g

-- Assuming by default 32-bit BitVector
consExpr g (HexLit l x) _
  | Just e <- bitVectorValue x
  = Just <$> cgEnvAddFresh "9" l (VI Local WriteLocal Initialized $ tBV32 `nubstrengthen` e) g
  | otherwise
  = Just <$> cgEnvAddFresh "10" l (VI Local WriteLocal Initialized tBV32) g

consExpr g (BoolLit l b) _
  = Just <$> cgEnvAddFresh "11" l (VI Local WriteLocal Initialized $ pSingleton tBool b) g

consExpr g (StringLit l s) _
  = Just <$> cgEnvAddFresh "12" l (VI Local WriteLocal Initialized $ tString `eSingleton` T.pack s) g

consExpr g (NullLit l) _
  = Just <$> cgEnvAddFresh "13" l (VI Local WriteLocal Initialized tNull) g

consExpr g (ThisRef l) _
  = case envFindTyWithAsgn this g of
      Just _  -> return  $ Just (this,g)
      Nothing -> cgError $ errorUnboundId (fSrc l) "this"
  where
    this = Id l "this"

consExpr g (VarRef l x) _
  | Just (VI loc WriteGlobal i t) <- tInfo
  = Just <$> cgEnvAddFresh "0" l (VI loc WriteLocal i t) g
  | Just (VI _ _ _ t) <- tInfo
  = addAnnot (srcPos l) x t >> return (Just (x, g))
  | otherwise
  = cgError $ errorUnboundId (fSrc l) x
  where
    tInfo = envFindTyWithAsgn x g

consExpr g (PrefixExpr l o e) _
  = do opTy         <- cgSafeEnvFindTyM (prefixOpId o) g
       consCall g l o [(e,Nothing)] opTy

consExpr g (InfixExpr l o@OpInstanceof e1 e2) _
  = mseq (consExpr g e2 Nothing) $ \(x, g') -> do
       t            <- cgSafeEnvFindTyM x g'
       case t of
         TClass x   -> do opTy <- cgSafeEnvFindTyM (infixOpId o) g
                          consCall g l o ([e1, StringLit l2 (cc x)] `zip` nths) opTy
         _          -> cgError $ unimplemented (srcPos l) "tcCall-instanceof" $ ppshow e2
  where
    l2 = getAnnotation e2
    cc (BGen (QN _ s) _) = F.symbolString s

consExpr g (InfixExpr l o e1 e2) _
  = do opTy <- cgSafeEnvFindTyM (infixOpId o) g
       consCall g l o ([e1, e2] `zip` nths) opTy

-- | e ? e1 : e2
consExpr g (CondExpr l e e1 e2) to
  = do  opTy    <- mkTy to <$> cgSafeEnvFindTyM (builtinOpId BICondExpr) g
        tt'     <- freshTyFun g l (rType tt)
        (v,g')  <- mapFst (VarRef l) <$> cgEnvAddFresh "14" l (VI Local WriteLocal Initialized tt') g
        consCallCondExpr g' l BICondExpr
          [(e,Nothing), (v,Nothing), (e1,rType <$> to), (e2,rType <$> to)]
          opTy
        -- consCallCondExpr g' l BICondExpr (FI Nothing ((,Nothing) <$> [e,v,e1,e2])) opTy
  where
    tt       = fromMaybe tTop to
    mkTy Nothing (TAll cv (TAll tv (TFun [B c_ tc, B t_ _   , B x_ xt, B y_ yt] o r))) =
                  TAll cv (TAll tv (TFun [B c_ tc, B t_ tTop, B x_ xt, B y_ yt] o r))
    mkTy _ t = t

-- | super(e1,..,en)
consExpr g (CallExpr l (SuperRef _) es) _
  | Just thisT        <- cge_this g
  , Just tSuper       <- getSuperType (envCHA g) thisT
  = error "consExpr super(...) - UNIMPLEMENTED"
  -- = consCall g l "super" (FI Nothing ((,Nothing) <$> es)) ct
  | otherwise
  = cgError $ errorUnboundId (fSrc l) "super"
  where

-- | e.m(es)
consExpr g c@(CallExpr l em@(DotRef _ e f) es) _
  = mseq (consExpr g e Nothing) $ \(x,g') -> cgSafeEnvFindTyM x g' >>= go g' x
  where
             -- Variadic call error
    go g x t | isVariadicCall f, [] <- es
             = cgError $ errorVariadicNoArgs (srcPos l) em

             -- Variadic call
             | isVariadicCall f, v:vs <- es
             = consCall g l em (argsThis v vs) t

             -- Accessing and calling a function field
             --
             -- TODO: 'this' should not appear in ft
             --        Add check for this.
             --
             | Just (_,ft) <- getProp g FieldAccess f t, isTFun ft
             = consCall g l c (args es) ft

             -- Invoking a method
             | Just (_,ft) <- getProp g MethodAccess f t
             = consCall g l c (argsThis (vr x) es) ft

             | otherwise
             = cgError $ errorCallNotFound (srcPos l) e f

    isVariadicCall f = F.symbol f == F.symbol "call"
    args vs          = (,Nothing) <$> vs
    argsThis v vs    = (v:vs) `zip` nths
    vr               = VarRef $ getAnnotation e

-- | e(es)
--
consExpr g (CallExpr l e es) _
  = mseq (consExpr g e Nothing) $ \(x, g') ->
      cgSafeEnvFindTyM x g' >>= consCall g' l e (es `zip` nths)

-- | e.f
--
--   Returns type: { v: _ | v = x.f }, if e => x and `f` is an immutable field
--                 { v: _ | _       }, otherwise
--
--   The TC phase should have resolved whether we need to restrict the range of
--   the type of `e` (possibly adding a relevant cast). So no need to repeat the
--   call here.
--
consExpr g (DotRef l e f) to
  = mseq (consExpr g e Nothing) $ \(x,g') -> do
      te <- cgSafeEnvFindTyM x g'
      case getProp g' FieldAccess f te of
        --
        -- Special-casing Array.length
        --
        Just (TRef (Gen (QN (QP _ _ []) s) _) _, _)
          | F.symbol "Array" == s && F.symbol "length" == F.symbol f ->
              consExpr g' (CallExpr l (DotRef l (vr x) (Id l "_get_length_")) []) to


        -- Do not nubstrengthen enumeration fields
        -- TODO: TEnum does not exist anymore - is this still ok?
        -- Just (TEnum _,t,_) -> Just <$> cgEnvAddFresh "1" l (t, Ambient, Initialized) g'

        -- TODO: this is the only place where we need the information of whether
        --       f is an immutable field
        Just (tE,t) -> Just <$> cgEnvAddFresh "2" l (VI Local WriteLocal Initialized $ mkTy x t) g'

        Nothing     -> cgError $  errorMissingFld (srcPos l) f te
  where
    vr       = VarRef $ getAnnotation e

    mkTy x t | Just m <- getFieldMutability (envCHA g) t (F.symbol f)
             , isImm m
             = fmap F.top t `eSingleton` mkOffset x f
             | otherwise      = substThis x t

-- | e1[e2]
consExpr g e@(BracketRef l e1 e2) _
  = mseq (consExpr g e1 Nothing) $ \(x1,g') -> do
      opTy <- do  cgSafeEnvFindTyM x1 g' >>= \case
                    -- TEnum _ -> cgError $ unimplemented (srcPos l) msg e
                    _       -> cgSafeEnvFindTyM (builtinOpId BIBracketRef) g'
      consCall g' l BIBracketRef ([vr x1, e2] `zip` nths) opTy
  where
    msg = "Support for dynamic access of enumerations"
    vr  = VarRef $ getAnnotation e1

-- | e1[e2] = e3
consExpr g (AssignExpr l OpAssign (LBracket _ e1 e2) e3) _
  = do  opTy <- cgSafeEnvFindTyM (builtinOpId BIBracketAssign) g
        consCall g l BIBracketAssign ([e1,e2,e3] `zip` nths) opTy

-- | [e1,...,en]
consExpr g (ArrayLit l es) _
  = do  opTy <- arrayLitTy (length es) <$> cgSafeEnvFindTyM (builtinOpId BIArrayLit) g
        consCall g l BIArrayLit (es `zip` nths) opTy

-- | {f1:e1,...,fn:en}
consExpr g (ObjectLit l bs) _
  = consCall g l "ObjectLit" (es `zip` nths) $ objLitTy l ps
  where
    (ps, es) = unzip bs

-- | new C(e, ...)
consExpr g (NewExpr l e es) _
  = mseq (consExpr g e Nothing) $ \(x,g') -> do
      t <- cgSafeEnvFindTyM x g'
      case extractCtor g t of
        Just ct -> consCall g l "constructor" (es `zip` nths) ct
        Nothing -> cgError $ errorConstrMissing (srcPos l) t

-- | super
consExpr g (SuperRef l) _
  | Just thisT  <- cge_this g
  , Just tSuper <- getSuperType (envCHA g) thisT
  = Just <$> cgEnvAddFresh "15" l (VI Local WriteGlobal Initialized tSuper) g
  | otherwise
  = cgError $ errorSuper (fSrc l)

-- | function(xs) { }
consExpr g (FuncExpr l fo xs body) tCtxO
  | Just ft       <-  funTy
  = do  kft       <-  freshTyFun g l ft
        fts       <-  cgFunTys l f xs kft
        forM_ fts  $  consCallable l g f xs body
        Just      <$> cgEnvAddFresh "16" l (VI Local WriteLocal Initialized kft) g
  | otherwise
  = cgError $ errorNoFuncAnn $ srcPos l
  where
    funTy | [ft] <- [ t | SigAnn _ t <- fFact l ] = Just ft
          | Just ft <- tCtxO = Just ft
          | otherwise        = Nothing
    f     = maybe (F.symbol "<anonymous>") F.symbol fo

-- not handled
consExpr _ e _ = cgError $ unimplemented l "consExpr" e where l = srcPos  e


-- To be replaced with bounded call
-- --------------------------------------------------------------------------------
-- consCast :: CGEnv -> AnnLq -> Expression AnnLq -> RefType -> CGM (Maybe (Id AnnLq, CGEnv))
-- --------------------------------------------------------------------------------
-- -- | Only freshen if TFun, otherwise the K-var will be instantiated with false
-- consCast g l e tc
--   = do  opTy    <- cgSafeEnvFindTyM (builtinOpId BICastExpr) g
--         tc'     <- freshTyFun g l (rType tc)
--         (x,g')  <- cgEnvAddFresh "3" l (tc', WriteLocal, Initialized) g
--         consCall g' l "user-cast" (FI Nothing [(VarRef l x, Nothing),(e, Just tc')]) opTy


-- | Dead code
--   Only prove the top-level false.
consDeadCode g l es t
  = do mapM_ (\e -> subType l e g t tBot) es
       return Nothing
    where
       tBot = t `nubstrengthen` F.bot (rTypeR t)

-- -- | UpCast(x, t1 => t2)
-- consUpCast g l x _ t2
--   = do VI a i tx <- safeEnvFindTyWithAsgnM x g
--        ztx       <- zipTypeUpM g x tx t2
--        cgEnvAddFresh "4" l (ztx,a,i) g
--
-- -- | DownCast(x, t1 => t2)
-- consDownCast g l x _ t2
--   = do VI a i tx <- safeEnvFindTyWithAsgn x g
--        txx       <- zipTypeUpM g x tx tx
--        tx2       <- zipTypeUpM g x t2 tx
--        ztx       <- zipTypeDownM g x tx t2
--        subType l (errorDownCast (srcPos l) txx t2) g txx tx2
--        cgEnvAddFresh "5" l (ztx,a,i) g
--

-- | `consCall g l fn ets ft0`:
--
--   * @ets@ is the list of arguments, as pairs of expressions and optionally
--     contextual types on them.
--   * @ft0@ is the function's signature -- the 'this' argument should have been
--     made explicit by this point.
--
--------------------------------------------------------------------------------
consCall :: PP a
         => CGEnv
         -> AnnLq
         -> a
         -> [(Expression AnnLq, Maybe RefType)]
         -> RefType
         -> CGM (Maybe (Id AnnLq, CGEnv))
--------------------------------------------------------------------------------

--   1. Fill in @instantiateFTy@ to get a monomorphic instance of @ft@
--      i.e. the callee's RefType, at this call-site (You may want
--      to use @freshTyInst@)
--   2. Use @consExpr@ to determine types for arguments @es@
--   3. Use @subType@ to add constraints between the types from (step 2) and (step 1)
--   4. Use the @F.subst@ returned in 3. to substitute formals with actuals in
--      output type of callee.

consCall g l fn ets ft0
  = mseq (consScan consExpr g ets) $ \(xes, g') -> do
      ts <- mapM (`cgSafeEnvFindTyM` g') xes
      case ol of
        -- If multiple are valid, pick the first one
        (ft:_) -> consInstantiate l g' fn ft ts xes
        _      -> cgError $ errorNoMatchCallee (srcPos l) fn (toType <$> ts)
                              (toType <$> callSigs)
  where
    ol = [ lt | Overload cx t <- fFact l
              , cge_ctx g == cx
              , lt <- callSigs
              , arg_type (toType t) == arg_type (toType lt) ]
    callSigs  = extractCall g ft0
    arg_type t = (\(a,b,_) -> (a,b)) <$> bkFun t

--------------------------------------------------------------------------------
consInstantiate :: PP a
                => AnnLq -> CGEnv -> a
                -> RefType            -- Function spec
                -> [RefType]          -- Input types
                -> [Id AnnLq]         -- Input ids
                -> CGM (Maybe (Id AnnLq, CGEnv))
--------------------------------------------------------------------------------
consInstantiate l g fn ft ts xes
  = do  (_,its1,ot) <- instantiateFTy l g fn ft
        ts1         <- zipWithM (instantiateTy l g) [1..] ts
        (ts3, ot')  <- subNoCapture l its1 xes ot
        _           <- zipWithM_ (subType l err g) (ts1) (map b_type ts3)
        Just       <$> cgEnvAddFresh "5" l (VI Local WriteLocal Initialized ot') g
  where
    err              = errorLiquid' l

-- Special casing conditional expression call here because we'd like the
-- arguments to be typechecked under a different guard each.
consCallCondExpr g l fn ets ft0
  = mseq (consCondExprArgs (srcPos l) g ets) $ \(xes, g') -> do
      ts <- T.mapM (`cgSafeEnvFindTyM` g') xes
      case [ lt | Overload cx t <- fFact l
                , cge_ctx g == cx
                , lt <- callSigs
                , toType t == toType lt ] of
        [ft] -> consInstantiate l g' fn ft ts xes
        _    -> cgError $ errorNoMatchCallee (srcPos l) fn (toType <$> ts) (toType <$> callSigs)
  where
    callSigs    = extractCall g ft0

----------------------------------------------------------------------------------
instantiateTy :: AnnLq -> CGEnv -> Int -> RefType -> CGM RefType
----------------------------------------------------------------------------------
instantiateTy l g i t = freshTyInst l g αs ts t'
    where
      (αs, t')        = mapFst (map btvToTV) $ bkAll t
      ts              = envGetContextTypArgs i g l αs

---------------------------------------------------------------------------------
instantiateFTy :: PP a => AnnLq -> CGEnv -> a -> RefType -> CGM  ([BTVar F.Reft], [Bind F.Reft], RefType)
---------------------------------------------------------------------------------
instantiateFTy l g fn ft
  = do  t'   <- freshTyInst l g αs ts t
        maybe err return $ bkFun t'
    where
      (αs, t) = mapFst (map btvToTV) $ bkAll ft
      ts      = envGetContextTypArgs 0 g l αs
      err     = cgError $ errorNonFunction (srcPos l) fn ft

-----------------------------------------------------------------------------------
consScan :: (CGEnv -> a -> b -> CGM (Maybe (c, CGEnv))) -> CGEnv -> [(a,b)] -> CGM (Maybe ([c], CGEnv))
-----------------------------------------------------------------------------------
-- consScan f g (FI (Just x) xs)
--   = do  z  <- (uncurry $ f g) x
--         case z of
--           Just (x', g') ->
--               do zs  <- fmap (mapFst reverse) <$> consFold step ([], g') xs
--                  case zs of
--                    Just (xs', g'') -> return $ Just (FI (Just x') xs', g'')
--                    _               -> return $ Nothing
--           _ -> return Nothing
--   where
--     step (ys, g) (x,y) = fmap (mapFst (:ys))   <$> f g x y
--
-- consScan f g (FI Nothing xs)
consScan f g xs
  = do  z <- fmap (mapFst reverse) <$> consFold step ([], g) xs
        case z of
          Just (xs', g') -> return $ Just (xs', g')
          _              -> return $ Nothing
  where
    step (ys, g) (x,y) = fmap (mapFst (:ys))   <$> f g x y

---------------------------------------------------------------------------------
consCondExprArgs :: SrcSpan -> CGEnv
                 -> [(Expression AnnLq, Maybe RefType)]
                 -> CGM (Maybe ([Id AnnLq], CGEnv))
---------------------------------------------------------------------------------
consCondExprArgs l g [(c,tc),(t,tt),(x,tx),(y,ty)]
  = mseq (consExpr g c tc) $ \(c_,gc) ->
      mseq (consExpr gc t tt) $ \(t_,gt) ->
        withGuard gt c_ True x tx >>= \case
          Just (x_, gx) ->
              withGuard gx c_ False y ty >>= \case
                Just (y_, gy) -> return $ Just ([c_,t_,x_,y_], gy)
                Nothing ->
                    do ttx       <- cgSafeEnvFindTyM x_ gx
                       let tty    = fromMaybe ttx ty    -- Dummy type if ty is Nothing
                       (y_, gy') <- cgEnvAddFresh "6" l (VI Local WriteLocal Initialized tty) gx
                       return    $ Just ([c_,t_,x_,y_], gy')
          Nothing ->
              withGuard gt c_ False y ty >>= \case
                Just (y_, gy) ->
                    do tty       <- cgSafeEnvFindTyM y_ gy
                       let ttx    = fromMaybe tty tx    -- Dummy type if tx is Nothing
                       (x_, gx') <- cgEnvAddFresh "7" l (VI Local WriteLocal Initialized ttx) gy
                       return     $ Just ([c_,t_,x_,y_], gx')
                Nothing       -> return $ Nothing
  where
    withGuard g cond b x tx =
      fmap (mapSnd envPopGuard) <$> consExpr (envAddGuard cond b g) x tx

consCondExprArgs l _ _ = cgError $ impossible l "consCondExprArgs"

---------------------------------------------------------------------------------
consFold :: (t -> b -> CGM (Maybe t)) -> t -> [b] -> CGM (Maybe t)
---------------------------------------------------------------------------------
consFold f          = foldM step . Just
  where
    step Nothing _  = return Nothing
    step (Just g) x = f g x

---------------------------------------------------------------------------------
consWhile :: CGEnv -> AnnLq -> Expression AnnLq -> Statement AnnLq -> CGM (Maybe CGEnv)
---------------------------------------------------------------------------------

{- Typing Rule for `while (cond) {body}`

      (a) xtIs         <- fresh G [ G(x) | x <- Φ]
      (b) GI            = G, xtIs
      (c) G            |- G(x)  <: GI(x)  , ∀x∈Φ      [base]
      (d) GI           |- cond : (xc, GI')
      (e) GI', xc:true |- body : GI''
      (f) GI''         |- GI''(x') <: GI(x)[Φ'/Φ]     [step]
      ---------------------------------------------------------
          G            |- while[Φ] (cond) body :: GI', xc:false

   The above rule assumes that phi-assignments have already been inserted. That is,

      i = 0;
      while (i < n){
        i = i + 1;
      }

   Has been SSA-transformed to

      i_0 = 0;
      i_2 = i_0;
      while [i_2] (i_2 < n) {
        i_1  = i_2 + 1;
        i_2' = i_1;
      }

-}
consWhile g l cond body
-- XXX: The RHS ty comes from PhiVarTy
  = do  (gI,tIs)         <- freshTyPhis l g xs (map toType ts)               -- (a) (b)
        _                <- consWhileBase l xs tIs g                         -- (c)
        mseq (consExpr gI cond Nothing) $ \(xc, gI') ->                      -- (d)
          do  z          <- consStmt (envAddGuard xc True gI') body          -- (e)
              whenJustM z $ consWhileStep l xs tIs                           -- (f)
              return      $ Just $ envAddGuard xc False gI'
    where
        (xs,ts) = unzip $ [xts | PhiVarTy xts <- fFact l]

consWhileBase l xs tIs g
  = do  baseT <- mapM (`cgSafeEnvFindTyM` g) xs
        zipWithM_ (subType l err g) baseT tIs
  where
    err = errorLiquid' l

consWhileStep l xs tIs gI''
  = do  stepTs <- mapM (`cgSafeEnvFindTyM` gI'') xs'
        zipWithM_ (subType l err gI'') stepTs tIs'                           -- (f)
  where
    tIs' = F.subst su <$> tIs
    xs'  = mkNextId   <$> xs
    su   = F.mkSubst   $  safeZip "consWhileStep" (F.symbol <$> xs) (F.eVar <$> xs')
    err  = errorLiquid' l

whenJustM Nothing  _ = return ()
whenJustM (Just x) f = f x

----------------------------------------------------------------------------------
envJoin :: AnnLq -> CGEnv -> Maybe CGEnv -> Maybe CGEnv -> CGM (Maybe CGEnv)
----------------------------------------------------------------------------------
envJoin _ _ Nothing x           = return x
envJoin _ _ x Nothing           = return x
envJoin l g (Just g1) (Just g2) = Just <$> envJoin' l g g1 g2

----------------------------------------------------------------------------------
envJoin' :: AnnLq -> CGEnv -> CGEnv -> CGEnv -> CGM CGEnv
----------------------------------------------------------------------------------

-- 1. use @envFindTy@ to get types for the phi-var x in environments g1 AND g2
--
-- 2. use @freshTyPhis@ to generate fresh types (and an extended environment with
--    the fresh-type bindings) for all the phi-vars using the unrefined types
--    from step 1
--
-- 3. generate subtyping constraints between the types from step 1 and the fresh
--    types
--
-- 4. return the extended environment
--
envJoin' l g g1 g2
  = do  --
        -- LOCALS: as usual
        --
        g1'       <- cgEnvAdds "envJoin-0" (zip xls l1s) g1
        g2'       <- cgEnvAdds "envJoin-1" (zip xls l2s) g2
        --
        -- t1s and t2s should have the same raw type, otherwise they wouldn't
        -- pass TC (we don't need to pad / fix them before joining).
        -- So we can use the raw type from one of the two branches and freshen
        -- up that one.
        -- TODO: Add a raw type check on t1 and t2
        --
        (g',ls)   <- freshTyPhis' l g xls $ (\(VI loc a i t) -> VI loc a i $ toType t) <$> l1s
        l1s'      <- mapM (`cgSafeEnvFindTyM` g1') xls
        l2s'      <- mapM (`cgSafeEnvFindTyM` g2') xls
        _         <- zipWithM_ (subType l err g1') l1s' ls
        _         <- zipWithM_ (subType l err g2') l2s' ls
        --
        -- GLOBALS:
        --
        let (xgs, gl1s, _) = unzip3 $ globals (zip3 xs t1s t2s)
        (g'',gls) <- freshTyPhis' l g' xgs $ (\(VI loc a i t) -> VI loc a i $ toType t) <$> gl1s
        gl1s'     <- mapM (`cgSafeEnvFindTyM` g1') xgs
        gl2s'     <- mapM (`cgSafeEnvFindTyM` g2') xgs
        _         <- zipWithM_ (subType l err g1') gl1s' gls
        _         <- zipWithM_ (subType l err g2') gl2s' gls
        --
        -- PARTIALLY UNINITIALIZED
        --
        -- let (xps, ps) = unzip $ partial $ zip3 xs t1s t2s
        -- If the variable was previously uninitialized, it will continue to be
        -- so; we don't have to update the environment in this case.
        return     $ g''
    where
        (t1s, t2s) = unzip $ catMaybes $ getPhiTypes l g1 g2 <$> xs
        -- t1s : the types of the phi vars in the 1st branch
        -- t2s : the types of the phi vars in the 2nd branch
        (xls, l1s, l2s) = unzip3 $ locals (zip3 xs t1s t2s)
        xs    = [ xs | PhiVarTC xs <- fFact l]
        err   = errorLiquid' l


getPhiTypes _ g1 g2 x =
  case (envFindTyWithAsgn x g1, envFindTyWithAsgn x g2) of
    (Just t1, Just t2) -> Just (t1,t2)
    (_      , _      ) -> Nothing


locals  ts = [(x,s1,s2) | (x, s1@(VI Local WriteLocal _ _),
                              s2@(VI Local WriteLocal _ _)) <- ts ]

globals ts = [(x,s1,s2) | (x, s1@(VI Local WriteGlobal Initialized _),
                              s2@(VI Local WriteGlobal Initialized _)) <- ts ]

-- partial ts = [(x,s2)    | (x, s2@(_, WriteGlobal, Uninitialized)) <- ts]
--           ++ [(x,s1)    | (x, s1@(_, WriteGlobal, Uninitialized)) <- ts]


errorLiquid' = errorLiquid . srcPos

traceTypePP l msg act
  = act >>= \case
      Just (x,g) ->
          do  t <- cgSafeEnvFindTyM x g
              return $ Just $ trace (ppshow (srcPos l) ++
                                     " " ++ msg ++
                                     ": " ++ ppshow x ++
                                     " :: " ++ ppshow t) (x,g)
      Nothing ->  return Nothing


-- Local Variables:
-- flycheck-disabled-checkers: (haskell-liquid)
-- End:
