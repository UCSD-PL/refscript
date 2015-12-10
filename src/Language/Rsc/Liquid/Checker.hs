{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE ViewPatterns              #-}

-- | Top Level for Refinement Type checker
module Language.Rsc.Liquid.Checker (verifyFile) where

import           Control.Applicative             (pure, (<$>))
import           Control.Arrow                   (first, (***))
import           Control.Exception               (throw)
import           Control.Monad
import           Data.Function                   (on)
import qualified Data.HashMap.Strict             as HM
import           Data.List                       (sortBy)
import           Data.Maybe                      (catMaybes, fromJust, fromMaybe)
import qualified Data.Text                       as T
import qualified Data.Traversable                as T
import qualified Language.Fixpoint.Config        as C
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Files         (Ext (..), extFileName)
import           Language.Fixpoint.Interface     (solve)
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Names         (symbolString)
import qualified Language.Fixpoint.Types         as F
import qualified Language.Fixpoint.Visitor       as V
import           Language.Rsc.Annotations
import           Language.Rsc.AST
import           Language.Rsc.ClassHierarchy
import           Language.Rsc.CmdLine
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
import           Language.Rsc.Typecheck.Sub
import           Language.Rsc.Typecheck.Subst    (apply, fromList)
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
import           Language.Rsc.TypeUtilities
import           System.Console.CmdArgs.Default
import           System.FilePath.Posix           (dropExtension)
import           Text.Printf

import qualified Data.Foldable                   as FO
import           Text.PrettyPrint.HughesPJ       hiding (first)

import           Debug.Trace                     hiding (traceShow)

type Result = (A.UAnnSol RefType, F.FixResult Error)
type Err a  = Either (F.FixResult Error) a

--------------------------------------------------------------------------------
verifyFile :: Config -> FilePath -> [FilePath] -> IO Result
--------------------------------------------------------------------------------
verifyFile cfg f fs = fmap (either (A.NoAnn,) id) $ runEitherIO $
  do  p     <- announce "Parse" $ EitherIO   $ parseRscFromFiles fs
      cfg'  <- liftIO           $ withPragmas cfg (pOptions p)
      _     <- pure             $ checkTypeWF p
      cha   <- liftEither       $ mkCHA p
      -- _   <- liftIO            $ dumpJS f cha "-parse" p
      ssa   <- announce "SSA"   $ EitherIO (ssaTransform p cha)

      cha0  <- liftEither       $ mkCHA ssa
      _     <- liftIO           $ dumpJS f cha0 "-ssa" ssa
      tc    <- announce "TC"    $ EitherIO (typeCheck cfg ssa cha)

      cha1  <- liftEither       $ mkCHA tc
      _     <- liftIO           $ dumpJS f cha1 "-tc" tc
      cgi   <- announce "CG"    $ pure (generateConstraints cfg f tc cha)
      res   <- announce "Solve" $ liftIO (solveConstraints cfg' f cgi)
      return   res

announce s a = liftIO (startPhase Loud s) >> a

(>>=>) :: IO (Either a b) -> (b -> IO c) -> IO (Either a c)
act >>=> k = do
  r <- act
  case r of
    Left l  -> return $  Left l -- (A.NoAnn, l)
    Right x -> Right <$> k x

eAct :: IO (Err a) -> IO a
eAct m = do
  x <- m
  case x of
    Left  l -> throw l
    Right r -> return r


-- | solveConstraint: solve with `ueqAllSorts` enabled.
--------------------------------------------------------------------------------
solveConstraints :: Config
                 -> FilePath
                 -> CGInfo
                 -> IO (A.UAnnSol RefType, F.FixResult Error)
--------------------------------------------------------------------------------
solveConstraints cfg f cgi
  = do F.Result r s <- solve fpConf (cgi_finfo cgi)
       -- let c0        = ppWCI cgi r
       let r'        = fmap (ci_info . F.sinfo) r
       let anns      = cgi_annot cgi
       let sol       = applySolution s
       return        $ (A.SomeAnn anns sol, r')
  where
    fpConf     = def { C.real        = real cfg
                     , C.ueqAllSorts = C.UAS True
                     , C.srcFile     = f
                     , C.extSolver   = extSolver cfg
                     }

-- TODO: move elsewhere
ppWCI cgi (F.Unsafe ws) = vcat $ map f ws
  where
    f wci = pp (F.sid wci) <+> pp (F.fromListSEnv (F.clhs bs wci)) $+$ nest 2 (text "=>" $+$ pp (F.crhs wci))
    cm    = F.cm $ cgi_finfo cgi
    bs    = F.bs $ cgi_finfo cgi
ppWCI _ F.Safe = text "SAFE"

instance PP (F.Result Cinfo) where
  pp (F.Result cinfo kvars)
    = vcat ((ppB <$>) (HM.toList kvars))
    where
      ppB (x, t) = pp x <+> dcolon <+> pp t

instance PP (F.FixResult (F.WrappedC Cinfo)) where
  pp (F.Unsafe ws) = vcat $ map ppW ws
    where
      ppW wci = text "ID" <+> pp (F.sid wci) -- text "<:" <+> pp (F.crhs wci)
--       _ = F.sinfo wci
--       _ = F.clhs undefined wci

  pp F.Safe = text "Safe"


instance PP F.KVar where
  pp k = pprint k

--------------------------------------------------------------------------------
applySolution :: F.FixSolution -> A.UAnnInfo RefType -> A.UAnnInfo RefType
--------------------------------------------------------------------------------
applySolution  = fmap . fmap . tx
  where
    tx         = F.mapPredReft . txPred
    txPred s   = F.simplify . V.mapKVars (appSol s)
    appSol s k = Just $ HM.lookupDefault F.PTop k s



-- --------------------------------------------------------------------------------
-- generateConstraints :: Config -> FilePath -> RefScript-> CGInfo
-- --------------------------------------------------------------------------------
-- generateConstraints cfg f pgm = getCGInfo cfg f pgm $ consRsc pgm
--



-- | Debug info
--
dumpJS f cha s p = writeFile (extFileName Ts (dropExtension f ++ s)) $ show
                 $  pp p
                $+$ inComments (pp cha)
                $+$ ppCasts p

ppCasts (Rsc { code = Src fs })
  | isEmpty castDoc = empty
  | otherwise       = inComments (nest 2 castDoc)
  where
    castDoc = fcat $ map ppEntry entries
    ppEntry = \(_, c) -> pp c
    entries = [ (srcPos a, c) | a <- concatMap FO.toList fs
                              , TCast _ c <- fFact a ]

--------------------------------------------------------------------------------
generateConstraints :: Config -> FilePath -> RefScript -> ClassHierarchy F.Reft -> CGInfo
--------------------------------------------------------------------------------
generateConstraints cfg f pgm = getCGInfo cfg f pgm . consRsc pgm

--------------------------------------------------------------------------------
consRsc :: RefScript -> ClassHierarchy F.Reft -> CGM ()
--------------------------------------------------------------------------------
consRsc p@(Rsc {code = Src fs}) cha
  = do  g   <- initGlobalEnv p cha
        _   <- consStmts g fs
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
    g     = CGE nms bnds ctx pth cha fenv grd cst mut thisT (-1)
    nms   = mkVarEnv (accumVars ss)
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
    g'    = CGE nms bnds ctx pth cha fenv grd cst mut thisT fnId
    nms   = mkVarEnv (accumVars s) `mappend` toFgn (envNames g)
    bnds  = envBounds g
    ctx   = cge_ctx g
    pth   = extendAbsPath (cge_path g) n
    cha   = cge_cha g
    fenv  = cge_fenv g
    grd   = cge_guards g
    cst   = cge_consts g
    mut   = Nothing
    thisT = Nothing
    fnId  = cge_fnid g

-- | `initCallableEnv l f i xs (αs, ts, t) g`
--
--    * Pushes a new context @i@
--    * Adds return type @t@
--    * Adds binders for the type variables @αs@
--    * Adds binders for the arguments @ts@
--    * Adds binder for the 'arguments' variable
--
--------------------------------------------------------------------------------
initCallableEnv :: (PP x, IsLocated x)
                => AnnLq -> CGEnv -> x
                -> IOverloadSig F.Reft
                -> [Id (AnnR F.Reft)]
                -> [Statement (AnnR F.Reft)]
                -> CGM CGEnv
--------------------------------------------------------------------------------
initCallableEnv l g f fty xs s
  = do  g1 <- cgEnvAdds l ("init-func-" ++ ppshow f ++ "-0") params g0
        g2 <- cgEnvAdds l ("init-func-" ++ ppshow f ++ "-1") arg g1
        freshenCGEnvM g2
  where
    g0     = CGE nms bnds ctx pth cha fenv grd cst mut thisT fnId
             -- No FP binding for these
    nms    = toFgn (envNames g)
           & mappend (mkVarEnv (accumVars s))
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
    params = [(x, VI Local WriteLocal Initialized t_) | (x, t_) <- safeZip "initCallableEnv" xs ts ]
    arg    = single (argId (srcPos l) (fId l), mkArgTy l ts)
    ts     = map b_type xts
    αs     = map btvToTV bs
    (i, (bs,xts,t)) = fty
    fnId   = fId l


--------------------------------------------------------------------------------
-- | Constraint generation
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
consFun :: CGEnv -> Statement AnnLq -> CGM ()
--------------------------------------------------------------------------------
consFun _ (FunctionStmt _ _ _ Nothing)
  = return ()
consFun g (FunctionStmt l f xs (Just body))
  = case envFindTy f (cge_names g) of
      Just (VI _ _ _ t) -> cgFunTys l f xs t >>= mapM_ (consCallable l g f xs body)
      Nothing   -> cgError $ errorMissingSpec (srcPos l) f

consFun _ s
  = die $ bug (srcPos s) "consFun called not with FunctionStmt"

-- | @consCallable@ checks a function body against a *one* of multiple
--   conjuncts of an overloaded (intersection) type signature.
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
--
--   Mutabilities have been checked at TC
--
--   TODO: add indexer updates
--
consStmt g (ExprStmt l (AssignExpr _ OpAssign (LDot _ e1 f) e2))
  = mseq (consExpr g e1 Nothing) $ \(x1,g') -> do
      t <- cgSafeEnvFindTyM x1 g'
      case getProp l g' f t of
        Right (ftys . map snd -> ts) -> fmap snd <$> consExprTs l g BISetProp e2 ts
        Left e -> cgError e
  where
      ftys fs = [ t | FI _ _ t <- fs ]

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
  mseq (cgSafeEnvFindTyM (builtinOpId BITruthy) g
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
  = do _ <- subType l Nothing g tVoid (cgEnvFindReturn g)
       return Nothing

-- return e
consStmt g (ReturnStmt l (Just e))
  -- Special case for UniqueMutable
  | VarRef lv x <- e
  , Just t <- cgEnvFindTy x g
  , isUMRef t
  = do  g' <- cgEnvAdds l "Return" [(fn, VI Local Ambient Initialized $ finalizeTy t)] g
        consStmt g' (ReturnStmt l (Just (CallExpr l (VarRef lv fn) [e])))
  -- Normal case
  | otherwise
  = do  _ <- consCall g l "return" [(e, Just retTy)] $ returnTy retTy True
        return Nothing
  where
    retTy = cgEnvFindReturn g
    fn    = Id l "__finalize__"

-- throw e
consStmt g (ThrowStmt _ e)
  = consExpr g e Nothing >> return Nothing

-- function f(x1...xn){ s }
consStmt g s@FunctionStmt{}
  = consFun g s >> return (Just g)

--
-- class A<V extends S> [extends B<T>] {..}
--
--  * Add the type vars V in the environment
--
--  * Compute type for "this" and add that to the env as well.
--
consStmt g (ClassStmt l x ce)
  = do  d <- resolveTypeM l g nm
        mapM_ (consClassElt g d) ce
        return $ Just g
  where
    nm = QN (cge_path g) (F.symbol x)

consStmt g (InterfaceStmt _ _)
  = return $ Just g

consStmt g EnumStmt{}
  = return $ Just g

consStmt g (ModuleStmt _ n body)
  = initModuleEnv g n body >>= (`consStmts` body) >> return (Just g)

-- OTHER (Not handled)
consStmt _ s
  = errorstar $ "consStmt: not handled " ++ ppshow s


--------------------------------------------------------------------------------
consVarDecl :: CGEnv -> VarDecl AnnLq -> CGM (Maybe CGEnv)
--------------------------------------------------------------------------------
-- PV: Some ugly special casing for Function expressions
--
-- TODO: Fix this with contextual typing
--
consVarDecl g (VarDecl _ x (Just e@FuncExpr{}))
  = (snd <$>) <$> consExpr g e (v_type <$> envFindTy x (cge_names g))

consVarDecl g (VarDecl l x (Just e))
  = case envFindTy x (cge_names g) of
      -- | Local
      Nothing ->
        mseq (consExpr g e Nothing) $ \(y,gy) -> do
          eT      <- cgSafeEnvFindTyM y gy
          Just   <$> cgEnvAdds l "consVarDecl" [(x, VI Local WriteLocal Initialized eT)] gy

      Just (VI lc  WriteLocal _ t) ->
        mseq (consExpr g e $ Just t) $ \(y,gy) -> do
          eT      <- cgSafeEnvFindTyM y gy
          _       <- subType l Nothing gy eT t
          Just   <$> cgEnvAdds l "consVarDecl" [(x, VI lc WriteLocal Initialized eT)] gy

      -- | Global
      Just (VI lc WriteGlobal _ t) -> do
        fta       <- freshenType WriteGlobal g l t
        mseq (consExpr g e $ Just fta) $ \(y, gy) -> do
          eT      <- cgSafeEnvFindTyM y gy
          _       <- subType l Nothing gy eT fta
          _       <- subType l Nothing gy fta t
          Just   <$> cgEnvAdds l "consVarDecl" [(x, VI lc WriteGlobal Initialized fta)] gy

      -- | ReadOnly
      Just (VI lc RdOnly _ t) ->
        mseq (consExpr g e (Just t)) $ \(y,gy) -> do
          eT      <- cgSafeEnvFindTyM y gy
          _       <- subType l Nothing gy eT t
          Just   <$> cgEnvAdds l "consVarDecl" [(x, VI lc RdOnly Initialized t)] gy

      _ -> cgError $ errorVarDeclAnnot (srcPos l) x

consVarDecl g (VarDecl l x Nothing)
  = case envFindTy x (cge_names g) of
      Just (VI lc Ambient _ t) ->
          Just <$> cgEnvAdds l "consVarDecl" [(x, VI lc Ambient Initialized t)] g
      _ ->   -- The rest should have fallen under the 'undefined' initialization case
          error "LQ: consVarDecl this shouldn't happen"


-- | `consExprT l g e t` checks expression @e@ against type @t@.
--   Special-casing FuncExpr because we can't infer a type for a
--   function expression and then check for it - it needs to be
--   present in the first place.
--   XXX: perhaps other cases should work similarly.
--
--------------------------------------------------------------------------------
consExprT :: AnnLq -> CGEnv -> String -> Expression AnnLq -> RefType -> CGM (Maybe (Id AnnLq, CGEnv))
--------------------------------------------------------------------------------
consExprT _ g _  e@FuncExpr{} t = consExpr g e (Just t)
consExprT l g fn e            t = consCall g l fn [(e, Just t)] (idTy t)

consExprTs l g fn e ts = consCall g l fn [(e, Nothing)] (idTys ts)


--------------------------------------------------------------------------------
consClassElt :: CGEnv -> TypeDecl F.Reft -> ClassElt AnnLq -> CGM ()
--------------------------------------------------------------------------------
consClassElt g0 (TD sig@(TS _ (BGen nm bs) _) ms) (Constructor l xs body)
  = do  let g = g0
              & initClassInstanceEnv sig
              & initClassCtorEnv sig
        g'   <- cgEnvAdds l "ctor" exitP g
        ts   <- cgFunTys l ctor xs ctorT
        mapM_ (consCallable l g' ctor xs body) ts
  where
    thisT = TRef (Gen nm (map btVar bs)) fTop
    ctor  = builtinOpId BICtor
    cExit = builtinOpId BICtorExit

    -- substOffsetThis exitTy ???
    exitP = [(cExit, VI Local Ambient Initialized $ mkFun (bs, xts, ret))]

    ret   = thisT `strengthen` F.reft (F.vv Nothing) (F.pAnd $ bnd <$> out)
    xts   = sortBy c_sym [ B x t' | (x, FI _ _ t) <- F.toListSEnv (i_mems ms)
                                  , let t' = unqualifyThis g0 thisT t ]
    out   = [ f | (f, FI _ Final _) <- F.toListSEnv (i_mems ms) ]
    bnd f = F.PAtom F.Eq (mkOffset v_sym $ symbolString f) (F.eVar f)
    v_sym = F.symbol $ F.vv Nothing
    c_sym = on compare b_sym

    ctorT = fromMaybe (die $ unsupportedNonSingleConsTy (srcPos l)) (tm_ctor ms)

-- | Static field
--
consClassElt g (TD sig ms) (MemberVarDecl l True x (Just e))
  | Just (FI _ _ t) <- F.lookupSEnv (F.symbol x) (s_mems ms)
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
  | Just (MI _ mts) <- F.lookupSEnv (F.symbol x) (s_mems ms)
  = do  let t = mkAnd $ map snd mts
        its <- cgFunTys l x xs t
        mapM_ (consCallable l g x xs body) its
  | otherwise
  = cgError  $ errorClassEltAnnot (srcPos l) (sigTRef sig) x

-- | Instance method
--
--   TODO: The method's mutability should influence the type of tThis that is used
--         as a binder to this.
--   TODO: Also might not need 'cge_this'
--
consClassElt g (TD sig ms) (MemberMethDecl l False x xs body)
  | Just (MI _ mts) <- F.lookupSEnv (F.symbol x) (i_mems ms)
  = do  let (ms', ts) = unzip mts
        its          <- cgFunTys l x xs $ mkAnd ts
        let mts'      = zip ms' its
        mapM_ (\(m,t) -> do
            g'   <- cgEnvAdds l "consClassElt-meth" [eThis] (mkEnv m)
            consCallable l g' x xs body t
          ) mts'

  | otherwise
  = cgError $ errorClassEltAnnot (srcPos l) (sigTRef sig) x
  where
    mkEnv m     = g
                & initClassInstanceEnv sig
                & initClassMethEnv m sig
    TS _ bgen _ = sig
    BGen nm bs  = bgen
    tThis       = TRef (Gen nm (map btVar bs)) fTop
    idThis      = Id l "this"
    eThis       = (idThis, VI Local RdOnly Initialized tThis)

--------------------------------------------------------------------------------
consAsgn :: AnnLq -> CGEnv -> Id AnnLq -> Expression AnnLq -> CGM (Maybe CGEnv)
--------------------------------------------------------------------------------
consAsgn l g x e =
  case envFindTyWithAsgn x g of
    -- This is the first time we initialize this variable
    Just (VI lc WriteGlobal Uninitialized t) ->
      do  t' <- freshenType WriteGlobal g l t
          mseq (consExprT l g "assign" e t') $ \(_, g') -> do
            g'' <- cgEnvAdds l "consAsgn-0" [(x, VI lc WriteGlobal Initialized t')] g'
            return $ Just g''

    Just (VI _ WriteGlobal _ t) -> mseq (consExprT l g "assign" e t) $ \(_, g') ->
                                     return $ Just g'
    Just (VI lc a i t) -> mseq (consExprT l g "assign" e t) $ \(x', g') -> do
                             t_     <- cgSafeEnvFindTyM x' g'
                             Just  <$> cgEnvAdds l "consAsgn-1" [(x, VI lc a i t_)] g'
    Nothing -> mseq (consExpr g e Nothing) $ \(x', g') -> do
                 t      <- cgSafeEnvFindTyM x' g'
                 Just  <$> cgEnvAdds l "consAsgn-1" [(x, VI Local WriteLocal Initialized t)] g'


-- --------------------------------------------------------------------------------
-- withContextual :: PP a => AnnLq -> a -> Maybe RefType -> CGM (Maybe (Id AnnLq, CGEnv))
--                                                       -> CGM (Maybe (Id AnnLq, CGEnv))
-- --------------------------------------------------------------------------------
-- withContextual l e (Just s) act = act >>= go
--   where
--     go (Just (x, g)) = do t <- cgSafeEnvFindTyM x g
--                           subType l errMsg g t s
--                           return (Just (x, g))
--     go Nothing       = return Nothing
--     errMsg           = errorContextual l e s
-- withContextual _ _ _ act = act

-- | @consExpr g e@ returns a pair (g', x') where x' is a fresh,
-- temporary (A-Normalized) variable holding the value of `e`,
-- g' is g extended with a binding for x' (and other temps required for `e`)
--------------------------------------------------------------------------------
consExpr :: CGEnv -> Expression AnnLq -> Maybe RefType -> CGM (Maybe (Id AnnLq, CGEnv))
--------------------------------------------------------------------------------
-- | DeadCast
consExpr g (Cast_ l e) s
  = case envGetContextCast g l of
      CDead [] t -> subType l (Just $ bugDeadCast l) g t (tBot t)     >> return Nothing
      CDead es t -> mapM_ (\e -> subType l (Just e)  g t (tBot t)) es >> return Nothing
      _          -> consExpr g e s
  where
       tBot t = t `strengthen` F.bot (rTypeR t)

-- | <T>e
consExpr g ex@(Cast l e) s
  | [tc] <- [ ct | UserCast ct <- fFact l ]
  = consCall g l "Cast" [(e, Just tc)] (castTy tc)
  | otherwise
  = die $ bugNoCasts (srcPos l) ex

consExpr g e@(IntLit l i) s
  = Just <$> cgEnvAddFresh "8" l (tNum `eSingleton` i) g

-- Assuming by default 32-bit BitVector
consExpr g e@(HexLit l x) s
  | Just e <- bitVectorValue x
  = Just <$> cgEnvAddFresh "9" l (tBV32 `strengthen` e) g
  | otherwise
  = Just <$> cgEnvAddFresh "10" l tBV32 g

consExpr g e@(BoolLit l b) s
  = Just <$> cgEnvAddFresh "11" l (pSingleton tBool b) g

consExpr g e@(StringLit l x) s
  = Just <$> cgEnvAddFresh "12" l (tString `eSingleton` T.pack x) g

consExpr g e@(NullLit l) s
  = Just <$> cgEnvAddFresh "13" l tNull g

consExpr g e@(ThisRef l) s
  = case envFindTyWithAsgn x g of
      Just _  -> return $ Just (x, g)
      Nothing -> cgError $ errorUnboundId (fSrc l) "this"
  where
    x = thisId l

consExpr g e@(VarRef l x) s
  -- | undefined
  | F.symbol x == F.symbol "undefined"
  = Just <$> cgEnvAddFresh "0" l tUndef g

  | Just (VI _ WriteGlobal _ t) <- tInfo
  = Just <$> cgEnvAddFresh "0" l t g

  | Just (VI _ a _ t) <- tInfo
  = do  addAnnot (srcPos l) x t
        Just <$> cgEnvAddFresh "cons VarRef" l t g

  | otherwise
  = cgError $ errorUnboundId (fSrc l) x
  where
    tInfo = envFindTyWithAsgn x g

consExpr g ex@(PrefixExpr l o e) s
  = do opTy <- cgSafeEnvFindTyM (prefixOpId o) g
       consCall g l o [(e,Nothing)] opTy

consExpr g (InfixExpr l o@OpInstanceof e1 e2) _
  = mseq (consExpr g e2 Nothing) $ \(x, g') -> do
       t            <- cgSafeEnvFindTyM x g'
       case t of
         TClass x_  -> do opTy <- cgSafeEnvFindTyM (infixOpId o) g
                          consCall g l o (zwNth [e1, StringLit l2 (cc x_)]) opTy
         _          -> cgError $ unimplemented (srcPos l) "tcCall-instanceof" $ ppshow e2
  where
    l2 = getAnnotation e2
    cc (BGen (QN _ s) _) = symbolString s

consExpr g (InfixExpr l o e1 e2) _
  = do opTy <- cgSafeEnvFindTyM (infixOpId o) g
       consCall g l o (zwNth [e1, e2]) opTy

-- | e ? e1 : e2
consExpr g (CondExpr l e e1 e2) (Just t)
  = do  opTy <- mkCondExprTy l g t
        consCallCondExpr g l BICondExpr (zwNth [e, e1, e2]) opTy

consExpr _ e@CondExpr{} Nothing
  = error $ "Cannot check condExpr" ++ ppshow e ++ " with no contextual type."

-- | super(e1,..,en)
consExpr g (CallExpr l (SuperRef _) _) _
  | Just thisT        <- cge_this g
  , Just tSuper       <- getSuperType (envCHA g) thisT
  = error "consExpr super(...) - UNIMPLEMENTED"
  -- = consCall g l "super" (FI Nothing ((,Nothing) <$> es)) ct
  | otherwise
  = cgError $ errorUnboundId (fSrc l) "super"
  where

-- | e.m(es)
consExpr g ex@(CallExpr l em@(DotRef _ e f) es) _
  | isVariadicCall f
  = cgError (unimplemented l "Variadic" ex)

  | otherwise
  = mseq (consExpr g e Nothing) $ \(xRcvr, g') -> do
      tRcvr <- cgSafeEnvFindTyM xRcvr g'
      case getProp l g f tRcvr of
        Right (unzip -> (_, fs)) ->
            case getMutability (envCHA g) tRcvr of
              Just mRcvr -> callOne g' xRcvr tRcvr mRcvr fs
              Nothing -> cgError (bugGetMutability l tRcvr)
        Left er -> cgError er
  where
    callOne g_ _ tRcvr _ [FI o _ ft]
      | o == Req
      = consCall g_ l em (es `zip` nths) ft
      | otherwise
      = cgError (errorCallOptional l f tRcvr)

    callOne g_ xRcvr tRcvr mRcvr [MI o mts]
      | o == Req
      = let ft = mkAnd [ ft_ | (m, ft_) <- mts, isSubtype g_ mRcvr m ]
               & substThis xRcvr in
        consCall g_ l em (es `zip` nths) ft
      | otherwise
      = cgError (errorCallOptional l f tRcvr)

    callOne _ _ _ _ _
      = error "Call to multuple possible bindings not supported"

    isVariadicCall f_ = F.symbol f_ == F.symbol "call"

-- | e(es)
--
consExpr g (CallExpr l e es) _
  = mseq (consExpr g e Nothing) $ \(x, g') ->
      do  ft <- cgSafeEnvFindTyM x g'
          consCall g' l e (es `zip` nths) ft

-- | e.f
--
--   Returns type: { v: _ | v = x.f }, if e => x and `f` is an immutable field
--                 { v: _ | _       }, otherwise
--
consExpr g ef@(DotRef l e f) _
  = mseq (consExpr g e Nothing) $ \(x, g') -> do
      tRcvr <- cgSafeEnvFindTyM x g'
      case getProp l g' f tRcvr of
        Right [(t, FI o a tf)] ->
            case getMutability (envCHA g) tRcvr of
              Just mRcvr ->
                  do  funTy <- mkDotRefFunTy g f tRcvr mRcvr a tf
                      consCall g' l ef [(VarRef (getAnnotation e) x, Nothing)] funTy
              Nothing -> cgError $ bugGetMutability l tRcvr

        Right tfs -> cgError $ unsupportedUnionAccess l tRcvr f
        Left e -> cgError e

-- | e1[e2]
consExpr g e@(BracketRef l e1 e2) _
  = mseq (consExpr g e1 Nothing) $ \(x1,g') -> do
      opTy <- cgSafeEnvFindTyM x1 g' >>= \case
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
consExpr g e@(ArrayLit l es) to
  = arrayLitTy l g e to (length es) >>= \case
      Left ee    -> cgError ee
      Right opTy -> consCall g l BIArrayLit (es `zip` nths) opTy

-- | { f1: e1, ..., fn: en }
consExpr g e@(ObjectLit l pes) to
  = mseq (consScan consExpr g ets) $ \(xes, g') -> do
      ts <- mapM (\x -> (`singleton` x) <$> cgSafeEnvFindTyM x g') xes
      Just <$> cgEnvAddFresh "17" l (TObj tUQ (typeMembersFromList (zip ps (fs ts))) fTop) g'
  where
    ctx     | Just t <- to = i_mems $ typeMembersOfType (envCHA g) t
            | otherwise    = mempty
    ps      = map (F.symbol . fst) pes
    ets     = [(e, fmap f_ty $ F.symbol p `F.lookupSEnv` ctx) | (p, e) <- pes]
    fs      = map $ FI Req Inherited
    ss      = F.symbol


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
  = Just <$> cgEnvAddFresh "15" l tSuper g
  | otherwise
  = cgError $ errorSuper (fSrc l)

-- | function(xs) { }
consExpr g (FuncExpr l fo xs body) tCtxO
  | Just ft       <-  funTy
  = do  kft       <-  freshTyFun g l ft
        fts       <-  cgFunTys l f xs kft
        forM_ fts  $  consCallable l g f xs body
        Just      <$> cgEnvAddFresh "16" l kft g
  | otherwise
  = cgError $ errorNoFuncAnn $ srcPos l
  where
    funTy | [ft] <- [t | SigAnn _ t <- fFact l] = Just ft
          | Just ft <- tCtxO = Just ft
          | otherwise        = Nothing
    f     = maybe (F.symbol "<anonymous>") F.symbol fo

-- not handled
consExpr _ e _ = cgError $ unimplemented l "consExpr" e where l = srcPos  e


--------------------------------------------------------------------------------
validOverloads :: CGEnv -> AnnLq -> RefType -> [RefType]
--------------------------------------------------------------------------------
validOverloads g l ft0
  = [ mkFun t | Overload cx i <- fFact l              -- all overloads
              , cge_ctx g == cx                       -- right context
              , (j, t) <- extractCall g ft0           -- extract callables
              , i == j ]                              -- pick the one resolved at TC

--------------------------------------------------------------------------------
consCall :: PP a => CGEnv -> AnnLq -> a -> [(Expression AnnLq, Maybe RefType)]
                 -> RefType -> CGM (Maybe (Id AnnLq, CGEnv))
--------------------------------------------------------------------------------
consCall g l fn ets (validOverloads g l -> fts)
  = mseq (consScan consExpr g ets) $ \(xes, g') -> do
      ts <- mapM (`cgSafeEnvFindTyM` g') xes
      case fts of
        ft:_ -> consCheckArgs l g' fn ft ts xes
        _    -> cgError $ errorNoMatchCallee (srcPos l) fn ts fts

-- | `consCheckArgs` does the subtyping between the types of the arguments
--   @xes@ and the formal paramaters of @ft@.
--------------------------------------------------------------------------------
consCheckArgs :: PP a => AnnLq -> CGEnv -> a
                      -> RefType                          -- Function spec
                      -> [RefType]                        -- Input types
                      -> [Id AnnLq]                       -- Input ids
                      -> CGM (Maybe (Id AnnLq, CGEnv))
--------------------------------------------------------------------------------
consCheckArgs l g fn ft ts xes
  = do  (rhs, rt) <- instantiateFTy l g fn xes ft
        lhs       <- zipWithM  (instantiateTy l g) [1..] ts
        _         <- zipWithM_ (subType l Nothing g) lhs rhs
        Just      <$> cgEnvAddFresh "5" l rt g

--------------------------------------------------------------------------------
instantiateTy :: AnnLq -> CGEnv -> Int -> RefType -> CGM RefType
--------------------------------------------------------------------------------
instantiateTy l g i (bkAll -> (αs, t))
  = freshTyInst l g αs τs t where τs = envGetContextTypArgs 0 g l αs

--------------------------------------------------------------------------------
instantiateFTy :: PP a => AnnLq -> CGEnv -> a -> [Id AnnLq] -> RefType
                       -> CGM ([RefType], RefType)
--------------------------------------------------------------------------------
instantiateFTy l g fn xes ft@(bkAll -> (αs, t))
  = bkFun <$> freshTyInst l g αs τs t >>= \case
      Just (_, bs, rt) -> first (map b_type) <$> substNoCapture xes (bs, rt)
      _                -> cgError $ errorNonFunction (srcPos l) fn ft
    where
      τs = envGetContextTypArgs 0 g l αs


-- | consCallCondExpr: Special casing conditional expression call here because we'd like the
--   arguments to be typechecked under a different guard each.
--
consCallCondExpr g l fn ets (validOverloads g l -> fts)
  = mseq (consCondExprArgs (srcPos l) g ets) $ \(xes, g') -> do
      ts <- T.mapM (`cgSafeEnvFindTyM` g') xes
      case fts of
        [ft] -> consCheckArgs l g' fn ft ts xes
        _    -> cgError $ errorNoMatchCallee (srcPos l) fn ts fts


---------------------------------------------------------------------------------
consScan :: (CGEnv -> a -> b -> CGM (Maybe (c, CGEnv))) -> CGEnv -> [(a,b)] -> CGM (Maybe ([c], CGEnv))
---------------------------------------------------------------------------------
consScan f g xs
  = do  z <- fmap (mapFst reverse) <$> consFold step ([], g) xs
        case z of
          Just (xs', g') -> return $ Just (xs', g')
          _              -> return   Nothing
  where
    step (ys, g) (x,y) = fmap (mapFst (:ys))   <$> f g x y

---------------------------------------------------------------------------------
consCondExprArgs :: SrcSpan -> CGEnv
                 -> [(Expression AnnLq, Maybe RefType)]
                 -> CGM (Maybe ([Id AnnLq], CGEnv))
---------------------------------------------------------------------------------
consCondExprArgs l g [(c,tc),(x,tx),(y,ty)]
  = mseq (consExpr g c tc) $ \(c_,gc) ->
      withGuard gc c_ True x tx >>= \case
        Just (x_, gx) ->
            withGuard gx c_ False y ty >>= \case
              Just (y_, gy) -> return $ Just ([c_, x_, y_], gy)
              Nothing ->
                  do ttx       <- cgSafeEnvFindTyM x_ gx
                     let tty    = fromMaybe ttx ty    -- Dummy type if ty is Nothing
                     (y_, gy') <- cgEnvAddFresh "6" l tty gx
                     return    $ Just ([c_, x_, y_], gy')
        Nothing ->
            withGuard gc c_ False y ty >>= \case
              Just (y_, gy) ->
                  do tty       <- cgSafeEnvFindTyM y_ gy
                     let ttx    = fromMaybe tty tx    -- Dummy type if tx is Nothing
                     (x_, gx') <- cgEnvAddFresh "7" l ttx gy
                     return     $ Just ([c_, x_, y_], gx')
              Nothing       -> return Nothing
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
        (xs,ts) = unzip [xts | PhiVarTy xts <- fFact l]

consWhileBase l xs tIs g
  = do  baseT <- mapM (`cgSafeEnvFindTyM` g) xs
        zipWithM_ (subType l Nothing g) baseT tIs

consWhileStep l xs tIs gI''
  = do  stepTs <- mapM (`cgSafeEnvFindTyM` gI'') xs'
        zipWithM_ (subType l Nothing gI'') stepTs tIs'                           -- (f)
  where
    tIs' = F.subst su <$> tIs
    xs'  = mkNextId   <$> xs
    su   = F.mkSubst   $  safeZip "consWhileStep" (F.symbol <$> xs) (F.eVar <$> xs')

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
        g1'       <- cgEnvAdds l "envJoin-0" (zip xls l1s) g1
        g2'       <- cgEnvAdds l "envJoin-1" (zip xls l2s) g2
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
        _         <- zipWithM_ (subType l Nothing g1') l1s' ls
        _         <- zipWithM_ (subType l Nothing g2') l2s' ls
        --
        -- GLOBALS:
        --
        let (xgs, gl1s, _) = unzip3 $ globals (zip3 xs t1s t2s)
        (g'',gls) <- freshTyPhis' l g' xgs $ (\(VI loc a i t) -> VI loc a i $ toType t) <$> gl1s
        gl1s'     <- mapM (`cgSafeEnvFindTyM` g1') xgs
        gl2s'     <- mapM (`cgSafeEnvFindTyM` g2') xgs
        _         <- zipWithM_ (subType l Nothing g1') gl1s' gls
        _         <- zipWithM_ (subType l Nothing g2') gl2s' gls
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


traceTypePP l msg act
  = do  z <- act
        case z of
          Just (x,g) -> do  t <- cgSafeEnvFindTyM x g
                            return $ Just $ trace (str x t) (x,g)
          Nothing -> return Nothing
  where
    str x t = boldBlue (printf "\nTrace: [%s] %s\n" (ppshow (srcPos l)) (ppshow msg)) ++
              printf "%s: %s" (ppshow x) (ppshow t)


-- Local Variables:
-- flycheck-disabled-checkers: (haskell-liquid)
-- End:
