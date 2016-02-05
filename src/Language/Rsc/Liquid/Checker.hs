{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE ViewPatterns              #-}

-- | Top Level for Refinement Type checker
module Language.Rsc.Liquid.Checker (verifyFile) where

import           Control.Arrow                   (first)
import           Control.Monad
import           Data.Function                   (on)
import qualified Data.HashMap.Strict             as HM
import           Data.List                       (sortBy)
import           Data.Maybe                      (catMaybes, fromMaybe)
import qualified Data.Text                       as T
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Solver        (solve)
import qualified Language.Fixpoint.Types         as F
import qualified Language.Fixpoint.Types.Config  as C
import           Language.Fixpoint.Types.Errors
import           Language.Fixpoint.Types.Names   (symbolString)
import qualified Language.Fixpoint.Types.Visitor as V
import           Language.Fixpoint.Utils.Files   (Ext (..), extFileName)
import           Language.Rsc.Annotations
import           Language.Rsc.AST
import           Language.Rsc.ClassHierarchy
import           Language.Rsc.CmdLine
import           Language.Rsc.Constraints
import           Language.Rsc.Core.EitherIO
import           Language.Rsc.Core.Env
import           Language.Rsc.Environment
import           Language.Rsc.Errors
import           Language.Rsc.Liquid.CGMonad
import           Language.Rsc.Liquid.Constraints
import           Language.Rsc.Liquid.Environment
import           Language.Rsc.Liquid.Types
import           Language.Rsc.Locations
import           Language.Rsc.Lookup
import           Language.Rsc.Misc
import           Language.Rsc.Names
import           Language.Rsc.Parser
import           Language.Rsc.Pretty
import           Language.Rsc.Program
import           Language.Rsc.SSA.SSA
import           Language.Rsc.Symbols
import qualified Language.Rsc.SystemUtils        as A
import           Language.Rsc.Typecheck.Checker  (typeCheck)
import           Language.Rsc.Typecheck.Sub
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
import           Language.Rsc.TypeUtilities
import           System.Console.CmdArgs.Default
import           System.FilePath.Posix           (dropExtension)
-- import           Text.Printf

import qualified Data.Foldable                   as FO
import           Text.PrettyPrint.HughesPJ       hiding (first)

-- import           Debug.Trace                     hiding (traceShow)

type Result = (A.UAnnSol RefType, F.FixResult Error)

--------------------------------------------------------------------------------
verifyFile :: Config -> FilePath -> [FilePath] -> IO Result
--------------------------------------------------------------------------------
verifyFile cfg f fs = fmap (either (A.NoAnn,) id) $ runEitherIO $
  do  p     <- announce "Parse" $ EitherIO   $ parseRscFromFiles fs
      cfg'  <- liftIO           $ withPragmas cfg (pOptions p)
      -- _     <- pure             $ checkTypeWF p
      cha   <- liftEither       $ mkCHA p
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

-- (>>=>) :: IO (Either a b) -> (b -> IO c) -> IO (Either a c)
-- act >>=> k = do
--   r <- act
--   case r of
--     Left l  -> return $  Left l -- (A.NoAnn, l)
--     Right x -> Right <$> k x

-- eAct :: IO (Err a) -> IO a
-- eAct m = do
--   x <- m
--   case x of
--     Left  l -> throw l
--     Right r -> return r


-- | solveConstraint: solve with `ueqAllSorts` enabled.
--------------------------------------------------------------------------------
solveConstraints
  :: Config -> FilePath -> CGInfo -> IO (A.UAnnSol RefType, F.FixResult Error)
--------------------------------------------------------------------------------
solveConstraints cfg f cgi
  = do  F.Result r s <- solve fpConf (cgi_finfo cgi)
        r'           <- pure (fmap ciToError r)
        anns         <- pure (cgi_annot cgi)
        sol          <- pure (applySolution s)
        return          (A.SomeAnn anns sol, r')
  where
    fpConf        = def { C.real        = real cfg
                        , C.ueqAllSorts = C.UAS True
                        , C.srcFile     = f
                        , C.save        = True
                        }

-- NOT VALID WITH NEW L-F -- solveConstraints cfg f cgi
-- NOT VALID WITH NEW L-F --   = do F.Result r s <- solve fpConf (cgi_finfo cgi)
-- NOT VALID WITH NEW L-F --        -- let c0        = ppWCI cgi r
-- NOT VALID WITH NEW L-F --        let r'        = fmap (ci_info . F.sinfo) r
-- NOT VALID WITH NEW L-F --        let anns      = cgi_annot cgi
-- NOT VALID WITH NEW L-F --        let sol       = applySolution s
-- NOT VALID WITH NEW L-F --        return        $ (A.SomeAnn anns sol, r')
-- NOT VALID WITH NEW L-F --   where
-- NOT VALID WITH NEW L-F --     fpConf     = def { C.real        = real cfg
-- NOT VALID WITH NEW L-F --                      , C.ueqAllSorts = C.UAS True
-- NOT VALID WITH NEW L-F --                      , C.srcFile     = f
-- NOT VALID WITH NEW L-F --                      }


-- NOT VALID WITH NEW L-F -- -- TODO: move elsewhere
-- NOT VALID WITH NEW L-F -- ppWCI cgi (F.Unsafe ws) = vcat $ map f ws
-- NOT VALID WITH NEW L-F --   where
-- NOT VALID WITH NEW L-F --     f wci = pp (F.sid wci) <+> pp (F.fromListSEnv (F.clhs bs wci)) $+$ nest 2 (text "=>" $+$ pp (F.crhs wci))
-- NOT VALID WITH NEW L-F --     cm    = F.cm $ cgi_finfo cgi
-- NOT VALID WITH NEW L-F --     bs    = F.bs $ cgi_finfo cgi
-- NOT VALID WITH NEW L-F -- ppWCI _ F.Safe = text "SAFE"
-- NOT VALID WITH NEW L-F --
-- NOT VALID WITH NEW L-F -- instance PP (F.Result Cinfo) where
-- NOT VALID WITH NEW L-F --   pp (F.Result cinfo kvars)
-- NOT VALID WITH NEW L-F --     = vcat ((ppB <$>) (HM.toList kvars))
-- NOT VALID WITH NEW L-F --     where
-- NOT VALID WITH NEW L-F --       ppB (x, t) = pp x <+> dcolon <+> pp t
-- NOT VALID WITH NEW L-F --
-- NOT VALID WITH NEW L-F -- instance PP (F.FixResult (F.WrappedC Cinfo)) where
-- NOT VALID WITH NEW L-F --   pp (F.Unsafe ws) = vcat $ map ppW ws
-- NOT VALID WITH NEW L-F --     where
-- NOT VALID WITH NEW L-F --       ppW wci = text "ID" <+> pp (F.sid wci) -- text "<:" <+> pp (F.crhs wci)
-- NOT VALID WITH NEW L-F -- --       _ = F.sinfo wci
-- NOT VALID WITH NEW L-F -- --       _ = F.clhs undefined wci
-- NOT VALID WITH NEW L-F --
-- NOT VALID WITH NEW L-F --   pp F.Safe = text "Safe"
-- NOT VALID WITH NEW L-F --
-- NOT VALID WITH NEW L-F --
-- NOT VALID WITH NEW L-F -- instance PP F.KVar where
-- NOT VALID WITH NEW L-F --   pp k = pprint k

--------------------------------------------------------------------------------
applySolution :: F.FixSolution -> A.UAnnInfo RefType -> A.UAnnInfo RefType
--------------------------------------------------------------------------------
applySolution  = fmap . fmap . tx
  where
    tx         = F.mapPredReft . txPred
    txPred s   = F.simplify . V.mapKVars (appSol s)
    appSol s k = Just $ HM.lookupDefault F.PTop k s


-- | Debug info
--
dumpJS f cha s p = writeFile (extFileName Result (dropExtension f ++ s)) $ show
                 $ pp p
               $+$ pp cha
               $+$ ppCasts p

ppCasts (Rsc { code = Src fs })
  = pp (take 80 (repeat '=')) $+$
    text "Casts"              $+$
    pp (take 80 (repeat '-')) $+$
    vcat es                   $+$
    pp (take 80 (repeat '='))
  where
    ass = map FO.toList fs
    as  = concat ass
    es  = [pp "TYPE-CAST" $+$ pp t            | a <- as, TypeCast _ t <- fFact a]
       ++ [pp "DEAD-CAST" $+$ vcat (map pp e) | a <- as, DeadCast _ e <- fFact a ]

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
    nms   = symEnv ss
    bnds  = mempty
    ctx   = emptyContext
    pth   = mkAbsPath []
    fenv  = F.emptyIBindEnv
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
    nms   = symEnv s `mappend` toFgn (envNames g)
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
  = do  g1 <- freshenCGEnvM g0
        g2 <- cgEnvAdds l ("init-func-" ++ ppshow f ++ "-0") params g1
        g3 <- cgEnvAdds l ("init-func-" ++ ppshow f ++ "-1") arg g2
        return g3
  where
    g0     = CGE nms bnds ctx pth cha fenv grd cst mut thisT fnId
             -- No FP binding for these
    nms    = toFgn (envNames g)
           & mappend (symEnv s)
           & envAdds tyBs
           & envAddReturn f (SI rSym Local ReturnVar Initialized t)
    rSym   = F.symbol "return"
    bnds   = envAdds [(v,tv) | BTV v _ (Just tv) <- bs] $ cge_bounds g
    ctx    = pushContext i (cge_ctx g)
    pth    = cge_path g
    cha    = cge_cha g
    fenv   = cge_fenv g
    grd    = []
    cst    = cge_consts g
    mut    = cge_mut g
    thisT  = cge_this g

    tyBs   = [(Loc (srcPos l) α, SI (F.symbol α) Local Ambient Initialized $ tVar α) | α <- αs]
    params = [ SI (F.symbol x) Local WriteLocal Initialized t_ |
               (x, t_) <- safeZip "initCallableEnv" xs ts ]
    arg    = [ mkArgumentsSI l ts ]
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
      Just s  -> cgFunTys l f xs (v_type s)
             >>= mapM_ (consCallable l g f xs body)
      Nothing -> cgError $ errorMissingSpec (srcPos l) f

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
  = mseq (consExpr g e1 Nothing) $ \(x1,g1) -> do
      t1        <- cgSafeEnvFindTyM x1 g1
      m1o       <- pure (getMutability (envCHA g1) t1)
      m1fo      <- pure (getFieldMutability (envCHA g1) t1 f)
      case (m1o, m1fo) of
        (Just m1, Just m1f)
          | isSubtype g m1f tMU || isSubtypeWithUq g m1 tUQ ->
            case getProp l g1 f t1 of
              Left e -> cgError e
              Right (unzip -> (ts, fs)) -> do
                  subType l Nothing g1 t1 (tOr ts)
                  tus <- pure [ (t,()) | FI _ _ _ t <- fs ]
                  z   <- consScan (\g_ -> const . consExprT g_ e2) g1 tus
                  return (fmap snd z)

          | otherwise -> cgError (errorImmutableRefAsgn l f e1 t1)

        (Nothing, _) -> cgError (errorExtractMut l t1 e1)
        (_, Nothing) -> cgError (errorExtractFldMut l f t1)

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
  = subType l Nothing g tVoid (cgEnvFindReturn g) >> return Nothing

-- return e
consStmt g (ReturnStmt l (Just e))
  = mseq (consExpr g e (Just rt)) $ \(y,gy) -> do
      eT  <- cgSafeEnvFindTyM y gy
      _   <- subType l Nothing gy eT rt
      return Nothing
  where
    rt = cgEnvFindReturn g

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
consVarDecl g (VarDecl l x (Just e))
  = case envFindTy x (cge_names g) of
      -- Local (no type annotation)
      Nothing ->
        mseq (consExpr g e Nothing) $ \(y,gy) -> do
          eT      <- cgSafeEnvFindTyM y gy
          Just   <$> cgEnvAdds l "cvd" [SI (F.symbol x) Local WriteLocal Initialized eT] gy

      Just s@(SI _ _ WriteLocal  _ _) -> go s
      Just s@(SI _ _ WriteGlobal _ _) -> go s
      Just s@(SI _ _ RdOnly      _ _) -> go s
      Just   (SI _ _ _           _ _) -> cgError $ errorVarDeclAnnot (srcPos l) x
  where
  go (SI n lc a _ t) = freshenTypeOpt g l t >>= \case
      Just fta ->
        mseq (consExpr g e (Just fta)) $ \(y,gy) -> do
          eT      <- cgSafeEnvFindTyM y gy
          _       <- subType l Nothing gy eT fta
          _       <- subType l Nothing gy fta t
          Just   <$> cgEnvAdds l "consVarDecl" [SI n lc a Initialized fta] gy
      Nothing ->
        mseq (consExpr g e (Just t)) $ \(y,gy) -> do
          eT      <- cgSafeEnvFindTyM y gy
          _       <- subType l Nothing gy eT t
          Just   <$> cgEnvAdds l "consVarDecl" [SI n lc a Initialized t] gy


consVarDecl g (VarDecl l x Nothing)
  = case envFindTy x (cge_names g) of
      Just (SI n lc Ambient _ t) ->
          Just <$> cgEnvAdds l "consVarDecl" [SI n lc Ambient Initialized t] g
      _ ->   -- The rest should have fallen under the 'undefined' initialization case
          error "LQ: consVarDecl this shouldn't happen"


-- | `consExprT l g e t` checks expression @e@ against type @t@.
--   Special-casing FuncExpr because we can't infer a type for a
--   function expression and then check for it - it needs to be
--   present in the first place.
--   XXX: perhaps other cases should work similarly.
--
--------------------------------------------------------------------------------
consExprT :: CGEnv -> Expression AnnLq -> RefType -> CGM (Maybe (Id AnnLq, CGEnv))
--------------------------------------------------------------------------------
consExprT g e@FuncExpr{} t = consExpr g e (Just t)
consExprT g e            t = consCall g l "consExprT" [(e, Just t)] (idTy t)
  where l = getAnnotation e


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
    sExit = F.symbol cExit

    -- substOffsetThis exitTy ???
    exitP = [SI sExit Local Ambient Initialized $ mkFun (bs, xts, ret)]

    ret   = thisT `strengthen` F.reft (F.vv Nothing) (F.pAnd $ bnd <$> out)
    xts   = sortBy c_sym [ B x t' | (x, FI _ _ _ t) <- F.toListSEnv (i_mems ms)
                                  , let t' = unqualifyThis g0 thisT t ]
    out   = [ f | (f, FI _ _ m _) <- F.toListSEnv (i_mems ms)
                ,  isSubtype g0 m tIM
            ]
    bnd f = F.PAtom F.Eq (mkOffsetSym v_sym $ symbolString f) (F.eVar f)
    v_sym = F.symbol $ F.vv Nothing
    c_sym = on compare b_sym

    ctorT = fromMaybe (die $ unsupportedNonSingleConsTy (srcPos l)) (tm_ctor ms)

-- | Static field
--
consClassElt g (TD sig ms) (MemberVarDecl l True x (Just e))
  | Just (FI _ _ _ t) <- F.lookupSEnv (F.symbol x) (s_mems ms)
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
  | Just (MI _ _ mts) <- F.lookupSEnv (F.symbol x) (s_mems ms)
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
  | Just (MI _ _ mts) <- F.lookupSEnv (F.symbol x) (i_mems ms)
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
    eThis       = SI thisSym Local RdOnly Initialized tThis

--------------------------------------------------------------------------------
consAsgn :: AnnLq -> CGEnv -> Id AnnLq -> Expression AnnLq -> CGM (Maybe CGEnv)
--------------------------------------------------------------------------------
consAsgn l g x e =
  case envFindTyWithAsgn x g of
    -- This is the first time we initialize this variable
    Just (SI n lc WriteGlobal Uninitialized t) ->
      do  t' <- freshenType g l t
          mseq (consExprT g e t') $ \(_, g') -> do
            g'' <- cgEnvAdds l "consAsgn-0" [SI n lc WriteGlobal Initialized t'] g'
            return $ Just g''

    Just (SI _ _ WriteGlobal _ t) -> mseq (consExprT g e t) $ \(_, g') ->
                                     return $ Just g'
    Just (SI n lc a i t) -> mseq (consExprT g e t) $ \(x', g') -> do
                              t_     <- cgSafeEnvFindTyM x' g'
                              Just  <$> cgEnvAdds l "consAsgn-1" [SI n lc a i t_] g'
    Nothing -> mseq (consExpr g e Nothing) $ \(x', g') -> do
                 t      <- cgSafeEnvFindTyM x' g'
                 Just  <$> cgEnvAdds l "consAsgn-1" [SI (F.symbol x) Local WriteLocal Initialized t] g'


-- | @consExpr g e@ returns a pair (g', x') where x' is a fresh,
--   temporary (A-Normalized) variable holding the value of `e`,
--   g' is g extended with a binding for x' (and other temps required for `e`)
--------------------------------------------------------------------------------
consExpr :: CGEnv -> Expression AnnLq -> Maybe RefType -> CGM (Maybe (Id AnnLq, CGEnv))
--------------------------------------------------------------------------------
-- | Dead-casts / Type-casts
consExpr g (Cast_ l e) s
  = case envGetContextCast g l of
      -- Type-cast
      CType s  -> mseq (consExpr g e (Just $ ofType s)) $ \(x, g') -> do
                    t   <- cgSafeEnvFindTyM x g'
                    _   <- subType l Nothing g t (ofType s)
                    t'  <- pure (narrowType g t s)
                    Just <$> cgEnvAddFresh "cast_" l t' g
      -- Dead-cast: Do not attempt to check the enclosed expression
      CDead [] -> subType l (Just (errorDeadCast l)) g tBool tBot >> return Nothing
      CDead es -> subType l (Just (F.catErrors es))  g tBool tBot >> return Nothing
      CNo      -> consExpr g e s

-- | <T>e
consExpr g ex@(Cast l e) _
  | [tc] <- [ ct | UserCast ct <- fFact l ]
  = consCall g l "Cast" [(e, Just tc)] (castTy tc)
  | otherwise
  = die $ bugNoCasts (srcPos l) ex

consExpr g (IntLit l i) _
  = Just <$> cgEnvAddFresh "8" l (tNum `eSingleton` i) g

-- Assuming by default 32-bit BitVector
consExpr g (HexLit l x) _
  | Just e <- bitVectorValue x
  = Just <$> cgEnvAddFresh "9" l (tBV32 `strengthen` e) g
  | otherwise
  = Just <$> cgEnvAddFresh "10" l tBV32 g

consExpr g (BoolLit l b) _
  = Just <$> cgEnvAddFresh "11" l (pSingleton tBool b) g

consExpr g (StringLit l x) _
  = Just <$> cgEnvAddFresh "12" l (tString `eSingleton` T.pack x) g

consExpr g (NullLit l) _
  = Just <$> cgEnvAddFresh "13" l tNull g

consExpr g (ThisRef l) _
  = case envFindTyWithAsgn x g of
      Just _  -> return $ Just (x, g)
      Nothing -> cgError $ errorUnboundId (fSrc l) "this"
  where
    x = thisId l

consExpr g (VarRef l x) _
  -- | undefined
  | F.symbol x == F.symbol "undefined"
  = Just <$> cgEnvAddFresh "0" l tUndef g

  | Just (SI _ _ WriteGlobal _ t) <- tInfo
  = Just <$> cgEnvAddFresh "0" l t g

  | Just (SI _ _ _ _ t) <- tInfo
  = do  addAnnot (srcPos l) x t
        Just <$> cgEnvAddFresh "cons VarRef" l t g

  | otherwise
  = cgError $ errorUnboundId (fSrc l) x
  where
    tInfo = envFindTyWithAsgn x g

consExpr g (PrefixExpr l o e) _
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
  = mseq checkCond $ \(xc, g') -> do
      z1 <- consExpr (envAddGuard xc True  g') e1 (Just t)
      z2 <- consExpr (envAddGuard xc False g') e2 (Just t)
      case (z1, z2) of
        (Just (x1, g1'), Just (x2, g2')) -> do
            t1            <- cgSafeEnvFindTyM x1 g1'
            t2            <- cgSafeEnvFindTyM x2 g2'
            (xf, gf, tf)  <- freshTyCondExpr l g' (toType t)
            _             <- subType l Nothing g1' t1 tf
            _             <- subType l Nothing g2' t2 tf
            return         $ Just (xf, gf)
        _ -> return Nothing
  where
    checkCond = do
        t   <- cgSafeEnvFindTyM (builtinOpId BITruthy) g
        consCall g l "truthy" [(e, Nothing)] t

consExpr _ e@(CondExpr l _ _ _) Nothing
  = cgError $ unimpCondExpCtxType l e

-- | super(e1,..,en)
consExpr g (CallExpr l (SuperRef _) _) _
  | Just thisT        <- cge_this g
  , Just _ {-tSuper-} <- getSuperType (envCHA g) thisT
  = error "consExpr super(...) - UNIMPLEMENTED"
  -- = consCall g l "super" (FI Nothing ((,Nothing) <$> es)) ct
  | otherwise
  = cgError $ errorUnboundId (fSrc l) "super"
  where

-- | e.m(es)
consExpr g ex@(CallExpr l em@(DotRef _ e f) es) _
  | isVariadicCall f = cgError (unimplemented l "Variadic" ex)
  | otherwise        = checkNonVariadic

  where
    -- Variadic check
    isVariadicCall f_ = F.symbol f_ == F.symbol "call"

    checkNonVariadic =
      mseq (consExpr g e Nothing) $ \(xR, g') ->
        cgSafeEnvFindTyM xR g' >>= checkWithProp xR g' . getProp l g' f

    -- Only support single members at the moment
    checkWithProp xR g_ (Right [(tR, m)]) = checkTM xR g_ tR m
    checkWithProp _  _  (Right _)         = error "TODO: add case in checkWithProp Right _"
    checkWithProp _  _  (Left er)         = cgError er

    -- Check a single type member
    checkTM _  g_ _  (FI _ Req _ ft) = consCall g_ l em (es `zip` nths) ft
    checkTM _  _  _  (FI f _ _ _)    = cgError (errorOptFunUnsup l f e)
    checkTM xR g_ tR (MI _ Req mts)  =
      case getMutability (envCHA g_) tR of
        Just mR ->
            case [ ft_ | (m, ft_) <- mts, isSubtype g_ mR m ] of
              [] -> cgError $ errorMethMutIncomp l em mts mR
              ts -> consCall g_ l em (es `zip` nths) (substThis xR (mkAnd ts))
        Nothing -> cgError $ errorNoMutAvailable l e tR

    checkTM _ _ _ (MI _ Opt _) =
      error "TODO: Add error message at checkTypeMember MI Opt"

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
consExpr g0 (DotRef l e f) _
  = mseq (consExpr g0 e Nothing) $ \(x, g1) -> do
      cgSafeEnvFindTyM x g1 >>= checkAccess g1 x . getProp l g1 f

  where
    -- The receiver will be cast already to the type for which the
    -- property acces succeeds.
    --
    -- Array accesses have already been translated to method calls to
    -- __getLength.
    --
    checkAccess g x (Right tf) = Just <$> addWithOpt (opt tf) (fieldsTy g x tf) g
    checkAccess _ _ (Left e)   = cgError e

    -- The accessed type
    fieldsTy g x tf   = tOr [ doField g x m t | (_, FI _ _ m t) <- tf ]

    doField g x m t | isSubtype g m tIM = immFieldTy x t
                    | otherwise         = otherFieldTy x t

    -- XXX: Don't fTop here cause it breaks functions
    immFieldTy   x t = t `eSingleton` mkOffsetSym x f
    otherFieldTy x t = substThis x t

    addWithOpt Opt = cgEnvAddFreshWithInit InitUnknown "DotRef" l
    addWithOpt Req = cgEnvAddFreshWithInit InitUnknown "DotRef" l

    opt tf = mconcat [ o | (_, FI _ o _ _) <- tf ]

-- | e1[e2]
--
--    TODO: TEnum
--
consExpr g (BracketRef l e1 e2) _
  = mseq (consExpr g e1 Nothing) $ \(x1,g') -> do
      opTy  <- cgSafeEnvFindTyM (builtinOpId BIBracketRef) g'
      consCall g' l BIBracketRef ([vr x1, e2] `zip` nths) opTy
  where
    vr  = VarRef $ getAnnotation e1

-- | e1[e2] = e3
--
consExpr g (AssignExpr l OpAssign (LBracket _ e1 e2) e3) _
  = do  opTy <- cgSafeEnvFindTyM (builtinOpId BIBracketAssign) g
        consCall g l BIBracketAssign ([e1,e2,e3] `zip` nths) opTy

-- | [e1,...,en]
--
consExpr g e@(ArrayLit l es) to
  = arrayLitTy l g e to (length es) >>= \case
      Left ee    -> cgError ee
      Right opTy -> consCall g l BIArrayLit (zip es nths) opTy

-- | { f1: e1, ..., fn: en }
--
consExpr g (ObjectLit l pes) to
  = mseq (consScan consExpr g ets) $ \(xes, g') -> do
      ts      <- mapM (`cgSafeEnvFindTyM` g') xes
      t       <- pure (TObj tUQ (tmsFromList (zipWith toFI ps ts)) fTop)
      Just   <$> cgEnvAddFresh "ObjectLit" l t g'
  where
    toFI p t   = FI (F.symbol p) Req tUQ t
    ets        = map (\(p,e) -> (e, pTy p)) pes
    pTy p      | Just f@FI{} <- lkup p = Just (f_ty f)
               | otherwise             = Nothing
    lkup p     = F.lookupSEnv (F.symbol p) ctxTys
    ctxTys     = maybe mempty (i_mems . typeMembersOfType (envCHA g)) to
    (ps , _)   = unzip pes

-- | new C(e, ...)
--
consExpr g (NewExpr l e es) s
  = mseq (consExpr g e Nothing) $ \(x,g1) -> do
      t <- cgSafeEnvFindTyM x g1
      case extractCtor g1 t of
        Just ct -> mseq (consCall g1 l "ctor" (es `zip` nths) ct) $ \(x, g2) -> do
                      tNew    <- cgSafeEnvFindTyM x g2
                      tNew'   <- pure (adjustCtxMut tNew s)
                      Just   <$> cgEnvAddFresh "18" l tNew' g2
        Nothing -> cgError $ errorConstrMissing (srcPos l) t

-- | super
--
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
consCall g l fn ets ft@(validOverloads g l -> fts)
  = mseq (consScan consExpr g ets) $ \(xes, g') -> do
      ts <- mapM (`cgSafeEnvFindTyM` g') xes
      case fts of
        ft : _ -> consCheckArgs l g' fn ft ts xes
        _      -> cgError $ errorNoMatchCallee (srcPos l) fn ts ft

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
        lhs       <- mapM (instantiateTy l g fn) ts
        _         <- zipWithM_ (subType l Nothing g) lhs rhs
        Just     <$> cgEnvAddFresh "5" l rt g

--------------------------------------------------------------------------------
instantiateTy :: PP a => AnnLq -> CGEnv -> a -> RefType -> CGM RefType
--------------------------------------------------------------------------------
instantiateTy l g fn (bkAll -> (βs, t))
  = freshTyInst l g βs τs t
  where
    τs = envGetContextTypArgs 0 g l fn βs

--------------------------------------------------------------------------------
instantiateFTy :: PP a => AnnLq -> CGEnv -> a -> [Id AnnLq] -> RefType
                       -> CGM ([RefType], RefType)
--------------------------------------------------------------------------------
instantiateFTy l g fn xes ft@(bkAll -> (βs, t))
  = bkFun <$> freshTyInst l g βs τs t >>= \case
      Just (_, xts, rt) -> first (map b_type) <$> substNoCapture xes (xts, rt)
      _                 -> cgError $ errorNonFunction (srcPos l) fn ft
    where
      τs = envGetContextTypArgs 0 g l fn βs

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
        g1'       <- cgEnvAdds l "envJoin-0" l1s g1
        g2'       <- cgEnvAdds l "envJoin-1" l2s g2
        --
        -- t1s and t2s should have the same raw type, otherwise they wouldn't
        -- pass TC (we don't need to pad / fix them before joining).
        -- So we can use the raw type from one of the two branches and freshen
        -- up that one.
        -- TODO: Add a raw type check on t1 and t2
        --
        (g',ls)   <- freshTyPhis' l g $ (\(SI x loc a i t) -> SI x loc a i (toType t)) <$> l1s
        l1s'      <- mapM (`cgSafeEnvFindTyM` g1') xls
        l2s'      <- mapM (`cgSafeEnvFindTyM` g2') xls
        _         <- zipWithM_ (subType l Nothing g1') l1s' ls
        _         <- zipWithM_ (subType l Nothing g2') l2s' ls
        --
        -- GLOBALS:
        --
        let (xgs, gl1s, _) = unzip3 $ globals (zip3 xs t1s t2s)
        (g'',gls) <- freshTyPhis' l g' $ (\(SI x loc a i t) -> SI x loc a i (toType t)) <$> gl1s
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


locals  ts = [(x,s1,s2) | (x, s1@(SI _ Local WriteLocal _ _),
                              s2@(SI _ Local WriteLocal _ _)) <- ts ]

globals ts = [(x,s1,s2) | (x, s1@(SI _ Local WriteGlobal Initialized _),
                              s2@(SI _ Local WriteGlobal Initialized _)) <- ts ]


-- traceTypePP l msg act
--   = do  z <- act
--         case z of
--           Just (x,g) -> do  t <- cgSafeEnvFindTyM x g
--                             return $ Just $ trace (str x t) (x,g)
--           Nothing -> return Nothing
--   where
--     str x t = boldBlue (printf "\nTrace: [%s] %s\n" (ppshow (srcPos l)) (ppshow msg)) ++
--               printf "%s: %s" (ppshow x) (ppshow t)


-- Local Variables:
-- flycheck-disabled-checkers: (haskell-liquid)
-- End:
