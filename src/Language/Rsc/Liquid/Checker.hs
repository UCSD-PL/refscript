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
import qualified Data.Foldable                   as FO
import           Data.Function                   (on)
import qualified Data.HashMap.Strict             as HM
import           Data.List                       (partition, sortBy)
import           Data.Maybe                      (fromMaybe)
import qualified Data.Text                       as T
import qualified Data.Traversable                as TR
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
import           Text.PrettyPrint.HughesPJ       hiding (first)


-- import           Debug.Trace                     hiding (traceShow)

type Result = (A.UAnnSol RefType, F.FixResult Error)


-- | TODO: checkTypeWF

--------------------------------------------------------------------------------
verifyFile :: Config -> FilePath -> [FilePath] -> IO Result
--------------------------------------------------------------------------------
verifyFile cfg f fs = fmap (either (A.NoAnn,) id) $ runEitherIO $ do
    p         <- parseRscFromFiles fs
    cfg'      <- liftIO (withPragmas cfg (pOptions p))
    cha       <- liftEither (mkCHA p)
    ssa       <- ssaTransform p cha
    _         <- dumpJS cfg' f "SSA" ssa
    tc        <- typeCheck cfg' ssa cha
    _         <- dumpJS cfg' f "TC" tc
    cgi       <- generateConstraints cfg' f tc cha
    res       <- solveConstraints cfg' f cgi
    return   res


-- | solveConstraint: solve with `ueqAllSorts` enabled.
--------------------------------------------------------------------------------
solveConstraints :: Config -> FilePath -> CGInfo -> EitherIO e Result
--------------------------------------------------------------------------------
solveConstraints cfg f cgi = liftIO $ do
    _            <- startPhase Loud "Solving"
    F.Result r s <- solve fpConf (cgi_finfo cgi)
    r'           <- pure (fmap ciToError r)
    anns         <- pure (cgi_annot cgi)
    sol          <- pure (applySolution s)
    return          (A.SomeAnn anns sol, r')
  where
    fpConf  = def { -- C.real        = real cfg
                    C.ueqAllSorts = C.UAS True
                  , C.srcFile     = f
                  , C.save        = dumpDebug cfg
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
dumpJS cfg f s p =
  when (dumpDebug cfg) $ do
    cha <- liftEither $ mkCHA p
    liftIO $ writeFile (extFileName Result (dropExtension f ++ s)) $
             show $ pp p $+$ pp cha $+$ ppCasts p

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
generateConstraints
  :: Config -> FilePath -> RefScript -> ClassHierarchy F.Reft -> EitherIO e CGInfo
--------------------------------------------------------------------------------
generateConstraints cfg f pgm cha = do
  liftIO $ startPhase Loud "CG"
  return $ getCGInfo cfg f pgm (consRsc pgm cha)

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

-- TODO: checkSyms?
--------------------------------------------------------------------------------
initGlobalEnv :: RefScript -> ClassHierarchy F.Reft -> CGM CGEnv
--------------------------------------------------------------------------------
initGlobalEnv pgm@(Rsc { code = Src ss }) cha = do
    nms   <- either cgError return (symEnv ss)
    let g  = CGE nms bnds ctx pth cha fenv grd cst mut thisT (-1)
    cha'  <- freshenCHA g (envCHA   g)
    nms'  <- freshenEnv g (envNames g)
    return $ g { cge_cha = cha', cge_names = nms' }
  where
    bnds   = mempty
    ctx    = emptyContext
    pth    = mkAbsPath []
    fenv   = F.emptyIBindEnv
    grd    = []
    cst    = consts pgm
    mut    = Nothing
    thisT  = Nothing

-- TODO: CheckSyms
--------------------------------------------------------------------------------
initModuleEnv :: F.Symbolic n => CGEnv -> n -> [Statement AnnLq] -> CGM CGEnv
--------------------------------------------------------------------------------
initModuleEnv g n s = do
    nms1    <- either cgError return (symEnv s)
    let nms  = nms1 `mappend` nms0
    let g'   = CGE nms bnds ctx pth cha fenv grd cst mut thisT fnId
    nms'    <- freshenEnv g' (envNames g')
    return   $ g' { cge_names = nms' }
  where
    nms0     = toFgn (envNames g)
    bnds     = envBounds g
    ctx      = cge_ctx g
    pth      = extendAbsPath (cge_path g) n
    cha      = cge_cha g
    fenv     = cge_fenv g
    grd      = cge_guards g
    cst      = cge_consts g
    mut      = Nothing
    thisT    = Nothing
    fnId     = cge_fnid g

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
initCallableEnv l g f fty xs s = do
    nms1   <- either cgError return (symEnv s)
    let nms = envAddReturn f (SI rSym Local ReturnVar Initialized t)
            $ envAdds tyBs
            $ nms1 `mappend` nms0

    let g0  = CGE nms bnds ctx pth cha fenv grd cst mut thisT fnId
    nms'   <- freshenEnv g0 nms
    let g1  = g0 { cge_names = nms' }
    g2     <- cgEnvAdds l ("init-func-" ++ ppshow f ++ "-0") params g1
    g3     <- cgEnvAdds l ("init-func-" ++ ppshow f ++ "-1") [arg] g2
    return    g3
  where

    nms0     = toFgn (envNames g)
               -- No FP binding for these
    rSym     = returnSymbol
    bnds     = envAdds [(v,tv) | BTV v _ (Just tv) <- bs] $ cge_bounds g
    ctx      = pushContext i (cge_ctx g)
    pth      = cge_path g
    cha      = cge_cha g
    fenv     = cge_fenv g
    grd      = []
    cst      = cge_consts g
    mut      = cge_mut g
    thisT    = cge_this g

    tyBs     = [lsia α   | α <- αs]
    params   = [siw x t_ | (x, t_) <- safeZip "initCallableEnv" xs ts]
    arg      = mkArgumentsSI l ts
    ts       = map b_type xts
    αs       = map btvToTV bs
    fnId     = fId l
    lsia x   = (Loc (srcPos l) x, sia x (tVar x))
    sia  x t = SI (F.symbol x) Local Ambient    Initialized t
    siw  x t = SI (F.symbol x) Local WriteLocal Initialized t
    (i, (bs,xts,t)) = fty


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
      Just s  -> do
          _   <- validateTFun l g (v_type s)
          ft  <- cgFunTys l f xs (v_type s)
          mapM_ (consCallable l g f xs body) ft
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
consStmt g (ExprStmt l (AssignExpr _ OpAssign v@(LVar _ x) e))
  = case envFindTyWithAsgn v g of

      -- This is the first time we initialize this variable
      Just (SI n lc WriteGlobal Uninitialized t) -> do
          t' <- freshenType g l lc t
          mseq (consExprT g e t') $ \(_, g') ->
            Just <$> cgEnvAdds l "consAsgn-0" [SI n lc WriteGlobal Initialized t'] g'

      Just (SI _ _ WriteGlobal _ t) ->
          mseq (consExprT g e t) $ \(_, g') -> return $ Just g'

      Just (SI n lc a i t) ->
          mseq (consExprT g e t) $ \(x', g') -> do
            t_     <- safeEnvFindTy l g' x'
            Just  <$> cgEnvAdds l "consAsgn-1" [SI n lc a i t_] g'
      Nothing ->
          mseq (consExpr g e Nothing) $ \(x', g') -> do
            t      <- safeEnvFindTy l g' x'
            Just  <$> cgEnvAdds l "consAsgn-1" [SI (F.symbol x) Local WriteLocal Initialized t] g'

-- e1.f = e2
--
--   Mutabilities have been checked at TC
--
--   TODO: add indexer updates
--
consStmt g (ExprStmt l (AssignExpr _ OpAssign (LDot l1 e1 f) e2))
  = mseq (consExpr g e1 Nothing) $ \(x1,g1) -> do
      t1        <- safeEnvFindTy l1 g1 x1
      m1o       <- pure (getMutability (envCHA g1) t1)
      m1fo      <- pure (getFieldMutability (envCHA g1) t1 f)
      case (m1o, m1fo) of
        (Just m1, Just m1f)
          | isSubtype g m1f tMU || isSubtypeWithUq g m1 tUQ ->
            case getProp l g1 x1 f t1 of
              Left e -> cgError e
              Right (unzip -> (ts, fs)) -> do
                  subType l Nothing g1 t1 (tOr ts)
                  tus <- pure [ ( substThis x1 t, ()) | FI _ _ _ t <- fs ]
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
  mseq (safeEnvFindTy l g (builtinOpId BITruthy)
        >>= consCall g l (builtinOpId BITruthy) [(e, Nothing)]) $ \(xe,ge) -> do

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
      eT  <- safeEnvFindTy l gy y
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
  = do  d     <- resolveTypeM l g nm
        mapM_ (consStaticClassElt g d) ceS
        mapM_ (consInstanceClassElt g d) ceI
        return $ Just g
  where
    nm         = QN (cge_path g) (F.symbol x)
    (ceS, ceI) = partition isStaticClassElt ce

consStmt g (InterfaceStmt _ _)
  = return $ Just g

consStmt g (EnumStmt l n _) =
    Just <$> cgEnvAdds l "enum" [si] g
  where
    si    = SI nSym exprt RdOnly init tEnum
    tEnum = TRef (Gen name []) fTop
    path  = envPath g
    name  = QN path nSym
    nSym  = F.symbol n
    exprt = Exported      -- TODO: do this check
    init  = Initialized   -- TODO: do this check

consStmt g (ModuleStmt _ n body)
  = initModuleEnv g n body >>= (`consStmts` body) >> return (Just g)

-- OTHER (Not handled)
consStmt _ s
  = errorstar $ "consStmt: not handled " ++ ppshow s


--------------------------------------------------------------------------------
consVarDecl :: CGEnv -> VarDecl AnnLq -> CGM (Maybe CGEnv)
--------------------------------------------------------------------------------
consVarDecl g v@(VarDecl l x (Just e)) =
  case varDeclSymbol v of
    Left err                                -> cgError err
    Right Nothing                           -> goInfer WriteLocal Nothing -- Local (no type annotation)
    Right (Just s@(SI _ _ WriteLocal  _ t)) -> goRO   s t
    Right (Just s@(SI _ _ WriteGlobal _ t)) -> goRest s t
    Right (Just s@(SI _ _ RdOnly      _ t)) -> goRO   s t
    Right (Just   (SI _ _ _           _ _)) -> cgError $ errorVarDeclAnnot (srcPos l) x

  where

    goInfer a s = mseq (consExpr g e s) $ \(y,gy) -> do
                    eT    <- safeEnvFindTy l gy y
                    Just <$> cgEnvAdds l "cvd" [si a eT] gy

    goRO _ t@TPrim{} = goInfer RdOnly (Just t)
    goRO s t         = freshenType g l (v_loc s) t >>= go s

    goRest s t       = freshenType g l (v_loc s) t >>= go s

    go (SI _ lc a _ _) t
      | onlyCtxTyped e = fmap snd <$> consExpr g e (Just t) >>=
                         TR.mapM (cgEnvAdds l "consVarDecl" [SI xSym lc a Initialized t])
      | otherwise      = mseq (consExpr g e (Just t)) $ \(y,gy) -> do
                           eT      <- safeEnvFindTy l gy y
                           _       <- subType l Nothing gy eT t
                           Just   <$> cgEnvAdds l "consVarDecl" [SI xSym lc a Initialized t] gy

    si a = SI xSym Local a Initialized
    xSym = F.symbol x

consVarDecl g v@(VarDecl l _ Nothing) =
  case varDeclSymbol v of
    Left err -> cgError err

    Right (Just (SI n lc Ambient _ t)) ->
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
consExprT g e            t = consCall g l (builtinOpId BIExprT) [(e, Just t)] (idTy t)
  where l = getAnnotation e


--------------------------------------------------------------------------------
consStaticClassElt :: CGEnv -> TypeDecl F.Reft -> ClassElt AnnLq -> CGM ()
--------------------------------------------------------------------------------

-- | Static field
--
consStaticClassElt g (TD sig _ ms) (MemberVarDecl l True x (Just e)) =
  case F.lookupSEnv (F.symbol x) (s_mems ms) of
    Just MI{} ->
        cgError $ bugStaticField l x sig
    Just (FI _ _ _ t) ->
        void $ consCall g l (builtinOpId BIFieldInit) [(e, Just t)] (mkInitFldTy t)
    Nothing ->
        cgError $ errorClassEltAnnot (srcPos l) (sigTRef sig) x

consStaticClassElt _ _ (MemberVarDecl l _ x Nothing)
  = cgError $ unsupportedStaticNoInit (srcPos l) x

-- | Static method
--
consStaticClassElt g (TD sig _ ms) (MemberMethDecl l True x xs body)
  | Just (MI _ _ mts) <- F.lookupSEnv (F.symbol x) (s_mems ms)
  = do  let t = tAnd $ map snd mts
        its <- cgFunTys l x xs t
        mapM_ (consCallable l g x xs body) its
  | otherwise
  = cgError  $ errorClassEltAnnot (srcPos l) (sigTRef sig) x

consStaticClassElt _ _ c =
  error (show $ pp "consStaticClassElt - not a static element: " $+$ pp c)


--------------------------------------------------------------------------------
consInstanceClassElt :: CGEnv -> TypeDecl F.Reft -> ClassElt AnnLq -> CGM ()
--------------------------------------------------------------------------------
-- | Constructor
--
consInstanceClassElt g typDecl (Constructor l xs body) = do
    g2    <- cgEnvAdds l "ctor" exitP g1
    ts    <- cgFunTys l ctor xs ctorT
    mapM_ (consCallable l g2 ctor xs body) ts
  where
    TD sig@(TS _ (BGen nm bs) h) _ ms = typDecl
    g1         =  initClassCtorEnv sig g
    thisT      = TRef (Gen nm (map btVar bs)) fTop
    ctor       = builtinOpId BICtor
    cExit      = builtinOpId BICtorExit
    sExit      = F.symbol cExit
    allMembers = typeMembersOfType (envCHA g) thisT      -- Including inherited

    --  (_f1: T1, ...) => { A | offset(v, "f1") = _f1, ... }
    --
    --  No type variables for constructor
    --
    exitP = [SI sExit Local Ambient Initialized $ mkFun ([], xts, ret)]
    xts   = sortBy c_sym (toBinds allMembers)

    -- Use the parent type as base return type, strengthened
    -- by the field initializations
    ret   = unqualifyThis $ parT `strengthen` F.reft (F.vv Nothing) (F.pAnd (map bnd out))

    parT  | ([p],_) <- h = TRef p fTop
          | otherwise    = tUqObj

    -- unqualifyThis :: offset(this, "f") ==> f
    --
    toBinds ms = [ B x Req (unqualifyThis t) | (x, FI _ _ _ t) <- lMems ms ]

    out   = [ f | (f, FI _ _ m _) <- lMems allMembers, isSubtype g m tIM ]

    bnd f = mkOffsetSym v_sym (symbolString f) `aeq` F.eVar f

    v_sym = F.symbol $ F.vv Nothing
    c_sym = on compare (show . b_sym)
    lMems = F.toListSEnv . i_mems
    aeq   = F.PAtom F.Eq

    -- The type that needs to be established (including class invariant)
    ctorT = case fmap bkAnd (tm_ctor ms) >>= mapM bkFun of
              Just tys -> tAnd (map ctorT1 tys)
              Nothing  -> die (unsupportedNonSingleConsTy (srcPos l))

    ctorT1 (_, bs, rt) = mkFun ([], bs, prepRt rt)

    -- Check for:
    --
    --  * Class invariants
    --
    --  * Whatever refinement is given at the constructor's sig
    --
    prepRt t = parT `strengthen` clInv `strengthen` rTypeR t

    -- No `this` allowed
    -- No return type refinements for constructor
    -- substThisWithSelf . (`strengthen` clInv)
    clInv  = getClassInvariant g nm


-- | Instance method
--
--   TODO: The method's mutability should influence the type of tThis that is used
--         as a binder to this.
--   TODO: Also might not need 'cge_this'
--
consInstanceClassElt g (TD sig _ ms) (MemberMethDecl l False x xs body)
  | Just (MI _ _ mts) <- F.lookupSEnv (F.symbol x) (i_mems ms)
  = do  let (ms', ts) = unzip mts
        its          <- cgFunTys l x xs $ tAnd ts
        let mts'      = zip ms' its
        mapM_ (\(m,t) -> do
            g1   <- initClassMethEnv l m sig g
            consCallable l g1 x xs body t
          ) mts'

  | otherwise
  = cgError $ errorClassEltAnnot (srcPos l) (sigTRef sig) x

-- | Instance variable (checked at constructor check)
--
consInstanceClassElt _ _ (MemberVarDecl _ _ _ Nothing)
  = return ()

consInstanceClassElt _ _ (MemberVarDecl l _ x _)
  = die $ bugClassInstVarInit (srcPos l) x

consInstanceClassElt _ _ c = error (show $ pp "consInstanceClassElt - not an instance element: " $+$ pp c)


-- | `consExpr g e` returns:
--
--   * `Just (g', x')` where
--     - `x'` is a fresh, temporary (A-Normalized) variable holding value `e`,
--     - `g'` is `g` extended with a binding for `x'` (and other temp vars)
--
--   * `Nothing` when the code should be considered dead-code.
--
--------------------------------------------------------------------------------
consExpr
  :: CGEnv -> Expression AnnLq -> Maybe RefType -> CGM (Maybe (Id AnnLq, CGEnv))
--------------------------------------------------------------------------------
-- | Dead-casts / Type-casts
consExpr g (Cast_ l e) s
  = case envGetContextCast g l of

      -- Type-cast
      CType s  -> mseq (consExpr g e (Just $ ofType s)) $ \(x, g') -> do
                    t   <- (`uSingleton` x) <$> safeEnvFindTy l g' x
                    _   <- subType l Nothing g' t (ofType s)
                    case narrowType g' t s of
                      Just t' -> Just <$> cgEnvAddFresh "cast_" l t' g'
                      Nothing -> return (Just (x, g'))

      -- Dead-cast: Do not attempt to check the enclosed expression
      CDead [] -> subType l (Just (errorDeadCast l)) g tBool tBot  >> return Nothing
      CDead es -> mapM_ (\e -> subType l (Just e) g tBool tBot) es >> return Nothing

      CNo      -> consExpr g e s

-- | <T>e
consExpr g ex@(Cast l e) _
  | [ty] <- [ t_ | UserCast t_ <- fFact l ]
  = do  ty' <- freshTyFun g l ty
        consCall g l (builtinOpId BICastExpr) [(e, Just ty')] (castTy ty')
  | otherwise
  = die $ bugNoCasts (srcPos l) ex

consExpr g (IntLit l i) _
  = Just <$> cgEnvAddFresh "8" l (tNum `eSingleton` i) g

consExpr g (NumLit l i) _
  = Just <$> cgEnvAddFresh "8" l (tReal `eSingleton` i) g

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

--   -- | If `x` is a unique reference and it this does not appear to be
--   --   place where unique references can appear, then flag an error.
--   | Just (SI _ _ _ _ t) <- tInfo
--   , Just m <- getMutability (envCHA γ) t
--   , isSubtypeWithUq g m tUQ
--   , not (isUniqueEnabled e)
--   = cgError (errorUniqueRef l (Just e))


  | Just (SI _ _ WriteGlobal _ t) <- tInfo
  = Just <$> cgEnvAddFresh "0" l t g

  | Just (SI _ _ _ _ t) <- tInfo
  = Just <$> cgEnvAddFresh "cons VarRef" l t g

  | otherwise
  = cgError $ errorUnboundId (fSrc l) x
  where
    tInfo = envFindTyWithAsgn x g

consExpr g (PrefixExpr l o e) _
  = do opTy <- safeEnvFindTy l g (prefixOpId o)
       consCall g l (prefixOpId o) [(e,Nothing)] opTy

consExpr g (InfixExpr l o@OpInstanceof e1 e2) _
  = mseq (consExpr g e2 Nothing) $ \(x, g') -> do
       t            <- safeEnvFindTy l g' x
       case t of
         TClass x_  -> do opTy <- safeEnvFindTy l g (infixOpId o)
                          consCall g l (infixOpId o) (zwNth [e1, StringLit l2 (cc x_)]) opTy
         _          -> cgError $ unimplemented (srcPos l) "tcCall-instanceof" $ ppshow e2
  where
    l2 = getAnnotation e2
    cc (BGen (QN _ s) _) = symbolString s

consExpr g (InfixExpr l o e1 e2) _
  = do opTy <- safeEnvFindTy l g (infixOpId o)
       consCall g l (infixOpId o) (zwNth [e1, e2]) opTy

-- | e ? e1 : e2
consExpr g (CondExpr l e e1 e2) (Just t)
  = mseq checkCond $ \(xc, g') -> do
      z1 <- consExpr (envAddGuard xc True  g') e1 (Just t)
      z2 <- consExpr (envAddGuard xc False g') e2 (Just t)
      case (z1, z2) of
        (Nothing, _) -> return z2
        (_, Nothing) -> return z1
        (Just (x1, g1'), Just (x2, g2')) -> do
            t1            <- safeEnvFindTy l g1' x1
            t2            <- safeEnvFindTy l g2' x2
            (xf, gf, tf)  <- freshTyCondExpr l g' (toType t)
            _             <- subType l Nothing g1' t1 tf
            _             <- subType l Nothing g2' t2 tf
            return         $ Just (xf, gf)
  where
    checkCond = do
        t   <- safeEnvFindTy l g (builtinOpId BITruthy)
        consCall g l (builtinOpId BITruthy) [(e, Nothing)] t

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
consExpr g (CallExpr l em@(DotRef l1 e f) es) _
  -- | isVariadicCall f = cgError (unimplemented l "Variadic" ex)
  | otherwise        = checkNonVariadic

  where
    -- Variadic check
    -- isVariadicCall f_ = F.symbol f_ == F.symbol "call"

    checkNonVariadic =
      mseq (consExpr g e Nothing) $ \(xR, g') -> do
        tR  <- safeEnvFindTy l1 g' xR
        case getProp l g' xR f tR of
          Left e  -> cgError e
          Right p -> checkWithProp xR g' p

    -- Only support single members at the moment
    checkWithProp xR g_ [(tR, m)] = checkTM xR g_ tR m
    checkWithProp _  _  _         = error "TODO: add case in checkWithProp Right _"

    -- Check a single type member
    checkTM _  g_ _  (FI _ Req _ ft) = consCall g_ l biID (es `zip` nths) ft
    checkTM _  _  _  (FI f _ _ _)    = cgError (errorOptFunUnsup l f e)
    checkTM xR g_ tR (MI _ Req mts)  =
      case getMutability (envCHA g_) tR of
        Just mR ->
            case [ ft_ | (m, ft_) <- mts, isSubtypeWithUq g_ mR m ] of
              [] -> cgError $ errorMethMutIncomp l em mts mR
              ts -> consCall g_ l biID (es `zip` nths) (substThis xR (tAnd ts))
        Nothing -> cgError $ errorNoMutAvailable l e tR

    checkTM _ _ _ (MI _ Opt _) =
      error "TODO: Add error message at checkTypeMember MI Opt"
    biID = builtinOpId BIDotRefCallExpr

-- | e(es)
--
consExpr g (CallExpr l e es) _
  = mseq (consExpr g e Nothing) $ \(x, g') ->
      do  ft <- safeEnvFindTy l g' x
          consCall g' l (builtinOpId BICallExpr) (es `zip` nths) ft

-- | e.f
--
--   Returns type: { v: _ | v = x.f }, if e => x and `f` is an immutable field
--                 { v: _ | _       }, otherwise
--
consExpr g0 (DotRef l e f) _
  = mseq (consExpr g0 e Nothing) $ \(x, g1) -> do
      te <- safeEnvFindTy l g1 x
      case getProp l g1 x f te of
        Right tf -> checkAccess g1 x tf
        Left  e  -> cgError e

  where
    -- The receiver will be cast already to the type for which the
    -- property acces succeeds.
    --
    -- Array accesses have already been translated to method calls to
    -- __getLength.
    --
    checkAccess g x tf = Just <$> addWithOpt (opt tf) (fieldsTy g x tf) g

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
      opTy  <- safeEnvFindTy l g' (builtinOpId BIBracketRef)
      consCall g' l (builtinOpId BIBracketRef) ([vr x1, e2] `zip` nths) opTy
  where
    vr  = VarRef $ getAnnotation e1

-- | e1[e2] = e3
--
consExpr g (AssignExpr l OpAssign (LBracket _ e1 e2) e3) _
  = do  opTy <- safeEnvFindTy l g (builtinOpId BIBracketAssign)
        consCall g l (builtinOpId BIBracketAssign) ([e1,e2,e3] `zip` nths) opTy

-- | [e1,...,en]
--
consExpr g e@(ArrayLit l es) to
  = arrayLitTy l g e to (length es) >>= \case
      Left ee    -> cgError ee
      Right opTy -> consCall g l (builtinOpId BIArrayLit) (zip es nths) opTy

-- | { f1: e1, ..., fn: en }
--
consExpr g (ObjectLit l pes) to
  = mseq (consScan consExpr g ets) $ \(xes, g') -> do
      ts      <- mapM (safeEnvFindTy l g') xes
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
      t <- safeEnvFindTy l g1 x
      case extractCtor g1 t of
        Just ct ->
            let ct' = substThisCtor ct in
            mseq (consCall g1 l (builtinOpId BICtor) (es `zip` nths) ct') $ \(x, g2) -> do
              tNew    <- safeEnvFindTy l g2 x
              tNew'   <- pure (adjustCtxMut tNew s)
              tNew''  <- pure (substThis (rTypeValueVar tNew') tNew')
              Just   <$> cgEnvAddFresh "18" l tNew'' g2
        Nothing ->
            cgError $ errorConstrMissing (srcPos l) t

-- | super
--
consExpr g (SuperRef l) _
  | Just thisT  <- cge_this g
  , Just tSuper <- getSuperType (envCHA g) thisT
  = Just <$> cgEnvAddFresh "15" l tSuper g
  | otherwise
  = cgError $ errorSuper (fSrc l)

-- | function(xs) { }
consExpr g (FuncExpr l fo xs body) (Just ft)
  = do  fts       <-  cgFunTys l f xs ft
        forM_ fts  $  consCallable l g f xs body
        Just      <$> cgEnvAddFresh "16" l ft g
  where
    f = fromMaybe (builtinOpId BIAnonymousFun) (fmap (fmap srcPos) fo)

consExpr _ (FuncExpr l _ _ _) Nothing
  = cgError (errorNoFuncAnn l)

-- not handled
consExpr _ e _ = cgError $ unimplemented l "consExpr" e where l = srcPos  e


--------------------------------------------------------------------------------
validOverloads :: (F.Symbolic f) => CGEnv -> AnnLq -> f -> RefType -> [RefType]
--------------------------------------------------------------------------------
validOverloads g l fn ft0
  = [ mkFun t | Overload cx fn0 i <- fFact l          -- all overloads
              , cge_ctx g == cx                       -- right context
              , F.symbol fn0 == F.symbol fn
              , (j, t) <- overloads g ft0             -- extract overloads
              , i == j ]                              -- pick the one resolved at TC

--------------------------------------------------------------------------------
consCall :: CGEnv -> AnnLq -> Identifier -> [(Expression AnnLq, Maybe RefType)]
         -> RefType -> CGM (Maybe (Id AnnLq, CGEnv))
--------------------------------------------------------------------------------
consCall g l fn ets ft@(validOverloads g l fn -> fts)
  = mseq (consScan consExpr g ets) $ \(xes, g') -> do
      ts <- mapM (safeEnvFindTy l g') xes
      case fts of
        ft : _ -> consCheckArgs l g' fn ft ts xes
        _      -> cgError $ errorNoMatchCallee (srcPos l) fn ts ft

-- | `consCheckArgs` does the subtyping between the types of the arguments
--   @xes@ and the formal paramaters of @ft@.
--
--
--    TODO: check bounds ??? (like in TC)
--
--------------------------------------------------------------------------------
consCheckArgs :: PP a => AnnLq -> CGEnv -> a
                      -> RefType                          -- Function spec
                      -> [RefType]                        -- Input types
                      -> [Id AnnLq]                       -- Input ids
                      -> CGM (Maybe (Id AnnLq, CGEnv))
--------------------------------------------------------------------------------
consCheckArgs l g fn ft ts xes
  = do  (rhs, rt) <- instantiateFTy l g fn xes ft
        lhs       <- zipWithM (instantiateTy l g fn) [1..] ts
        _         <- zipWithM_ (subType l Nothing g) lhs rhs
        Just     <$> cgEnvAddFresh "5" l rt g

-- The integer argument `n` corresponds to the order of the argument
--------------------------------------------------------------------------------
instantiateTy :: PP a => AnnLq -> CGEnv -> a -> Int -> RefType -> CGM RefType
--------------------------------------------------------------------------------
instantiateTy l g fn n (bkAll -> (βs, t))
  = freshTyInst l g βs τs t
  where
    τs = envGetContextTypArgs n g l fn βs

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
consWhile
  :: CGEnv -> AnnLq -> Expression AnnLq -> Statement AnnLq -> CGM (Maybe CGEnv)
---------------------------------------------------------------------------------
--
--    Typing Rule for `while (cond) {body}`
--
--       (a) xtIs         <- fresh G [ G(x) | x <- Φ]
--       (b) GI            = G, xtIs
--       (c) G            |- G(x)  <: GI(x)  , ∀x∈Φ      [base]
--       (d) GI           |- cond : (xc, GI')
--       (e) GI', xc:true |- body : GI''
--       (f) GI''         |- GI''(x') <: GI(x)[Φ'/Φ]     [step]
--       ---------------------------------------------------------
--           G            |- while[Φ] (cond) body :: GI', xc:false
--
--    The above rule assumes that phi-assignments have already been inserted. That is,
--
--       i = 0;
--       while (i < n){
--         i = i + 1;
--       }
--
--    Has been SSA-transformed to
--
--       i_0 = 0;
--       i_2 = i_0;
--       while [i_2] (i_2 < n) {
--         i_1  = i_2 + 1;
--         i_2' = i_1;
--       }
--
consWhile g l cond body = do
    (gI, tIs) <- freshTyPhis l g (zipWith si xs ts)        -- (a) (b)
    _         <- consWhileBase l xs tIs g                   -- (c)
    mseq (consExpr gI cond Nothing) $ \(xc, gc) -> do       -- (d)
      z     <- consStmt (envAddGuard xc True gc) body       -- (e)
      ss'   <- whenJustM z (consWhileStep l xs xs' tIs)     -- (f)
      gc'   <- cgEnvAdds l "consWhile" ss' gc
      return $ Just (envAddGuard xc False gc')
  where
      (xs, xs', ts) = unzip3 [ x_ | PhiLoopTC x_ <- fFact l ]
      si x t = SI (F.symbol x) Local WriteLocal Initialized t

consWhileBase l xs tIs g = do
    baseT <-  mapM (safeEnvFindTy l g) xs
    zipWithM_ (subType l Nothing g) baseT tIs

-- Return the type binding that should be propagated. The var is the output
-- SSAed variable from the loop body, and the bound type is the invariant type.
--
consWhileStep l xs xs' tIs gI'' = do
    stepTs <- mapM (safeEnvFindSI l gI'') xs'
    zipWithM_ (subType l Nothing gI'') (map v_type stepTs) tIs'                       -- (f)
    return    (zipWith setSiType stepTs tIs)

  where
    tIs' = F.subst su <$> tIs
    su   = F.mkSubst   $  safeZip "consWhileStep" (F.symbol <$> xs) (F.eVar <$> xs')

whenJustM Nothing  _ = return []
whenJustM (Just x) f = f x


--
--    // G0 --> x0
--    if (cond) []{
--        ...
--        // G1   --> x'
--    } else {
--        ...
--        // G2   --> x'
--    }
--    // G'
--
--    G0 |- fresh T
--
--    G1[x'] = T1, G2[x'] = T2
--
--    G1 |- x': T1 <: T
--    G2 |- x': T2 <: T
--
----------------------------------------------------------------------------------
envJoin :: AnnLq -> CGEnv -> Maybe CGEnv -> Maybe CGEnv -> CGM (Maybe CGEnv)
----------------------------------------------------------------------------------
envJoin _ _ Nothing x           = return x
envJoin _ _ x Nothing           = return x
envJoin l g (Just g1) (Just g2) = do
    s1s   <-  mapM (safeEnvFindSI l g1) xs    -- SI entry at the end of the 1st branch
    s2s   <-  mapM (safeEnvFindSI l g2) xs    -- SI entry at the end of the 2nd branch
    g'    <- foldM (\g_ (s1, s2) -> checkPhi g_ (siJoin g1 s1 s2) s1 s2) g (zip s1s s2s)
    return $ Just g'
  where
    -- SSAed Φ-vars before the conditional and at the end of the branches
    xs        = [ x | PhiVar x <- fFact l]

    checkPhi g_ s_ s1 s2 = do
        (g', [r_])  <- freshTyPhis l g_ [s_]
        ts          <- zipWithM (\g_ -> safeEnvFindTy l g_ . v_name) [g1,g2] [s1,s2]
        zipWithM_ (\l_ g_ -> subType l Nothing g_ l_ r_) ts [g1,g2]
        return g'

siJoin g s1 s2
    | isSubtype g (v_type s1) (v_type s2) = s2
    | isSubtype g (v_type s2) (v_type s1) = s1
    | otherwise = s1 { v_type = tOr [v_type s1, v_type s2] }

-- Local Variables:
-- flycheck-disabled-checkers: (haskell-liquid)
-- End:
