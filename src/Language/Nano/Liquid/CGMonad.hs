{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE DoAndIfThenElse           #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances      #-}

-- | Operations pertaining to Constraint Generation


module Language.Nano.Liquid.CGMonad (

  -- * Constraint Generation Monad
    CGM

  -- * Constraint Information
  , CGInfo (..)

  -- * Execute Action and Get FInfo
  , getCGInfo

  -- * Throw Errors
  , cgError

  -- * Fresh Templates for Unknown Refinement Types
  , freshTyFun, freshenType, freshTyInst, freshTyPhis, freshTyPhis'
  , freshTyObj, freshenCGEnvM

  -- * Strengthen (optimized)
  , nubstrengthen

  -- * Freshable
  , Freshable (..)

  -- * Environment API
  , envAddFresh, envAdds
  , envAddReturn, envAddGuard, envPopGuard
  , envFindTy, envFindTyWithAsgn
  , safeEnvFindTy, safeEnvFindTyWithAsgn, safeEnvFindTyNoSngl
  , envFindReturn, envPushContext
  , envGetContextCast, envGetContextTypArgs

  -- * Add Subtyping Constraints
  , subType, wellFormed -- , safeExtends

  -- * Add Type Annotations
  , addAnnot

  -- * Function Types
  , cgFunTys, subNoCapture

  -- * Zip type wrapper
  , zipTypeUpM, zipTypeDownM

  , checkSyms

  ) where

import           Control.Applicative
import           Control.Exception (throw)
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Except

import           Data.Maybe                     (fromMaybe, catMaybes, maybeToList)
import           Data.Monoid                    (mappend, mempty)
import qualified Data.HashMap.Strict            as HM
import qualified Data.Map.Strict                as M
import qualified Data.List                      as L
import           Data.Function                  (on)
import           Text.PrettyPrint.HughesPJ
import           Language.Nano.Misc             (single, concatMapM)
import           Language.Nano.Types
import           Language.Nano.Errors
import           Language.Nano.Annots
import qualified Language.Nano.Env              as E
import           Language.Nano.Locations
import           Language.Nano.Environment
import           Language.Nano.Names
import           Language.Nano.CmdLine
import           Language.Nano.Program
import           Language.Nano.Typecheck.Resolve
import qualified Language.Nano.SystemUtils      as S
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Typecheck.Sub
import           Language.Nano.Liquid.Environment
import           Language.Nano.Liquid.Types

import           Language.Fixpoint.Names (symbolString, symbolText)
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Errors

import           Language.Nano.Syntax
import           Language.Nano.Syntax.PrettyPrint

-- import           Debug.Trace                        (trace)

-------------------------------------------------------------------------------
-- | Top level type returned after Constraint Generation
-------------------------------------------------------------------------------

data CGInfo = CGI { cgi_finfo :: F.FInfo Cinfo
                  , cgi_annot :: S.UAnnInfo RefType
                  }

-- Dump the refinement subtyping constraints
instance PP CGInfo where
  pp (CGI finfo _) = cat (map pp (HM.elems $ F.cm finfo))

instance PP (F.SubC c) where
  pp s = text "TODO: pp SubC" -- pp (F.clhs s) <+> text " <: " <+> pp (F.crhs s)


-------------------------------------------------------------------------------
getCGInfo :: Config -> NanoRefType -> CGM a -> CGInfo
-------------------------------------------------------------------------------
getCGInfo cfg pgm = cgStateCInfo pgm . execute cfg pgm . (>> fixCWs)
  where
    fixCWs       = (,) <$> fixCs <*> fixWs
    fixCs        = get >>= concatMapM splitC . cs
    fixWs        = get >>= concatMapM splitW . ws


execute :: Config -> NanoRefType -> CGM a -> (a, CGState)
execute cfg pgm act
  = case runState (runExceptT act) $ initState cfg pgm of
      (Left e, _)   -> throw e
      (Right x, st) -> (x, st)


-------------------------------------------------------------------------------
initState       :: Config -> Nano AnnTypeR F.Reft -> CGState
-------------------------------------------------------------------------------
initState c p   = CGS F.emptyBindEnv [] [] 0 mempty invars c (max_id p)
  where
    invars        = HM.fromList [(tc, t) | t@(Loc _ (TApp tc _ _)) <- invts p]


-------------------------------------------------------------------------------
cgStateCInfo :: NanoRefType -> (([F.SubC Cinfo], [F.WfC Cinfo]), CGState) -> CGInfo
-------------------------------------------------------------------------------
cgStateCInfo pgm ((fcs, fws), cg) = CGI finfo (cg_ann cg)
  where
    finfo    = F.fi fcs fws (binds cg) lits F.ksEmpty (pQuals pgm) mempty junkFile
    lits     = F.sr_sort <$> measureEnv pgm
    junkFile = "FIXME.ts"
-- OLD? patchSymLits fi = fi { F.lits = F.symConstLits fi ++ F.lits fi }


-- | Get binding from object type

---------------------------------------------------------------------------------------
measureEnv   ::  Nano a F.Reft -> F.SEnv F.SortedReft
---------------------------------------------------------------------------------------
measureEnv   = fmap rTypeSortedReft . E.envSEnv . consts

---------------------------------------------------------------------------------------
-- | Constraint Generation Monad
---------------------------------------------------------------------------------------

data CGState = CGS {
  --
  -- ^ global list of fixpoint binders
  --
    binds       :: F.BindEnv
  --
  -- ^ subtyping constraints
  --
  , cs          :: ![SubC]
  --
  -- ^ well-formedness constraints
  --
  , ws          :: ![WfC]
  --
  -- ^ freshness counter
  --
  , cg_cnt      :: !Integer
  --
  -- ^ recorded annotations
  --
  , cg_ann      :: S.UAnnInfo RefType
  --
  -- ^ type constructor invariants
  --
  , invs        :: TConInv
  --
  -- ^ configuration options
  --
  , cg_opts     :: Config
  --
  -- ^ AST Counter
  --
  , cg_ast_cnt  :: NodeId

  }

type CGM     = ExceptT Error (State CGState)

type TConInv = HM.HashMap TCon (Located RefType)


---------------------------------------------------------------------------------------
cgError     :: Error -> CGM b
---------------------------------------------------------------------------------------
cgError err = throwE $ catMessage err "CG-ERROR\n"

---------------------------------------------------------------------------------------
-- | Environment API
---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------
envPushContext :: (CallSite a) => a -> CGEnv -> CGEnv
---------------------------------------------------------------------------------------
envPushContext c g = g {cge_ctx = pushContext c (cge_ctx g)}

---------------------------------------------------------------------------------------
envGetContextCast :: CGEnv -> AnnTypeR -> Cast F.Reft
---------------------------------------------------------------------------------------
envGetContextCast g a
  = case [c | TCast cx c <- ann_fact a, cx == cge_ctx g] of
      [ ] -> CNo
      [c] -> c
      cs  -> fromMaybe (err cs) $ L.find isDeadCast cs
  where
    isDeadCast CDead{} = True
    isDeadCast _       = False
    err cs             = die $ errorMultipleCasts (srcPos a) cs



---------------------------------------------------------------------------------------
envGetContextTypArgs :: Int -> CGEnv -> AnnTypeR -> [TVar] -> [RefType]
---------------------------------------------------------------------------------------
-- NOTE: If we do not need to instantiate any type parameter (i.e. length αs ==
-- 0), DO NOT attempt to compare that with the TypInst that might hide within
-- the expression, cause those type instantiations might serve anothor reason
-- (i.e. might be there for a separate instantiation).
envGetContextTypArgs _ _ _ [] = []
envGetContextTypArgs n g a αs
  = case tys of
      [i] | length i == length αs -> i
      _                           -> die $ bugMissingTypeArgs $ srcPos a
  where
    tys = [i | TypInst m ξ' i <- ann_fact a
             , ξ' == cge_ctx g
             , n == m ]


---------------------------------------------------------------------------------------
envAddFresh :: IsLocated l
            => String
            -> l
            -> (RefType, Assignability, Initialization)
            -> CGEnv
            -> CGM (Id AnnTypeR, CGEnv)
---------------------------------------------------------------------------------------
envAddFresh msg l (t,a,i) g
  = do x  <- freshId l
       g' <- envAdds ("envAddFresh: " ++ msg) [(x, EE a i t)] g
       addAnnot l x t
       return (x, g')

freshId a = Id <$> freshenAnn a <*> fresh

freshenAnn :: IsLocated l => l -> CGM AnnTypeR
freshenAnn l
  = do n     <- cg_ast_cnt <$> get
       modify $ \st -> st {cg_ast_cnt = 1 + n}
       return $ Ann n (srcPos l) []

---------------------------------------------------------------------------------------
envAdds :: EnvKey x => String -> [(x, CGEnvEntry)] -> CGEnv -> CGM CGEnv
---------------------------------------------------------------------------------------
envAdds msg xts g = foldM (envAddGroup msg ks) g es
  where
    es = objFields g <$> xts
    ks = F.symbol . fst <$> xts

---------------------------------------------------------------------------------------
envAddGroup :: String -> [F.Symbol] -> CGEnv -> (F.Symbol,[(F.Symbol,CGEnvEntry)]) -> CGM CGEnv
---------------------------------------------------------------------------------------
envAddGroup msg ks g (x,xts) = do
    zipWithM_ (checkSyms msg g ks) xs ts
    es    <- L.zipWith3 EE as is <$> zipWithM inv is ts
    ids   <- toIBindEnv . catMaybes <$> zipWithM addFixpointBind xs es
    return $ g { cge_names = E.envAdds (zip xs es) $ cge_names g
               , cge_fenv  = F.insertSEnv x ids    $ cge_fenv  g }
  where
    (xs,as,is,ts)   = L.unzip4 [(x,a,i,t) | (x, EE a i t) <- xts ]
    inv Initialized = addInvariant g
    inv _           = return
    toIBindEnv      = (`F.insertsIBindEnv` F.emptyIBindEnv)

-- | Valid symbols:
--
--   * The respective defined symbol @x@
--   * The value variable (v)
--   * Explicitly acceptable symbols (@ok@)
--   * Internal binders
--   * Constant measures
--   * Additional builtin symbols
--   * ReadOnly ... binders in the environment
--
---------------------------------------------------------------------------------------
checkSyms :: EnvKey a => String -> CGEnv -> [a] -> a -> RefType -> CGM ()
---------------------------------------------------------------------------------------
checkSyms m g ok x t = mapM_ cgError errs
  where
    errs             = efoldRType h f F.emptySEnv [] t

    h _              = ()
    f γ t' s         = s ++ catMaybes (chk γ t' <$> F.syms (noKVars $ rTypeReft t'))

    chk γ t' s       | s `L.elem` biReserved
                     = Just $ unimplementedReservedSyms l
                     | s == x_sym
                     = Nothing
                     | s == rTypeValueVar t'
                     = Nothing
                     | s `L.elem` ok_syms
                     = Nothing
                     | s `F.memberSEnv` γ
                     = Nothing
                     | s `F.memberSEnv` cge_consts g
                     = Nothing
                     | s `L.elem` biExtra
                     = Nothing
                     | Just (EE a _ _) <- envLikeFindTy' s g
                     = if a `L.elem` validAsgn then Nothing
                                               else Just $ errorAsgnInRef l x t a
                     | otherwise
                     = Just $ errorUnboundSyms l (F.symbol x) t s m

    l                = srcPos x
    biReserved       = F.symbol <$> ["func", "obj"]
    -- FIXME: Check for this
    biExtra          = F.symbol <$> ["bvor", "bvand", "builtin_BINumArgs", "offset", "this"]
    x_sym            = F.symbol x
    ok_syms          = F.symbol <$> ok
    validAsgn        = [ReadOnly,ImportDecl,WriteLocal]

-- | Bindings for IMMUTABLE fields
---------------------------------------------------------------------------------------
objFields :: EnvKey x => CGEnv -> (x, CGEnvEntry) -> (F.Symbol, [(F.Symbol,CGEnvEntry)])
---------------------------------------------------------------------------------------
objFields g (x, e@(EE a _ t))
              | a `elem` [WriteGlobal,ReturnVar] = (x_sym, [(x_sym,e)])
              | otherwise = (x_sym, (x_sym,e):xts)
  where
    x_sym     = F.symbol x
    xts       = [(mkQualSym x f, ee f o tf) | FieldSig f o m tf <- ms
                                            , isImmutable m
                                            , f /= protoSym ]
    -- This should remain as is: x.f bindings are not going to change
    ee f o tf | optionalFieldType o = EE a Initialized $ orUndef $ ty tf f
              | otherwise           = EE a Initialized $           ty tf f
    -- v = offset(x,"f")
    ty t f    = substThis x t `eSingleton` mkOffset x f

    ms        | Just (TCons _ ms _) <- expandType Coercive g t = M.elems ms
              | otherwise = []

---------------------------------------------------------------------------------------
addFixpointBind :: EnvKey x => x -> CGEnvEntry -> CGM (Maybe F.BindId)
---------------------------------------------------------------------------------------
-- No binding for globals or RetVal: shouldn't appear in refinements
addFixpointBind _ (EE WriteGlobal _ _) = return Nothing
addFixpointBind _ (EE ReturnVar _ _) = return Nothing
addFixpointBind x (EE _ _ t)
  = do  bs           <- binds <$> get
        let (i, bs')  = F.insertBindEnv (F.symbol x) r bs
        modify        $ \st -> st { binds = bs' }
        return        $ Just i
  where
       r              = rTypeSortedReft t


---------------------------------------------------------------------------------------
addInvariant :: CGEnv -> RefType -> CGM RefType
---------------------------------------------------------------------------------------
addInvariant g t
  = do  extraInvariants <- extraInvs <$> cg_opts <$> get
        if extraInvariants then (hasProp . hierarchy . truthy . typeof t . invs) <$> get
                           else (          hierarchy . truthy . typeof t . invs) <$> get
  where
    -- | typeof
    typeof t@(TApp tc _ o)  i = maybe t (strengthenOp t o . rTypeReft . val) $ HM.lookup tc i
    typeof t@(TRef {}) _      = t `nubstrengthen` F.reft (vv t) (typeofExpr $ F.symbol "object")
    typeof t@(TCons {}) _     = t `nubstrengthen` F.reft (vv t) (typeofExpr $ F.symbol "object")
    typeof   (TFun a b c _) _ = TFun a b c typeofReft
    typeof t                _ = t
    -- | Truthy
    truthy t                  | isTObj t
                              = t `nubstrengthen` F.reft (vv t) (F.eProp $ vv t)
                              | otherwise          = t

    strengthenOp t o r        | r `L.elem` ofRef o = t
                              | otherwise          = t `nubstrengthen` r
    typeofReft                = F.reft  (vv t) $ F.pAnd [ typeofExpr $ F.symbol "function"
                                                        , F.eProp    $ vv t                ]
    typeofExpr s              = F.PAtom F.Eq (F.EApp (F.dummyLoc (F.symbol "ttag")) [F.eVar $ vv t])
                                             (F.expr $ symbolText s)

    ofRef (F.Reft (s, ra))    = F.reft s <$> F.conjuncts ra

    -- | { f: T } --> hasProperty("f", v)
    hasProp ty                = t `nubstrengthen` keyReft (boundKeys g ty)
    keyReft ks                = F.reft (vv t) $ F.pAnd (F.PBexp . hasPropExpr <$> ks)
    hasPropExpr s             = F.EApp (F.dummyLoc (F.symbol "hasProperty"))
                                [F.expr (symbolText s), F.eVar $ vv t]

    -- | extends class / interface
    hierarchy t@(TRef c _ _)  | isClassType g t
                              = t `nubstrengthen` rExtClass t (name <$> classAncestors g c)
                              | otherwise
                              = t `nubstrengthen` rExtIface t (name <$> interfaceAncestors g c)
    hierarchy t               = t

    name (QN AK_ _ _ s)       = s

    rExtClass t cs            = F.reft (vv t) (F.pAnd $ refa t "extends_class"     <$> cs)
    rExtIface t cs            = F.reft (vv t) (F.pAnd $ refa t "extends_interface" <$> cs)

    refa t s c                = F.PBexp $ F.EApp (sym s) [ F.expr $ rTypeValueVar t, F.expr $ symbolText c]
    vv                        = rTypeValueVar
    sym s                     = F.dummyLoc $ F.symbol s


----------------------------------------------------------------------------------------
nubstrengthen                   :: RefType -> F.Reft -> RefType
----------------------------------------------------------------------------------------
nubstrengthen (TApp c ts r) r'  = TApp c ts  $ r' `mappend` r
nubstrengthen (TRef c ts r) r'  = TRef c ts  $ r' `mappend` r
nubstrengthen (TCons ts m r) r' = TCons ts m $ r' `mappend` r
nubstrengthen (TVar α r)    r'  = TVar α     $ r' `mappend` r
nubstrengthen (TFun a b c r) r' = TFun a b c $ r' `mappend` r
nubstrengthen t _               = t




-- IN FIXPOINT meetReft (F.Reft (v, ras)) (F.Reft (v', ras'))
-- IN FIXPOINT   | v == v'            = F.Reft (v , L.nubBy cmp $ ras  ++ ras')
-- IN FIXPOINT   | v == F.dummySymbol = F.Reft (v', L.nubBy cmp $ ras' ++ (ras `F.subst1`  (v , F.EVar v')))
-- IN FIXPOINT   | otherwise          = F.Reft (v , L.nubBy cmp $ ras  ++ (ras' `F.subst1` (v', F.EVar v )))
-- IN FIXPOINT   where
-- IN FIXPOINT     cmp = (==) `on` (show . F.toFix)




---------------------------------------------------------------------------------------
addAnnot       :: (IsLocated l, F.Symbolic x) => l -> x -> RefType -> CGM ()
---------------------------------------------------------------------------------------
addAnnot l x t = modify $ \st -> st {cg_ann = S.addAnnot (srcPos l) x t (cg_ann st)}

---------------------------------------------------------------------------------------
envAddReturn        :: (IsLocated f)  => f -> RefType -> CGEnv -> CGM CGEnv
---------------------------------------------------------------------------------------
envAddReturn f t g  = return $ g { cge_names = E.envAddReturn f e $ cge_names g }
  where
    e   = EE ReturnVar Initialized t

---------------------------------------------------------------------------------------
envAddGuard       :: (F.Symbolic x, IsLocated x) => x -> Bool -> CGEnv -> CGEnv
---------------------------------------------------------------------------------------
envAddGuard x b g = g { cge_guards = guard b x : cge_guards g }
  where
    guard True    = F.eProp
    guard False   = F.PNot . F.eProp

---------------------------------------------------------------------------------------
envPopGuard       :: CGEnv -> CGEnv
---------------------------------------------------------------------------------------
envPopGuard g = g { cge_guards = grdPop $ cge_guards g }
  where
    grdPop (_:xs) = xs
    grdPop []     = []


instance F.Expression a => F.Expression (Located a) where
  expr (Loc _ a) = F.expr a

instance F.Expression TVar where
  expr (TV a _) = F.expr a


-- | A helper that returns the @RefType@ of variable @x@. Interstring cases:
--
--   * Global variables (that can still be assigned) should not be strengthened
--     with single
--
--   * Class names (they might contain static fields)
--
--   * Local (non-assignable) variables (strengthened with singleton for base-types)
--
---------------------------------------------------------------------------------------
envFindTy :: (EnvKey x, F.Expression x) => x -> CGEnv -> Maybe RefType
---------------------------------------------------------------------------------------
envFindTy x g = ee_type <$> envFindTyWithAsgn x g

type EnvKey x = (IsLocated x, F.Symbolic x, PP x, F.Expression x)

envFindTyNoSngl x g = ee_type <$> envFindTy_ x g


-- Only include the "singleton" refinement in the case where Assignability is
-- either ReadOnly of WriteLocal (SSAed)
---------------------------------------------------------------------------------------
envFindTyWithAsgn :: (EnvKey x, F.Expression x) => x -> CGEnv -> Maybe CGEnvEntry
---------------------------------------------------------------------------------------
envFindTyWithAsgn x = (singleton <$>) . envFindTy_ x
  where
    singleton  e@(EE WriteGlobal _ _) = adjustInit e
    singleton    (EE a           i t) = EE a i $ t `eSingleton` x
    adjustInit s@(EE _ Initialized _) = s
    adjustInit   (EE a i           t) = EE a i $ orUndef t

---------------------------------------------------------------------------------------
envFindTy_ :: EnvKey x => x -> CGEnv -> Maybe CGEnvEntry
---------------------------------------------------------------------------------------
envFindTy_ x g
  | Just t  <- E.envFindTy x $ cge_names g = Just t
  | Just g' <- cge_parent g                = envFindTy_ x g'
  |otherwise                               = Nothing

---------------------------------------------------------------------------------------
safeEnvFindTy :: (EnvKey x, F.Expression x) => x -> CGEnv -> CGM RefType
---------------------------------------------------------------------------------------
safeEnvFindTy x g | Just t <- envFindTy x g = return t
                  | otherwise = cgError $ bugEnvFindTy (srcPos x) x

---------------------------------------------------------------------------------------
safeEnvFindTyNoSngl :: EnvKey x => x -> CGEnv -> CGM RefType
---------------------------------------------------------------------------------------
safeEnvFindTyNoSngl x g | Just t <- envFindTyNoSngl x g = return t
                        | otherwise = cgError $ bugEnvFindTy (srcPos x) x

---------------------------------------------------------------------------------------
safeEnvFindTyWithAsgn :: EnvKey x => x -> CGEnv -> CGM CGEnvEntry
---------------------------------------------------------------------------------------
safeEnvFindTyWithAsgn x g | Just t <- envFindTyWithAsgn x g = return t
                          | otherwise = cgError $ bugEnvFindTy (srcPos x) x

---------------------------------------------------------------------------------------
envFindReturn :: CGEnv -> RefType
---------------------------------------------------------------------------------------
envFindReturn = ee_type . E.envFindReturn . cge_names


---------------------------------------------------------------------------------------
-- | Fresh Templates
---------------------------------------------------------------------------------------

-- | Instantiate Fresh Type (at Function-site)
---------------------------------------------------------------------------------------
freshTyFun :: (IsLocated l) => CGEnv -> l -> RefType -> CGM RefType
---------------------------------------------------------------------------------------
freshTyFun g l t
  | not (isTFun t)     = return t
  | isTrivialRefType t = freshTy "freshTyFun" (toType t) >>= wellFormed l g
  | otherwise          = return t


freshenType WriteGlobal g l t
  | isTrivialRefType t = freshTy "freshenType-WG" (toType t) >>= wellFormed l g
  | otherwise          = return t

freshenType _ g l t
  | not (isTFun t)     = return t
  | isTrivialRefType t = freshTy "freshenType-RO" (toType t) >>= wellFormed l g
  | otherwise          = return t


-- | Instantiate Fresh Type (at Call-site)
freshTyInst l g αs τs tbody
  = do ts    <- mapM (freshTy "freshTyInst") τs
       _     <- mapM (wellFormed l g) ts
       return $ apply (fromList $ zip αs ts) tbody

-- | Instantiate Fresh Type (at Phi-site)
---------------------------------------------------------------------------------------
freshTyPhis :: AnnTypeR -> CGEnv -> [Id AnnTypeR] -> [Type] -> CGM (CGEnv, [RefType])
---------------------------------------------------------------------------------------
freshTyPhis l g xs τs
  = do ts <- mapM (freshTy "freshTyPhis") τs
       g' <- envAdds "freshTyPhis" (zip xs (EE WriteLocal Initialized <$> ts)) g
       _  <- mapM (wellFormed l g') ts
       return (g', ts)


-- | Instantiate Fresh Type (at Phi-site)
---------------------------------------------------------------------------------------
freshTyPhis' :: AnnTypeR -> CGEnv -> [Id AnnTypeR] -> [EnvEntry ()] -> CGM (CGEnv, [RefType])
---------------------------------------------------------------------------------------
freshTyPhis' l g xs es
  = do ts' <- mapM (freshTy "freshTyPhis") ts
       g'  <- envAdds "freshTyPhis" (zip xs (L.zipWith3 EE as is ts')) g
       _   <- mapM (wellFormed l g') ts'
       return (g', ts')
  where
    (as,is,ts) = L.unzip3 [ (a,i,t) | EE a i t <- es ]


-- | Fresh Object Type
---------------------------------------------------------------------------------------
freshTyObj :: (IsLocated l) => l -> CGEnv -> RefType -> CGM RefType
---------------------------------------------------------------------------------------
freshTyObj l g t = freshTy "freshTyArr" t >>= wellFormed l g


---------------------------------------------------------------------------------------
freshenCGEnvM :: CGEnv -> CGM CGEnv
---------------------------------------------------------------------------------------
freshenCGEnvM g
  = do  names   <- E.envFromList  <$> mapM (go g) (E.envToList  $ cge_names g)
        modules <- E.qenvFromList <$> mapM (freshenModuleDefM g) (E.qenvToList $ cge_mod g)
        return $ g { cge_names = names, cge_mod = modules }
  where
    go _ (x, EE a i v@TVar{}) = return (x, EE a i v)
    go g (x, EE ReadOnly i t) = (x,) . EE ReadOnly i <$> freshenType ReadOnly g (srcPos x) t
    go _ (x, EE a i t)        = return (x, EE a i t)

freshenModuleDefM g (a, m)
  = do  vars     <- E.envFromList <$> mapM f (E.envToList $ m_variables m)
        types    <- E.envFromList <$> mapM h (E.envToList $ m_types m)
        return (a,m { m_variables = vars, m_types = types })
  where
    f (x, (v,w,t,i)) =
      case w of
        ReadOnly   -> do  ft    <- freshenType ReadOnly g x t
                          return   (x, (v, w, ft,i))
        _          -> return (x,(v,w,t,i))

    -- KVar class definitions only
    h (x, t) | t_class t == ClassKind
             = do es <- M.fromList <$> mapM (freshElt x) (M.toList $ t_elts t)
                  return (x, t { t_elts = es })
             | otherwise
             = return (x,t)
    freshElt x (s,b) = (s,) <$> mapEltM (freshTyFun g x) b


---------------------------------------------------------------------------------------
-- | Adding Subtyping Constraints
---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------
subType :: AnnTypeR -> Error -> CGEnv -> RefType -> RefType -> CGM ()
---------------------------------------------------------------------------------------
subType l err g t1 t2 =
  -- t1'    <- addInvariant g t1  -- enhance LHS with invariants
  modify  $ \st -> st { cs = Sub g (ci err l) t1 t2 : cs st }


-- FIXME: Restore this check !!!
-- ---------------------------------------------------------------------------------------
-- safeExtends :: SrcSpan -> CGEnv -> IfaceDef F.Reft -> CGM ()
-- ---------------------------------------------------------------------------------------
-- safeExtends l g (ID _ _ _ (Just (p, ts)) es) = zipWithM_ sub t1s t2s
--   where
--     sub t1 t2  = subType l g (zipType δ t1 t2) t2
--     (t1s, t2s) = unzip [ (t1,t2) | pe <- expand True δ (findSymOrDie p δ, ts)
--                                  , ee <- es
--                                  , sameBinder pe ee
--                                  , let t1 = eltType ee
--                                  , let t2 = eltType pe ]
-- safeExtends _ _ _ (ID _ _ _ Nothing _) = return ()


--------------------------------------------------------------------------------
-- | Adding Well-Formedness Constraints
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
wellFormed       :: (IsLocated l) => l -> CGEnv -> RefType -> CGM RefType
--------------------------------------------------------------------------------
wellFormed l g t
  = do modify $ \st -> st { ws = W g (ci err l) t : ws st }
       return t
    where
       err = errorWellFormed (srcPos l)

--------------------------------------------------------------------------------
-- | Generating Fresh Values
--------------------------------------------------------------------------------

class Freshable a where
  fresh   :: CGM a
  true    :: a -> CGM a
  true    = return
  refresh :: a -> CGM a
  refresh = return

instance Freshable Integer where
  fresh = do modify $ \st -> st { cg_cnt = 1 + cg_cnt st }
             cg_cnt <$> get

instance Freshable F.Symbol where
  fresh = F.tempSymbol (F.symbol "nano") <$> fresh

instance Freshable String where
  fresh = symbolString <$> fresh

-- | Freshen up
freshTy :: RefTypable a => s -> a -> CGM RefType
freshTy _ τ = refresh $ rType τ

instance Freshable F.Pred where
  fresh  = kv <$> fresh
    where
      kv = (`F.PKVar` mempty) . F.intKvar

instance Freshable F.Reft where
  fresh                  = errorstar "fresh F.Reft"
  true    (F.Reft (v,_)) = return $ F.Reft (v, mempty)
  refresh (F.Reft (_,_)) = curry F.Reft <$> freshVV <*> fresh
    where freshVV        = F.vv . Just  <$> fresh


instance Freshable F.SortedReft where
  fresh                  = errorstar "fresh F.Reft"
  true    (F.RR so r)    = F.RR so <$> true r
  refresh (F.RR so r)    = F.RR so <$> refresh r

instance Freshable RefType where
  fresh   = errorstar "fresh RefType"
  refresh = mapReftM refresh
  true    = trueRefType

trueRefType    :: RefType -> CGM RefType
trueRefType    = mapReftM true


--------------------------------------------------------------------------------
-- | Splitting Subtyping Constraints
--------------------------------------------------------------------------------
--
-- The types that are split should be aligned by type
--
-- Premise: |t1| = |t2| -- raw type equality
--
--------------------------------------------------------------------------------
splitC :: SubC -> CGM [FixSubC]
--------------------------------------------------------------------------------

-- | Function types
--
splitC (Sub g i tf1@(TFun s1 xt1s t1 _) tf2@(TFun s2 xt2s t2 _))
  = do bcs       <- bsplitC g i tf1 tf2
       g'        <- envTyAdds "splitC" i (thisB ++ xt2s) g
       cs        <- splitOC g i s1 s2
       cs'       <- concatMapM splitC $ zipWith (Sub g' i) t2s t1s'
       cs''      <- splitC $ Sub g' i (F.subst su t1) t2
       return     $ bcs ++ cs ++ cs' ++ cs''
    where
       thisB      = B (F.symbol "this") <$> maybeToList s2
       t2s        = b_type <$> xt2s
       t1s'       = F.subst su (b_type <$> xt1s)
       su         = F.mkSubst $ zipWith bSub xt1s xt2s
       bSub b1 b2 = (b_sym b1, F.eVar $ b_sym b2)

-- | TAlls
--
splitC (Sub g i (TAll α1 t1) (TAll α2 t2))
  | α1 == α2
  = splitC $ Sub g i t1 t2
  | otherwise
  = splitC $ Sub g i t1 t2'
  where
    θ   = fromList [(α2, tVar α1 :: RefType)]
    t2' = apply θ t2

-- | TVars
--
splitC (Sub g i t1@(TVar α1 _) t2@(TVar α2 _))
  | α1 == α2
  = bsplitC g i t1 t2
  | otherwise
  = splitIncompatC g i t1

-- | Unions
--
splitC (Sub g i t1@(TApp TUn t1s r1) t2@(TApp TUn t2s _))
  | F.isFalse (F.simplify r1)
  = return []
  | otherwise
  = (++) <$> bsplitC g i t1 t2
         <*> concatMapM splitC (safeZipWith "splitc-3" (Sub g i) s1s s2s)
    where
       s1s = L.sortBy (compare `on` toType) t1s
       s2s = L.sortBy (compare `on` toType) t2s

splitC (Sub g i t1@(TApp TUn t1s _) t2)
  | [t1] <- L.filter (on (==) toType t2) t1s
  , t1s' <- L.filter (on (/=) toType t2) t1s
  = (++) <$> splitC (Sub g i t1 t2) <*> concatMapM (splitIncompatC g i) t1s'
  | otherwise
  = splitIncompatC g i t1

-- |Type references
--
--  FIXME: restore co/contra-variance
--
splitC (Sub g i t1@(TRef x1 (m1:t1s) r1) t2@(TRef x2 (m2:t2s) r2))
  --
  -- * Trivial case (do not even descend)
  --
  | F.isFalse (F.simplify r1)
  = return []
  --
  -- * Incompatible mutabilities
  --
  | not (isSubtype g m1 m2)
  = splitIncompatC g i t1
  --
  -- * Both immutable, same name, non arrays: Co-variant subtyping
  --
  | x1 == x2 && isImmutable m2 && not (isArr t1)
  = do  cs    <- bsplitC g i t1 t2
        cs'   <- concatMapM splitC $ safeZipWith "splitc-4" (Sub g i) t1s t2s
        return $ cs ++ cs'
  --
  -- * Non-immutable, same name: invariance
  --
  | x1 == x2 && not (F.isFalse r2)
  = do  cs    <- bsplitC g i t1 t2
        cs'   <- concatMapM splitC $ safeZipWith "splitc-5" (Sub g i) t1s t2s
        cs''  <- concatMapM splitC $ safeZipWith "splitc-6" (Sub g i) t2s t1s
        return $ cs ++ cs' ++ cs''

  | x1 == x2
  = bsplitC g i t1 t2

  | otherwise
  = splitIncompatC g i t1

-- | Rest of TApp
--
splitC (Sub g i t1@(TApp c1 t1s _) t2@(TApp c2 t2s _))
  | c1 == c2
  = do  cs    <- bsplitC g i t1 t2
        cs'   <- concatMapM splitC (safeZipWith "splitc-5" (Sub g i) t1s t2s)
        return $ cs ++ cs'
  | otherwise = splitIncompatC g i t1

-- | These need to be here due to the lack of a folding operation
--
splitC (Sub g i t1@(TRef {}) t2) =
  case expandType Coercive g t1 of
    Just t1' -> splitC (Sub g i t1' t2)
    Nothing  -> cgError $ errorUnfoldType l t1 where l = srcPos i

splitC (Sub g i t1 t2@(TRef {})) =
  case expandType Coercive g t2 of
    Just t2' -> splitC (Sub g i t1 t2')
    Nothing  -> cgError $ errorUnfoldType l t2 where l = srcPos i

-- | TCons
--
splitC (Sub g i@(Ci _ l) t1@(TCons _ e1s r1) t2@(TCons _ e2s _))
  | F.isFalse (F.simplify r1)
  = return []
  | otherwise
  = do  cs     <- bsplitC g i t1 t2
        (x,g') <- envAddFresh "" l (t1,ReadOnly,Initialized) g
        cs'    <- splitEs g' (F.symbol x) i e1s e2s
        return $ cs ++ cs'

splitC (Sub _ _ (TClass _) (TClass _)) = return []
splitC (Sub _ _ (TModule _) (TModule _)) = return []

splitC (Sub g i t1 _)
  = splitIncompatC g i t1

splitOC g i (Just t) (Just t') = splitC (Sub g i t t')
splitOC _ _ _        _         = return []

---------------------------------------------------------------------------------------
splitIncompatC :: CGEnv -> a -> RefType -> CGM [F.SubC a]
---------------------------------------------------------------------------------------
splitIncompatC g i t = bsplitC g i t (mkBot t)

---------------------------------------------------------------------------------------
mkBot   :: (F.Reftable r) => RType r -> RType r
---------------------------------------------------------------------------------------
mkBot t = setRTypeR t (F.bot r) where r = rTypeR t

--
--  Gather the types of the LHS IMMUTABLE fields and add them to the
--  environment with the relevant field symbol as binder
--
--  FIXME:
--
--  * Add special cases: IndexSig ...
--
---------------------------------------------------------------------------------------
splitEs :: CGEnv
        -> F.Symbol
        -> Cinfo
        -> TypeMembers F.Reft -> TypeMembers F.Reft
        -> CGM [FixSubC]
---------------------------------------------------------------------------------------
splitEs g x i@(Ci _ l) e1s e2s
  = do  g' <- foldM step g imBs
        concatMapM (uncurry $ splitE g' i) es
  where
    e1s'     = (substThis x <$>) <$> e1s
    e2s'     = (substThis x <$>) <$> e2s
    es       = M.elems $ M.intersectionWith (,) e1s' e2s'
    step g p = snd <$> envAddFresh "" l (mkEntry p) g
    imBs     = [ (f,t) | (FieldSig f _ m t, _) <- es
                       , isImmutable m, f /= protoSym ]
    mkEntry (f,t) = (t `eSingleton` mkOffset x f, ReadOnly, Initialized)


splitE g i (CallSig t1) (CallSig t2) = splitC (Sub g i t1 t2)
splitE g i (ConsSig t1) (ConsSig t2) = splitC (Sub g i t1 t2)

splitE g i (IndexSig _ _ t1) (IndexSig _ _ t2)
  = do  cs    <- splitC (Sub g i t1 t2)
        cs'   <- splitC (Sub g i t2 t1)
        return $ cs ++ cs'

splitE g i (FieldSig _ _ μf1 t1) (FieldSig _ _ μf2 t2)
  = splitWithMut g i (μf1,t1) (μf2,t2)

splitE g i (MethSig _ t1) (MethSig _ t2)
  = splitC (Sub g i t1 t2)

splitE _ _ _ _ = return []


splitWithMut g i (_,t1) (μf2,t2)
  | isImmutable μf2
  = splitC (Sub g i t1 t2)
  | otherwise
  = (++) <$> splitC (Sub g i t1 t2)
         <*> splitC (Sub g i t2 t1)


-- splitMaybe g i (Just t1) (Just t2) = splitC (Sub g i t1 t2)
-- splitMaybe _ _ _         _         = return []

---------------------------------------------------------------------------------------
bsplitC :: CGEnv -> a -> RefType -> RefType -> CGM [F.SubC a]
---------------------------------------------------------------------------------------
-- NOTE: addInvariant nonly needed in LHS
bsplitC g ci t1 t2 = bsplitC' g ci <$> addInvariant g t1 <*> return t2

conjoinPred :: F.Pred -> F.SortedReft -> F.SortedReft
conjoinPred p r    = r {F.sr_reft = F.Reft (v, F.pAnd [pr, p]) }
  where
    F.Reft (v, pr) = F.sr_reft r

bsplitC' g ci t1 t2
  | F.isFunctionSortedReft r1 && F.isNonTrivial r2
  = F.subC bs (conjoinPred p $ r1 {F.sr_reft = typeofReft t1}) r2 Nothing [] ci
  | F.isNonTrivial r2
  = F.subC bs (conjoinPred p r1) r2 Nothing [] ci
  | otherwise
  = []
  where
    bs             = foldl F.unionIBindEnv F.emptyIBindEnv
                   $ snd <$> F.toListSEnv (cge_fenv g)
    p              = F.pAnd $ cge_guards g
    (r1,r2)        = (rTypeSortedReft t1, rTypeSortedReft t2)
    typeofReft t   = F.reft (vv t) $ F.pAnd [ typeofExpr (F.symbol "function") t
                                            , F.eProp $ vv t ]
    typeofExpr s t = F.PAtom F.Eq (F.EApp (F.dummyLoc (F.symbol "ttag")) [F.eVar $ vv t])
                                  (F.expr $ symbolText s)
    vv             = rTypeValueVar



instance PP F.SortedReft where
  pp (F.RR _ b) = pp b

---------------------------------------------------------------------------------------
-- | Splitting Well-Formedness Constraints
---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------
splitW :: WfC -> CGM [FixWfC]
---------------------------------------------------------------------------------------
splitW (W g i ft@(TFun s ts t _))
  = do let bws = bsplitW g ft i
       g'     <- envTyAdds "splitW" i ts g
       ws     <- concatMapM splitW (W g' i <$> maybeToList s)
       ws'    <- concatMapM splitW [W g' i ti | B _ ti <- ts]
       ws''   <-            splitW (W g' i t)
       return  $ bws ++ ws ++ ws' ++ ws''

splitW (W g i (TAll _ t))
  = splitW (W g i t)

splitW (W g i t@(TVar _ _))
  = return $ bsplitW g t i

splitW (W g i t@(TApp _ ts _))
  =  do let ws = bsplitW g t i
        ws'   <- concatMapM splitW [W g i ti | ti <- ts]
        return $ ws ++ ws'

splitW (W g i t@(TRef _ ts _))
  =  do let ws = bsplitW g t i
        ws'   <- concatMapM splitW [W g i ti | ti <- ts]
        return $ ws ++ ws'

splitW (W g i (TAnd ts))
  = concatMapM splitW [W g i t | t <- ts]

splitW (W g i t@(TCons _ es _))
  = do let bws = bsplitW g t i
       -- FIXME: add field bindings in g?
       ws     <- concatMapM splitW [ W g i $ eltType e | e <- M.elems es ]
       return  $ bws ++ ws

splitW (W _ _ (TClass _ ))
  = return []

splitW (W _ _ (TModule _ ))
  = return []

splitW (W _ _ (TEnum _ ))
  = return []

splitW (W _ _ t) = error $ render $ text "Not supported in splitW: " <+> pp t

bsplitW g t i
  | F.isNonTrivial r'
  = F.wfC bs r' {- Nothing -} i
  | otherwise
  = []
  where
    r' = rTypeSortedReft t
    bs = foldl F.unionIBindEnv F.emptyIBindEnv
       $ snd <$> F.toListSEnv (cge_fenv g)


envTyAdds msg l xts = envAdds msg' [ (symbolId l x, EE WriteLocal Initialized t)
                                   | B x t <- xts]
  where
    msg' = msg ++ " - envTyAdds " ++ ppshow (srcPos l)


------------------------------------------------------------------------------
cgFunTys :: (IsLocated l, F.Symbolic b, PP x, PP [b])
         => l
         -> x
         -> [b]
         -> RefType
         -> CGM [(Int, ([TVar], Maybe RefType, [RefType], RefType))]
------------------------------------------------------------------------------
cgFunTys l f xs ft        | Just ts <- bkFuns ft
                          = zip ([0..] :: [Int]) <$> mapM fTy ts
                          | otherwise
                          = cgError $ errorNonFunction (srcPos l) f ft
  where
    fTy (αs, s, yts, t)   | Just yts' <- padUndefineds xs yts
                          = uncurry (αs,s,,) <$> subNoCapture l yts' xs t
                          | otherwise
                          = cgError $ errorArgMismatch (srcPos l)


-- | Avoids capture of function binders by existing value variables
subNoCapture _ yts xs t   = (,) <$> mapM (mapReftM ff) ts <*> mapReftM ff t
  where
    --
    -- If the symbols we are about to introduce are already capture by some
    -- value variable, then (1) create a fresh value var. and (2) proceed with
    -- the substitution after replacing with the new val. var.
    --
    ff r@(F.Reft (v,ras)) | v `L.elem` xss
                          = do v' <- freshVV
                               return $ F.subst su $ F.Reft . (v',)
                                      $ F.subst (F.mkSubst [(v, F.expr v')]) ras
                          | otherwise
                          = return $ F.subst su r

    freshVV               = F.vv . Just <$> fresh
    xss                   = F.symbol <$> xs

    su                    = F.mkSubst $ safeZipWith "subNoCapture" fSub yts xs

    ts                    = [ t | B _ t <- yts ]

    fSub yt x             = (b_sym yt, F.eVar x)


--------------------------------------------------------------------------------
-- | zipType wrapper
--
--   FIXME: What is the purpose of this substitution ???
--
zipTypeUpM g x t1 t2
  | Just (f, F.Reft (s, ras)) <- zipType g x t1 t2
  = let su  = F.mkSubst [(s, F.expr x)] in
    let rs  = F.simplify $ F.Reft (s, F.subst su ras) `F.meet` F.uexprReft x in
    return  $ f rs
  | otherwise
  = cgError $ bugZipType (srcPos x) t1 t2

zipTypeDownM g x t1 t2
  | Just (f, r) <- zipType g x t1 t2
  = return $ f r
  | otherwise
  = cgError $ bugZipType (srcPos x) t1 t2

-- Local Variables:
-- flycheck-disabled-checkers: (haskell-liquid)
-- End:
