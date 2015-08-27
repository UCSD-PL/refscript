{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DoAndIfThenElse           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

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
  , cgEnvAddFresh, cgEnvAdds
  , envAddGuard, envPopGuard
  , envFindTy, envFindTyWithAsgn
  , safeEnvFindTy, safeEnvFindTyWithAsgn
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
import           Control.Exception                (throw)
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Except
import           Data.Function                    (on)
import qualified Data.HashMap.Strict              as HM
import qualified Data.List                        as L
import qualified Data.Map.Strict                  as M
import           Data.Maybe                       (catMaybes, maybeToList)
import           Data.Monoid                      (mappend, mempty)
import qualified Data.Traversable                 as T
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Types          as F
import           Language.Nano.Annots
import           Language.Nano.AST
import           Language.Nano.ClassHierarchy
import           Language.Nano.CmdLine
import           Language.Nano.Core.Env
import           Language.Nano.Environment
import           Language.Nano.Errors
import           Language.Nano.Liquid.Constraint
import           Language.Nano.Liquid.Environment
import           Language.Nano.Liquid.Types
import           Language.Nano.Locations
import           Language.Nano.Names
import           Language.Nano.Pretty
import           Language.Nano.Program
import qualified Language.Nano.SystemUtils        as S
import           Language.Nano.Typecheck.Sub
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Typecheck.Types
import           Language.Nano.Types
import           Text.PrettyPrint.HughesPJ

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
  pp s = pp (F.lhsCs s) <+> text " <: " <+> pp (F.rhsCs s)


---------------------------------------------------------------------------------------
-- | Constraint Generation Monad
---------------------------------------------------------------------------------------

data CGState = CGS {
  --
  -- ^ global list of fixpoint binders
  --
    binds      :: F.BindEnv
  --
  -- ^ subtyping constraints
  --
  , cs         :: ![SubC]
  --
  -- ^ well-formedness constraints
  --
  , ws         :: ![WfC]
  --
  -- ^ freshness counter
  --
  , cg_cnt     :: !Integer
  --
  -- ^ recorded annotations
  --
  , cg_ann     :: S.UAnnInfo RefType
  --
  -- ^ type constructor invariants
  --
  , invs       :: TConInv
  --
  -- ^ configuration options
  --
  , cg_opts    :: Config
  --
  -- ^ AST Counter
  --
  , cg_ast_cnt :: NodeId

  }

type CGM     = ExceptT Error (State CGState)

type TConInv = HM.HashMap TPrim (Located RefType)

-------------------------------------------------------------------------------
getCGInfo :: Config -> RefScript -> CGM a -> CGInfo
-------------------------------------------------------------------------------
getCGInfo cfg pgm = cgStateCInfo pgm . execute cfg pgm . (>> fixCWs)
  where
    fixCWs       = (,) <$> fixCs <*> fixWs
    fixCs        = get >>= concatMapM splitC . cs
    fixWs        = get >>= concatMapM splitW . ws

-------------------------------------------------------------------------------
execute :: Config -> RefScript -> CGM a -> (a, CGState)
-------------------------------------------------------------------------------
execute cfg pgm act
  = case runState (runExceptT act) $ initState cfg pgm of
      (Left e, _)   -> throw e
      (Right x, st) -> (x, st)

-------------------------------------------------------------------------------
initState :: Config -> RefScript -> CGState
-------------------------------------------------------------------------------
initState c p   = CGS F.emptyBindEnv [] [] 0 mempty invars c (maxId p)
  where
    invars      = HM.fromList [(p, t) | t@(Loc _ (TPrim p _)) <- invts p]

-------------------------------------------------------------------------------
cgStateCInfo :: RefScript -> (([F.SubC Cinfo], [F.WfC Cinfo]), CGState) -> CGInfo
-------------------------------------------------------------------------------
cgStateCInfo pgm ((fcs, fws), cg) = CGI (patchSymLits fi) (cg_ann cg)
  where
    fi   = F.FI { F.cm       = HM.fromList $ F.addIds fcs
                , F.ws       = fws
                , F.bs       = binds cg
                , F.gs       = measureEnv pgm
                , F.lits     = []
                , F.kuts     = F.ksEmpty
                , F.quals    = pQuals pgm
                , F.bindInfo = mempty
                }

patchSymLits fi = fi { F.lits = F.symConstLits fi ++ F.lits fi }


-- | Get binding from object type

---------------------------------------------------------------------------------------
measureEnv :: RefScript -> F.SEnv F.SortedReft
---------------------------------------------------------------------------------------
measureEnv = fmap rTypeSortedReft . envSEnv . consts


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
envGetContextCast :: CGEnv -> AnnLq -> Cast F.Reft
---------------------------------------------------------------------------------------
envGetContextCast g a
  = case [c | TCast cx c <- fFact a, cx == cge_ctx g] of
      [ ] -> CNo
      [c] -> c
      cs  -> case L.find isDeadCast cs of
               Just dc -> dc
               Nothing -> die $ errorMultipleCasts (srcPos a) cs
  where
    isDeadCast CDead{} = True
    isDeadCast _       = False

---------------------------------------------------------------------------------------
envGetContextTypArgs :: Int -> CGEnv -> AnnLq -> [TVar] -> [RefType]
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
    tys = [i | TypInst m ξ' i <- fFact a
             , ξ' == cge_ctx g
             , n == m ]


---------------------------------------------------------------------------------------
cgEnvAddFresh :: IsLocated l => String -> l -> VarInfo F.Reft -> CGEnv -> CGM (Id AnnLq, CGEnv)
---------------------------------------------------------------------------------------
cgEnvAddFresh msg l v g
  = do x  <- freshId l
       g' <- cgEnvAdds ("cgEnvAddFresh: " ++ msg) [(x, v)] g
       addAnnot l x $ v_type v
       return (x, g')

freshId a = Id <$> freshenAnn a <*> fresh

freshenAnn :: IsLocated l => l -> CGM AnnLq
freshenAnn l
  = do n     <- cg_ast_cnt <$> get
       modify $ \st -> st {cg_ast_cnt = 1 + n}
       return $ FA n (srcPos l) []

---------------------------------------------------------------------------------------
cgEnvAdds :: EnvKey x => String -> [(x, CGEnvEntry)] -> CGEnv -> CGM CGEnv
---------------------------------------------------------------------------------------
cgEnvAdds msg xts g = foldM (envAddGroup msg ks) g es
  where
    es = objFields g <$> xts
    ks = F.symbol . fst <$> xts

---------------------------------------------------------------------------------------
envAddGroup :: String -> [F.Symbol] -> CGEnv -> (F.Symbol,[(F.Symbol,CGEnvEntry)]) -> CGM CGEnv
---------------------------------------------------------------------------------------
envAddGroup msg ks g (x,xts) = do
    zipWithM_ (checkSyms msg g ks) xs ts
    es    <- L.zipWith3 VI as is <$> zipWithM inv is ts
    ids   <- toIBindEnv . catMaybes <$> zipWithM addFixpointBind xs es
    return $ g { cge_names = envAdds (zip xs es) $ cge_names g
               , cge_fenv  = F.insertSEnv x ids    $ cge_fenv  g }
  where
    (xs,as,is,ts)   = L.unzip4 [(x,a,i,t) | (x, VI a i t) <- xts ]
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
                     | Just (VI a _ _) <- envLikeFindTy' s g
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
    validAsgn        = [Ambient, WriteLocal]

-- | Bindings for IMMUTABLE fields
---------------------------------------------------------------------------------------
objFields :: EnvKey x => CGEnv -> (x, CGEnvEntry) -> (F.Symbol, [(F.Symbol,CGEnvEntry)])
---------------------------------------------------------------------------------------
objFields g (x, e@(VI a _ t))
              | a `elem` [WriteGlobal,ReturnVar] = (x_sym, [(x_sym,e)])
              | otherwise = (x_sym, (x_sym,e):xts)
  where
    x_sym     = F.symbol x
    xts       = [(mkQualSym x f, vi f o ft) | (f, FI o m ft) <- fs, isImm m ]

    -- This should remain as is: x.f bindings are not going to change
    vi f Opt tf = VI a Initialized $ orUndef $ ty tf f
    vi f Req tf = VI a Initialized $           ty tf f
    -- v = offset(x,"f")
    ty t f = substThis x t `eSingleton` mkOffset x f

    fs | Just (TObj ms _) <- expandType Coercive (envCHA g) t
       = F.toListSEnv (tm_prop ms)
       | otherwise = []

---------------------------------------------------------------------------------------
addFixpointBind :: EnvKey x => x -> CGEnvEntry -> CGM (Maybe F.BindId)
---------------------------------------------------------------------------------------
-- No binding for globals or RetVal: shouldn't appear in refinements
addFixpointBind _ (VI WriteGlobal _ _) = return Nothing
addFixpointBind _ (VI ReturnVar _ _) = return Nothing
addFixpointBind x (VI _ _ t)
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
    typeof t@(TPrim p o)    i = maybe t (strengthenOp t o . rTypeReft . val) $ HM.lookup p i
    typeof t@(TRef _ _ _)   _ = t `nubstrengthen` F.reft (vv t) (typeofExpr $ F.symbol "object")
    typeof t@(TObj _ _)     _ = t `nubstrengthen` F.reft (vv t) (typeofExpr $ F.symbol "object")
    typeof   (TFun a b c _) _ = TFun a b c typeofReft
    typeof t                _ = t
    -- | Truthy
    truthy t                  | maybeTObj t
                              = t `nubstrengthen` F.reft (vv t) (F.eProp $ vv t)
                              | otherwise          = t

    strengthenOp t o r        | r `L.elem` ofRef o = t
                              | otherwise          = t `nubstrengthen` r
    typeofReft                = F.reft  (vv t) $ F.pAnd [ typeofExpr $ F.symbol "function"
                                                        , F.eProp    $ vv t                ]
    typeofExpr s              = F.PAtom F.Eq (F.EApp (F.dummyLoc (F.symbol "ttag")) [F.eVar $ vv t])
                                             (F.expr $ F.symbolText s)

    ofRef (F.Reft (s, ra))    = F.reft s <$> F.raConjuncts ra

    -- | { f: T } --> hasProperty("f", v)
    hasProp ty                = t `nubstrengthen` keyReft (boundKeys g ty)
    keyReft ks                = F.reft (vv t) $ F.pAnd (F.PBexp . hasPropExpr <$> ks)
    hasPropExpr s             = F.EApp (F.dummyLoc (F.symbol "hasProperty"))
                                [F.expr (F.symbolText s), F.eVar $ vv t]

    -- | extends class / interface
    hierarchy t@(TRef c _ _)  | isClassType g t
                              = t `nubstrengthen` rExtClass t (name <$> classAncestors g c)
                              | otherwise
                              = t `nubstrengthen` rExtIface t (name <$> interfaceAncestors g c)
    hierarchy t               = t

    name (QN AK_ _ _ s)       = s

    rExtClass t cs            = F.reft (vv t) (F.pAnd $ refa t "extends_class"     <$> cs)
    rExtIface t cs            = F.reft (vv t) (F.pAnd $ refa t "extends_interface" <$> cs)

    refa t s c                = F.PBexp $ F.EApp (sym s) [ F.expr $ rTypeValueVar t, F.expr $ F.symbolText c]
    vv                        = rTypeValueVar
    sym s                     = F.dummyLoc $ F.symbol s


----------------------------------------------------------------------------------------
nubstrengthen                   :: RefType -> F.Reft -> RefType
----------------------------------------------------------------------------------------
nubstrengthen (TPrim c r)  r' = TPrim c  $ r' `mappend` r
nubstrengthen (TRef n r)   r' = TRef n   $ r' `mappend` r
nubstrengthen (TObj ms r)  r' = TObj ms  $ r' `mappend` r
nubstrengthen (TVar α r)   r' = TVar α   $ r' `mappend` r
nubstrengthen (TFun a b r) r' = TFun a b $ r' `mappend` r
nubstrengthen t _             = t




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


-- | Monadic environment search wrappers

---------------------------------------------------------------------------------------
safeEnvFindTyM :: (EnvKey x, F.Expression x) => x -> CGEnv -> CGM RefType
---------------------------------------------------------------------------------------
safeEnvFindTyM x (envNames -> γ) | Just t <- envFindTy x γ
                                 = return t
                                 | otherwise
                                 = cgError $ bugEnvFindTy (srcPos x) x

---------------------------------------------------------------------------------------
safeEnvFindTyWithAsgnM :: EnvKey x => x -> CGEnv -> CGM CGEnvEntry
---------------------------------------------------------------------------------------
safeEnvFindTyWithAsgnM x g | Just t <- envFindTyWithAsgn x g
                           = return t
                           | otherwise
                           = cgError $ bugEnvFindTy (srcPos x) x


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
freshTyPhis :: AnnLq -> CGEnv -> [Id AnnLq] -> [Type] -> CGM (CGEnv, [RefType])
---------------------------------------------------------------------------------------
freshTyPhis l g xs τs
  = do ts <- mapM (freshTy "freshTyPhis") τs
       g' <- cgEnvAdds "freshTyPhis" (zip xs (VI WriteLocal Initialized <$> ts)) g
       _  <- mapM (wellFormed l g') ts
       return (g', ts)


-- | Instantiate Fresh Type (at Phi-site)
---------------------------------------------------------------------------------------
freshTyPhis' :: AnnLq -> CGEnv -> [Id AnnLq] -> [EnvEntry ()] -> CGM (CGEnv, [RefType])
---------------------------------------------------------------------------------------
freshTyPhis' l g xs es
  = do ts' <- mapM (freshTy "freshTyPhis") ts
       g'  <- cgEnvAdds "freshTyPhis" (zip xs (L.zipWith3 VI as is ts')) g
       _   <- mapM (wellFormed l g') ts'
       return (g', ts')
  where
    (as,is,ts) = L.unzip3 [ (a,i,t) | VI a i t <- es ]


-- | Fresh Object Type
---------------------------------------------------------------------------------------
freshTyObj :: (IsLocated l) => l -> CGEnv -> RefType -> CGM RefType
---------------------------------------------------------------------------------------
freshTyObj l g t = freshTy "freshTyArr" t >>= wellFormed l g


---------------------------------------------------------------------------------------
freshenCGEnvM :: CGEnv -> CGM CGEnv
---------------------------------------------------------------------------------------
freshenCGEnvM g
  = do  names <- envFromList  <$> mapM (go g) (envToList  $ cge_names g)
        cha   <- freshenCHA (envCHA g)
        return $ g { cge_names = names, cge_cha = cha }
  where
    go _ (x, VI a i v@TVar{}) = return (x, VI a i v)
    go g (x, VI ReadOnly i t) = (x,) . VI ReadOnly i <$> freshenType ReadOnly g (srcPos x) t
    go _ (x, VI a i t)        = return (x, VI a i t)

freshenCHA (CHA g n m)
  = do  m' <- qenvFromList <$> mapM (freshenModuleDefM g) (qenvToList m)
        return (CHA g n m')

freshenModuleDefM g (a, m)
  = do  vars  <- envFromList <$> mapM f (envToList $ m_variables m)
        types <- envFromList <$> mapM h (envToList $ m_types m)
        return (a,m { m_variables = vars, m_types = types })
  where
    f (x, VI Ambient i t) = (x,) . VI a i <$> freshenType ReadOnly g x t
    f kv = kv

    -- KVar class definitions only
    h (x, d@(TD s ms)) | sigKind s == ClassTDK
                       = (x,) . TD s <$> mapTypeMembersM (freshTyFun x) ms
                       | otherwise
                       = return (x, d)


---------------------------------------------------------------------------------------
-- | Adding Subtyping Constraints
---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------
subType :: AnnLq -> Error -> CGEnv -> RefType -> RefType -> CGM ()
---------------------------------------------------------------------------------------
subType l err g t1 t2 =
  do  -- t1'    <- addInvariant g t1  -- enhance LHS with invariants
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
  = do modify $ \st -> st { ws = (W g (ci err l) t) : ws st }
       return t
    where
       err = errorWellFormed (srcPos l)

--------------------------------------------------------------------------------
-- | Generating Fresh Values
--------------------------------------------------------------------------------

class Freshable a where
  fresh   :: CGM a
  true    :: a -> CGM a
  true    = return . id
  refresh :: a -> CGM a
  refresh = return . id

instance Freshable Integer where
  fresh = do modify $ \st -> st { cg_cnt = 1 + (cg_cnt st) }
             cg_cnt <$> get

instance Freshable F.Symbol where
  fresh = F.tempSymbol (F.symbol "nano") <$> fresh

instance Freshable String where
  fresh = F.symbolString <$> fresh

-- | Freshen up
freshTy :: RefTypable a => s -> a -> CGM RefType
freshTy _ τ = refresh $ rType τ

instance Freshable F.Refa where
  fresh = F.Refa . (`F.PKVar` mempty) . F.intKvar <$> fresh

instance Freshable [F.Refa] where
  fresh = single <$> fresh

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
splitC (Sub g i (TOr t1s) (TOr t2s))
  = concatMapM splitC (safeZipWith "splitc-3" (Sub g i) s1s s2s)
    where
      s1s = L.sortBy (compare `on` toType) t1s
      s2s = L.sortBy (compare `on` toType) t2s

-- -- TODO: Do the matching right
-- splitC (Sub g i t1@(TOr t1s _) t2)
--   | [t1] <- L.filter (on (==) toType t2) t1s
--   , t1s' <- L.filter (on (/=) toType t2) t1s
--   = (++) <$> splitC (Sub g i t1 t2) <*> concatMapM (splitIncompatC g i) t1s'
--   | otherwise
--   = splitIncompatC g i t1

-- |Type references
--
--  FIXME: restore co/contra-variance
--
splitC (Sub g i t1@(TRef (Gen x1 (m1:t1s)) r1) t2@(TRef (Gen x2 (m2:t2s)) r2))
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
  | x1 == x2 && isImm m2 && not (isArr t1)
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
splitC (Sub g i t1@(TPrim c1 _) t2@(TPrim c2 _))
  | c1 == c2  = bsplitC g i t1 t2
  | otherwise = splitIncompatC g i t1

-- | These need to be here due to the lack of a folding operation
--
splitC (Sub g i t1@(TRef _ _) t2) =
  case expandType Coercive g t1 of
    Just t1' -> splitC (Sub g i t1' t2)
    Nothing  -> cgError $ errorUnfoldType l t1 where l = srcPos i

splitC (Sub g i t1 t2@(TRef _ _)) =
  case expandType Coercive g t2 of
    Just t2' -> splitC (Sub g i t1 t2')
    Nothing  -> cgError $ errorUnfoldType l t2 where l = srcPos i

-- | TObj
--
splitC (Sub g i@(Ci _ l) t1@(TObj m1 r1) t2@(TObj m2 _))
  | F.isFalse (F.simplify r1)
  = return []
  | otherwise
  = do  cs     <- bsplitC g i t1 t2
        (x,g') <- cgEnvAddFresh "" l (VI Ambient Initialized t1) g
        cs'    <- splitMs g' (F.symbol x) i m1 m2
        return $ cs ++ cs'

splitC (Sub _ _ (TClass _) (TClass _)) = return []
splitC (Sub _ _ (TMod _) (TMod _)) = return []

splitC (Sub g i t1 _) = splitIncompatC g i t1

splitOC g i (Just t) (Just t') = splitC (Sub g i t t')
splitOC _ _ _        _         = return []

---------------------------------------------------------------------------------------
splitIncompatC :: CGEnv -> a -> RefType -> CGM [F.SubC a]
---------------------------------------------------------------------------------------
splitIncompatC g i t = bsplitC g i t (mkBot t)

---------------------------------------------------------------------------------------
mkBot :: (F.Reftable r) => RType r -> RType r
---------------------------------------------------------------------------------------
mkBot t = t `strengthen` F.bot (rTypeR t)


splitMs :: CGEnv -> F.Symbol -> Cinfo
        -> F.SEnv (FieldInfo F.Reft)
        -> F.SEnv (FieldInfo F.Reft)
        -> CGM [FixSubC]
splitMs g x c m1 m2
  = concatMap (\(f, (FI _ _ _, FI _ _ _)) -> undefined) match
  where
    -- diff21 = toListSEnv $ p2 `differenceSEnv` p1
    -- diff12 = toListSEnv $ p1 `differenceSEnv` p2
    match  = F.toListSEnv $ F.intersectWithSEnv (,) m1 m2



-- --  Gather the types of the LHS IMMUTABLE fields and add them to the
-- --  environment with the relevant field symbol as binder
-- ---------------------------------------------------------------------------------------
-- splitEs :: CGEnv
--         -> F.Symbol
--         -> Cinfo
--         -> TypeMembers F.Reft -> TypeMembers F.Reft
--         -> CGM [FixSubC]
-- ---------------------------------------------------------------------------------------
-- splitEs g x i@(Ci _ l) e1s e2s
--   = do  g' <- foldM step g imBs
--         concatMapM (uncurry $ splitE g' i) es
--   where
--     e1s'     = (substThis x <$>) <$> e1s
--     e2s'     = (substThis x <$>) <$> e2s
--     es       = M.elems $ M.intersectionWith (,) e1s' e2s'
--     step g p = snd <$> cgEnvAddFresh "" l (mkEntry p) g
--     imBs     = [ (f,t) | (FieldSig f _ m t, _) <- es
--                        , isImmutable m, f /= protoSym ]
--     mkEntry (f,t) = (t `eSingleton` mkOffset x f, ReadOnly, Initialized)
--
--
-- splitE g i (CallSig t1) (CallSig t2) = splitC (Sub g i t1 t2)
-- splitE g i (ConsSig t1) (ConsSig t2) = splitC (Sub g i t1 t2)
--
-- splitE g i (IndexSig _ _ t1) (IndexSig _ _ t2)
--   = do  cs    <- splitC (Sub g i t1 t2)
--         cs'   <- splitC (Sub g i t2 t1)
--         return $ cs ++ cs'
--
-- splitE g i (FieldSig _ _ μf1 t1) (FieldSig _ _ μf2 t2)
--   = splitWithMut g i (μf1,t1) (μf2,t2)
--
-- splitE g i (MethSig _ t1) (MethSig _ t2)
--   = splitC (Sub g i t1 t2)
--
-- splitE _ _ _ _ = return []
--
--
-- splitWithMut g i (_,t1) (μf2,t2)
--   | isImmutable μf2
--   = splitC (Sub g i t1 t2)
--   | otherwise
--   = (++) <$> splitC (Sub g i t1 t2)
--          <*> splitC (Sub g i t2 t1)
--
--
-- -- splitMaybe g i (Just t1) (Just t2) = splitC (Sub g i t1 t2)
-- -- splitMaybe _ _ _         _         = return []

---------------------------------------------------------------------------------------
bsplitC :: CGEnv -> a -> RefType -> RefType -> CGM [F.SubC a]
---------------------------------------------------------------------------------------
-- NOTE: addInvariant nonly needed in LHS
bsplitC g ci t1 t2 = bsplitC' g ci <$> addInvariant g t1 <*> return t2

bsplitC' g ci t1 t2
  | F.isFunctionSortedReft r1 && F.isNonTrivial r2
  -- = F.subC (cge_fenv g) F.PTrue (r1 {F.sr_reft = typeofReft t1}) r2 Nothing [] ci
  = F.subC bs p (r1 {F.sr_reft = typeofReft t1}) r2 Nothing [] ci
  | F.isNonTrivial r2
  = F.subC bs p r1 r2 Nothing [] ci
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
                                  (F.expr $ F.symbolText s)
    vv             = rTypeValueVar

instance PP (F.SortedReft) where
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

splitW (W g i t@(TPrim _ _))
  =  do let ws = bsplitW g t i
        -- ws'   <- concatMapM splitW [W g i ti | ti <- ts]
        return $ ws -- ++ ws'

splitW (W g i t@(TRef _ ts _))
  =  do let ws = bsplitW g t i
        ws'   <- concatMapM splitW [W g i ti | ti <- ts]
        return $ ws ++ ws'

splitW (W g i (TAnd ts))
  = concatMapM splitW [W g i t | t <- ts]

splitW (W g i t@(TObj ms _))
  = do let bws = bsplitW g t i
       -- FIXME: add field bindings in g?
       ws     <- concatMapM splitW [ W g i t | t <- typesOfTM ms ]
       return  $ bws ++ ws

splitW (W _ _ (TClass _ ))
  = return []

splitW (W _ _ (TMod _ ))
  = return []

splitW (W _ _ t) = error $ render $ text "Not supported in splitW: " <+> pp t

bsplitW g t i
  | F.isNonTrivial r'
  = [F.wfC bs r' Nothing i]
  | otherwise
  = []
  where
    r' = rTypeSortedReft t
    bs = foldl F.unionIBindEnv F.emptyIBindEnv
       $ snd <$> F.toListSEnv (cge_fenv g)


envTyAdds msg l xts g
  = cgEnvAdds (msg ++ " - envTyAdds " ++ ppshow (srcPos l))
      [(symbolId l x, VI WriteLocal Initialized t) | B x t <- xts] g
    where
      symbolId l x = Id l $ F.symbolString $ F.symbol x



------------------------------------------------------------------------------
cgFunTys :: (IsLocated l, F.Symbolic b, PP x, PP [b])
         => l
         -> x
         -> [b]
         -> RefType
         -> CGM [(Int, ([TVar], Maybe (RefType), [RefType], RefType))]
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


-- --------------------------------------------------------------------------------
-- -- | zipType wrapper
-- --
-- --   FIXME: What is the purpose of this substitution ???
-- --
-- zipTypeUpM g x t1 t2
--   | Just (f, (F.Reft (s,ras))) <- zipType g x t1 t2
--   = let su  = F.mkSubst [(s, F.expr x)] in
--     let rs  = F.simplify $ F.Reft (s, F.subst su ras) `F.meet` F.uexprReft x in
--     return  $ f rs
--   | otherwise
--   = cgError $ bugZipType (srcPos x) t1 t2
--
-- zipTypeDownM g x t1 t2
--   | Just (f, r) <- zipType g x t1 t2
--   = return $ f r
--   | otherwise
--   = cgError $ bugZipType (srcPos x) t1 t2

-- Local Variables:
-- flycheck-disabled-checkers: (haskell-liquid)
-- End:
