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

module Language.Rsc.Liquid.CGMonad (

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

  -- * Freshable
  , Freshable (..)

  -- * Environment API
  , cgEnvAddFresh
  , cgEnvAdds

  , resolveTypeM

  , cgSafeEnvFindTyM

  , envAddGuard, envPopGuard
  , envFindTyWithAsgn
  , envPushContext, envGetContextCast, envGetContextTypArgs

  -- * Add Subtyping Constraints
  , subType, wellFormed -- , safeExtends

  -- * Add Type Annotations
  , addAnnot

  -- * Function Types
  , cgFunTys, subNoCapture

  -- * Zip type wrapper
  -- , zipTypeUpM, zipTypeDownM

  , unqualifyThis

  ) where

import           Control.Applicative
import           Control.Exception               (throw)
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Except
import           Data.Function                   (on)
import qualified Data.HashMap.Strict             as HM
import qualified Data.List                       as L
import           Data.Maybe                      (catMaybes)
import           Data.Monoid                     (mappend, mempty)
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Types         as F
import           Language.Rsc.Annotations
import           Language.Rsc.AST
import           Language.Rsc.ClassHierarchy
import           Language.Rsc.CmdLine
import           Language.Rsc.Core.Env
import           Language.Rsc.Environment
import           Language.Rsc.Errors
import           Language.Rsc.Liquid.Constraint
import           Language.Rsc.Liquid.Environment
import           Language.Rsc.Liquid.Types
import           Language.Rsc.Locations
import           Language.Rsc.Names
import           Language.Rsc.Pretty
import           Language.Rsc.Program
import qualified Language.Rsc.SystemUtils        as S
import           Language.Rsc.Transformations
import           Language.Rsc.Typecheck.Sub
import           Language.Rsc.Typecheck.Subst
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
import           Text.PrettyPrint.HughesPJ

import           Debug.Trace                     (trace)

--------------------------------------------------------------------------------
-- | Top level type returned after Constraint Generation
--------------------------------------------------------------------------------

data CGInfo = CGI { cgi_finfo :: F.FInfo Cinfo
                  , cgi_annot :: S.UAnnInfo RefType
                  }

-- Dump the refinement subtyping constraints
instance PP CGInfo where
  pp (CGI finfo _) = cat (map pp (HM.elems $ F.cm finfo))


--------------------------------------------------------------------------------
-- | Constraint Generation Monad
--------------------------------------------------------------------------------

data CGState = CGS {
  --
  -- ^ global list of fixpoint binders
  --
    cg_binds   :: F.BindEnv
  --
  -- ^ subtyping constraints
  --
  , cg_cs      :: ![SubC]
  --
  -- ^ well-formedness constraints
  --
  , cg_ws      :: ![WfC]
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

--------------------------------------------------------------------------------
getCGInfo :: Config -> RefScript -> CGM a -> CGInfo
--------------------------------------------------------------------------------
getCGInfo cfg pgm = cgStateCInfo pgm . execute cfg pgm . (>> fixCWs)
  where
    fixCWs       = (,) <$> fixCs <*> fixWs
    fixCs        = get >>= concatMapM splitC . cg_cs
    fixWs        = get >>= concatMapM splitW . cg_ws

--------------------------------------------------------------------------------
execute :: Config -> RefScript -> CGM a -> (a, CGState)
--------------------------------------------------------------------------------
execute cfg pgm act
  = case runState (runExceptT act) $ initState cfg pgm of
      (Left e, _)   -> throw e
      (Right x, st) -> (x, st)

--------------------------------------------------------------------------------
initState :: Config -> RefScript -> CGState
--------------------------------------------------------------------------------
initState c p = CGS F.emptyBindEnv [] [] 0 mempty invars c (maxId p)
  where
    invars    = HM.fromList [(pr, t) | t@(Loc _ (TPrim pr _)) <- invts p]

--------------------------------------------------------------------------------
cgStateCInfo :: RefScript -> (([F.SubC Cinfo], [F.WfC Cinfo]), CGState) -> CGInfo
--------------------------------------------------------------------------------
cgStateCInfo pgm ((fcs, fws), cg) = CGI (patchSymLits fi) (cg_ann cg)
  where
    fi   = F.FI { F.cm       = HM.fromList $ F.addIds fcs
                , F.ws       = fws
                , F.bs       = cg_binds cg
                , F.gs       = measureEnv pgm
                , F.lits     = []
                , F.kuts     = F.ksEmpty
                , F.quals    = pQuals pgm
                , F.bindInfo = mempty
                }

patchSymLits fi = fi { F.lits = F.symConstLits fi ++ F.lits fi }


-- | Get binding from object type

--------------------------------------------------------------------------------
measureEnv :: RefScript -> F.SEnv F.SortedReft
--------------------------------------------------------------------------------
measureEnv = fmap rTypeSortedReft . envSEnv . consts

--------------------------------------------------------------------------------
cgError     :: Error -> CGM b
--------------------------------------------------------------------------------
cgError err = throwE $ catMessage err "CG-ERROR\n"

--------------------------------------------------------------------------------
-- | Environment API
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
envPushContext    :: (CallSite a) => a -> CGEnv -> CGEnv
envGetContextCast :: CGEnv -> AnnLq -> Cast F.Reft
--------------------------------------------------------------------------------
envPushContext c g = g { cge_ctx = pushContext c (cge_ctx g) }

envGetContextCast g a
  = case [(errs, t) | CDead errs t <- [ c | TCast cx c <- fFact a, cx == cge_ctx g]] of
      [ ] -> CNo
      cs  -> let (errs', ts) = unzip cs in
             CDead (concat errs') (mkUnion ts)

--------------------------------------------------------------------------------
envGetContextTypArgs :: Int -> CGEnv -> AnnLq -> [TVar] -> [RefType]
--------------------------------------------------------------------------------
-- NOTE: If we do not need to instantiate any type parameter (i.e. length αs ==
-- 0), DO NOT attempt to compare that with the TypInst that might hide within
-- the expression, cause those type instantiations might serve anothor reason
-- (i.e. might be there for a separate instantiation).
envGetContextTypArgs _ _ _ [] = []
envGetContextTypArgs n g a αs
  | [i] <- tys, length i == length αs = i
  | otherwise = die $ bugMissingTypeArgs $ srcPos a
  where
    tys = [i | TypInst m ξ' i <- fFact a
             , ξ' == cge_ctx g
             , n == m ]


-- | Monadic environment search wrappers

--------------------------------------------------------------------------------
cgSafeEnvFindTyM :: (EnvKey x, F.Expression x) => x -> CGEnv -> CGM RefType
--------------------------------------------------------------------------------
cgSafeEnvFindTyM x (envNames -> γ)
  | Just t <- envFindTy x γ
  = return $ v_type t
  | otherwise
  = cgError $ bugEnvFindTy (srcPos x) x

--------------------------------------------------------------------------------
cgEnvAddFresh :: IsLocated l => String -> l -> VarInfo F.Reft -> CGEnv -> CGM (Id AnnLq, CGEnv)
--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
cgEnvAdds :: EnvKey x => String -> [(x, CGEnvEntry)] -> CGEnv -> CGM CGEnv
--------------------------------------------------------------------------------
cgEnvAdds msg xts g = foldM (envAddGroup msg ks) g es
  where
    es = objFields g <$> xts
    ks = F.symbol . fst <$> xts

--------------------------------------------------------------------------------
envAddGroup :: String -> [F.Symbol] -> CGEnv
            -> (F.Symbol, [(F.Symbol, CGEnvEntry)]) -> CGM CGEnv
--------------------------------------------------------------------------------
envAddGroup msg ks g (x, xts)
  = do  mapM_ cgError $ concat $ zipWith (checkSyms msg g ks) xs ts
        es    <- L.zipWith4 VI ls as is <$> zipWithM inv is ts
        ids   <- toIBindEnv . catMaybes <$> zipWithM addFixpointBind xs es
        return $ g { cge_names = envAdds (zip xs es) $ cge_names g
                   , cge_fenv  = F.insertSEnv x ids  $ cge_fenv  g }
  where
    (xs,ls,as,is,ts) = L.unzip5 [(x,loc,a,i,t) | (x, VI loc a i t) <- xts ]
    inv Initialized  = addInvariant g
    inv _            = return
    toIBindEnv       = (`F.insertsIBindEnv` F.emptyIBindEnv)

-------------------------------------------------------------------------------
resolveTypeM :: IsLocated a => a -> CGEnv -> AbsName -> CGM (TypeDecl F.Reft)
-------------------------------------------------------------------------------
resolveTypeM l γ x
  | Just t <- resolveTypeInEnv γ x = return t
  | otherwise = die $ bugClassDefNotFound (srcPos l) x


-- | Bindings for IMMUTABLE fields
--------------------------------------------------------------------------------
objFields :: EnvKey x => CGEnv -> (x, CGEnvEntry) -> (F.Symbol, [(F.Symbol,CGEnvEntry)])
--------------------------------------------------------------------------------
objFields g (x, e@(VI loc a _ t))
              | a `elem` [WriteGlobal,ReturnVar] = (x_sym, [(x_sym,e)])
              | otherwise = (x_sym, (x_sym,e):xts)
  where
    x_sym     = F.symbol x
    xts       = [(mkQualSym x f, vi f o ft) | (f, FI o m ft) <- fs, isImm m ]

    -- This should remain as is: x.f bindings are not going to change
    vi f Opt tf = VI loc a Initialized $ orUndef $ ty tf f
    vi f Req tf = VI loc a Initialized $           ty tf f
    -- v = offset(x,"f")
    ty t f = substThis x t `eSingleton` mkOffset x f

    fs | Just (TObj ms _) <- expandType Coercive (envCHA g) t
       = F.toListSEnv (tm_prop ms)
       | otherwise = []

--------------------------------------------------------------------------------
addFixpointBind :: EnvKey x => x -> CGEnvEntry -> CGM (Maybe F.BindId)
--------------------------------------------------------------------------------
-- No binding for globals or RetVal: shouldn't appear in refinements
addFixpointBind _ (VI _ WriteGlobal _ _) = return Nothing
addFixpointBind _ (VI _ ReturnVar _ _) = return Nothing
addFixpointBind x (VI _ _ _ t)
  = do  bs           <- cg_binds <$> get
        let (i, bs')  = F.insertBindEnv (F.symbol x) r bs
        modify        $ \st -> st { cg_binds = bs' }
        return        $ Just i
  where
       r              = rTypeSortedReft t

--------------------------------------------------------------------------------
addInvariant :: CGEnv -> RefType -> CGM RefType
--------------------------------------------------------------------------------
addInvariant g t
  = do  extraInvariants <- extraInvs <$> cg_opts <$> get
        if extraInvariants then (hasProp . hierarchy . truthy . typeof t . invs) <$> get
                           else (          hierarchy . truthy . typeof t . invs) <$> get
  where
    cha = envCHA g
    -- | typeof
    typeof t@(TPrim p o)    i = maybe t (strengthenOp t o . rTypeReft . val) $ HM.lookup p i
    typeof t@(TRef _ _)     _ = t `strengthen` F.reft (vv t) (typeofExpr $ F.symbol "object")
    typeof t@(TObj _ _)     _ = t `strengthen` F.reft (vv t) (typeofExpr $ F.symbol "object")
    typeof   (TFun a b _)   _ = TFun a b typeofReft
    typeof t                _ = t
    -- | Truthy
    truthy t               | maybeTObj t
                           = t `strengthen` F.reft (vv t) (F.eProp $ vv t)
                           | otherwise          = t

    strengthenOp t o r     | r `L.elem` ofRef o = t
                           | otherwise          = t `strengthen` r
    typeofReft             = F.reft  (vv t) $ F.pAnd [ typeofExpr $ F.symbol "function"
                                                     , F.eProp    $ vv t                ]
    typeofExpr s           = F.PAtom F.Eq (F.EApp (F.dummyLoc (F.symbol "ttag")) [F.eVar $ vv t])
                                          (F.expr $ F.symbolText s)

    ofRef (F.Reft (s, ra)) = F.reft s <$> F.raConjuncts ra

    -- | { f: T } --> hasProperty("f", v)
    hasProp ty             = t `strengthen` keyReft (boundKeys cha ty)
    keyReft ks             = F.reft (vv t) $ F.pAnd (F.PBexp . hasPropExpr <$> ks)
    hasPropExpr s          = F.EApp (F.dummyLoc (F.symbol "hasProperty"))
                                [F.expr (F.symbolText s), F.eVar $ vv t]

    -- | extends class / interface
    hierarchy t@(TRef c _)
      | isClassType cha t
      = t `strengthen` rExtClass t (name <$> classAncestors cha (g_name c))
      | otherwise
      = t `strengthen` rExtIface t (name <$> interfaceAncestors cha (g_name c))
    hierarchy t = t

    name (QN _ s) = s

    rExtClass t cs = F.reft (vv t) (F.pAnd $ refa t "extends_class"     <$> cs)
    rExtIface t cs = F.reft (vv t) (F.pAnd $ refa t "extends_interface" <$> cs)

    refa t s c     = F.PBexp $ F.EApp (sym s) [ F.expr $ rTypeValueVar t, F.expr $ F.symbolText c]
    vv             = rTypeValueVar
    sym s          = F.dummyLoc $ F.symbol s





-- IN FIXPOINT meetReft (F.Reft (v, ras)) (F.Reft (v', ras'))
-- IN FIXPOINT   | v == v'            = F.Reft (v , L.nubBy cmp $ ras  ++ ras')
-- IN FIXPOINT   | v == F.dummySymbol = F.Reft (v', L.nubBy cmp $ ras' ++ (ras `F.subst1`  (v , F.EVar v')))
-- IN FIXPOINT   | otherwise          = F.Reft (v , L.nubBy cmp $ ras  ++ (ras' `F.subst1` (v', F.EVar v )))
-- IN FIXPOINT   where
-- IN FIXPOINT     cmp = (==) `on` (show . F.toFix)




--------------------------------------------------------------------------------
addAnnot       :: (IsLocated l, F.Symbolic x) => l -> x -> RefType -> CGM ()
--------------------------------------------------------------------------------
addAnnot l x t = modify $ \st -> st {cg_ann = S.addAnnot (srcPos l) x t (cg_ann st)}

--------------------------------------------------------------------------------
envAddGuard       :: (F.Symbolic x, IsLocated x) => x -> Bool -> CGEnv -> CGEnv
--------------------------------------------------------------------------------
envAddGuard x b g = g { cge_guards = guard b x : cge_guards g }
  where
    guard True    = F.eProp
    guard False   = F.PNot . F.eProp

--------------------------------------------------------------------------------
envPopGuard       :: CGEnv -> CGEnv
--------------------------------------------------------------------------------
envPopGuard g = g { cge_guards = grdPop $ cge_guards g }
  where
    grdPop (_:xs) = xs
    grdPop []     = []


instance F.Expression a => F.Expression (Located a) where
  expr (Loc _ a) = F.expr a

instance F.Expression TVar where
  expr (TV a _) = F.expr a


-- IN FIXPOINT meetReft (F.Reft (v, ras)) (F.Reft (v', ras'))
-- IN FIXPOINT   | v == v'            = F.Reft (v , L.nubBy cmp $ ras  ++ ras')
-- IN FIXPOINT   | v == F.dummySymbol = F.Reft (v', L.nubBy cmp $ ras' ++ (ras `F.subst1`  (v , F.EVar v')))
-- IN FIXPOINT   | otherwise          = F.Reft (v , L.nubBy cmp $ ras  ++ (ras' `F.subst1` (v', F.EVar v )))
-- IN FIXPOINT   where
-- IN FIXPOINT     cmp = (==) `on` (show . F.toFix)


--------------------------------------------------------------------------------
-- | Fresh Templates
--------------------------------------------------------------------------------

-- | Instantiate Fresh Type (at Function-site)
--------------------------------------------------------------------------------
freshTyFun :: (IsLocated l) => CGEnv -> l -> RefType -> CGM RefType
--------------------------------------------------------------------------------
freshTyFun g l t
  | not (isTFun t)     = return t
  | isTrivialRefType t = freshTy "freshTyFun" (toType t) >>= wellFormed l g
  | otherwise          = return t

freshenVI _ _ v@(VI Exported _ _ _)
                       = return v
freshenVI g l v@(VI loc a@WriteGlobal i t)
  | isTrivialRefType t = freshTy "freshenVI" (toType t)
                     >>= (VI loc a i <$>) . wellFormed l g
  | otherwise          = return v

freshenVI g l v@(VI loc a i t)
  | not (isTFun t)     = return v
  | isTrivialRefType t = freshTy "freshenVI" (toType t)
                     >>= (VI loc a i <$>) . wellFormed l g
  | otherwise          = return v

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
--------------------------------------------------------------------------------
freshTyPhis :: AnnLq -> CGEnv -> [Id AnnLq] -> [Type] -> CGM (CGEnv, [RefType])
--------------------------------------------------------------------------------
freshTyPhis l g xs τs
  = do ts <- mapM (freshTy "freshTyPhis") τs
       g' <- cgEnvAdds "freshTyPhis" (zip xs (VI Local WriteLocal Initialized <$> ts)) g
       _  <- mapM (wellFormed l g') ts
       return (g', ts)


-- | Instantiate Fresh Type (at Phi-site)
--------------------------------------------------------------------------------
freshTyPhis' :: AnnLq -> CGEnv -> [Id AnnLq] -> [EnvEntry ()] -> CGM (CGEnv, [RefType])
--------------------------------------------------------------------------------
freshTyPhis' l g xs es
  = do ts' <- mapM (freshTy "freshTyPhis") ts
       g'  <- cgEnvAdds "freshTyPhis" (zip xs (L.zipWith4 VI ls as is ts')) g
       _   <- mapM (wellFormed l g') ts'
       return (g', ts')
  where
    (ls,as,is,ts) = L.unzip4 [ (l,a,i,t) | VI l a i t <- es ]


-- | Fresh Object Type
--------------------------------------------------------------------------------
freshTyObj :: (IsLocated l) => l -> CGEnv -> RefType -> CGM RefType
--------------------------------------------------------------------------------
freshTyObj l g t = freshTy "freshTyArr" t >>= wellFormed l g

--------------------------------------------------------------------------------
freshenCGEnvM :: CGEnv -> CGM CGEnv
--------------------------------------------------------------------------------
freshenCGEnvM g@(CGE { cge_names = nms, cge_cha = CHA gr n m })
  = do  nms'  <- envFromList <$> mapM go (envToList nms)
        cha'  <- CHA gr n <$> qenvFromList
                          <$> freshenModuleDefM g `mapM` qenvToList m
        return $ g { cge_names = nms'
                   , cge_cha   = cha' }
  where
    go (x, VI loc a i v@TVar{}) = return (x, VI loc a i v)
    go (x, v) = (x,) <$> freshenVI g (srcPos x) v

--------------------------------------------------------------------------------
freshenModuleDefM :: CGEnv -> (AbsPath, ModuleDef F.Reft) -> CGM (AbsPath, ModuleDef F.Reft)
--------------------------------------------------------------------------------
freshenModuleDefM g (a, m)
  = do  vars  <- envFromList <$> mapM goV (envToList $ m_variables m)
        types <- envFromList <$> mapM goT (envToList $ m_types m)
        return (a, m { m_variables = vars, m_types = types })
  where
    -- XXX: is this right?
    goV (x, v) = (x,) <$> freshenVI g x v

    -- KVar class definitions only
    goT (x, d@(TD s ms)) | sigKind s == ClassTDK
                         = (x,) . TD s <$> mapTypeMembersM (freshTyFun g x) ms
                         | otherwise
                         = return (x, d)


--------------------------------------------------------------------------------
-- | Adding Subtyping Constraints
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
subType :: AnnLq -> Error -> CGEnv -> RefType -> RefType -> CGM ()
--------------------------------------------------------------------------------
subType l err g t1 t2 =
  do  -- t1'    <- addInvariant g t1  -- enhance LHS with invariants
      modify $ \st -> st { cg_cs = Sub g (ci err l) t1 t2 : cg_cs st }


-- TODO: Restore this check !!!
-- --------------------------------------------------------------------------------
-- safeExtends :: SrcSpan -> CGEnv -> IfaceDef F.Reft -> CGM ()
-- --------------------------------------------------------------------------------
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
  = do modify $ \st -> st { cg_ws = (W g (ci err l) t) : cg_ws st }
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
  fresh = F.tempSymbol (F.symbol "rsc") <$> fresh

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
splitC (Sub g i tf1@(TFun xt1s t1 _) tf2@(TFun xt2s t2 _))
  = do bcs       <- bsplitC g i tf1 tf2
       g'        <- envTyAdds "splitC" i xt2s g
       cs        <- concatMapM splitC $ zipWith (Sub g' i) t2s t1s'
       cs'       <- splitC $ Sub g' i (F.subst su t1) t2
       return     $ bcs ++ cs ++ cs'
    where
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
    θ   = fromList [(btvToTV α2, btVar α1 :: RefType)]
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
splitC (Sub g c t1 t2)
  | any isTUnion [t1, t2]
  = do  mCs     <- concatMapM (\(_,t,t') -> splitC (Sub g c t t')) mts
        nCs     <- concatMapM (splitIncompatC g c) t1s'
        return   $ mCs ++ nCs
    where
      (t1s, t2s) = mapPair bkUnion (t1, t2)
      it1s       = ([0..]::[Int]) `zip` t1s
      mts        = [ (i, τ1, τ2) | (i, τ1) <- it1s, τ2 <- t2s, τ1 ~~ τ2 ]
      t1s'       = [ τ1          | (i, τ1) <- it1s, i `notElem` map fst3 mts ]
      (~~)       = on (isConvertible (void g)) toType

-- | Type references
--
--  TODO: restore co/contra-variance
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
  | x1 == x2 && isImm m2 && not (isArrayType t1)
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

splitC (Sub g i t1@(TPrim c1 _) t2@(TPrim c2 _))
  | c1 == c2  = bsplitC g i t1 t2
  | otherwise = splitIncompatC g i t1

-- | TObj
--
splitC (Sub g i@(Ci _ l) t1@(TObj m1 r1) t2@(TObj m2 _))
  | F.isFalse (F.simplify r1)
  = return []
  | otherwise
  = do  cs     <- bsplitC g i t1 t2
        (x,g') <- cgEnvAddFresh "" l (VI Local Ambient Initialized t1) g
        cs'    <- splitTM g' (F.symbol x) i m1 m2
        return $ cs ++ cs'

-- | Object-like: after expansion should fall under previous case
--
splitC (Sub g i t1 t2)
  | all maybeTObj [t1, t2]
  = case (expandType NonCoercive (envCHA g) t1, expandType NonCoercive (envCHA g) t2) of
      (Just t1', Just t2') -> splitC (Sub g i t1' t2')
      _ -> cgError $ errorUnfoldTypes l t1 t2 where l = srcPos i

splitC (Sub g i t1 _) = splitIncompatC g i t1

--------------------------------------------------------------------------------
splitIncompatC :: CGEnv -> a -> RefType -> CGM [F.SubC a]
--------------------------------------------------------------------------------
splitIncompatC g i t = bsplitC g i t (mkBot t)

--------------------------------------------------------------------------------
mkBot :: (F.Reftable r) => RType r -> RType r
--------------------------------------------------------------------------------
mkBot t = t `strengthen` F.bot (rTypeR t)

-- TODO: add symbol x in env
--------------------------------------------------------------------------------
splitTM :: CGEnv -> F.Symbol -> Cinfo
        -> TypeMembers F.Reft -> TypeMembers F.Reft
        -> CGM [FixSubC]
--------------------------------------------------------------------------------
splitTM g x c (TM p1 m1 sp1 sm1 c1 k1 s1 n1) (TM p2 m2 sp2 sm2 c2 k2 s2 n2)
  = concatMapM (splitF g c) (ps ++ sps) +++
    concatMapM (splitM g c) (ms ++ sms) +++
    concatMapM splitT (cs ++ ks ++ ss ++ ns)
  where
    (+++) = liftM2 (++)
    ps  = F.toListSEnv $ F.intersectWithSEnv (,) p1 p2
    ms  = F.toListSEnv $ F.intersectWithSEnv (,) m1 m2
    sps = F.toListSEnv $ F.intersectWithSEnv (,) sp1 sp2
    sms = F.toListSEnv $ F.intersectWithSEnv (,) sm1 sm2
    cs  = [ (t1,t2) | Just t1 <- [c1], Just t2 <- [c2] ]
    ks  = [ (t1,t2) | Just t1 <- [k1], Just t2 <- [k2] ]
    ss  = [ (t1,t2) | Just t1 <- [s1], Just t2 <- [s2] ]
    ns  = [ (t1,t2) | Just t1 <- [n1], Just t2 <- [n2] ]
    splitT (t1,t2) = splitC (Sub g c t1 t2)

splitF g i (_, (FI _ m1 t1, FI _ m2 t2))
  | isImm m2  = splitC (Sub g i t1 t2)
  | otherwise = (++) <$> splitC (Sub g i t1 t2)
                     <*> splitC (Sub g i t2 t1)

splitM g i (_, (MI _ _ t1, MI _ _ t2))
  = splitC (Sub g i t1 t2)


--------------------------------------------------------------------------------
bsplitC :: CGEnv -> a -> RefType -> RefType -> CGM [F.SubC a]
--------------------------------------------------------------------------------
-- NOTE: addInvariant only needed in LHS
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
    bs             = foldl F.unionIBindEnv F.emptyIBindEnv $ snd <$> F.toListSEnv (cge_fenv g)
    p              = F.pAnd $ cge_guards g
    (r1,r2)        = (rTypeSortedReft t1, rTypeSortedReft t2)
    typeofReft t   = F.reft (vv t) $ F.pAnd [ typeofExpr (F.symbol "function") t
                                            , F.eProp $ vv t ]
    typeofExpr s t = F.PAtom F.Eq (F.EApp (F.dummyLoc (F.symbol "ttag")) [F.eVar $ vv t])
                                  (F.expr $ F.symbolText s)
    vv             = rTypeValueVar


--------------------------------------------------------------------------------
-- | Splitting Well-Formedness Constraints
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
splitW :: WfC -> CGM [FixWfC]
--------------------------------------------------------------------------------
splitW (W g i ft@(TFun ts t _))
  = do let bws = bsplitW g ft i
       g'    <- envTyAdds "splitW" i ts g
       ws    <- concatMapM splitW [W g' i ti | B _ ti <- ts]
       ws'   <-            splitW (W g' i t)
       return  $ bws ++ ws ++ ws'

splitW (W g i (TAll _ t))
  = splitW (W g i t)

splitW (W g i t@(TVar _ _))
  = return $ bsplitW g t i

splitW (W g i t@(TPrim _ _))
  =  return $ bsplitW g t i

splitW (W g i t@(TRef (Gen _ ts) _))
  =  do let ws = bsplitW g t i
        ws'   <- concatMapM splitW [W g i ti | ti <- ts]
        return $ ws ++ ws'

splitW (W g i (TAnd ts))
  = concatMapM splitW [W g i t | t <- ts]

splitW (W g i t@(TObj ms _))
  = do let bws = bsplitW g t i
       -- TODO: add field bindings in g?
       ws     <- concatMapM splitW [ W g i t' | t' <- typesOfTM ms ]
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
    bs = foldl F.unionIBindEnv F.emptyIBindEnv $ snd <$> F.toListSEnv (cge_fenv g)


envTyAdds msg l xts g
  = cgEnvAdds (msg ++ " - envTyAdds " ++ ppshow (srcPos l))
      [(symbolId l x, VI Local WriteLocal Initialized t) | B x t <- xts] g
    where
      symbolId l x = Id l $ F.symbolString $ F.symbol x



--------------------------------------------------------------------------------
cgFunTys :: (IsLocated l, F.Symbolic b, PP x, PP [b])
         => l -> x -> [b] -> RefType -> CGM [(Int, ([BTVar F.Reft], [Bind F.Reft], RefType))]
--------------------------------------------------------------------------------
cgFunTys l f xs ft   | Just ts <- bkFuns ft
                     = zip ([0..] :: [Int]) <$> mapM fTy ts
                     | otherwise
                     = cgError $ errorNonFunction (srcPos l) f ft
  where
    fTy (αs, yts, t) | Just yts' <- padUndefineds xs yts
                     = uncurry (αs,,) <$> subNoCapture l yts' xs t
                     | otherwise
                     = cgError $ errorArgMismatch (srcPos l)

-- | Avoids capture of function binders by existing value variables
--------------------------------------------------------------------------------
subNoCapture :: F.Symbolic a => t -> [Bind F.Reft] -> [a] -> RefType -> CGM ([Bind F.Reft], RefType)
--------------------------------------------------------------------------------
subNoCapture _ yts xs t   | length yts /= length xs
                          = error "subNoCapture - length test failed"
                          | otherwise
                          = (,) <$> mapM (\(B s t) -> B s <$> mapReftM ff t) yts
                                <*> mapReftM ff t
  where
    -- If the symbols we are about to introduce are already capture by some
    -- value variable, then (1) create a fresh value var. and (2) proceed with
    -- the substitution after replacing with the new val. var.
    ff r@(F.Reft (v,ras)) | v `L.elem` xss
                          = do v' <- freshVV
                               return $ F.subst su $ F.Reft . (v',)
                                      $ F.subst (F.mkSubst [(v, F.expr v')]) ras
                          | otherwise
                          = return $ F.subst su r
    freshVV               = F.vv . Just <$> fresh
    xss                   = F.symbol <$> xs
    su                    = F.mkSubst $ safeZipWith "subNoCapture" fSub yts xs
    fSub yt x             = (b_sym yt, F.eVar x)


-- | Substitute occurences of 'this.f' in type @t'@, with 'f'
--------------------------------------------------------------------------------
unqualifyThis :: CGEnv -> RefType -> RefType -> RefType
--------------------------------------------------------------------------------
unqualifyThis g t = F.subst $ F.mkSubst fieldSu
  where
    fieldSu | Just (TObj fs _) <- expandType Coercive (envCHA g) t
            = [ subPair f | (f, FI _ m _) <- F.toListSEnv $ tm_prop fs, isImm m ]
            | otherwise
            = []
    this      = F.symbol $ builtinOpId BIThis
    qFld x f  = F.qualifySymbol (F.symbol x) f
    subPair f = (qFld this f, F.expr f)


-- --------------------------------------------------------------------------------
-- -- | zipType wrapper
-- --
-- --   TODO: What is the purpose of this substitution ???
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
