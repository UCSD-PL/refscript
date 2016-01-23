{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DoAndIfThenElse           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
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
  , envPushContext

  , Cast(..), envGetContextCast, envGetContextTypArgs

  -- * Add Subtyping Constraints
  , subType, wellFormed -- , safeExtends

  -- * Add Type Annotations
  , addAnnot

  -- * Function Types
  , cgFunTys, substNoCapture

  -- * Type narrowing
  , narrowType

  , unqualifyThis

  ) where

import           Control.Applicative             hiding (empty)
import           Control.Arrow                   ((***))
import           Control.Exception               (throw)
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Except
import           Data.Function                   (on)
import qualified Data.HashMap.Strict             as HM
import qualified Data.List                       as L
import           Data.Maybe                      (catMaybes)
import           Data.Monoid                     (mappend, mempty)
import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Types         as F
import           Language.Fixpoint.Types.Errors
import           Language.Fixpoint.Types.Names   (isPrefixOfSym, symbolString, symbolText, tempPrefix)
import           Language.Fixpoint.Types.Visitor (SymConsts (..))
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
import           Language.Rsc.Misc               (concatMapM, mapPair, single)
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

-------------------------------------------------------------------------------
getCGInfo :: Config -> FilePath -> RefScript -> CGM a -> CGInfo
-------------------------------------------------------------------------------
getCGInfo cfg f p = cgStateCInfo f p . execute cfg p . (>> fixCWs)
  where
    fixCWs        = (,) <$> fixCs <*> fixWs
    fixCs         = get >>= concatMapM splitC . cg_cs
    fixWs         = get >>= concatMapM splitW . cg_ws


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
cgStateCInfo :: FilePath -> RefScript -> (([F.SubC Cinfo], [F.WfC Cinfo]), CGState) -> CGInfo
--------------------------------------------------------------------------------
cgStateCInfo f pgm ((fcs, fws), cg) = CGI finfo (cg_ann cg)
  where
    finfo    = F.fi fcs fws bs lits mempty (pQuals pgm) mempty f
    bs       = cg_binds cg
    lits     = lits1 `mappend` lits2
    lits1    = F.sr_sort <$> measureEnv pgm
    lits2    = cgLits bs fcs

cgLits :: F.BindEnv -> [F.SubC a] -> F.SEnv F.Sort
cgLits bs cs = F.fromListSEnv cts
  where
    cts      = [ (F.symbol c, F.strSort) | c <- csLits ++ bsLits ]
    csLits   = concatMap symConsts cs
    bsLits   = symConsts bs


-- OLD CODE -- --------------------------------------------------------------------------------
-- OLD CODE -- cgStateCInfo :: RefScript -> (([F.SubC Cinfo], [F.WfC Cinfo]), CGState) -> CGInfo
-- OLD CODE -- --------------------------------------------------------------------------------
-- OLD CODE -- cgStateCInfo pgm ((fcs, fws), cg) = CGI (patchSymLits fi) (cg_ann cg)
-- OLD CODE --   where
-- OLD CODE --     fi   = F.FI { F.cm       = HM.fromList $ F.addIds fcs
-- OLD CODE --                 , F.ws       = fws
-- OLD CODE --                 , F.bs       = cg_binds cg
-- OLD CODE --                 , F.gs       = measureEnv pgm
-- OLD CODE --                 , F.lits     = []
-- OLD CODE --                 , F.kuts     = F.ksEmpty
-- OLD CODE --                 , F.quals    = pQuals pgm
-- OLD CODE --                 , F.bindInfo = mempty
-- OLD CODE --                 }
--

-- OLD CODE -- patchSymLits fi = fi { F.lits = F.symConstLits fi ++ F.lits fi }


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
--------------------------------------------------------------------------------
envPushContext c g = g { cge_ctx = pushContext c (cge_ctx g) }


data Cast = CNo | CDead [Error] | CType Type

instance PP Cast where
  pp CNo       = pp "No cast"
  pp (CDead e) = pp "Deadcast" $+$ vcat (map pp e)
  pp (CType t) = pp "TypeCast to" <+> pp t

--------------------------------------------------------------------------------
envGetContextCast :: CGEnv -> AnnLq -> Cast
--------------------------------------------------------------------------------
envGetContextCast g a
  | [t ] <- [ t_  | TypeCast ξ t_  <- fFact a, ξ == cge_ctx g ]
  = CType t
  | [es] <- [ es_ | DeadCast ξ es_ <- fFact a, ξ == cge_ctx g ]
  = CDead es
  | otherwise
  = CNo


--   = case [(errs, t) | CDead errs t <- [ c | TCast cx c <- fFact a, cx == cge_ctx g]] of
--       [ ] -> CNo
--       cs  -> let (errs', ts) = unzip cs in
--              CDead (concat errs') (mkUnion ts)

-- | Returns the type instantiations for parameters @αs@ in context @n@
--------------------------------------------------------------------------------
envGetContextTypArgs :: Int -> CGEnv -> AnnLq -> [BTVar F.Reft] -> [RefType]
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
cgEnvAddFresh :: IsLocated l => String -> l -> RefType -> CGEnv -> CGM (Id AnnLq, CGEnv)
--------------------------------------------------------------------------------
cgEnvAddFresh msg l t g
  = do x  <- freshId l
       g' <- cgEnvAdds l ("cgEnvAddFresh: " ++ msg) [(x, v)] g
       addAnnot l x t
       return (x, g')
  where
    v = VI Local RdOnly Initialized t

freshId a = Id <$> freshenAnn a <*> fresh

freshenAnn :: IsLocated l => l -> CGM AnnLq
freshenAnn l
  = do n     <- cg_ast_cnt <$> get
       modify $ \st -> st {cg_ast_cnt = 1 + n}
       return $ FA n (srcPos l) []

--------------------------------------------------------------------------------
cgEnvAdds :: (IsLocated l, EnvKey x) => l -> String -> [(x, CGEnvEntry)] -> CGEnv -> CGM CGEnv
--------------------------------------------------------------------------------
cgEnvAdds l msg xts g = foldM (envAddGroup l msg ks) g es
  where
    es = objFields g <$> xts
    ks = F.symbol . fst <$> xts

--------------------------------------------------------------------------------
envAddGroup :: IsLocated l => l -> String -> [F.Symbol] -> CGEnv
            -> (F.Symbol, [(F.Symbol, CGEnvEntry)]) -> CGM CGEnv
--------------------------------------------------------------------------------
envAddGroup l msg ks g (x, xts)
  = do  mapM_ cgError $ concat $ zipWith (checkSyms l msg g ks) xs ts
        es    <- L.zipWith4 VI ls as is <$> zipWithM inv is ts
        ids   <- toIBindEnv . catMaybes <$> zipWithM (addFixpointBind g) xs es
        return $ g { cge_names = envAdds (zip xs es) $ cge_names g
                   , cge_fenv  = F.insertSEnv x ids  $ cge_fenv  g }
  where
    (xs,ls,as,is,ts) = L.unzip5 [(x,loc,a,i,t) | (x, VI loc a i t) <- xts ]
    inv Initialized  = addInvariant g
    inv _            = return
    toIBindEnv       = (`F.insertsIBindEnv` F.emptyIBindEnv)

-------------------------------------------------------------------------------
-- resolveTypeM :: IsLocated a => a -> CGEnv -> AbsName -> CGM (TypeDecl F.Reft)
-------------------------------------------------------------------------------
resolveTypeM l γ x
  | Just t <- resolveTypeInEnv γ x
  = return t
  | otherwise
  = die $ bugClassDefNotFound (srcPos l) x


-- | Bindings for IMMUTABLE fields
--------------------------------------------------------------------------------
objFields :: EnvKey x => CGEnv -> (x, CGEnvEntry) -> (F.Symbol, [(F.Symbol,CGEnvEntry)])
--------------------------------------------------------------------------------
objFields g (x, e@(VI loc a _ t))
              | a `elem` [WriteGlobal,ReturnVar] = (x_sym, [(x_sym,e)])
              | otherwise = (x_sym, (x_sym,e):xts)
  where
    x_sym     = F.symbol x
    xts       = [(mkQualSym x f, vi f o ft) | (f, FI o Final ft) <- fs ]

    -- This should remain as is: x.f bindings are not going to change
    vi f Opt tf = VI loc a Initialized $ orUndef $ ty tf f
    vi f Req tf = VI loc a Initialized $           ty tf f
    -- v = offset(x,"f")
    ty t f = substThis x t `eSingleton` mkOffset x f

    fs | Just (TObj _ ms _) <- expandType Coercive (envCHA g) t
       = F.toListSEnv (i_mems ms)
       | otherwise = []

--------------------------------------------------------------------------------
addFixpointBind :: EnvKey x => CGEnv -> x -> CGEnvEntry -> CGM (Maybe F.BindId)
--------------------------------------------------------------------------------
-- No binding for globals or RetVal: shouldn't appear in refinements
addFixpointBind _ _ (VI _ WriteGlobal _ _) = return Nothing
addFixpointBind _ _ (VI _ ReturnVar _ _) = return Nothing
addFixpointBind g x (VI _ _ _ t)
  = do  bs           <- cg_binds <$> get
        t'           <- addInvariant g t
        let r         = rTypeSortedReft t'
            (i, bs')  = F.insertBindEnv (F.symbol x) r bs
        modify        $ \st -> st { cg_binds = bs' }
        return        $ Just i

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
    typeof t@(TPrim p o)   i = maybe t (strengthenOp t o . rTypeReft . val) $ HM.lookup p i
    typeof t@(TRef{})      _ = t `strengthen` F.reft (vv t) (typeofExpr $ F.symbol "object")
    typeof t@(TObj{})      _ = t `strengthen` F.reft (vv t) (typeofExpr $ F.symbol "object")
    typeof   (TFun a b _)  _ = TFun a b typeofReft
    typeof   (TOr ts r)    i = TOr (map (`typeof` i) ts) r
    typeof t               _ = t

    -- | Truthy
    truthy t               | maybeTObj t
                           = t `strengthen` F.reft (vv t) (F.eProp $ vv t)
                           | otherwise          = t

    strengthenOp t o r     | r `L.elem` ofRef o = t
                           | otherwise          = t `strengthen` r
    typeofReft             = F.reft  (vv t) $ F.pAnd [ typeofExpr $ F.symbol "function"
                                                     , F.eProp    $ vv t                ]
    typeofExpr s           = F.PAtom F.Eq (F.EApp (F.dummyLoc (F.symbol "ttag")) [F.eVar $ vv t])
                                          (F.expr $ F.symbolSafeText s)

    ofRef (F.Reft (s, ra))    = F.reft s <$> F.conjuncts ra

    -- | { f: T } --> hasProperty("f", v)
    hasProp ty             = t `strengthen` keyReft (boundKeys cha ty)
    keyReft ks             = F.reft (vv t) $ F.pAnd (F.PBexp . hasPropExpr <$> ks)
    hasPropExpr s          = F.EApp (F.dummyLoc (F.symbol "hasProperty"))
                                [F.expr (F.symbolSafeText s), F.eVar $ vv t]

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

    refa t s c     = F.PBexp $ F.EApp (sym s) [ F.expr $ rTypeValueVar t
                                              , F.expr $ F.symbolSafeText c]
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

--------------------------------------------------------------------------------
freshenVI :: IsLocated l => CGEnv -> l -> VarInfo F.Reft -> CGM (VarInfo F.Reft)
--------------------------------------------------------------------------------
freshenVI _ _ v@(VI _ Ambient _ _)
  = return v
freshenVI _ _ v@(VI Exported _ _ _)
  = return v
freshenVI g l v@(VI loc a@WriteGlobal i t)
  | isTrivialRefType t
  = freshTy "freshenVI" (toType t) >>= (VI loc a i <$>) . wellFormed l g
  | otherwise
  = return v
freshenVI g l v@(VI loc a i t)
  | not (isTFun t)
  = return v
  | isTrivialRefType t
  = freshTy "freshenVI" (toType t) >>= (VI loc a i <$>) . wellFormed l g
  | otherwise
  = return v

--------------------------------------------------------------------------------
freshenType :: IsLocated l => Assignability -> CGEnv -> l -> RefType -> CGM RefType
--------------------------------------------------------------------------------
freshenType WriteGlobal g l t
  | isTrivialRefType t
  = freshTy "freshenType-WG" (toType t) >>= wellFormed l g
  | otherwise
  = return t
freshenType _ g l t
  | not (isTFun t)
  = return t
  | isTrivialRefType t
  = freshTy "freshenType-RO" (toType t) >>= wellFormed l g
  | otherwise
  = return t

-- | 1. Instantiates fresh types (at call-site)
--   2. Adds well-formedness constraints for instantiated type variables
--   3. Adds subtyping constraints when type vars have bounds
--   4. Returns the instantiated body of the function.
--------------------------------------------------------------------------------
freshTyInst :: AnnLq -> CGEnv -> [BTVar F.Reft] -> [RefType] -> RefType -> CGM RefType
--------------------------------------------------------------------------------
freshTyInst l g bs τs tbody
  = do ts    <- mapM  (freshTy "freshTyInst") τs
       _     <- mapM_ (wellFormed l g) ts
       _     <- zipWithM_ subt ts bs
       let θ  = fromList (zip αs ts)
       return $ apply θ tbody
  where
    αs        = btvToTV <$> bs
    subt t b  | BTV v _ (Just b) <- b
              = subType l (Just $ errorBoundSubt l v b) g t b
              | otherwise
              = return ()


-- | Instantiate Fresh Type (at Phi-site)
--------------------------------------------------------------------------------
freshTyPhis :: AnnLq -> CGEnv -> [Id AnnLq] -> [Type] -> CGM (CGEnv, [RefType])
--------------------------------------------------------------------------------
freshTyPhis l g xs τs
  = do ts <- mapM (freshTy "freshTyPhis") τs
       g' <- cgEnvAdds l "freshTyPhis" (zip xs (VI Local WriteLocal Initialized <$> ts)) g
       _  <- mapM (wellFormed l g') ts
       return (g', ts)


-- | Instantiate Fresh Type (at Phi-site)
--------------------------------------------------------------------------------
freshTyPhis' :: AnnLq -> CGEnv -> [Id AnnLq] -> [EnvEntry ()] -> CGM (CGEnv, [RefType])
--------------------------------------------------------------------------------
freshTyPhis' l g xs es
  = do ts' <- mapM (freshTy "freshTyPhis") ts
       g'  <- cgEnvAdds l "freshTyPhis" (zip xs (L.zipWith4 VI ls as is ts')) g
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
subType :: AnnLq -> Maybe Error -> CGEnv -> RefType -> RefType -> CGM ()
--------------------------------------------------------------------------------
subType l (Just err) g t1 t2
  = modify $ \st -> st { cg_cs = Sub g (ci err l) t1 t2 : cg_cs st }

subType l Nothing g t1 t2
  = subType l (Just $ mkLiquidError l g t1 t2) g t1 t2

mkLiquidError l g t1 t2 = mkErr l $ show
              $ text "Liquid Type Error" $+$
                nest 2
                (
                  text "In Environment:"  $+$ nest 4 (pp γ ) $+$
                  text "With guards:"     $+$ nest 4 (vcat $ map pp gr) $+$
                  text "Left hand side:"  $+$ nest 4 (pp τ1) $+$
                  text "Right hand side:" $+$ nest 4 (pp τ2)
                )
  where
    γ         = {- F.subst sbt -} (cge_names g)
    gr        = {- F.subst sbt -} (cge_guards g)
    τ1        = {- F.subst sbt -} t1
    τ2        = {- F.subst sbt -} t2
    tmp       = [ x | (x, _) <- F.toListSEnv (envNames g), isPrefixOfSym tempPrefix x ]
    miniTmp   = map (F.expr . F.symbol . ("_" ++) . single) ['a'..]
    sbt       = F.mkSubst (zip tmp miniTmp)

--
-- TODO: KVar subst
--
instance F.Subable a => F.Subable (Env a) where
  substa f = envFromList . map ((***) (F.substa f . F.symbol) (F.substa f)) . envToList
  substf f = envFromList . map ((***) (F.substf f . F.symbol) (F.substf f)) . envToList
  subst su = envFromList . map ((***) (F.subst su . F.symbol) (F.subst su)) . envToList
  syms x   = concat [ F.syms (F.symbol x) ++ F.syms t | (x, t) <- envToList x ]

instance (PP r, F.Reftable r, F.Subable r) => F.Subable (VarInfo r) where
  substa f (VI l a i t) = VI l a i $ F.substa f t
  substf f (VI l a i t) = VI l a i $ F.substf f t
  subst su (VI l a i t) = VI l a i $ F.subst su t
  syms     (VI l a i t) = F.syms t


-- errorLiquid l g t1 t2         = mkErr k $ printf "Liquid Type Error" where k = srcPos l


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
  fresh = symbolString <$> fresh

-- | Freshen up
freshTy :: RefTypable a => s -> a -> CGM RefType
freshTy _ τ = refresh $ rType τ

-- OLD CODE -- instance Freshable F.Refa where
-- OLD CODE --   fresh = F.Refa . (`F.PKVar` mempty) . F.intKvar <$> fresh
-- OLD CODE --
-- OLD CODE -- instance Freshable [F.Refa] where
-- OLD CODE --   fresh = single <$> fresh

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
splitC :: SubC -> CGM [FixSubC]
--------------------------------------------------------------------------------

-- | S-Mut
--
splitC (Sub g i t1 t2)
  | mutRelated t1, mutRelated t2
  = return []
  | mutRelated t1
  = error "Only `splitC` mutability types with eachother."
  | mutRelated t2
  = error "Only `splitC` mutability types with eachother."

-- | S-Fun
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

-- | S-And-L
--
splitC (Sub _ _ TAnd{} _)
  = error "TAnd not supported in splitC"

-- | S-And-R
--
splitC (Sub _ _ _ TAnd{})
  = error "TAnd not supported in splitC"

-- | S-All
--
splitC (Sub g i (TAll α1 t1) (TAll α2 t2))
  | α1 == α2
  = splitC $ Sub g i t1 t2
  | otherwise
  = splitC $ Sub g i t1 t2'
  where
    θ   = fromList [(btvToTV α2, btVar α1 :: RefType)]
    t2' = apply θ t2

-- | S-Var
--
splitC (Sub g i t1@(TVar α1 _) t2@(TVar α2 _))
  | α1 == α2
  = bsplitC g i t1 t2
  | otherwise
  = splitIncompatC g i t1

-- | S-Union-L
--
splitC (Sub g c t1@(TOr t1s r) t2)
  = do  m1      <- bsplitC g c t1 t2
        ms      <- concatMapM (\t -> splitC (Sub g c (t `strengthen` r) t2)) t1s
        return   $ m1 ++ ms

-- | S-Union-R
--
splitC (Sub g c s t@(TOr ts _))
  = do  m0      <- bsplitC g c s t
        mss     <- mapM (splitC . Sub g c s) ts
        ts      <- pure (map (sameTag g s) ts)
        case L.find fst (zip ts mss) of
          Just (_, ms) -> return (m0 ++ ms)
          Nothing      -> splitIncompatC g c s


-- splitC (Sub g c s t@(TOr ts _))
--   = do  m0        <- bsplitC g c s t
--         (mi, g')  <- foldM step ([], g) (init ts)
--         ml        <- splitC $ Sub g' c s (last ts)
--         return     $ m0 ++ mi ++ ml
--   where
--     step (ms, g) t2 =
--       do  bk    <- refresh tBool
--           _     <- wellFormed c g bk
--           pk    <- pure   $ F.reftPred (rTypeReft bk)
--           g'    <- pure   $ g { cge_guards =        pk : cge_guards g }
--           g''   <- pure   $ g { cge_guards = F.PNot pk : cge_guards g }
--           ms'   <- splitC $ Sub g' c s t2
--           return $ (ms ++ ms', g'')

-- | S-Ref
--
--  TODO: restore co/contra-variance
--
splitC (Sub g i t1@(TRef n1@(Gen x1 (m1:t1s)) r1) t2@(TRef n2@(Gen x2 (m2:t2s)) r2))
  --
  -- * Trivial case (do not even descend)
  --
  | F.isFalse (F.simplify r1)
  = return []
  --
  -- * Incompatible mutabilities
  --
  | not (isSubtype (srcPos i) g m1 m2)
  = splitIncompatC g i t1
  --
  -- * Both immutable, same name, non arrays: Co-variant subtyping
  --
  | x1 == x2
  , isIM m2
  , not (isArrayType t1)
  = do  cs    <- bsplitC g i t1 t2
        cs'   <- concatMapM splitC $ safeZipWith "splitc-4" (Sub g i) t1s t2s
        return $ cs ++ cs'
  --
  -- * Non-immutable, same name: invariance
  --
  --  TODO: For arrays do the same trick as with objects and UQ
  --
  | x1 == x2
  , not (F.isFalse r2)
  = do  cs    <- bsplitC g i t1 t2
        cs'   <- concatMapM splitC $ safeZipWith "splitc-5" (Sub g i) t1s t2s
        cs''  <- concatMapM splitC $ safeZipWith "splitc-6" (Sub g i) t2s t1s
        return $ cs ++ cs' ++ cs''

  | x1 == x2
  = bsplitC g i t1 t2

  | Just n1@(Gen x1' _) <- weaken (envCHA g) n1 x2    -- UPCAST
  , x1' == x2 -- sanity check
  = splitC (Sub g i (TRef n1 r1) t2)

  | Just n2@(Gen x2' _) <- weaken (envCHA g) n2 x1    -- DOWNCAST
  , x2' == x1 -- sanity check
  = splitC (Sub g i t1 (TRef n2 r2))

  | otherwise
  = splitIncompatC g i t1

-- | S-Prim
--
splitC (Sub g i t1@(TPrim c1 _) t2@(TPrim c2 r2))
  | isTTop t2 = bsplitC g i t1 (rTop t1 `strengthen` r2)
  | isTAny t2 = bsplitC g i t1 (rTop t1 `strengthen` r2)
  | c1 == c2  = bsplitC g i t1 t2
  | otherwise = splitIncompatC g i t1

-- | S-Obj
--
--    TODO: case for Unique
--
splitC (Sub g i@(Ci _ l) t1@(TObj m1 ms1 r1) t2@(TObj m2 ms2 _))
  | F.isFalse (F.simplify r1)
  = return []
  | otherwise
  = do  cs     <- bsplitC g i t1 t2
        (x,g') <- cgEnvAddFresh "" l t1 g
        cs'    <- splitTM g' (F.symbol x) i (m1, ms1) (m2, ms2)
        return $ cs ++ cs'

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
        -> (Mutability, TypeMembers F.Reft) -> (Mutability, TypeMembers F.Reft)
        -> CGM [FixSubC]
--------------------------------------------------------------------------------
splitTM g x c (m1, TM p1 sp1 c1 k1 s1 n1) (m2, TM p2 sp2 c2 k2 s2 n2)
  = concatMapM (splitElt g c m1 m2 ) (ms ++ sms) +++
    concatMapM splitT (cs ++ ks ++ ss ++ ns)
  where
    (+++) = liftM2 (++)
    ms  = F.toListSEnv $ F.intersectWithSEnv (,) p1 p2
    sms = F.toListSEnv $ F.intersectWithSEnv (,) sp1 sp2
    cs  = [ (t1,t2) | Just t1 <- [c1], Just t2 <- [c2] ]
    ks  = [ (t1,t2) | Just t1 <- [k1], Just t2 <- [k2] ]
    ss  = [ (t1,t2) | Just t1 <- [s1], Just t2 <- [s2] ]
    ns  = [ (t1,t2) | Just t1 <- [n1], Just t2 <- [n2] ]
    splitT (t1,t2) = splitC (Sub g c t1 t2)

--------------------------------------------------------------------------------
splitElt :: CGEnv -> Cinfo -> Mutability -> Mutability
         -> (t, (TypeMember F.Reft, TypeMember F.Reft)) -> CGM [FixSubC]
--------------------------------------------------------------------------------
splitElt g i m1 _ (_, (FI _ p1 t1, FI _ p2 t2))
  | isUQ    m1 = splitC (Sub g i t1 t2)
  | isFinal p2 = splitC (Sub g i t1 t2)
  | otherwise  = (++) <$> splitC (Sub g i t1 t2)
                      <*> splitC (Sub g i t2 t1)

splitElt g i _ _ (_, (m, m'))
  = cgError $ unsupportedSplitElt (srcPos i) m m'


subCTag :: [Int]
subCTag = [1]

conjoinPred :: F.Pred -> F.SortedReft -> F.SortedReft
conjoinPred p r    = r {F.sr_reft = F.Reft (v, F.pAnd [pr, p]) }
  where
    F.Reft (v, pr) = F.sr_reft r



--------------------------------------------------------------------------------
bsplitC :: CGEnv -> a -> RefType -> RefType -> CGM [F.SubC a]
--------------------------------------------------------------------------------
-- NOTE: addInvariant only needed in LHS
bsplitC g ci t1 t2 = bsplitC' g ci <$> addInvariant g t1 <*> return t2

bsplitC' g ci t1 t2
  | F.isFunctionSortedReft r1 && F.isNonTrivial r2
  = F.subC bs (conjoinPred p $ r1 {F.sr_reft = typeofReft t1}) r2 Nothing subCTag ci
  | F.isNonTrivial r2
  = F.subC bs (conjoinPred p r1) r2 Nothing subCTag ci
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


-- OLD CODE -- bsplitC' g ci t1 t2
-- OLD CODE --   | F.isFunctionSortedReft r1 && F.isNonTrivial r2
-- OLD CODE --   -- = F.subC (cge_fenv g) F.PTrue (r1 {F.sr_reft = typeofReft t1}) r2 Nothing [] ci
-- OLD CODE --   = F.subC bs p (r1 {F.sr_reft = typeofReft t1}) r2 Nothing [] ci
-- OLD CODE --   | F.isNonTrivial r2
-- OLD CODE --   = F.subC bs p r1 r2 Nothing [] ci
-- OLD CODE --   | otherwise
-- OLD CODE --   = []
-- OLD CODE --   where
-- OLD CODE --     bs             = foldl F.unionIBindEnv F.emptyIBindEnv $ snd <$> F.toListSEnv (cge_fenv g)
-- OLD CODE --     p              = F.pAnd $ cge_guards g
-- OLD CODE --     (r1,r2)        = (rTypeSortedReft t1,  rTypeSortedReft t2)
-- OLD CODE --     typeofReft t   = F.reft (vv t) $ F.pAnd [ typeofExpr (F.symbol "function") t
-- OLD CODE --                                             , F.eProp $ vv t ]
-- OLD CODE --     typeofExpr s t = F.PAtom F.Eq (F.EApp (F.dummyLoc (F.symbol "ttag")) [F.eVar $ vv t])
-- OLD CODE --                                   (F.expr $ F.symbolSafeText s)
-- OLD CODE --     vv             = rTypeValueVar


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

splitW (W g i (TAnd (map snd -> ts)))
  = concatMapM splitW [W g i t | t <- ts]

splitW (W g i t@(TOr ts r))
  = do let ws = bsplitW g t i
       ws'   <- concatMapM splitW [W g i t | t <- ts]
       return $ ws ++ ws'

splitW (W g i t@(TObj _ ms _))
  = do let bws = bsplitW g t i
       -- TODO: add field bindings in g?
       ws     <- concatMapM splitW [ W g i t' | t' <- typesOfTM ms ]
       return  $ bws ++ ws

splitW (W _ _ (TClass _ ))
  = return []

splitW (W _ _ (TMod _ ))
  = return []

splitW (W _ _ t)
  = error $ render $ text "Not supported in splitW: " <+> pp t

-- OLD CODE -- bsplitW g t i
-- OLD CODE --   | F.isNonTrivial r'
-- OLD CODE --   = [F.wfC bs r' Nothing i]
-- OLD CODE --   | otherwise
-- OLD CODE --   = []
-- OLD CODE --   where
-- OLD CODE --     r' = rTypeSortedReft t
-- OLD CODE --     bs = foldl F.unionIBindEnv F.emptyIBindEnv $ snd <$> F.toListSEnv (cge_fenv g)

bsplitW g t i
  | F.isNonTrivial r'
  = F.wfC bs r' {- Nothing -} i
  | otherwise
  = []
  where
    r' = rTypeSortedReft t
    bs = foldl F.unionIBindEnv F.emptyIBindEnv
       $ snd <$> F.toListSEnv (cge_fenv g)



envTyAdds msg l xts g
  = cgEnvAdds l (msg ++ " - envTyAdds " ++ ppshow (srcPos l))
      [(symbolId l x, VI Local WriteLocal Initialized t) | B x t <- xts] g
    where
      symbolId l x = Id l $ symbolString $ F.symbol x



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
                     = uncurry (αs,,) <$> substNoCapture xs (yts', t)
                     | otherwise
                     = cgError $ errorArgMismatch (srcPos l)

-- | `substNoCapture xs (yts,t)` substitutes formal parameters in `yts` with
--   actual parameters passed as bindings in `xs`.
--
--   Avoids capture of function binders by existing value variables
--------------------------------------------------------------------------------
substNoCapture :: F.Symbolic a => [a] -> ([Bind F.Reft], RefType) -> CGM ([Bind F.Reft], RefType)
--------------------------------------------------------------------------------
substNoCapture xs (yts, rt)
  | length yts /= length xs
  = error "substNoCapture - length test failed"
  | otherwise
  = (,) <$> mapM (onT (mapReftM ff)) yts <*> mapReftM ff rt
  where
    -- If the symbols we are about to introduce are already captured by some vv:
    -- (1) create a fresh vv., and
    -- (2) proceed with the substitution after replacing with the new vv.
    ff r@(F.Reft (v,ras)) | v `L.elem` xss
                          = do v' <- freshVV
                               return $ F.subst su $ F.Reft . (v',)
                                      $ F.subst (F.mkSubst [(v, F.expr v')]) ras
                          | otherwise
                          = return $ F.subst su r
    freshVV               = F.vv . Just <$> fresh
    xss                   = F.symbol <$> xs
    su                    = F.mkSubst $ safeZipWith "substNoCapture" fSub yts xs
    fSub                  = curry $ (***) b_sym F.eVar
    onT f (B s t)         = B s <$> f t

-- | Substitute occurences of 'this.f' in type @t'@, with 'f'
--------------------------------------------------------------------------------
unqualifyThis :: CGEnv -> RefType -> RefType -> RefType
--------------------------------------------------------------------------------
unqualifyThis g t = F.subst $ F.mkSubst fieldSu
  where
    fieldSu | Just (TObj _ fs _) <- expandType Coercive (envCHA g) t
            = [ subPair f | (f, FI _ Final _) <- F.toListSEnv $ i_mems fs ]
            | otherwise
            = []
    this      = F.symbol $ builtinOpId BIThis
    qFld x f  = qualifySymbol (F.symbol x) f
    subPair f = (qFld this f, F.expr f)

-------------------------------------------------------------------------------
narrowType :: (IsLocated l) => l -> CGEnv -> RefType-> Type -> RefType
-------------------------------------------------------------------------------
narrowType l g t1@(TOr t1s r) t2
  = mkUnion' (L.filter (\t1 -> isSubtype l g t1 (ofType t2)) t1s) r

narrowType _ _ t1 _ = t1


--------------------------------------------------------------------------------
-- | Runtime tag checks
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
sameTag :: CGEnv -> RefType -> RefType -> Bool
--------------------------------------------------------------------------------
sameTag _ (TPrim c1 _) (TPrim c2 _) = c1 == c2
sameTag _ (TVar  v1 _) (TVar  v2 _) = v1 == v2
sameTag g t1@TRef{}    t2@TRef{}    = isSubtype dummySpan g t1 t2 || isSubtype dummySpan g t2 t1
sameTag _ TObj{}       TObj{}       = True
sameTag _ TObj{}       TRef{}       = True
sameTag _ TRef{}       TObj{}       = True
sameTag _ TFun{}       TFun{}       = True
sameTag _ _            _            = False


-- Local Variables:
-- flycheck-disabled-checkers: (haskell-liquid)
-- End:
