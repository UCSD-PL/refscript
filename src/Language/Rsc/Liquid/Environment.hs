{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

module Language.Rsc.Liquid.Environment (

  -- * Validation
    validateTFun

  -- * Initializers
  , initClassCtorEnv
  , initClassMethEnv

  -- * Fresh Templates for Unknown Refinement Types
  , freshTyFun
  , freshenTypeOpt, freshenType
  , freshTyInst
  , freshTyPhi
  , freshTyPhis
  , freshTyPhis'
  , freshTyCondExpr
  , freshTyObj, freshenCGEnvM

  , cgEnvAdds
  , cgSafeEnvFindTyM
  , cgEnvFindReturn
  , cgEnvFindTy
  , cgEnvAddFresh
  , cgEnvAddFreshWithInit
  , addInvariant

  , envTyAdds
  , envFindTyWithAsgn

  , envGetContextTypArgs

  -- * Casts
  , Cast(..)
  , envGetContextCast

  -- * Aux
  , traceTypePP

) where

import           Control.Monad
import           Data.Default
import qualified Data.HashMap.Strict             as HM
import qualified Data.List                       as L
import           Data.Maybe                      (catMaybes, fromMaybe)
import           Debug.Trace                     hiding (traceShow)
import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Types         as F
import           Language.Fixpoint.Types.Errors
import           Language.Rsc.Annotations
import           Language.Rsc.ClassHierarchy
import           Language.Rsc.CmdLine
import           Language.Rsc.Core.Env
import           Language.Rsc.Environment
import           Language.Rsc.Errors
import           Language.Rsc.Liquid.CGMonad
import           Language.Rsc.Liquid.Refinements
import           Language.Rsc.Liquid.Types
import           Language.Rsc.Locations
import           Language.Rsc.Module
import           Language.Rsc.Names
import           Language.Rsc.Pretty
import           Language.Rsc.Symbols
import           Language.Rsc.Transformations
import           Language.Rsc.Traversals
import           Language.Rsc.Typecheck.Sub
import           Language.Rsc.Typecheck.Subst
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
import           Text.PrettyPrint.HughesPJ
import           Text.Printf

type EnvKey x = (IsLocated x, F.Symbolic x, PP x, F.Expression x)


instance PP CGEnv where
  pp g = pp "GUARDS:" $+$
         nest 2 (pp (cge_guards g)) $+$
         pp "VARIABLES" $+$
         pp (cge_names g)



-- Only include the "singleton" refinement in the case where Assignability is
-- either ReadOnly of WriteLocal (SSAed)
---------------------------------------------------------------------------------------
envFindTyWithAsgn :: (EnvKey x, F.Expression x) => x -> CGEnv -> Maybe CGEnvEntry
---------------------------------------------------------------------------------------
envFindTyWithAsgn x (envNames -> γ) = fmap singleton (envFindTy x γ)
  where
    singleton v@(SI _ _ WriteGlobal Initialized   _) = v
    singleton v@(SI _ _ WriteGlobal Uninitialized t) = v { v_type = orUndef t }
    singleton v = v { v_type = eSingleton (v_type v) x }

---------------------------------------------------------------------------------------
cgEnvFindReturn :: CGEnv -> RefType
cgEnvFindTy     :: F.Symbolic x => x -> CGEnvR r -> Maybe (RTypeQ AK r)
---------------------------------------------------------------------------------------
cgEnvFindReturn = v_type . envFindReturn . cge_names
cgEnvFindTy x   = fmap v_type . envFindTy x . cge_names


---------------------------------------------------------------------------------------
-- | Well-Formedness
---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------
checkSyms :: (IsLocated l, EnvKey a, PP m) => l -> m -> CGEnv -> [a] -> RefType -> [Error]
---------------------------------------------------------------------------------------
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
checkSyms l m g ok t = efoldRType h f F.emptySEnv [] t
  where
    h _        = ()
    f γ t' s   = let rt   = rTypeReft t'     in
                 let noKv = noKVars rt       in
                 let ss   = F.syms noKv      in
                 s ++ catMaybes (fmap (chk γ t') ss)

    chk γ t' s | s `elem` biReserved
               = Just $ unimplementedReservedSyms l
               | s == rTypeValueVar t'
               = Nothing
               | s `elem` ok_syms
               = Nothing
               | s `F.memberSEnv` γ
               = Nothing
               | s `F.memberSEnv` cge_consts g
               = Nothing
               | s `elem` biExtra
               = Nothing
               | Just (SI _ _ a _ _) <- chkEnvFindTy' s g
               = if a `elem` validAsgn
                   then Nothing
                   else Just $ errorAsgnInRef l s t a
               | otherwise
               = Just $ errorUnboundSyms l t s m

    biReserved = map F.symbol ["func", "obj"]
    biExtra    = map F.symbol ["bvor", "bvand", "builtin_BINumArgs", "offset"]
    ok_syms    = map F.symbol ok
    validAsgn  = [RdOnly, Ambient, WriteLocal]

    -- errMsg     = show $ pp "type" $+$ nest 4 (pp t') $+$
    --                                   pp "rt"   $+$ nest 4 (pp rt) $+$
    --                                   pp "noKV" $+$ nest 4 (pp noKv) $+$
    --                                   pp "syms" $+$ nest 4 (pp s) $+$
    --                                   pp "errors"


-------------------------------------------------------------------------------
validateTFun :: IsLocated l => l -> CGEnv -> RefType -> CGM ()
-------------------------------------------------------------------------------
validateTFun l g = maybe (return ()) (validateFSigs l g) . bkFuns

-------------------------------------------------------------------------------
validateFSigs :: IsLocated l => l -> CGEnv -> [OverloadSig F.Reft] -> CGM ()
-------------------------------------------------------------------------------
validateFSigs l g = mapM_ (validateFSig l g)


-------------------------------------------------------------------------------
validateFSig :: IsLocated l => l -> CGEnv -> OverloadSig F.Reft -> CGM  ()
-------------------------------------------------------------------------------
validateFSig l g (bvs, bs, t) = do
    forM_ bvEs cgError
    forM_ tyEs cgError
  where
    validateTy ok t = checkSyms l "validateFSig" g ok t
    bvEs  = concatMap (validateTy ([] :: [F.Symbol])) (catMaybes (map btv_constr bvs))
    tyEs  = concatMap (validateTy bSyms) (map b_type bs ++ [t])
    bSyms = map b_sym bs



-------------------------------------------------------------------------------
-- | Environment initialization
-------------------------------------------------------------------------------

-- | `initClassCtorEnv` makes `this` a Unique & Uninitialized binding
-------------------------------------------------------------------------------
initClassCtorEnv :: IsLocated l => l -> TypeSig F.Reft -> CGEnv -> CGM CGEnv
-------------------------------------------------------------------------------
initClassCtorEnv l  (TS _ (BGen nm bs) _) g =
    -- cgEnvAdds l "initClassCtorEnv" [eThis] g'
    return g'
  where
    g'    = g { cge_bounds = envAdds bts (cge_bounds g)
              , cge_this   = Just tThis
              , cge_mut    = Just tUQ
              }
    bts   = [(s,t) | BTV s _ (Just t) <- bs]
    tThis = adjUQ (TRef (Gen nm (map btVar bs)) fTop)
    -- eThis = SI thisSym Local RdOnly Uninitialized tThis
    adjUQ (TRef (Gen n (_:ps)) r) = TRef (Gen n (tUQ:ps)) r
    adjUQ t                       = t


-------------------------------------------------------------------------------
initClassMethEnv
  :: IsLocated l => l -> Mutability -> TypeSig F.Reft -> CGEnv -> CGM CGEnv
-------------------------------------------------------------------------------
initClassMethEnv l m (TS _ (BGen nm bs) _) g = do
    cgEnvAdds l "initClassMethEnv" [eThis] g'
  where
    g'    = g { cge_bounds  = envAdds bts (cge_bounds g)
              , cge_this    = Just $ TRef (Gen nm (map btVar bs)) fTop
              , cge_mut     = Just m
              }
    bts   = [(s,t) | BTV s _ (Just t) <- bs]
    tThis = adjMut $ TRef (Gen nm (map btVar bs)) fTop
    eThis = SI thisSym Local RdOnly Initialized tThis

    adjMut (TRef (Gen n (_:ps)) r) = TRef (Gen n (m:ps)) r
    adjMut t                       = t



--------------------------------------------------------------------------------
-- | Cast access
--------------------------------------------------------------------------------

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


-- | Returns the type instantiations for parameters @αs@ in context @n@
--------------------------------------------------------------------------------
envGetContextTypArgs
  :: PP f => Int -> CGEnv -> AnnLq -> f -> [BTVar F.Reft] -> [RefType]
--------------------------------------------------------------------------------
-- NOTE: If we do not need to instantiate any type parameter (i.e. length αs ==
-- 0), DO NOT attempt to compare that with the TypInst that might hide within
-- the expression, cause those type instantiations might serve anothor reason
-- (i.e. might be there for a separate instantiation).
--
envGetContextTypArgs _ _ _ _ [] = []
envGetContextTypArgs n g a f αs
  | [its] <- tys
  , length its == length αs
  = its
  | otherwise
  = die (bugMissingTypeArgs a f)
  where
    tys = [i | TypInst m ξ' i <- fFact a, (ξ', n) == (cge_ctx g, m) ]


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
cgEnvAddFreshWithInit
  :: IsLocated l
  => Initialization -> String -> l -> RefType -> CGEnv -> CGM (Id AnnLq, CGEnv)
--------------------------------------------------------------------------------
cgEnvAddFreshWithInit i msg l t g
  = do x  <- freshId l
       g' <- cgEnvAdds l ("cgEnvAddFresh: " ++ msg) [v x] g
       addAnnot l x t
       return (x, g')
  where
    v x = SI (F.symbol x) Local RdOnly i t

--------------------------------------------------------------------------------
cgEnvAddFresh
  :: IsLocated l => String -> l -> RefType -> CGEnv -> CGM (Id AnnLq, CGEnv)
--------------------------------------------------------------------------------
cgEnvAddFresh = cgEnvAddFreshWithInit Initialized

freshId a = Id <$> freshenAnn a <*> fresh

freshenAnn :: IsLocated l => l -> CGM AnnLq
freshenAnn l = FA <$> cgTickAST <*> pure (srcPos l) <*> pure []

--------------------------------------------------------------------------------
cgEnvAdds :: (IsLocated l, PP m) => l -> m -> [CGEnvEntry] -> CGEnv -> CGM CGEnv
--------------------------------------------------------------------------------
cgEnvAdds l msg xts g = foldM step g es
  where
    step g_ es_ = envAddGroup l msg ks g_ es_
    es = map (objFields g) xts
    ks = map F.symbol      xts


-- | Bindings for IMMUTABLE fields
--------------------------------------------------------------------------------
objFields :: CGEnv -> CGEnvEntry -> [CGEnvEntry]
--------------------------------------------------------------------------------
objFields g e@(SI x loc a _ t)
  | a `elem` [ WriteGlobal, ReturnVar ] = [e]
  | otherwise                           = e:xts
  where
    xts | Just (TObj _ ms _) <- expandType def (envCHA g) t
        , fs                 <- F.toListSEnv (i_mems ms)
        = [ mkSI f o ft       | (_, FI f o m ft) <- fs
                              , isSubtype g m tIM ]
        | otherwise
        = []

    -- This should remain as is: x.f bindings are not going to change
    mkSI f Opt tf = SI (mkQualSym x f) loc a Initialized $ ty tf f
    mkSI f Req tf = SI (mkQualSym x f) loc a InitUnknown $ ty tf f

    -- v = offset(x,"f")
    ty t f = -- tracePP (ppshow t ++ ppshow " after substing " ++ ppshow (thisSym, x)) $
             substThis x $ t `eSingleton` mkOffsetSym x f


--------------------------------------------------------------------------------
envAddGroup
  :: (IsLocated l, PP m)
  => l -> m -> [F.Symbol] -> CGEnv -> [CGEnvEntry] -> CGM CGEnv
--------------------------------------------------------------------------------
envAddGroup l msg ks g0 xts
  = do  -- Flag any errors
        _           <- mapM_ cgError errors
        es          <- L.zipWith5 SI xs ls as is <$> zipWithM inv is ts
        g1          <- foldM addFixpointBind g0 es
        return       $ g1 { cge_names = envAdds (zip xs es) (cge_names g1) }
  where
    errors           = concat (zipWith (\x -> checkSyms l msg g0 (x:ks)) xs ts)

    (xs,ls,as,is,ts) = L.unzip5 [(x, loc, a, i, strOr a x t) | SI x loc a i t <- xts ]

    -- Invariant strengthening
    inv Initialized  = addInvariant g0
    inv _            = return

    -- Union strengthening
    strOr a x t@(TOr ts r)
      | a `elem` [WriteGlobal, ReturnVar] = t
      | otherwise = TOr (map (`eSingleton` x) ts) r
    strOr _ _ t = t



-- | Adds a fixpoint binding to g's cge_fenv, updating the relevant
--   environments of the monad's state.
--------------------------------------------------------------------------------
addFixpointBind :: CGEnv -> CGEnvEntry -> CGM CGEnv
--------------------------------------------------------------------------------
-- No binding for globals or RetVal: shouldn't appear in refinements
addFixpointBind g (SI _ _ WriteGlobal _ _) = return g
addFixpointBind g (SI _ _ ReturnVar   _ _) = return g
addFixpointBind g (SI x _ _           _ t)
  = do  bs           <- getCgBinds
        rbs          <- getCgRevBinds
        let ibs0      = cge_fenv g

        -- Remove the old binding from IBindEnv of the CGEnv
        let ibs1      = case F.lookupSEnv x rbs of
                          Just i  -> F.deleteIBindEnv i ibs0
                          Nothing -> ibs0

        -- Add Type Invariants (XXX: again?)
        t'           <- addInvariant g t
        let r         = rTypeSortedReft t'

        -- Update the Monad's BindEnv
        let (i, bs')  = F.insertBindEnv x r bs

        -- Update the Monad's (Sym -> Bid) mapping
        let rbs'      = F.insertSEnv x i rbs

        -- Set the Monad' envs
        setCgBinds    bs'
        setCgRevBinds rbs'

        -- Update the CGEnv's IBindEnv
        let ibs2      = F.insertsIBindEnv [i] ibs1
        return        $ g { cge_fenv = ibs2 }


--------------------------------------------------------------------------------
addInvariant :: CGEnv -> RefType -> CGM RefType
--------------------------------------------------------------------------------
addInvariant g t
  = do  extraInvariants <- extraInvs <$> getCgOpts
        if extraInvariants then (hasProp . hierarchy . truthy . typeof t) <$> getCgInvs
                           else (          hierarchy . truthy . typeof t) <$> getCgInvs
  where
    cha = envCHA g
    -- | typeof
    typeof t@(TPrim p o)   i = maybe t (strengthenOp t o . rTypeReft . val) $ HM.lookup p i
    typeof t@(TRef{})      _ = t `strengthen` F.reft (vv t) (typeofExpr $ F.symbol "object")
    typeof t@(TObj{})      _ = t `strengthen` F.reft (vv t) (typeofExpr $ F.symbol "object")
    typeof   (TFun a b _)  _ = TFun a b typeofReft
    typeof t               _ = t

    -- | Truthy
    truthy t               | notNullObj t
                           = t `strengthen` F.reft (vv t) (F.eProp $ vv t)
                           | otherwise          = t

    strengthenOp t o r     | r `L.elem` ofRef o = t
                           | otherwise          = t `strengthen` r
    typeofReft             = F.reft  (vv t) $ F.pAnd [ typeofExpr $ F.symbol "function"
                                                     , F.eProp    $ vv t                ]
    typeofExpr s           = F.PAtom F.Eq (F.mkEApp (F.dummyLoc (F.symbol "ttag")) [F.eVar $ vv t])
                                          (F.expr $ F.symbolSafeText s)

    ofRef (F.Reft (s, ra))    = F.reft s <$> F.conjuncts ra

    -- | { f: T } --> hasProperty(v, "f")
    hasProp ty             = t `strengthen` keyReft (boundKeys cha ty)
    keyReft ks             = F.reft (vv t) $ F.pAnd (hasPropExpr <$> ks)
    hasPropExpr s          = F.mkEApp (F.dummyLoc (F.symbol "hasProperty"))
                                [F.eVar (vv t), F.expr (F.symbolSafeText s)]

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

    refa t s c     =F.mkEApp (sym s) [ F.expr $ rTypeValueVar t
                                     , F.expr $ F.symbolSafeText c]
    vv             = rTypeValueVar
    sym s          = F.dummyLoc $ F.symbol s


--------------------------------------------------------------------------------
-- | Fresh Templates
--------------------------------------------------------------------------------

-- | Freshen up
freshTy :: RefTypable a => s -> a -> CGM RefType
freshTy _ τ = refresh $ rType τ


-- | Instantiate Fresh Type (at Function-site)
--------------------------------------------------------------------------------
freshTyFun :: (IsLocated l) => CGEnv -> l -> RefType -> CGM RefType
--------------------------------------------------------------------------------
freshTyFun g l t
  | not (isTFun t)     = return t
  | isTrivialRefType t = freshTy "freshTyFun" (toType t) >>= wellFormed l g
  | otherwise          = return t

--------------------------------------------------------------------------------
freshenVI :: IsLocated l => CGEnv -> l -> SymInfo F.Reft -> CGM (SymInfo F.Reft)
--------------------------------------------------------------------------------
freshenVI _ _ v@(SI _ _ Ambient _ _)
  = return v
freshenVI _ _ v@(SI _ Exported _ _ _)
  = return v
freshenVI g l v@(SI x loc a@WriteGlobal i t)
  | isTrivialRefType t
  = freshTy "freshenVI" (toType t) >>= (SI x loc a i <$>) . wellFormed l g
  | otherwise
  = return v
freshenVI g l v@(SI x loc a i t)
  | not (isTFun t)
  = return v
  | isTrivialRefType t
  = freshTy "freshenVI" (toType t) >>= (SI x loc a i <$>) . wellFormed l g
  | otherwise
  = return v

--------------------------------------------------------------------------------
freshenTypeOpt :: IsLocated l => CGEnv -> l -> RefType -> CGM (Maybe RefType)
--------------------------------------------------------------------------------
freshenTypeOpt g l t
  | isTrivialRefType t = Just <$> (freshTy "ft-WG" t >>= wellFormed l g)
  | otherwise          = return Nothing

--------------------------------------------------------------------------------
freshenType :: IsLocated l => CGEnv -> l -> RefType -> CGM RefType
--------------------------------------------------------------------------------
freshenType g l t = fromMaybe t <$> freshenTypeOpt g l t

-- | 1. Instantiates fresh types (at call-site)
--   2. Adds well-formedness constraints for instantiated type variables
--   3. Adds subtyping constraints when type vars have bounds
--   4. Returns the instantiated body of the function.
--
--   τs: the type instantiations (from TC)
--------------------------------------------------------------------------------
freshTyInst :: AnnLq -> CGEnv -> [BTVar F.Reft] -> [RefType] -> RefType -> CGM RefType
--------------------------------------------------------------------------------
freshTyInst l g bs τs tbody
  = do ts    <- mapM  (freshTy "freshTyInst") τs
       _     <- mapM_ (wellFormed l g) ts
       _     <- mapM_ ff (safeZip "freshTyInst" ts bs)
       θ     <- pure (fromList (zip αs ts))
       return $ apply θ tbody
  where
    ff (t, BTV _ _ (Just b)) = subType l Nothing g t b
    ff (_, BTV _ _ Nothing ) = return ()
    αs        = btvToTV <$> bs

-- | Instantiate Fresh Type (at Phi-site)
--------------------------------------------------------------------------------
freshTyPhis :: AnnLq -> CGEnv -> [Id AnnLq] -> [Type] -> CGM (CGEnv, [RefType])
--------------------------------------------------------------------------------
freshTyPhis l g xs τs
  = do ts <- mapM (freshTy "freshTyPhis") τs
       g' <- cgEnvAdds l "freshTyPhis" (zipWith si xs ts) g
       _  <- mapM (wellFormed l g') ts
       return (g', ts)
  where
    si x t = SI (F.symbol x) Local WriteLocal Initialized t


-- | Instantiate Fresh Type (at Phi-site)
--------------------------------------------------------------------------------
freshTyPhis' :: F.Reftable r => AnnLq -> CGEnv -> [SymInfo r] -> CGM (CGEnv, [RefType])
--------------------------------------------------------------------------------
freshTyPhis' l g es
  = do  ts' <- mapM (freshTy "freshTyPhis") ts
        g'  <- cgEnvAdds l "freshTyPhis" (L.zipWith5 SI xs ls as is ts') g
        _   <- mapM (wellFormed l g') ts'
        return (g', ts')
  where
    (xs,ls,as,is,ts) = L.unzip5 [ (F.symbol x,l,a,i,t) | SI x l a i t <- es ]

--------------------------------------------------------------------------------
freshTyPhi :: F.Reftable r => AnnLq -> CGEnv -> SymInfo r -> CGM (CGEnv, RefType)
--------------------------------------------------------------------------------
freshTyPhi l g s = do
    (g, ts) <- freshTyPhis' l g [s]
    case ts of
      [t] -> return (g, t)
      _   -> error "freshTyPhi impossible"

-- | Instantiate Fresh Type at conditional expression
--------------------------------------------------------------------------------
freshTyCondExpr :: AnnLq -> CGEnv -> Type -> CGM (Id AnnLq, CGEnv, RefType)
--------------------------------------------------------------------------------
freshTyCondExpr l g t
  = do  t'      <- freshTy "freshTyCondExpr" t
        (x, g') <- cgEnvAddFresh "freshTyCondExpr" l t' g
        _       <- wellFormed l g' t'
        return    $ (x, g', t')

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
    go (k, SI x loc a i v@TVar{}) = return (k, SI x loc a i v)
    go (k, v                    ) = (k,) <$> freshenVI g (srcPos k) v

--------------------------------------------------------------------------------
freshenModuleDefM
  :: CGEnv -> (AbsPath, ModuleDef F.Reft) -> CGM (AbsPath, ModuleDef F.Reft)
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


envTyAdds msg l xts g = cgEnvAdds l msg' sis g
  where
    sis  = [ SI x Local WriteLocal Initialized t | B x _ t <- xts ]
    msg' = msg ++ " - envTyAdds " ++ ppshow (srcPos l)

traceTypePP l msg act
  = do  z <- act
        case z of
          Just (x,g) -> do  t <- cgSafeEnvFindTyM x g
                            return $ Just $ trace (str x t) (x,g)
          Nothing -> return Nothing
  where
    str x t = boldBlue (printf "\nTrace: [%s] %s\n" (ppshow (srcPos l)) (ppshow msg)) ++
              printf "%s: %s" (ppshow x) (ppshow t)


