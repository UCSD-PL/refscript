{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}

-- | Module pertaining to Refinement Type descriptions and conversions
--   Likely mergeable with @Language.Nano.Typecheck.Types@

module Language.Nano.Liquid.Types (

  -- * Refinement Types
    RefType

  -- * Constraint Environments
  , CGEnvR(..), CGEnv

  -- * Constraint Information
  , Cinfo (..), ci

  -- * Constraints
  , SubC (..) , WfC (..), FixSubC   , FixWfC

  -- * Some Operators on Pred
  , pAnd, pOr

  -- * Conversions
  , RefTypable (..), eSingleton, pSingleton

  -- * Manipulating RefType
  , rTypeReft, rTypeSort, rTypeSortedReft, rTypeValueVar

  -- * Manipulating Reft
  , noKVars

  -- * Predicates On RefType
  , isTrivialRefType

  -- * Useful Operations
  , foldReft, efoldRType -- , emapReft, mapReftM

  -- * Annotations
  , AnnTypeR

  -- * Accessing Spec Annotations
  , getSpec, getRequires, getEnsures, getAssume, getAssert
  , getInvariant, getFunctionIds
    -- ,  returnSymbol, returnId, symbolId, mkId

  -- * Raw low-level Location-less constructors
  , rawStringSymbol

  -- * Zip types
  -- , zipType

  -- * 'this' related substitutions
  -- , substThis, unqualifyThis, mkQualSym, mkOffset
  -- , substOffsetThis

  ) where

import           Control.Applicative
import           Control.Monad                    (liftM, zipWithM)
import qualified Data.HashMap.Strict              as HM
import qualified Data.List                        as L
import qualified Data.Map.Strict                  as M
import           Data.Maybe                       (catMaybes, fromMaybe, maybeToList)
import           Data.Monoid                      (mconcat)
import qualified Data.Traversable                 as T
import qualified Language.Fixpoint.Bitvector      as BV
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc
import           Language.Fixpoint.PrettyPrint
import qualified Language.Fixpoint.Types          as F
import qualified Language.Fixpoint.Visitor        as V
import           Language.Nano.Annots
import           Language.Nano.AST
import           Language.Nano.ClassHierarchy
import           Language.Nano.Errors
import           Language.Nano.Liquid.Environment
import           Language.Nano.Locations
import           Language.Nano.Misc
import           Language.Nano.Names
import           Language.Nano.Pretty
import           Language.Nano.Program
import           Language.Nano.Typecheck.Sub
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Typecheck.Types
import           Language.Nano.Types
import           Text.PrettyPrint.HughesPJ
import           Text.Printf

-- import           Debug.Trace                        (trace)

-------------------------------------------------------------------------------------
-- | Refinement Types and Annotations
-------------------------------------------------------------------------------------

type RefType     = RType F.Reft
type AnnTypeR    = AnnType F.Reft

----------------------------------------------------------------------------
-- | Constraint Information
----------------------------------------------------------------------------

data Cinfo = Ci { ci_info    :: !Error
                , ci_srcspan :: !SrcSpan
                } deriving (Eq, Ord, Show)

ci   :: (IsLocated a) => Error -> a -> Cinfo
ci e = Ci e . srcPos

instance PP Cinfo where
  pp (Ci e l)   = text "CInfo:" <+> pp l <+> (parens $ pp e)

instance IsLocated Cinfo where
  srcPos = ci_srcspan

instance F.Fixpoint Cinfo where
  toFix = pp . ci_srcspan

----------------------------------------------------------------------------
-- | Constraints
----------------------------------------------------------------------------

-- | Subtyping Constraints

data SubC
  = Sub { senv  :: !CGEnv      -- ^ Environment
        , sinfo :: !Cinfo      -- ^ Source Information
        , slhs  :: !RefType    -- ^ Subtyping LHS
        , srhs  :: !RefType    -- ^ Subtyping RHS   ... senv |- slhs <: srhs
        }

-- | Wellformedness Constraints

data WfC
  = W { wenv  :: !CGEnv      -- ^ Scope/Environment
      , winfo :: !Cinfo      -- ^ Source Information
      , wtyp  :: !RefType    -- ^ Type to be Well-formed ... wenv |- wtyp
      }

instance PP F.Reft where
  pp = pprint

instance PP SubC where
  pp (Sub γ i t t') = pp (cge_names γ) $+$ pp (cge_guards γ)
                        $+$ ((text "|-") <+> (pp t $+$ text "<:" $+$ pp t'))
                        $+$ ((text "from:") <+> pp i)

instance PP WfC where
  pp (W γ t i)      = pp (cge_names γ)
                        $+$ (text "|-" <+> pp t)
                        $+$ ((text "from:") <+> pp i)

instance IsLocated SubC where
  srcPos = srcPos . sinfo

instance IsLocated WfC where
  srcPos = srcPos . winfo

-- | Aliases for Fixpoint Constraints

type FixSubC = F.SubC Cinfo
type FixWfC  = F.WfC  Cinfo


------------------------------------------------------------------
-- | Converting `Nano` values into `Fixpoint` values,
--   i.e. *language* level entities into *logic* level entities.
------------------------------------------------------------------

instance F.Expression (Id a) where
  expr = F.eVar

instance F.Expression (LValue a) where
  expr = F.eVar

instance F.Expression (Expression a) where
  expr (IntLit _ i)                 = F.expr i
  expr (VarRef _ x)                 = F.expr x
  expr (InfixExpr _ o e1 e2)        = F.EBin (bop o) (F.expr e1) (F.expr e2)
  expr (PrefixExpr _ PrefixMinus e) = F.EBin F.Minus (F.expr (0 :: Int)) (F.expr e)
  expr (Cast_ _ e)                  = F.expr e
  expr (Cast  _ e)                  = F.expr e
  expr e                            = convertError "F.Expr" e

instance F.Predicate  (Expression a) where
  prop (BoolLit _ True)            = F.PTrue
  prop (BoolLit _ False)           = F.PFalse
  prop (PrefixExpr _ PrefixLNot e) = F.PNot (F.prop e)
  prop e@(InfixExpr _ _ _ _ )      = eProp e
  prop e                           = convertError "F.Pred" e


------------------------------------------------------------------
eProp :: Expression a -> F.Pred
------------------------------------------------------------------

eProp (InfixExpr _ OpLT   e1 e2)       = F.PAtom F.Lt (F.expr e1) (F.expr e2)
eProp (InfixExpr _ OpLEq  e1 e2)       = F.PAtom F.Le (F.expr e1) (F.expr e2)
eProp (InfixExpr _ OpGT   e1 e2)       = F.PAtom F.Gt (F.expr e1) (F.expr e2)
eProp (InfixExpr _ OpGEq  e1 e2)       = F.PAtom F.Ge (F.expr e1) (F.expr e2)
-- FIXME:
-- eProp (InfixExpr _ OpEq   e1 e2)       = F.PAtom F.Eq (F.expr e1) (F.expr e2)
eProp (InfixExpr _ OpStrictEq   e1 e2) = F.PAtom F.Eq (F.expr e1) (F.expr e2)
eProp (InfixExpr _ OpNEq  e1 e2)       = F.PAtom F.Ne (F.expr e1) (F.expr e2)
eProp (InfixExpr _ OpLAnd e1 e2)       = pAnd (F.prop e1) (F.prop e2)
eProp (InfixExpr _ OpLOr  e1 e2)       = pOr  (F.prop e1) (F.prop e2)
eProp e                                = convertError "InfixExpr -> F.Prop" e

------------------------------------------------------------------
bop       :: InfixOp -> F.Bop
------------------------------------------------------------------

bop OpSub = F.Minus
bop OpAdd = F.Plus
bop OpMul = F.Times
bop OpDiv = F.Div
bop OpMod = F.Mod
bop o     = convertError "F.Bop" o

------------------------------------------------------------------
pAnd p q  = F.pAnd [p, q]
pOr  p q  = F.pOr  [p, q]



------------------------------------------------------------------------
-- | Embedding Values as RefTypes
------------------------------------------------------------------------

class RefTypable a where
  rType :: a -> RefType

instance RefTypable Type where
  rType = ofType

instance RefTypable RefType where
  rType = ofType . toType           -- removes all refinements

eSingleton      :: (F.Expression e) => RefType -> e -> RefType
eSingleton t e  = t `strengthen` (F.uexprReft e)


pSingleton      :: (F.Predicate p) => RefType -> p -> RefType
pSingleton t p  = t `strengthen` (F.propReft p)

------------------------------------------------------------------------------
-- | Converting RType to Fixpoint
------------------------------------------------------------------------------

rTypeSortedReft   ::  F.Reftable r => RTypeQ q r -> F.SortedReft
rTypeSortedReft t = F.RR (rTypeSort t) (rTypeReft t)

rTypeReft         :: (F.Reftable r) => RTypeQ q r -> F.Reft
rTypeReft         = fromMaybe fTop . fmap F.toReft . stripRTypeBase

rTypeValueVar     :: (F.Reftable r) => RTypeQ q r -> F.Symbol
rTypeValueVar t   = vv where F.Reft (vv,_) = rTypeReft t

------------------------------------------------------------------------------------------
rTypeSort :: F.Reftable r => RTypeQ q r -> F.Sort
------------------------------------------------------------------------------------------
rTypeSort (TVar α _)          = F.FObj $ F.symbol α
rTypeSort (TAll v t)          = rTypeSortForAll $ TAll v t
rTypeSort (TFun xts t _)      = F.FFunc 0 $ rTypeSort <$> (b_type <$> xts) ++ [t]
rTypeSort (TPrim c _)         = rTypeSortPrim c
rTypeSort (TRef (Gen n ts) _) = F.FApp (rawStringFTycon $ F.symbol n) (rTypeSort <$> ts)
rTypeSort (TObj _ _ )         = F.FApp (rawStringFTycon $ F.symbol "Object") []
rTypeSort (TType k _)         = F.FApp (rawStringFTycon $ F.symbol k ) []
rTypeSort (TMod _)            = F.FApp (rawStringFTycon $ F.symbol "module") []
rTypeSort t                   = error $ render $ text "BUG: Unsupported in rTypeSort"

rTypeSortPrim TBV32      = BV.mkSort BV.S32
rTypeSortPrim TNumber    = F.intSort
rTypeSortPrim TString    = F.strSort
rTypeSortPrim TBoolean   = F.FApp (rawStringFTycon "boolean") []
rTypeSortPrim TVoid      = F.FApp (rawStringFTycon "void") []
rTypeSortPrim TTop       = F.FApp (rawStringFTycon "top") []
rTypeSortPrim TNull      = F.FApp (rawStringFTycon "null") []
rTypeSortPrim TUndefined = F.FApp (rawStringFTycon "undefined") []
-- rTypeSortPrim TFPBool _ = F.boolSort
rTypeSortPrim c          = error $ "impossible: rTypeSortPrim " ++ show c

rTypeSortForAll t        = genSort n θ $ rTypeSort tbody
  where
    (αs, tbody)          = bkAll t
    n                    = length αs
    θ                    = HM.fromList $ zip (F.symbol <$> αs) (F.FVar <$> [0..])

genSort n θ (F.FFunc _ t) = F.FFunc n (F.sortSubst θ <$> t)
genSort n θ t             = F.FFunc n [F.sortSubst θ t]

------------------------------------------------------------------------------------------
stripRTypeBase :: RTypeQ q r -> Maybe r
------------------------------------------------------------------------------------------
stripRTypeBase (TPrim _ r)  = Just r
stripRTypeBase (TRef _ r)   = Just r
stripRTypeBase (TVar _ r)   = Just r
stripRTypeBase (TFun _ _ r) = Just r
stripRTypeBase (TObj _ r)   = Just r
stripRTypeBase _            = Nothing

------------------------------------------------------------------------------------------
noKVars :: F.Reft -> F.Reft
------------------------------------------------------------------------------------------
noKVars (F.Reft (x, F.Refa p)) = F.Reft (x, F.Refa $ dropKs p)
  where
    dropKs                     = F.pAnd . filter (not . isK) . F.conjuncts
    isK (F.PKVar {})           = True
    isK _                      = False


------------------------------------------------------------------------------------------
-- | Substitutions
------------------------------------------------------------------------------------------

instance (PPR r, F.Subable r) => F.Subable (RTypeQ q r) where
  syms        = foldReft (\r acc -> F.syms r ++ acc) []
  substa      = fmap . F.substa
  substf f    = emapReft (F.substf . F.substfExcept f) []
  subst su    = emapReft (F.subst  . F.substExcept su) []
  subst1 t su = emapReft (\xs r -> F.subst1Except xs r su) [] t

------------------------------------------------------------------------------------------
-- | Traversals over @RType@
------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------
emapReft  :: PPR r => ([F.Symbol] -> r -> r') -> [F.Symbol] -> RTypeQ q r -> RTypeQ q r'
------------------------------------------------------------------------------------------
emapReft f γ (TVar α r)     = TVar α (f γ r)
emapReft f γ (TPrim c r)    = TPrim c (f γ r)
emapReft f γ (TRef n r)     = TRef (emapReftGen f γ n) (f γ r)
emapReft f γ (TAll α t)     = TAll (emapReftBTV f γ α) (emapReft f γ t)
emapReft f γ (TFun xts t r) = TFun (emapReftBind f γ' <$> xts)
                                   (emapReft f γ' t) (f γ r)
                              where γ' = (b_sym <$> xts) ++ γ
emapReft f γ (TObj xts r)   = TObj (emapReftTM f γ xts) (f γ r)
emapReft f γ (TType k n)    = TType k (emapReftBGen f γ n)
emapReft _ _ (TMod m)       = TMod m
emapReft f γ (TOr ts)       = TOr (emapReft f γ <$> ts)
emapReft f γ (TAnd ts)      = TAnd (emapReft f γ <$> ts)
emapReft _ _ _              = error "Not supported in emapReft"

emapReftBTV f γ (BTV s l c) = BTV s l $ emapReft f γ <$> c
emapReftGen f γ (Gen n ts)  = Gen n $ emapReft f γ <$> ts
emapReftBGen f γ (BGen n ts) = BGen n $ emapReftBTV f γ <$> ts
emapReftBind f γ (B x t)    = B x $ emapReft f γ t
emapReftTM f γ (TM p m sp sm c k s n)
  = TM (fmap (emapReftFI f γ) p)
       (fmap (emapReftMI f γ) m)
       (fmap (emapReftFI f γ) sp)
       (fmap (emapReftMI f γ) sm)
       (emapReft f γ <$> c)
       (emapReft f γ <$> k)
       (emapReft f γ <$> s)
       (emapReft f γ <$> n)

emapReftFI f γ (FI m t1 t2) = FI m (emapReft f γ t1) (emapReft f γ t2)
emapReftMI f γ (MI m n  t2) = MI m n (emapReft f γ t2)

------------------------------------------------------------------------------------------
mapReftM :: (F.Reftable r, PP r, Applicative f, Monad f)
         => (r -> f r') -> RTypeQ q r -> f (RTypeQ q r')
------------------------------------------------------------------------------------------
mapReftM f (TVar α r)      = TVar α <$> f r
mapReftM f (TPrim c r)     = TPrim c <$> f r
mapReftM f (TRef n r)      = TRef <$> mapReftGenM f n <*> f r
mapReftM f (TFun xts t r)  = TFun <$> mapM (mapReftBindM f) xts <*> mapReftM f t <*> f r
mapReftM f (TAll α t)      = TAll <$> mapReftBTV f α <*> mapReftM f t
mapReftM f (TAnd ts)       = TAnd <$> mapM (mapReftM f) ts
mapReftM f (TOr ts)        = TOr <$> mapM (mapReftM f) ts
mapReftM f (TObj xts r)    = TObj <$> mapReftTM f xts <*> f r
mapReftM f (TType k n)     = TType k <$> mapReftBGenM f n
mapReftM _ (TMod a)        = return $ TMod a
mapReftM _ t               = error $ render $ text "Not supported in mapReftM: " <+> pp t

mapReftBTV f (BTV s l c)   = BTV s l <$> T.mapM (mapReftM f) c
mapReftGenM f (Gen n ts)   = Gen n <$> mapM (mapReftM f) ts
mapReftBGenM f (BGen n ts) = BGen n <$> mapM (mapReftBTV f) ts
mapReftBindM f (B x t)     = B x <$> mapReftM f t
mapReftTM f (TM p m sp sm c k s n)
  = TM <$> T.mapM (mapReftFI f) p
       <*> T.mapM (mapReftMI f) m
       <*> T.mapM (mapReftFI f) sp
       <*> T.mapM (mapReftMI f) sm
       <*> T.mapM (mapReftM f) c
       <*> T.mapM (mapReftM f) k
       <*> T.mapM (mapReftM f) s
       <*> T.mapM (mapReftM f) n

mapReftFI f (FI m t1 t2) = FI m <$> mapReftM f t1 <*> mapReftM f t2
mapReftMI f (MI m n t2) = MI m n <$> mapReftM f t2


------------------------------------------------------------------------------------------
-- | fold over @RType@
------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------
foldReft  :: PPR r => (r -> a -> a) -> a -> RTypeQ q r -> a
------------------------------------------------------------------------------------------
foldReft  f = efoldReft (\_ -> ()) (\_ -> f) F.emptySEnv

------------------------------------------------------------------------------------------
efoldReft :: PPR r => (RTypeQ q r -> b) -> (F.SEnv b -> r -> a -> a)
                   -> F.SEnv b -> a -> RTypeQ q r -> a
------------------------------------------------------------------------------------------
efoldReft g f = go
  where
    go γ z (TVar _ r)     = f γ r z
    go γ z (TPrim p r)    = f γ r z
    go γ z (TRef n r)     = f γ r $ gos γ z $ g_args n
    go γ z (TAll _ t)     = go γ z t
    go γ z (TFun xts t r) = f γ r $ go γ' (gos γ' z $ map b_type xts) t
                            where γ' = foldr (efoldExt g) γ xts
    go γ z (TAnd ts)      = gos γ z ts
    go γ z (TObj xts r)   = f γ r $ efoldTypeMembers g f xts γ z
    go γ z (TType _ n)    = gos γ z $ catMaybes $ btv_constr <$> b_args n
    go _ z (TMod _)       = z
    go _ _ t              = error $ "UNIMPLEMENTED[efoldReft]: " ++ ppshow t

    gos γ z ts            = L.foldl' (go γ) z ts

efoldExt g xt γ           = F.insertSEnv (b_sym xt) (g $ b_type xt) γ

-- TODO: add immutable field bindings ???
efoldTypeMembers g f (TM p m sp sm c k s n) γ z =
    L.foldl' (efoldReft g f γ) z $ pl ++ ml ++ spl ++ sml ++ cl ++ kl ++ sl ++ nl
  where
    pl  = L.foldr (\(_, FI _ t t') -> ([t,t'] ++) ) [] $ F.toListSEnv p
    ml  = L.foldr (\(_, MI _ _ t ) -> ([t]    ++) ) [] $ F.toListSEnv m
    spl = L.foldr (\(_, FI _ t t') -> ([t,t'] ++) ) [] $ F.toListSEnv sp
    sml = L.foldr (\(_, MI _ _ t ) -> ([t]    ++) ) [] $ F.toListSEnv sm
    cl  = maybeToList c
    kl  = maybeToList k
    sl  = maybeToList s
    nl  = maybeToList n


-- XXX: TODO Restore
------------------------------------------------------------------------------------------
efoldRType :: PPR r
           => (RTypeQ q r -> b) -> (F.SEnv b -> RTypeQ q r -> a -> a)
           -> F.SEnv b -> a -> RTypeQ q r -> a
------------------------------------------------------------------------------------------
efoldRType = undefined
-- efoldRType g f                 = go
--   where
--     go γ z t@(TVar _ _ )       = f γ t z
--     go γ z t@(TApp _ ts _)     = f γ t $ gos (efoldExt g (B (rTypeValueVar t) t) γ) z ts
--     go γ z t@(TRef _ ts _)     = f γ t $ gos (efoldExt g (B (rTypeValueVar t) t) γ) z ts
--     go γ z t@(TSelf m)         = f γ t $ go γ z m
--     go γ z t@(TAll _ t1)       = f γ t $ go γ z t1
--     go γ z t@(TFun s xts t1 _) = f γ t $ go γ' (gos γ' z (maybeToList s ++ map b_type xts)) t1
--                                    where γ' = foldr (efoldExt g) γ $ maybeToList thisBOpt ++ xts
--                                          thisBOpt = (B $ F.symbol $ builtinOpId BIThis) <$> s
--
--     go γ z   (TAnd ts)         = gos γ z ts
--
--     go γ z t@(TCons _ bs _ )   = f γ' t $ gos γ' z (f_type <$> M.elems bs)
--                                    where γ' = M.foldr (efoldExt' g) γ bs
--
--     go γ z t@(TClass _)        = f γ t z
--     go γ z t@(TModule _)       = f γ t z
--     go γ z t@(TEnum _)         = f γ t z
--     go _ _ t                   = error $ "Not supported in efoldRType: " ++ ppshow t
--
--     gos γ z ts                 = L.foldl' (go γ) z ts
--

------------------------------------------------------------------------------------------
isTrivialRefType :: RefType -> Bool
------------------------------------------------------------------------------------------
-- | The only allowed top-level refinement of a function type is the
--   ('function') tag, So ignore this for this check.
isTrivialRefType (TFun a b _) = isTrivialRefType' (TFun a b fTop)
isTrivialRefType t            = isTrivialRefType' t

isTrivialRefType' :: RefType -> Bool
isTrivialRefType' = foldReft (\r -> (f r &&)) True
  where
    f :: F.Reft -> Bool
    f = F.isTauto -- (F.Reft (_,ras)) = null ras

rawStringSymbol = F.locAt "RSC.Types.rawStringSymbol"
                . F.symbol

rawStringFTycon = F.symbolFTycon
                . F.locAt "RSC.Types.rawStringFTycon"
                . F.symbol



-----------------------------------------------------------------------------------
-- | Helpers for extracting specifications from @Nano@ @Statement@
-----------------------------------------------------------------------------------

getInvariant :: Statement a -> F.Pred

getInvariant = getSpec getInv . flattenStmt

getAssume    :: Statement a -> Maybe F.Pred
getAssume    = getStatementPred "assume"

getAssert    :: Statement a -> Maybe F.Pred
getAssert    = getStatementPred "assert"

getRequires  = getStatementPred "requires"
getEnsures   = getStatementPred "ensures"
getInv       = getStatementPred "invariant"

getStatementPred :: String -> Statement a -> Maybe F.Pred
getStatementPred name (ExprStmt _ (CallExpr _ (VarRef _ (Id _ f)) [p]))
  | name == f
  = Just $ F.prop p
getStatementPred _ _
  = Nothing

getSpec   :: (Statement a -> Maybe F.Pred) -> [Statement a] -> F.Pred
getSpec g = mconcat . catMaybes . map g

getFunctionIds :: Statement a -> [Id a]
getFunctionIds s = [f | (FunctionStmt _ f _ _) <- flattenStmt s]



normalize t
  | TPrim _ _ <- t = t
  | TVar _ _  <- t = t
--  | TOr ts    <- t = TOr $ unionCheck $ normalize <$> ts

--------------------------------------------------------------------------------
unionCheck :: IsLocated l => l -> CGEnv -> RefType -> [RefType] -> Either [Error] [RefType]
--------------------------------------------------------------------------------
unionCheck l γ t ts
  | not $ null samePrims = Left $ uncurry (errorUnionMergePrims l t) <$> samePrims
  | not $ null sameVars  = Left $ uncurry (errorUnionMergeVars l t) <$> sameVars
  | not $ null sameAnds  = Left $ uncurry (errorUnionMergeAnds l t) <$> sameAnds
  | not $ null sameObjs  = Left $ uncurry (errorUnionMergeObjs l t) <$> sameObjs
  | not $ null sameTys   = Left $ uncurry (errorUnionMergeTys l t) <$> sameTys
  | not $ null sameMods  = Left $ uncurry (errorUnionMergeMods l t) <$> sameMods
  | length alls > 0      = Left [ errorUnionMergeAlls l t ]
  | length funs > 0      = Left [ errorUnionMergeFuns l t ]
  | length exps > 0      = Left [ bugUnionMergeExps l t ]
  | otherwise            = Right $ prims ++ vars ++ ands ++ refs ++ objs ++ tys ++ mods ++ funs

  where
    sub   = isSubtype γ
    -- no unions here
    prims = [ t | t@(TPrim _ _ ) <- ts ]
    vars  = [ t | t@(TVar _ _  ) <- ts ]
    ands  = [ t | t@(TAnd _    ) <- ts ]
    refs  = [ t | t@(TRef _ _  ) <- ts ]
    objs  = [ t | t@(TObj _ _  ) <- ts ]
    tys   = [ t | t@(TType _ _ ) <- ts ]
    mods  = [ t | t@(TMod _    ) <- ts ]
    alls  = [ t | t@(TAll _ _  ) <- ts ]
    funs  = [ t | t@(TFun _ _ _) <- ts ]
    exps  = [ t | t@(TExp _    ) <- ts ]

    iprims = zip [0..] prims
    samePrims = [ (t1, t2) | (i1, t1@(TPrim p1 _)) <- iprims, (i2, t2@(TPrim p2 _)) <- iprims, p1 == p2, i1 /= i2 ]

    ivars = zip [0..] vars
    sameVars = [ (t1, t2) | (i1, t1@(TVar v1 _)) <- ivars, (i2, t2@(TVar v2 _)) <- ivars, v1 == v2, i1 /= i2 ]

    iands = zip [0..] ands
    sameAnds = [ (t1, t2) | (i1, t1@(TAnd _)) <- iands, (i2, t2@(TAnd _)) <- iands, i1 /= i2 ]

    iobjs = zip [0..] $ refs ++ objs
    sameObjs = [ (t1, t2) | (i1, t1) <- iobjs, (i2, t2) <- iobjs, i1 /= i2, t1 `sub` t2 || t2 `sub` t1 ]

    itys = zip [0..] tys
    sameTys = [ (t1, t2) | (i1, t1@(TType k1 n1)) <- itys, (i2, t2@(TType k2 n2)) <- itys
                          , i1 /= i2, k1 == k2, t1 `sub` t2 || t2 `sub` t1 ]

    imods = zip [0..] mods
    sameMods = [ (t1, t2) | (i1, t1@(TMod m1)) <- imods, (i2, t2@(TMod m2)) <- imods, i1 /= i2, m1 == m2 ]



--
-- -- | @zipType@ returns a type that is:
-- --
-- --  * semantically equivalent to @t1@
-- --
-- --  * structurally equivalent to @t2@
-- --
-- --------------------------------------------------------------------------------
-- zipType :: (F.Symbolic x, IsLocated x) => CGEnv -> x -> RefType -> RefType -> Maybe RefType
-- --------------------------------------------------------------------------------
-- --
-- -- Top type in LHS
-- --
-- zipType _ _ _ t2 | isTTop t2 = return t2
-- --
-- -- Unions
-- --
-- zipType γ _ (TOr t1s) (TOr t2s)  = mkUnionR <$> mapM rr t2s
--   where
--     rr               t2 = L.find (related γ t2) t1s `relate` t2
--     relate (Just t1) _  = return $ t1 `strengthen` noKVars r1
--     relate Nothing   t2 = return $ fmap F.bot t2
--
-- zipType γ x t1 t2@(TOr _) = zipType γ x (TOr [t1]) t2
--
-- zipType γ x (TOr t1s) t2 = L.find (related γ t2) t1s `relate` t2
--   where
--     relate (Just t1) t2 = zipType γ x (t1 `strengthen` noKVars r1) t2
--     relate Nothing   t2 = return (setRTypeR t, fromMaybe fTop $ rTypeROpt t) where t = fmap F.bot t2
--
-- --
-- -- No unions below this point
-- --
-- -- Class/interface types
-- --
-- --   C<Vi> extends C'<Wi>
-- --   --------------------------------
-- --   C<Si> || C'<Ti> = C'<Wi[Vi/Si]>
-- --
-- --   C </: C'
-- --   ------------------------------------------------------
-- --   C<Si> || C'<Ti> = toStruct(C<Si>) || toStruct(C<Ti>)
-- --
-- --
-- --   C<Si> || {F;M} = toStruct(C<Si>) || {F;M}
-- --
-- zipType γ x t1@(TRef x1 (m1:t1s) r1) t2@(TRef x2 (m2:t2s) _)
--   | x1 == x2
--   = do  ts    <- zipWithM (\t t' -> appZ <$> zipType γ x t t') t1s t2s
--         return $ (TRef x2 (m2:ts), r1)
--   --
--   --    t1 <: t2
--   --
--   | Just (_, m1':t1s') <- weaken γ (x1,m1:t1s) x2
--   = zipType γ x (TRef x2 (m1':t1s') r1 `strengthen` rtExt t1 (F.symbol x1)) t2
--   --
--   --    t2 <: t1
--   --
--   --    Only support this limited case with NO type arguments
--   --
--   | Just (_, [m2']) <- weaken γ (x2,m2:t2s) x1
--   = return (TRef x2 [m2'], r1)
--
--   -- DO NOT UNFOLD TO STRUCTURES
--   --
--   -- DANGER OF INFINITE LOOPS BECAUSE OF RECURSIVE TYPES
--   --
--   where
--     rtExt t c           = F.reft (vv t) (raExt t c)
--     raExt t c           = F.PBexp $ F.EApp (sym t) [F.expr $ vv t, F.expr $ F.symbolText c]
--     vv                  = rTypeValueVar
--     sym t
--       | isClassType γ t = F.dummyLoc $ F.symbol "extends_class"
--       | otherwise       = F.dummyLoc $ F.symbol "extends_interface"
--
-- zipType _ _ t1@(TRef _ [] _) _ = error $ "zipType on " ++ ppshow t1  -- Invalid type
-- zipType _ _ _ t2@(TRef _ [] _) = error $ "zipType on " ++ ppshow t2  -- Invalid type
--
-- zipType γ x t1@(TRef _ _ _) t2 = do t1' <- expandTypeWithSub γ x t1
--                                     t2' <- expandTypeWithSub γ x t2
--                                     zipType γ x t1' t2'
--
-- zipType γ x t1 t2@(TRef _ _ _) = do t1' <- expandTypeWithSub γ x t1
--                                     t2' <- expandTypeWithSub γ x t2
--                                     zipType γ x t1' t2'
--
-- zipType γ x t1@(TClass x1) t2@(TClass x2)
--   | x2 `elem` allAncestors γ x1
--   = return $ (setRTypeR (TClass x2), fTop)
--   | otherwise
--   = do  t1' <- expandTypeWithSub γ x t1
--         t2' <- expandTypeWithSub γ x t2
--         zipType γ x t1' t2'
--
-- zipType γ x t1@(TClass _) t2 = do t1' <- expandTypeWithSub γ x t1
--                                   zipType γ x t1' t2
--
-- zipType γ x t1 t2@(TClass _) = do t2' <- expandTypeWithSub γ x t2
--                                   zipType γ x t1 t2'
--
-- zipType _ _ (TApp c [] r) (TApp c' [] _) | c == c'  = return (TApp c [], r)
--
-- zipType _ _ (TVar v r) (TVar v' _)       | v == v'  = return (TVar v, r)
-- --
-- -- Function types
-- --
-- -- (Si) => S # (Ti) => T  =  (Si#Ti) => S#T
-- --
-- -- * Keep the LHS binders as they might appear in the
-- --   refinements which will belong to the LHS
-- --
-- zipType γ x (TFun (Just s1) x1s t1 r1) (TFun (Just s2) x2s t2 _) = do
--   s   <- appZ <$> zipType γ x s1 s2
--   xs  <- zipWithM (zipBind γ x) x1s x2s
--   t   <- appZ <$> zipType γ x t1 t2
--   return (TFun (Just s) xs t, r1)
--
-- zipType γ x (TFun Nothing x1s t1 r1) (TFun Nothing x2s t2 _) = do
--   xs  <- zipWithM (zipBind γ x) x1s x2s
--   t   <- appZ <$> zipType γ x t1 t2
--   return (TFun Nothing xs t, r1)
--
-- zipType _ _ (TFun _ _ _ _ ) (TFun _ _ _ _) = Nothing
--
-- -- Object types
-- --
-- -- { F1,F2 } | { F1',F3' } = { F1|F1',top(F3) }, where disjoint F2 F3'
-- --
-- zipType γ x (TCons _ e1s r1) (TCons m2 e2s _) = do
--     common'            <- T.mapM (uncurry $ zipElts γ x) common
--     return              $ (TCons m2 (common' `M.union` disjoint'), r1)
--   where
--     common              = M.intersectionWith (,) e1s e2s
--     disjoint'           = M.map (fmap $ const fTop) $ e2s `M.difference` e1s
-- --
-- -- Intersection types
-- --
-- -- s1 /\ s2 .. /\ sn | t1 /\ t2 .. tm = s1'|t1' /\ .. sk'|tk' /\ .. top(tm')
-- --
-- zipType γ x (TAnd t1s) (TAnd t2s) =
--     case [ (pick t2, t2) | t2 <- t2s ] of
--       []              -> error $ "zipType: impossible intersection types"
--       [(t1,t2)]       -> zipType γ x t1 t2
--       ts              -> do ts' <- mapM (\(t,t') -> appZ <$> zipType γ x t t') ts
--                             return (setRTypeR $ TAnd ts', fTop)
--   where
--     pick t2            = fromCandidates [ t1 | t1 <- t1s, t1 `matches` t2 ]
--     t `matches` t'     = isSubtype γ t t' || isSubtype γ t' t
--
--     fromCandidates [ ] = die $ bug (srcPos x)
--                        $ "zipType: cannot match " ++ ppshow t2s ++ " with any of " ++ ppshow t1s
--     fromCandidates [t] = t
--     fromCandidates  _  = die $ bug (srcPos x)
--                        $ "zipType: multiple matches of " ++ ppshow t2s ++ " with " ++ ppshow t1s
--
-- zipType γ x t1 (TAnd t2s) = zipType γ x (TAnd [t1]) (TAnd t2s)
-- zipType γ x (TAnd t1s) t2 = zipType γ x (TAnd t1s) (TAnd [t2])
--
-- -- FIXME: preserve t1's ref in all occurences of TVar v1 in the LHS
-- zipType γ x (TAll v1 t1) (TAll v2 t2) =
--     (,fTop) . setRTypeR . TAll v1 . appZ <$> zipType γ x t1 (apply θ t2)
--   where
--     θ = fromList [(v2, tVar v1 :: RefType)]
--
-- zipType _ x t1 t2 = errorstar $ printf "BUG[zipType] Unsupported:\n\t%s\n\t%s\nand\n\t%s"
--                         (ppshow $ srcPos x) (ppshow t1) (ppshow t2)
--
--
-- zipBind γ x (B s1 t1) (B _ t2) = B s1 <$> appZ <$> zipType γ x t1 t2
--
-- ------------------------------------------------------------------------------------------
-- zipElts :: (F.Symbolic x, IsLocated x)
--         => CGEnv -> x -> TypeMember F.Reft -> TypeMember F.Reft -> Maybe (TypeMember F.Reft)
-- ------------------------------------------------------------------------------------------
-- zipElts γ x (CallSig t1)        (CallSig t2)           = CallSig           <$> appZ <$> zipType γ x t1 t2
-- zipElts γ x (ConsSig t1)        (ConsSig t2)           = ConsSig           <$> appZ <$> zipType γ x t1 t2
-- zipElts γ x (IndexSig _ _ t1)   (IndexSig x2 b2 t2)    = IndexSig x2 b2    <$> appZ <$> zipType γ x t1 t2
-- zipElts γ x (FieldSig _ _ _ t1) (FieldSig x2 o2 m2 t2) = FieldSig x2 o2 m2 <$> appZ <$> zipType γ x t1 t2
-- zipElts γ x (MethSig _  t1)     (MethSig x2 t2)        = MethSig  x2       <$> appZ <$> zipType γ x t1 t2
-- zipElts _ _ _                   _                      = Nothing
--
-- appZ (f,r) = f r
--
-- expandTypeWithSub g x t = substThis' g (x,t) <$> expandType Coercive g t
--
-- substThis x t         = F.subst (F.mkSubst [(thisSym,F.expr x)]) t
--
-- substOffsetThis = emapReft (\_ -> V.trans vs () ()) []
--   where
--     vs     = V.defaultVisitor { V.txExpr = tx }
--     tx _ (F.EApp o [ F.EVar x, F.ESym (F.SL f) ])
--            | F.symbol o == offsetSym, F.symbol x == thisSym
--            = F.eVar f
--     tx _ e = e
--
--
-- -- | Substitute occurences of 'this' in type @t'@, given that the receiver
-- --   object is bound to symbol @x@ and it has a type @t@ under @g@.
-- -------------------------------------------------------------------------------
-- substThis' :: (IsLocated a, F.Symbolic a)
--            => CGEnv -> (a, RefType) -> RefType -> RefType
-- -------------------------------------------------------------------------------
-- substThis' g (x,t) = F.subst su
--   where
--     su            = F.mkSubst $ (this, F.expr $ F.symbol x) : fieldSu
--     this          = F.symbol $ builtinOpId BIThis
--
--     fieldSu       | Just (TCons _ fs _) <- expandType Coercive g t
--                   = [ subPair f | ((f,InstanceMember), FieldSig _ _ m _) <- M.toList fs
--                                 , isImmutable m ]
--                   | otherwise
--                   = []
--     qFld x f      = F.qualifySymbol (F.symbol x) f
--     subPair f     = (qFld this f, F.expr $ qFld x f)
--
--
--
-- -- | Substitute occurences of 'this.f' in type @t'@, with 'f'
-- -------------------------------------------------------------------------------
-- unqualifyThis :: CGEnv -> RefType -> RefType -> RefType
-- -------------------------------------------------------------------------------
-- unqualifyThis g t = F.subst $ F.mkSubst fieldSu
--   where
--     fieldSu       | Just (TCons _ fs _) <- expandType Coercive g t
--                   = [ subPair f | ((f,InstanceMember), FieldSig _ _ m _) <- M.toList fs
--                                 , isImmutable m ]
--                   | otherwise
--                   = []
--     this          = F.symbol $ builtinOpId BIThis
--     qFld x f      = F.qualifySymbol (F.symbol x) f
--     subPair f     = (qFld this f, F.expr f)
--
--
-- -------------------------------------------------------------------------------
-- mkQualSym :: (F.Symbolic x, F.Symbolic f) => x -> f -> F.Symbol
-- -------------------------------------------------------------------------------
-- mkQualSym    x f = F.qualifySymbol (F.symbol x) (F.symbol f)
--
-- -------------------------------------------------------------------------------
-- mkOffset :: (F.Symbolic f, F.Expression x) => x -> f -> F.Expr
-- -------------------------------------------------------------------------------
-- mkOffset x f = F.EApp offsetLocSym [F.expr x, F.expr $ F.symbolText $ F.symbol f]
