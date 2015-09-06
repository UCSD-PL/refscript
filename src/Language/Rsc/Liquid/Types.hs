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
--   Likely mergeable with @Language.Rsc.Typecheck.Types@

module Language.Rsc.Liquid.Types (

  -- * Refinement Types
    RefType

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
  , foldReft, efoldReft, efoldRType
  , mapReftM, mapReftTM
  , mapTypeMembersM

  -- * Accessing Spec Annotations
  , getSpec, getRequires, getEnsures, getAssume, getAssert
  , getInvariant, getFunctionIds

  -- * Raw low-level Location-less constructors
  , rawStringSymbol

  -- * 'this' related substitutions
  , substThis
  -- , unqualifyThis
  , mkQualSym
  , mkOffset
  -- , substOffsetThis
  , mkCastFunTy

  ) where

import           Control.Applicative
import           Data.Default
import qualified Data.HashMap.Strict          as HM
import qualified Data.List                    as L
import           Data.Maybe                   (catMaybes, fromMaybe, maybeToList)
import           Data.Monoid                  (mconcat)
import qualified Data.Traversable             as T
import qualified Language.Fixpoint.Bitvector  as BV
import qualified Language.Fixpoint.Types      as F
import           Language.Rsc.AST
import           Language.Rsc.Names
import           Language.Rsc.Pretty
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
import           Text.PrettyPrint.HughesPJ

-------------------------------------------------------------------------------------
-- | Refinement Types and Annotations
-------------------------------------------------------------------------------------

type RefType     = RType F.Reft


------------------------------------------------------------------
-- | Converting `Rsc` values into `Fixpoint` values,
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
-- TODO:
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
rTypeSort (TClass _)          = F.FApp (rawStringFTycon $ F.symbol "class" ) []
rTypeSort (TMod _)            = F.FApp (rawStringFTycon $ F.symbol "module") []
rTypeSort _                   = error $ render $ text "BUG: Unsupported in rTypeSort"

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
emapReft f γ (TClass n)     = TClass (emapReftBGen f γ n)
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
mapReftM :: (F.Reftable r, PP r, Applicative m, Monad m)
         => (r -> m r') -> RTypeQ q r -> m (RTypeQ q r')
------------------------------------------------------------------------------------------
mapReftM f (TVar α r)      = TVar α <$> f r
mapReftM f (TPrim c r)     = TPrim c <$> f r
mapReftM f (TRef n r)      = TRef <$> mapReftGenM f n <*> f r
mapReftM f (TFun xts t r)  = TFun <$> mapM (mapReftBindM f) xts <*> mapReftM f t <*> f r
mapReftM f (TAll α t)      = TAll <$> mapReftBTV f α <*> mapReftM f t
mapReftM f (TAnd ts)       = TAnd <$> mapM (mapReftM f) ts
mapReftM f (TOr ts)        = TOr <$> mapM (mapReftM f) ts
mapReftM f (TObj xts r)    = TObj <$> mapReftTM f xts <*> f r
mapReftM f (TClass n)      = TClass <$> mapReftBGenM f n
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
    go γ z (TPrim _ r)    = f γ r z
    go γ z (TRef n r)     = f γ r $ gos γ z $ g_args n
    go γ z (TAll _ t)     = go γ z t
    go γ z (TFun xts t r) = f γ r $ go γ' (gos γ' z $ map b_type xts) t
                            where γ' = foldr (efoldExt g) γ xts
    go γ z (TAnd ts)      = gos γ z ts
    go γ z (TObj xts r)   = f γ r $ efoldReftTM g f xts γ z
    go γ z (TClass n)     = gos γ z $ catMaybes $ btv_constr <$> b_args n
    go _ z (TMod _)       = z
    go _ _ t              = error $ "UNIMPLEMENTED[efoldReft]: " ++ ppshow t

    gos γ z ts            = L.foldl' (go γ) z ts

efoldExt g xt γ           = F.insertSEnv (b_sym xt) (g $ b_type xt) γ

------------------------------------------------------------------------------------------
efoldReftTM :: PPR r => (RTypeQ q r -> b) -> (F.SEnv b -> r -> a -> a)
                     -> TypeMembersQ q r -> F.SEnv b -> a -> a
------------------------------------------------------------------------------------------
efoldReftTM g f (TM p m sp sm c k s n) γ z =
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

------------------------------------------------------------------------------------------
mapTypeMembersM :: (Applicative m, Monad m)
                => (RType r -> m (RType r)) -> TypeMembers r -> m (TypeMembers r)
------------------------------------------------------------------------------------------
mapTypeMembersM f (TM p m sp sm c k s n)
  = TM <$> T.mapM (mapFieldInfoM f) p
       <*> T.mapM (mapMethInfoM f) m
       <*> T.mapM (mapFieldInfoM f) sp
       <*> T.mapM (mapMethInfoM f) sm
       <*> T.mapM f c
       <*> T.mapM f k
       <*> T.mapM f s
       <*> T.mapM f n

mapFieldInfoM f (FI o m t) = FI o <$> f m <*> f t
mapMethInfoM  f (MI o m t) = MI o       m <$> f t

------------------------------------------------------------------------------------------
efoldRType :: PPR r => (RTypeQ q r -> b) -> (F.SEnv b -> RTypeQ q r -> a -> a)
                    -> F.SEnv b -> a -> RTypeQ q r -> a
------------------------------------------------------------------------------------------
efoldRType g f              = go
  where
    go γ z t@(TVar _ _)     = f γ t z
    go γ z t@(TPrim _ _)    = f γ t z
    go γ z t@(TRef n _)     = f γ t $ gos γ z $ g_args n
    go γ z t@(TAll _ t')    = f γ t $ go γ z t'
    go γ z t@(TFun xts τ _) = f γ t $ go γ' (gos γ' z $ map b_type xts) τ
                              where γ' = foldr (efoldExt g) γ xts
    go γ z t@(TAnd ts)      = f γ t $ gos γ z ts
    go γ z t@(TOr ts)       = f γ t $ gos γ z ts
    go γ z t@(TObj xts _)   = f γ t $ efoldRTypeTM g f xts γ z
    go γ z t@(TClass n)     = f γ t $ gos γ z $ catMaybes $ btv_constr <$> b_args n
    go γ z t@(TMod _)       = f γ t z
    go _ _ t              = error $ "UNIMPLEMENTED[efoldRType]: " ++ ppshow t
    gos γ z ts            = L.foldl' (go γ) z ts

------------------------------------------------------------------------------------------
efoldRTypeTM :: PPR r => (RTypeQ q r -> b) -> (F.SEnv b -> RTypeQ q r -> a -> a)
                      -> TypeMembersQ q r -> F.SEnv b -> a -> a
------------------------------------------------------------------------------------------
efoldRTypeTM g f (TM p m sp sm c k s n) γ z =
    L.foldl' (efoldRType g f γ) z $ pl ++ ml ++ spl ++ sml ++ cl ++ kl ++ sl ++ nl
  where
    pl  = L.foldr (\(_, FI _ t t') -> ([t,t'] ++) ) [] $ F.toListSEnv p
    ml  = L.foldr (\(_, MI _ _ t ) -> ([t]    ++) ) [] $ F.toListSEnv m
    spl = L.foldr (\(_, FI _ t t') -> ([t,t'] ++) ) [] $ F.toListSEnv sp
    sml = L.foldr (\(_, MI _ _ t ) -> ([t]    ++) ) [] $ F.toListSEnv sm
    cl  = maybeToList c
    kl  = maybeToList k
    sl  = maybeToList s
    nl  = maybeToList n


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
-- | Helpers for extracting specifications from @Rsc@ @Statement@
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



-- NEW -- --------------------------------------------------------------------------------
-- NEW -- unionCheck :: IsLocated l => l -> CGEnv -> RefType -> [RefType] -> Either [Error] [RefType]
-- NEW -- --------------------------------------------------------------------------------
-- NEW -- unionCheck l γ t ts
-- NEW --   | not $ null samePrims = Left $ uncurry (errorUnionMergePrims l t) <$> samePrims
-- NEW --   | not $ null sameVars  = Left $ uncurry (errorUnionMergeVars l t) <$> sameVars
-- NEW --   | not $ null sameAnds  = Left $ uncurry (errorUnionMergeAnds l t) <$> sameAnds
-- NEW --   | not $ null sameObjs  = Left $ uncurry (errorUnionMergeObjs l t) <$> sameObjs
-- NEW --   | not $ null sameTys   = Left $ uncurry (errorUnionMergeTys l t) <$> sameTys
-- NEW --   | not $ null sameMods  = Left $ uncurry (errorUnionMergeMods l t) <$> sameMods
-- NEW --   | length alls > 0      = Left [ errorUnionMergeAlls l t ]
-- NEW --   | length funs > 0      = Left [ errorUnionMergeFuns l t ]
-- NEW --   | length exps > 0      = Left [ bugUnionMergeExps l t ]
-- NEW --   | otherwise            = Right $ prims ++ vars ++ ands ++ refs ++ objs ++ tys ++ mods ++ funs
-- NEW --
-- NEW --   where
-- NEW --     sub   = isSubtype γ
-- NEW --     -- no unions here
-- NEW --     prims = [ t | t@(TPrim _ _ ) <- ts ]
-- NEW --     vars  = [ t | t@(TVar _ _  ) <- ts ]
-- NEW --     ands  = [ t | t@(TAnd _    ) <- ts ]
-- NEW --     refs  = [ t | t@(TRef _ _  ) <- ts ]
-- NEW --     objs  = [ t | t@(TObj _ _  ) <- ts ]
-- NEW --     tys   = [ t | t@(TClass _  ) <- ts ]
-- NEW --     mods  = [ t | t@(TMod _    ) <- ts ]
-- NEW --     alls  = [ t | t@(TAll _ _  ) <- ts ]
-- NEW --     funs  = [ t | t@(TFun _ _ _) <- ts ]
-- NEW --     exps  = [ t | t@(TExp _    ) <- ts ]
-- NEW --
-- NEW --     iprims = zip [0..] prims
-- NEW --     samePrims = [ (t1, t2) | (i1, t1@(TPrim p1 _)) <- iprims, (i2, t2@(TPrim p2 _)) <- iprims, p1 == p2, i1 /= i2 ]
-- NEW --
-- NEW --     ivars = zip [0..] vars
-- NEW --     sameVars = [ (t1, t2) | (i1, t1@(TVar v1 _)) <- ivars, (i2, t2@(TVar v2 _)) <- ivars, v1 == v2, i1 /= i2 ]
-- NEW --
-- NEW --     iands = zip [0..] ands
-- NEW --     sameAnds = [ (t1, t2) | (i1, t1@(TAnd _)) <- iands, (i2, t2@(TAnd _)) <- iands, i1 /= i2 ]
-- NEW --
-- NEW --     iobjs = zip [0..] $ refs ++ objs
-- NEW --     sameObjs = [ (t1, t2) | (i1, t1) <- iobjs, (i2, t2) <- iobjs, i1 /= i2, t1 `sub` t2 || t2 `sub` t1 ]
-- NEW --
-- NEW --     itys = zip [0..] tys
-- NEW --     sameTys = [ (t1, t2) | (i1, t1@(TClass n1)) <- itys, (i2, t2@(TClass n2)) <- itys
-- NEW --                           , i1 /= i2, t1 `sub` t2 || t2 `sub` t1 ]
-- NEW --
-- NEW --     imods = zip [0..] mods
-- NEW --     sameMods = [ (t1, t2) | (i1, t1@(TMod m1)) <- imods, (i2, t2@(TMod m2)) <- imods, i1 /= i2, m1 == m2 ]



-------------------------------------------------------------------------------
substThis :: (F.Expression x, F.Subable t) => x -> t -> t
-------------------------------------------------------------------------------
substThis x = F.subst (F.mkSubst [(thisSym,F.expr x)])

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


-------------------------------------------------------------------------------
mkQualSym :: (F.Symbolic x, F.Symbolic f) => x -> f -> F.Symbol
-------------------------------------------------------------------------------
mkQualSym    x f = F.qualifySymbol (F.symbol x) (F.symbol f)

-------------------------------------------------------------------------------
mkOffset :: (F.Symbolic f, F.Expression x) => x -> f -> F.Expr
-------------------------------------------------------------------------------
mkOffset x f = F.EApp offsetLocSym [F.expr x, F.expr $ F.symbolText $ F.symbol f]


-- | Cast function
--
---------------------------------------------------------------------------------
mkCastFunTy :: RefType -> RefType
---------------------------------------------------------------------------------
mkCastFunTy t
  = TAll (BTV a def (Just t))
  $ TFun [B x α] (α `strengthen` (F.uexprReft x))  fTop
  where
    a = F.symbol "A"
    α = TVar (TV a def) fTop
    x = F.symbol "x"
