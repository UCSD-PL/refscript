{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DoAndIfThenElse           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE IncoherentInstances       #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE LiberalTypeSynonyms       #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}


module Language.Rsc.Typecheck.Sub (
    isSubtype
  , isSubtypeWithUq
  , isSubtypeC
  , isConvertibleC
  , isUnique
  , convert
  , SubConfA(..), allowUniqueCfg, disallowUniqueCfg
  , SubtypingResult (..)
  , ConversionResult (..)
  ) where

import           Data.Monoid
import           Data.Tuple                     (swap)
import           Language.Fixpoint.Types        (SEnv, Symbol, differenceSEnv, intersectWithSEnv, toListSEnv)
import           Language.Fixpoint.Types.Errors
import           Language.Rsc.ClassHierarchy
import           Language.Rsc.Environment
import           Language.Rsc.Errors
import           Language.Rsc.Locations
import           Language.Rsc.Pretty
import           Language.Rsc.Program           (ExprSSAR)
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
import           Text.PrettyPrint.HughesPJ      (vcat, (<+>))

type FE g r = (CheckingEnvironment r g, Functor g)
type PPRE r = (ExprReftable Int r, PPR r)


--------------------------------------------------------------------------------
-- | Subtyping configuration
--------------------------------------------------------------------------------
data SubConfA a r = SC {
    allow_unique :: Bool            -- Allow conversion of `unique` to other permissions
  , sub_lhs      :: RType r         -- The LHS type
  , sub_rhs      :: RType r         -- The RHS type
  , sub_fld      :: Maybe (Symbol)  -- Last relevant field
  , sub_var      :: Maybe a         -- A relevant expression
}

type SubConf r = SubConfA (ExprSSAR r) r

allowUniqueCfg    c = c { allow_unique = True }
disallowUniqueCfg c = c { allow_unique = False }

--------------------------------------------------------------------------------
isSubtypeC     :: (PPRE r, FE g r) => g r -> SubConf r -> RType r -> RType r -> Bool
isConvertibleC :: (PPRE r, FE g r) => g r -> SubConf r -> RType r -> RType r -> Bool
--------------------------------------------------------------------------------
isSubtypeC γ c t1 t2 = subtype dummySpan γ c t1 t2 `elem` [EqT, SubT]

isSubtype       γ t1 t2 = isSubtypeC γ (SC False t1 t2 Nothing Nothing) t1 t2
isSubtypeWithUq γ t1 t2 = isSubtypeC γ (SC True  t1 t2 Nothing Nothing) t1 t2

isConvertibleC γ c t1 t2
  | isSubtypeC γ c t1 t2
  = True

isConvertibleC γ c (TOr t1s _) t2
  | any (\t1 -> isSubtypeC γ c t1 t2) t1s
  = True

isConvertibleC γ c t1@(TRef _ _) t2@(TRef _ _)
  | not (mutRelated t1), not (mutRelated t2)
  , isSubtypeC γ c t1 t2
  = True

isConvertibleC _ _ _ _
  = False


data ConversionResult =
    ConvOK
  | ConvWith Type
  | ConvFail [Error]

instance PP ConversionResult where
  pp ConvOK        = pp "ConvOK"
  pp (ConvWith t)  = pp "ConvWith" <+> pp t
  pp (ConvFail es) = pp "ConvFail" <+> vcat (map pp es)


data SubtypingResult  = EqT | SubT | NoSub [Error] deriving (Eq, Ord, Show)

instance PP SubtypingResult where
  pp EqT       = pp "equal types"
  pp SubT      = pp "subtypes"
  pp (NoSub _) = pp "no subtypes"

instance Monoid SubtypingResult where
  mempty                        = EqT
  mappend (NoSub e1) (NoSub e2) = NoSub $ e1 ++ e2
  mappend _          (NoSub e2) = NoSub e2
  mappend (NoSub e1) _          = NoSub e1
  mappend EqT        EqT        = EqT
  mappend _          _          = SubT

-- | convert: an "optimistic" version of subtyping that allows:
--
--   * Partial union subtyping
--
--   * Contra-variance at reference types
--
--------------------------------------------------------------------------------
convert
  :: (PPRE r, FE g r, IsLocated l)
  => l -> g r -> SubConf r -> RType r -> RType r -> ConversionResult
--------------------------------------------------------------------------------
convert l g c t1 t2
  -- = case ltracePP l (ppshow t1 ++ " <: " ++ ppshow t2) $ subtype l g t1 t2 of
  = case subtype l g c t1 t2 of
    EqT      -> ConvOK
    SubT     -> ConvOK -- ConvWith (toType t2)
    NoSub es -> castable l g c es t1 t2

castable _ γ c _ (TOr t1s _) t2
  | any (\t1 -> isSubtypeC γ c t1 t2) t1s
  = ConvWith (toType t2)

-- XXX: Only non generic casting allowed at the moment
castable _ γ c _ t1@(TRef (Gen _ [_]) _) t2@(TRef (Gen _ [_]) _)
  | not (mutRelated t1), not (mutRelated t2), isSubtypeC γ c t1 t2
  = ConvWith (toType t2)

castable _ _ _ es _ _ = ConvFail es

--------------------------------------------------------------------------------
subtype :: (PPRE r, FE g r, IsLocated l)
        => l -> g r -> SubConf r -> RType r -> RType r -> SubtypingResult
--------------------------------------------------------------------------------
-- | Type Variables
subtype _ _ _ (TVar v1 _) (TVar v2 _)
  | v1 == v2  = EqT

-- | Unfold bounded variables
subtype l γ c t1@(TVar _ _) t2
  | Just t1' <- envFindBoundOpt γ t1
  = subtype l γ c t1' t2

-- | Primitive types
subtype _ _ _ (TPrim c1 _) (TPrim c2 _)
  | c1 == c2   = EqT
  | c2 == TAny = SubT
  | c2 == TTop = SubT

-- | Unions
subtype _ γ c (TOr ts1 _) t2
  | all (\t1 -> isSubtypeC γ c t1 t2) ts1
  = SubT

subtype _ γ c t1 (TOr ts2 _)
  | any (\t2 -> isSubtypeC γ c t1 t2) ts2
  = SubT

-- | Objects
subtype l γ c t1 t2
  | maybeTObj t1, maybeTObj t2 = subtypeObj l γ c t1 t2

-- | Functions
subtype l γ _ t1 t2
  | isTFun t1, isTFun t2 = subtypeFun l γ t1 t2

-- | Rest (Fail)
subtype l _ _ t1 t2
  = NoSub [ errorSubtype l t1 t2 ]


-- | subtype with default configuration
subtype' l g t1 t2 = subtype l g (SC False t1 t2 Nothing Nothing) t1 t2


--------------------------------------------------------------------------------
subtypeObj :: (PPRE r, FE g r, IsLocated l)
           => l -> g r -> SubConf r -> RType r -> RType r -> SubtypingResult
--------------------------------------------------------------------------------
-- | Cannot convert a structural object type to a nominal class type.
--   Interfaces are OK.
--
subtypeObj l γ _ t1 t2
  | not (isClassType (envCHA γ) t1) && isClassType (envCHA γ) t2
  = NoSub [errorObjectType l t1 t2]

subtypeObj l γ c t1@(TObj m1 e1s _) t2@(TObj m2 e2s _)
  = subtype l γ c m1 m2 `mappend` subtypeObjMembers l γ c (t1, e1s) (t2, e2s)

-- | Mutability subtyping
--
subtypeObj l γ c t1@(TRef (Gen x1 []) _) t2@(TRef (Gen x2 []) _)
  | not (mutRelated t1) = NoSub [errorMutUnknown l t1]
  | not (mutRelated t2) = NoSub [errorMutUnknown l t2]
  | allow_unique c      = withUqEnabled
  | otherwise           = withUqDisabled
  where
    withUqEnabled  | isUQ t1   = EqT
                   | x1 == x2  = EqT
                   | x1 <: x2  = SubT
                   | isUQ t1   = EqT   -- Allow the `Unique` coercion
                   | otherwise = NoSub [errorUniqueRef l (sub_var c)]

    withUqDisabled | isUQ t1   = NoSub [errorUniqueRef l (sub_var c)]
                   | isUQ t2   = NoSub [errorUniqueRef l (sub_var c)]
                   | x1 == x2  = EqT
                   | x1 <: x2  = SubT
                   | otherwise = NoSub [errorIncompMutTy l t1 t2
                                  (sub_lhs c) (sub_rhs c) (sub_fld c)]

    a <: b = isAncestorOf (envCHA γ) b a

subtypeObj l _ _ t1@(TRef (Gen _ []) _) _
  = NoSub [bugMutPartInvalid l t1]

subtypeObj l _ _ _ t2@(TRef (Gen _ []) _)
  = NoSub [bugMutPartInvalid l t2]

-- | Type Reference subtyping
subtypeObj l γ c (TRef g1@(Gen x1 (m1:t1s)) _) (TRef (Gen x2 (m2:t2s)) _)
  = case subtype l γ c m1 m2 of
      EqT     -> checkBaseType
      SubT    -> checkBaseType
      NoSub e -> NoSub e
  where
    checkBaseType
      | x1 == x2
      = checkTypArgs
      | Just (Gen _ (_:t1s')) <- weaken (envCHA γ) g1 x2
      = mconcat $ SubT : zipWith (subtype' l γ) t1s' t2s
      | otherwise
      = NoSub [errorSubtype l x1 x2]

    checkTypArgs
      | isSubtype γ m2 tIM
      = mconcat $ zipWith (subtype' l γ) t1s t2s
      | otherwise
      = mconcat $ zipWith (subtype' l γ) t1s t2s
               ++ zipWith (subtype' l γ) t2s t1s

-- | Class type subtyping
--
subtypeObj l γ _ t1@(TClass (BGen c1 ts1)) t2@(TClass (BGen c2 ts2))
  | c1 == c2
  , and $ uncurry (isSubtype γ)        <$> ts
  , and $ uncurry (isSubtype γ) . swap <$> ts
  = EqT
  | otherwise
  = NoSub [errorSubtype l t1 t2]
  where
    ts  = [ (t, t') | (Just t, Just t') <- zip bs1 bs2 ]
    bs1 = btv_constr <$> ts1
    bs2 = btv_constr <$> ts2

-- | Module subtyping
subtypeObj l _ _ (TMod m1) (TMod m2)
  | m1 == m2  = EqT
  | otherwise = NoSub [errorSubtype l m1 m2]

-- Structural subtyping (fall-back)
--
subtypeObj l γ c t1 t2 =
  case (expandType NonCoercive (envCHA γ) t1, expandType NonCoercive (envCHA γ) t2) of
    (Just ft1, Just ft2) -> subtypeObj l γ c ft1 ft2
    (Nothing , Nothing ) -> NoSub [errorUnresolvedTypes l t1 t2]
    (Nothing , _       ) -> NoSub [errorNonObjectType l t1]
    (_       , Nothing ) -> NoSub [errorNonObjectType l t2]


-- | Subtyping type members
--
subtypeObjMembers l γ c (t1, TM m1 _ c1 k1 s1 n1) (t2, TM m2 _ c2 k2 s2 n2)
  = subtypeMems  l γ c (t1, m1) (t2, m2) <>
    subtypeCalls l γ t1 c1 t2 c2 <>
    subtypeCtors l γ t1 k1 t2 k2 <>
    subtypeSIdxs l γ t1 s1 t2 s2 <>
    subtypeNIdxs l γ t1 n1 t2 n2

--------------------------------------------------------------------------------
subtypeMems :: (IsLocated a, PPRE r, FE g r)
            => a -> g r -> SubConf r
            -> (RType r, SEnv (TypeMember r))
            -> (RType r, SEnv (TypeMember r))
            -> SubtypingResult
--------------------------------------------------------------------------------
subtypeMems l γ c (ot1, mems1) (ot2, mems2)

  -- Props from `mems2` are missing from `mems1` - fail width subtyping
  --
  | not (null diff21)
  = NoSub [ errorObjSubtype l ot1 ot2 (map fst diff21) ]

  -- Descend into property subtyping
  --
  | otherwise
  = mconcat $ map subtypeMem common

  where
    diff21 = filter (isReqMember . snd)
           $ toListSEnv $ mems2 `differenceSEnv` mems1
    common = toListSEnv (intersectWithSEnv (,) mems1 mems2)

    subtypeMem (f, (FI _ o1 m1 t1, FI _ o2 m2 t2))
      | (o1, o2) == (Opt, Req)  -- Optionality check
      = NoSub [ errorIncompatOptional (srcPos l) f ]
      | otherwise
      = subtype l γ c' m1 m2 <> deepSub m1 t1 t2
      where
        c' = c { sub_fld = Just f }

    subtypeMem (_, (m1, m2))
      = NoSub [ unsupportedMethodComp (srcPos l) m1 m2 ]

    deepSub m1 t1 t2
      | isSubtype γ m1 tMU = subtype' l γ t1 t2 <> subtype' l γ t2 t1
      | otherwise          = subtype' l γ t1 t2


compareMaybe l γ f _ _ (Just c1) _ (Just c2) = f l γ c1 c2
compareMaybe _ _ _ _ _ Nothing   _ Nothing   = EqT
compareMaybe _ _ _ _ _ _         _ Nothing   = SubT
compareMaybe l _ _ e _ t1        _ t2        = NoSub [e (srcPos l) t1 t2]

subtypeCalls l γ = compareMaybe l γ subtypeFun errorIncompCallSigs
subtypeCtors l γ = compareMaybe l γ subtype'   errorIncompCtorSigs
subtypeSIdxs l γ = compareMaybe l γ subtype'   errorIncompSIdxSigs
subtypeNIdxs l γ = compareMaybe l γ subtype'   errorIncompNIdxSigs

--------------------------------------------------------------------------------
subtypeFun :: (PPRE r, FE g r, IsLocated l)
           => l -> g r -> RType r -> RType r -> SubtypingResult
--------------------------------------------------------------------------------
subtypeFun l γ t1@(TFun b1s o1 _) t2@(TFun b2s o2 _)
  = mconcat   $ lengthSub
              : subtype' l γ o1 o2
              : zipWith (subtype' l γ) args2 args1
  where
    lengthSub | length b1s == length b2s = EqT
              | otherwise                = NoSub [errorFunArgMismatch l t1 t2]
    args1     = map b_type b1s
    args2     = map b_type b2s

subtypeFun l γ t1@(TAnd _) t2@(TAnd t2s)
  | and $ isSubtype γ t1 <$> map snd t2s
  = SubT
  | otherwise
  = NoSub [errorSubtype l t1 t2]

subtypeFun l γ t1@(TAnd t1s) t2
  | or $ f <$> map snd t1s
  = SubT
  | otherwise
  = NoSub [errorSubtype l t1 t2]
  where
    f t1 = isSubtype γ t1 t2

subtypeFun l _ t1 t2 = NoSub [unsupportedConvFun l t1 t2]


-- | `isUnique g t` checks if type t corresponds to a unique reference
--
--------------------------------------------------------------------------------
isUnique :: (PPRE r, FE g r) => g r -> RType r -> Bool
--------------------------------------------------------------------------------
isUnique g (TObj m _ _) = isSubtypeWithUq g m tUQ
isUnique g (TRef s _)   | Gen _ (m : _) <- s
                        = isSubtypeWithUq g m tUQ
isUnique g v@(TVar _ _) | Just t <- envFindBoundOpt g v
                        = isUnique g t
isUnique _ _            = False

