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
  , isConvertible
  , convert
  , SubtypingResult (..)
  , ConversionResult (..)
  ) where

import           Control.Applicative            ((<$>))
import           Data.Default
import           Data.Maybe                     (fromMaybe)
import           Data.Monoid
import           Data.Tuple                     (swap)
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Types        (Reftable, SEnv, differenceSEnv, intersectWithSEnv, toListSEnv)
import           Language.Fixpoint.Types.Errors
import           Language.Rsc.Annotations
import           Language.Rsc.ClassHierarchy
import           Language.Rsc.Environment
import           Language.Rsc.Errors
import           Language.Rsc.Locations
import           Language.Rsc.Misc              (mapPair)
import           Language.Rsc.Names
import           Language.Rsc.Pretty
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
import           Text.PrettyPrint.HughesPJ      (vcat, (<+>))

type FE g r = (CheckingEnvironment r g, Functor g)
type PPRE r = (ExprReftable Int r, PPR r)

--------------------------------------------------------------------------------
isSubtype     :: (PPRE r, FE g r) => g r -> RType r -> RType r -> Bool
isConvertible :: (PPRE r, FE g r) => g r -> RType r -> RType r -> Bool
--------------------------------------------------------------------------------
isSubtype γ t1 t2 = subtype dummySpan γ t1 t2 `elem` [EqT, SubT]

isConvertible γ t1 t2
  | isSubtype γ t1 t2
  = True

isConvertible γ (TOr t1s _) t2
  | any (\t1 -> isSubtype γ t1 t2) t1s
  = True

isConvertible γ t1@(TRef _ _) t2@(TRef _ _)
  | not (mutRelated t1), not (mutRelated t2)
  , isSubtype γ t1 t2
  = True

isConvertible _ _ _
  = False


data ConversionResult =
    ConvOK
  | ConvWith Type
  | ConvFail [Error]

instance PP ConversionResult where
  pp ConvOK        = pp "ConvOK"
  pp (ConvWith t)  = pp "ConvWith" <+> pp t
  pp (ConvFail es) = pp "ConvFail" <+> vcat (map pp es)


data SubtypingResult  = EqT | SubT | SubErr [Error] deriving (Eq, Ord, Show)

instance PP SubtypingResult where
  pp EqT  = pp "equal types"
  pp SubT = pp "subtypes"
  pp _    = pp "error in subtyping"

instance Monoid SubtypingResult where
  mempty                          = EqT
  mappend (SubErr e1) (SubErr e2) = SubErr $ e1 ++ e2
  mappend _           (SubErr e2) = SubErr e2
  mappend (SubErr e1) _           = SubErr e1
  mappend EqT         EqT         = EqT
  mappend _           _           = SubT

-- | convert: an "optimistic" version of subtyping that allows:
--
--   * Partial union subtyping
--
--   * Contra-variance at reference types
--
--------------------------------------------------------------------------------
convert :: (PPRE r, FE g r, IsLocated l) => l -> g r -> RType r -> RType r -> ConversionResult
--------------------------------------------------------------------------------
convert l g t1 t2
  -- = case ltracePP l (ppshow t1 ++ " <: " ++ ppshow t2) $ subtype l g t1 t2 of
  = case subtype l g t1 t2 of
    EqT       -> ConvOK
    SubT      -> ConvOK -- ConvWith (toType t2)
    SubErr es -> castable l g es t1 t2

castable _ γ _ (TOr t1s _) t2
  | any (\t1 -> isSubtype γ t1 t2) t1s
  = ConvWith (toType t2)

-- XXX: Only non generic casting allowed at the moment
castable l γ _ t1@(TRef (Gen _ [_]) _) t2@(TRef (Gen _ [_]) _)
  | not (mutRelated t1), not (mutRelated t2), isSubtype γ t1 t2
  = ConvWith (toType t2)

castable _ _ es _ _ = ConvFail es

--------------------------------------------------------------------------------
subtype :: (PPRE r, FE g r, IsLocated l) => l -> g r -> RType r -> RType r -> SubtypingResult
--------------------------------------------------------------------------------
-- | Type Variables
subtype _ _ (TVar v1 _) (TVar v2 _)
  | v1 == v2  = EqT

-- | Unfold bounded variables
subtype l γ t1@(TVar v1 _) t2
  | Just t1' <- envFindBoundOpt γ t1
  = subtype l γ t1' t2

-- | Primitive types
subtype l γ (TPrim c1 _) (TPrim c2 _)
  | c1 == c2   = EqT
  | c2 == TAny = SubT
  | c2 == TTop = SubT

-- | Unions
subtype _ γ (TOr ts1 _) t2
  | all (\t1 -> isSubtype γ t1 t2) ts1
  = SubT

subtype _ γ t1 (TOr ts2 _)
  | any (\t2 -> isSubtype γ t1 t2) ts2
  = SubT

-- | Objects
subtype l γ t1 t2
  | maybeTObj t1, maybeTObj t2 = subtypeObj l γ t1 t2

-- | Functions
subtype l γ t1 t2
  | isTFun t1, isTFun t2 = subtypeFun l γ t1 t2

-- | Rest (Fail)
subtype l _ t1 t2
  = SubErr [errorUncaughtSub l t1 t2]

--------------------------------------------------------------------------------
subtypeObj :: (PPRE r, FE g r, IsLocated l) => l -> g r -> RType r -> RType r -> SubtypingResult
--------------------------------------------------------------------------------
-- | Cannot convert a structural object type to a nominal class type.
--   Interfaces are OK.
--
subtypeObj l γ t1 t2
  | not (isClassType (envCHA γ) t1) && isClassType (envCHA γ) t2
  = SubErr [errorObjectType l t1 t2]

subtypeObj l γ t1@(TObj e1s _) t2@(TObj e2s _)
  = subtypeObjMembers l γ (t1, e1s) (t2, e2s)


subtypeObj l γ t1@(TRef (Gen x1 []) _) t2@(TRef (Gen x2 []) _)
  | mutRelated t1, mutRelated t2, x1 == x2
  = EqT
  | mutRelated t1, mutRelated t2, isAncestor (envCHA γ) x2 x1
  = SubT
  | mutRelated t1, mutRelated t2
  = SubErr [errorIncompMutTy l t1 t2]

subtypeObj l γ t1@(TRef (Gen x1 (m1:t1s)) r1) t2@(TRef (Gen x2 (m2:t2s)) r2)
  --
  --
  -- | isUQ m1
  -- = subtypeObj l γ (TRef (Gen x1 (tIM:t1s)) r1) (TRef (Gen x2 (tIM:t2s)) r2)
  --
  -- * Incompatible mutabilities
  --
  | not (isSubtype γ m1 m2)
  = SubErr [errorIncompMutTy l t1 t2]
  --
  -- * Both immutable, same name, non arrays: co-variant subtyping on arguments
  --
  | x1 == x2
  , isIM m2
  , not (isArrayType t1)
  = mconcat $ subtype l γ m1 m2 : zipWith (subtype l γ) t1s t2s
  --
  -- * Non-immutable, same name: co- and contra-variant subtyping on arguments
  --
  | x1 == x2
  = mconcat $ subtype l γ m1 m2
            :  zipWith (subtype l γ) t1s t2s
            ++ zipWith (subtype l γ) t2s t1s
  --
  -- * Compatible mutabilities, differenet names:
  --
  | Just (Gen _ (m1':t1s')) <- weaken (envCHA γ) (Gen x1 (m1:t1s)) x2
  = mconcat $ SubT : zipWith (subtype l γ) (m1':t1s') (m2:t2s)

  | Just (Gen _ (m1':t1s')) <- weaken (envCHA γ) (Gen x2 (m2:t2s)) x1
  = mconcat $ SubT : zipWith (subtype l γ) (m1':t1s') (m2:t2s)

  | otherwise
  = SubErr [errorIncompatTypes (srcPos l) x1 x2]

subtypeObj l γ t1@(TClass (BGen c1 ts1)) t2@(TClass (BGen c2 ts2))
  | c1 == c2
  , and $ uncurry (isSubtype γ)        <$> ts
  , and $ uncurry (isSubtype γ) . swap <$> ts
  = EqT
  | otherwise
  = SubErr [errorTClassSubtype l t1 t2]
  where
    ts = [ (t1, t2) | (Just t1, Just t2) <- zip (btv_constr <$> ts1) (btv_constr <$> ts2) ]

subtypeObj l _ (TMod m1) (TMod m2)
  | m1 == m2  = EqT
  | otherwise = SubErr [errorTModule l m1 m2]
--
-- * Fall back to structural subtyping
--
subtypeObj l γ t1 t2 =
  case (expandType NonCoercive (envCHA γ) t1, expandType NonCoercive (envCHA γ) t2) of
    (Just ft1, Just ft2) -> subtypeObj l γ ft1 ft2
    (Nothing , Nothing ) -> SubErr [errorUnresolvedTypes l t1 t2]
    (Nothing , _       ) -> SubErr [errorNonObjectType l t1]
    (_       , Nothing ) -> SubErr [errorNonObjectType l t2]


subtypeObjMembers l γ (t1, TM m1 _ c1 k1 s1 n1) (t2, TM m2 _ c2 k2 s2 n2)
  = subtypeMems  l γ (t1, m1) (t2, m2) <>
    subtypeCalls l γ t1 c1 t2 c2 <>
    subtypeCtors l γ t1 k1 t2 k2 <>
    subtypeSIdxs l γ t1 s1 t2 s2 <>
    subtypeNIdxs l γ t1 n1 t2 n2

--------------------------------------------------------------------------------
subtypeMems :: (IsLocated a, PPRE r, FE g r)
            => a -> g r
            -> (RType r, SEnv (TypeMember r))
            -> (RType r, SEnv (TypeMember r))
            -> SubtypingResult
--------------------------------------------------------------------------------
subtypeMems l γ (t1, p1) (t2, p2)
  -- Props from p2 are missing from p1 - fail width
  | not (null diff21)
  = SubErr [ errorObjSubtype l t1 t2 (map fst diff21) ]

  -- Descend into property subtyping
  | otherwise
  = mconcat $ map (compareMem l γ) common

  where
    diff21 = toListSEnv $ p2 `differenceSEnv` p1
    common = ltracePP l "COmmon" $ toListSEnv $ intersectWithSEnv (,) p1 p2

--------------------------------------------------------------------------------
compareMem :: (FE g r, PPRE r, IsLocated a, PP f)
           => a -> g r
           -> (f, (TypeMemberQ AK r, TypeMemberQ AK r))
           -> SubtypingResult
--------------------------------------------------------------------------------
compareMem l γ (f, (FI _ o1 m1 t1, FI _ o2 m2 t2))
  -- Different optionality modifier
  | o1 /= o2
  = SubErr [ errorIncompatOptional (srcPos l) f ]

  -- Incompatible field assignabilities
  | m1 /= m2
  = SubErr [errorIncompMutElt (srcPos l) f m1 m2]

  -- Co-& Contra-Variance (mutable fields)
  | isSubtype γ m1 tMU
  = subtype l γ t1 t2 <> subtype l γ t2 t1

  -- Co-Variance (immutable (through this reference) fields)
  | otherwise
  = subtype l γ t1 t2

compareMem l _ (_, (m1, m2))
  = SubErr [ unsupportedMethodComp (srcPos l) m1 m2 ]


compareMaybe l γ f _ _ (Just c1) _ (Just c2) = f l γ c1 c2
compareMaybe _ _ _ _ _ Nothing   _ Nothing   = SubT
compareMaybe l _ _ e _ t1        _ t2        = SubErr [e (srcPos l) t1 t2]

subtypeCalls l γ = compareMaybe l γ subtypeFun errorIncompCallSigs
subtypeCtors l γ = compareMaybe l γ subtype    errorIncompCtorSigs
subtypeSIdxs l γ = compareMaybe l γ subtype    errorIncompSIdxSigs
subtypeNIdxs l γ = compareMaybe l γ subtype    errorIncompNIdxSigs

t1 `eqMutability` t2 | isMU t1, isMU t2  = True
                     | isIM t1, isIM t2  = True
                     | isRO t1, isRO t2  = True
                     | isUQ t1, isUQ t2  = True
                     | otherwise         = False

--------------------------------------------------------------------------------
subtypeFun :: (PPRE r, FE g r, IsLocated l) => l -> g r -> RType r -> RType r -> SubtypingResult
--------------------------------------------------------------------------------
subtypeFun l γ t1@(TFun b1s o1 _) t2@(TFun b2s o2 _)
  = mconcat   $ lengthSub
              : subtype l γ o1 o2
              : zipWith (subtype l γ) args2 args1
  where
    lengthSub | length b1s == length b2s = EqT
              | otherwise                = SubErr [errorFunArgMismatch l t1 t2]
    args1     = map b_type b1s
    args2     = map b_type b2s

subtypeFun l γ t1@(TAnd _) t2@(TAnd t2s)
  | and $ isSubtype γ t1 <$> map snd t2s
  = SubT
  | otherwise
  = SubErr [errorFuncSubtype l t1 t2]

subtypeFun l γ t1@(TAnd t1s) t2
  | or $ f <$> map snd t1s
  = SubT
  | otherwise
  = SubErr [errorFuncSubtype l t1 t2]
  where
    f t1 = isSubtype γ t1 t2

subtypeFun l _ t1 t2 = SubErr [unsupportedConvFun l t1 t2]

