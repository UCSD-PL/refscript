{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DoAndIfThenElse           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE IncoherentInstances       #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE LiberalTypeSynonyms       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}


module Language.Rsc.Typecheck.Sub (convert, isSubtype, isConvertible) where

import           Control.Applicative          ((<$>))
import           Data.Default
import           Data.Maybe                   (fromMaybe)
import           Data.Monoid
import           Data.Tuple                   (swap)
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Types      (differenceSEnv, intersectWithSEnv, toListSEnv)
import           Language.Rsc.Annotations
import           Language.Rsc.ClassHierarchy
import           Language.Rsc.Environment
import           Language.Rsc.Errors
import           Language.Rsc.Locations
import           Language.Rsc.Names
import           Language.Rsc.Pretty
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types

type FE g = (EnvLike () g, Functor g)

--------------------------------------------------------------------------------
isSubtype :: (PPR r, FE g) => g r -> RType r -> RType r -> Bool
--------------------------------------------------------------------------------
isSubtype γ t1 t2 =
  case convert def γ t1 t2 of
  -- case tracePP (ppshow t1 ++ " <: " ++ ppshow t2) $ convert def γ t1 t2 of
    CNo     -> True
    CUp _ _ -> True
    _       -> False

--------------------------------------------------------------------------------
isConvertible :: FE g => g () -> Type -> Type -> Bool
--------------------------------------------------------------------------------
isConvertible γ t1 t2 =
  case compareTypes def γ t1 t2 of
    SubErr _ -> False
    _        -> True

--------------------------------------------------------------------------------
convert :: (PPR r, FE g) => SrcSpan -> g r -> RType r -> RType r -> Cast r
--------------------------------------------------------------------------------
convert l γ t1 t2
  -- = case ltracePP l ("compareTypes: " ++ ppshow τ1 ++ " <: " ++ ppshow τ2) $ compareTypes l γ' τ1 τ2 of
  = case compareTypes l γ' τ1 τ2 of
      EqT       -> CNo
      SubT      -> CUp (rType t1) (rType t2)
      SupT      -> CDn (rType t1) (rType t2)
      SubErr [] -> CDead [errorDeadCast l (toType t1) (toType t2)] (rType t1)
      SubErr es -> CDead es (rType t1)
  where
    rType = ofType . toType
    τ1    = toType t1
    τ2    = toType t2
    γ'    = fmap (const ()) γ

--------------------------------------------------------------------------------
compareTypes :: FE g => SrcSpan -> g () -> Type -> Type -> SubTRes
--------------------------------------------------------------------------------
compareTypes _ _ _  t2 | isTTop t2                  = SubT
compareTypes _ _ t1 t2 | isTPrim t1  ,  isTPrim t2  = comparePrims t1 t2
compareTypes l γ t1 t2 | isTVar t1   || isTVar t2   = compareVars l γ t1 t2
compareTypes _ γ t1 t2 | isTUnion t1 || isTUnion t2 = compareUnions γ t1 t2
compareTypes l γ t1 t2 | maybeTObj t1, maybeTObj t2 = compareObjs l γ t1 t2
compareTypes l γ t1 t2 | isTFun t1   , isTFun t2    = compareFuns l γ t1 t2
compareTypes _ _ _  _                               = SubErr []     -- TODO

--------------------------------------------------------------------------------
comparePrims :: Type -> Type -> SubTRes
--------------------------------------------------------------------------------
comparePrims (TPrim c1 _) (TPrim c2 _)  | c1 == c2  = EqT
                                        | otherwise = SubErr []     -- TODO
comparePrims _            _             = SubErr [{- BUG -}]

--------------------------------------------------------------------------------
compareVars :: FE g => SrcSpan -> g () -> Type -> Type -> SubTRes
--------------------------------------------------------------------------------
compareVars _ _ (TVar v1 _) (TVar v2 _) | v1 == v2  = EqT
                                        | otherwise = SubErr []     -- TODO
compareVars l γ (TVar v1 _) t2          = compareTypes l γ bt1 t2
                                          where bt1 = fromMaybe tTop $ envFindBound v1 γ
compareVars _ _ _           (TVar _ _)  = SubErr []                 -- TODO

compareVars _ _ _           _           = SubErr [{- BUG -}]

--------------------------------------------------------------------------------
compareUnions :: FE g => g () -> Type -> Type -> SubTRes
--------------------------------------------------------------------------------
compareUnions γ t1 t2
  | upcast     = SubT
  | downcast   = SupT
  | otherwise  = SubErr [] -- TODO
  where
    (t1s, t2s) = mapPair bkUnion (t1, t2)
    sub        = isSubtype γ
    conv       = isConvertible γ
    upcast     = all ((`any` t2s) . sub ) t1s
    downcast   = any ((`any` t2s) . conv) t1s

--------------------------------------------------------------------------------
compareObjs :: FE g => SrcSpan -> g () -> Type -> Type -> SubTRes
--------------------------------------------------------------------------------
-- | Cannot convert a structural object type to a nominal class type.
--   Interfaces are OK.
--
compareObjs l γ t1 t2
  | not (isClassType (envCHA γ) t1) && isClassType (envCHA γ) t2
  = SubErr [errorObjectType l t1 t2]

compareObjs l γ t1@(TObj e1s _) t2@(TObj e2s _)
  = compareObjMembers l γ t1 e1s t2 e2s

compareObjs l γ t1@(TRef (Gen x1 []) _) t2@(TRef (Gen x2 []) _)
  | mutRelated t1, mutRelated t2, x1 == x2
  = EqT
  | mutRelated t1, mutRelated t2, isAncestor (envCHA γ) x2 x1
  = SubT

compareObjs l γ t1@(TRef (Gen x1 (m1:t1s)) _) t2@(TRef (Gen x2 (m2:t2s)) _)
  --
  -- * Incompatible mutabilities
  --
  | not (isSubtype γ m1 m2)
  = SubErr [errorIncompMutTy l t1 t2]
  --
  -- * Both immutable, same name, non arrays: co-variant subtyping on arguments
  --
  | x1 == x2 && isImm m2 && not (isArrayType t1)
  = mconcat $ compareTypes l γ m1 m2
            : zipWith (compareTypes l γ) t1s t2s
  --
  -- * Non-immutable, same name: co- and contra-variant subtyping on arguments
  --
  | x1 == x2
  = mconcat $ compareTypes l γ m1 m2
            :  zipWith (compareTypes l γ) t1s t2s
            ++ zipWith (compareTypes l γ) t2s t1s
  --
  -- * Compatible mutabilities, differenet names:
  --
  | isAncestor (envCHA γ) x1 x2 || isAncestor (envCHA γ) x2 x1
  = case (weaken (envCHA γ) (Gen x1 (m1:t1s)) x2, weaken (envCHA γ) (Gen x2 (m2:t2s)) x1) of
      --
      -- * Adjusting `t1` to reach `t2` moving upward in the type
      --   hierarchy -- this is equivalent to Upcast
      --
      (Just (Gen _ (m1':t1s')), _) -> mconcat $ SubT : zipWith (compareTypes l γ) (m1':t1s') (m2:t2s)
      --
      -- * Adjusting `t2` to reach `t1` moving upward in the type
      --   hierarchy -- this is equivalent to DownCast
      --
      (_, Just (Gen _ (m2':t2s'))) -> mconcat $ SupT : zipWith (compareTypes l γ) (m1:t1s) (m2':t2s')

      (_, _) -> SubErr [bugWeakenAncestors (srcPos l) x1 x2]

compareObjs l γ (TClass c1) (TClass c2) = convertTClass l γ c1 c2

compareObjs l _ (TMod m1) (TMod m2) = convertTModule l m1 m2
--
-- * Fall back to structural subtyping
--
compareObjs l γ t1 t2 =
  case (expandType NonCoercive (envCHA γ) t1, expandType NonCoercive (envCHA γ) t2) of
    (Just ft1, Just ft2) -> compareObjs l γ ft1 ft2
    (Nothing , Nothing ) -> SubErr [errorUnresolvedTypes l t1 t2]
    (Nothing , _       ) -> SubErr [errorNonObjectType l t1]
    (_       , Nothing ) -> SubErr [errorNonObjectType l t2]


compareObjMembers l γ t1 (TM p1 m1 _ _ c1 k1 s1 n1) t2 (TM p2 m2 _ _ c2 k2 s2 n2)
  = compareProps l γ t1 p1 t2 p2 <>
    compareMeths l γ t1 m1 t2 m2 <>
    compareCalls l γ t1 c1 t2 c2 <>
    compareCtors l γ t1 k1 t2 k2 <>
    compareSIdxs l γ t1 s1 t2 s2 <>
    compareNIdxs l γ t1 n1 t2 n2

compareProps = compareMembers compareProp
compareMeths = compareMembers compareMeth

compareMembers op l γ t1 p1 t2 p2
  | null diff12, null diff21                              -- Same exact fields in @t1@ and @t2@
  = mconcat $ op l γ <$> match
  | null diff21                                           -- Width-subtyping: fields of @t1@ are a
                                                          -- superset of fields of @t2@.
  = mconcat $ SubT : map (op l γ) match
  | otherwise                                             -- No subtype
  = SubErr [errorObjSubtype l t1 t2 $ fst <$> diff21]
  where
    diff21 = toListSEnv $ p2 `differenceSEnv` p1
    diff12 = toListSEnv $ p1 `differenceSEnv` p2
    match  = toListSEnv $ intersectWithSEnv (,) p1 p2

compareProp l γ (f, (FI o1 m1 t1, FI o2 m2 t2))
  | o1 /= o2
  = SubErr [errorIncompatOptional (srcPos l) f]
  | isSubtype γ m1 m2 && isImm m2                         -- Co-Variance
  = compareTypes l γ t1 t2
  | isSubtype γ m1 m2                                     -- Co-& Contra-Variance
  = compareTypes l γ t1 t2 <> compareTypes l γ t2 t1
  | otherwise
  = SubErr [errorIncompMutElt (srcPos l) f]

compareMeth l γ (m, (MI o1 m1 t1, MI o2 m2 t2))
  | o1 /= o2
  = SubErr [errorIncompatOptional (srcPos l) m]
  | m1 /= m2
  = SubErr [errorIncompMethMut (srcPos l) m]
  | otherwise
  = compareTypes l γ t1 t2

compareCalls l γ _  (Just f1) _  (Just f2) = compareFuns l γ f1 f2
compareCalls l _ t1 _         t2 _         = SubErr [errorIncompCallSigs (srcPos l) t1 t2]

compareCtors l γ _  (Just k1) _  (Just k2) = compareFuns l γ k1 k2
compareCtors l _ t1 _         t2 _         = SubErr [errorIncompCtorSigs (srcPos l) t1 t2]

compareSIdxs l γ _  (Just s1) _  (Just s2) = compareTypes l γ s1 s2 <> compareTypes l γ s2 s1
compareSIdxs l _ t1 _         t2 _         = SubErr [errorIncompSIdxSigs (srcPos l) t1 t2]

compareNIdxs l γ _  (Just n1) _  (Just n2) = compareTypes l γ n1 n2 <> compareTypes l γ n2 n1
compareNIdxs l _ t1 _         t2 _         = SubErr [errorIncompNIdxSigs (srcPos l) t1 t2]

t1 `eqMutability` t2 | isMut t1, isMut t2  = True
                     | isImm t1, isImm t2  = True
                     | isRO  t1, isRO  t2  = True
                     | isUM  t1, isUM  t2  = True
                     | otherwise           = False

--------------------------------------------------------------------------------
compareFuns :: FE g => SrcSpan -> g () -> Type -> Type -> SubTRes
--------------------------------------------------------------------------------
compareFuns l γ (TFun b1s o1 _) (TFun b2s o2 _)
  = mconcat   $ lengthSub
              : compareTypes l γ o1 o2
              : zipWith (compareTypes l γ) args2 args1
  where
    lengthSub | length b1s == length b2s = EqT
              | otherwise                = SubErr [] -- TODO
    args1     = map b_type b1s
    args2     = map b_type b2s

compareFuns l γ t1@(TAnd _) t2@(TAnd t2s)
  | and $ isSubtype γ t1 <$> t2s
  = SubT
  | otherwise
  = SubErr [errorFuncSubtype l t1 t2]

compareFuns l γ t1@(TAnd t1s) t2
  | or $ f <$> t1s
  = SubT
  | otherwise
  = SubErr [errorFuncSubtype l t1 t2]
  where
    f t1 = isSubtype γ t1 t2


compareFuns l _ t1 t2 = SubErr [unsupportedConvFun l t1 t2]

--------------------------------------------------------------------------------
convertTClass :: FE g => SrcSpan -> g () -> BTGen () -> BTGen () -> SubTRes
--------------------------------------------------------------------------------
convertTClass l γ t1@(BGen c1 ts1) t2@(BGen c2 ts2)
  | c1 == c2
  , and $ uncurry (isSubtype γ)        <$> ts
  , and $ uncurry (isSubtype γ) . swap <$> ts
  = EqT
  | otherwise
  = SubErr [errorTClassSubtype l t1 t2]

  where
    ts = [ (t1, t2) | (Just t1, Just t2) <- zip (btv_constr <$> ts1) (btv_constr <$> ts2) ]

--------------------------------------------------------------------------------
convertTModule :: SrcSpan -> AbsPath -> AbsPath -> SubTRes
--------------------------------------------------------------------------------
convertTModule l c1 c2 | c1 == c2  = EqT
                       | otherwise = SubErr [errorTModule l c1 c2]

--------------------------------------------------------------------------------
convertTEnum :: SrcSpan -> AbsName -> AbsName -> SubTRes
--------------------------------------------------------------------------------
convertTEnum l e1 e2 | e1 == e2  = EqT
                     | otherwise = SubErr [errorTEnumSubtype l e1 e2]

