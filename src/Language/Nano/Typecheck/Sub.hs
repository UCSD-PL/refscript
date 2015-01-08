{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE IncoherentInstances       #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LiberalTypeSynonyms       #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DoAndIfThenElse           #-}


module Language.Nano.Typecheck.Sub (convert, isSubtype, Related (..)) where

import           Control.Applicative                ((<$>))

import           Data.Tuple                         (swap)
import           Data.Monoid
import qualified Data.HashSet                       as S
import qualified Data.Map.Strict                    as M
import           Data.Maybe                         (fromMaybe)
import           Control.Monad.State
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc 

import           Language.ECMAScript3.PrettyPrint

import           Language.Nano.Annots
import           Language.Nano.Types
import           Language.Nano.Misc                 (mappendM)
import           Language.Nano.Names
import           Language.Nano.Locations
import           Language.Nano.Environment
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Resolve
import           Language.Nano.Errors

-- import           Debug.Trace                      (trace)

instance PP a => PP (S.HashSet a) where
  pp = pp . S.toList 

--------------------------------------------------------------------------------
isSubtype :: (PPR r, Functor g, EnvLike () g) => g r -> RType r -> RType r -> Bool
--------------------------------------------------------------------------------
isSubtype γ t1 t2 =
  case convert (srcPos dummySpan) γ t1 t2 of
    Right CNo       -> True
    Right (CUp _ _) -> True
    _               -> False

--------------------------------------------------------------------------------
isConvertible :: (EnvLike () g, Functor g) => g () -> Type -> Type -> Bool
--------------------------------------------------------------------------------
isConvertible γ t1 t2 =
  case convert' (srcPos dummySpan) γ t1 t2 of
    Left _          -> False
    Right CDDead    -> False
    _               -> True


-- | `convert`
--------------------------------------------------------------------------------
convert :: (PPR r, Functor g, EnvLike () g) 
        => SourceSpan -> g r -> RType r -> RType r -> Either Error (Cast r)
--------------------------------------------------------------------------------
convert l γ t1 t2  
  =  do c <- convert' l γ' τ1 τ2
        case c of 
          CDNo   -> return $ CNo
          CDUp   -> return $ CUp (rType t1) (rType t2)
          CDDn   -> return $ CDn (rType t1) (rType t2)
          CDDead -> return $ CDead (errorDeadCast l t1 t2) (rType t1)
  where
    rType = ofType . toType
    τ1    = toType t1
    τ2    = toType t2
    γ'    = fmap (\_ -> ()) γ


--------------------------------------------------------------------------------
convert' :: (Functor g, EnvLike () g)
         => SourceSpan -> g () -> Type -> Type -> Either Error CastDirection
--------------------------------------------------------------------------------
convert' _ _ t1 t2 | toType t1 == toType t2    = Right CDNo
convert' _ _ _  t2 | isTop t2                  = Right CDUp
convert' l γ t1 t2 | any isUnion       [t1,t2] = convertUnion   l γ t1 t2
convert' l γ t1 t2 | all isPrimitive   [t1,t2] = convertSimple  l γ t1 t2
convert' l γ t1 t2 | all isFlattenable [t1,t2] = convertObj     l γ t1 t2
convert' l γ t1 t2 | all isTFun        [t1,t2] = convertFun     l γ t1 t2
convert' l γ t1 t2                             = convertSimple  l γ t1 t2


-- | `convertObj`
--------------------------------------------------------------------------------
convertObj :: (Functor g, EnvLike () g)
           => SourceSpan -> g () -> Type -> Type -> Either Error CastDirection
--------------------------------------------------------------------------------

-- | Cannot convert a structural object type to a nominal class type. 
--   Interfaces are OK.
--
convertObj l γ t1 t2
  | not (isClassType γ t1) && isClassType γ t2
  = Left $ errorObjectType l t1 t2

convertObj l γ t1@(TCons μ1 e1s _) t2@(TCons μ2 e2s _)
  | mutabilitySub && isImmutable μ2 = covariantConvertObj l γ (μ1,e1s) (μ2,e2s)
  | mutabilitySub                   = invariantConvertObj l γ (μ1,e1s) (μ2,e2s)
  | otherwise                       = Left $ errorIncompMutTy l t1 t2
  where
      mutabilitySub = isSubtype γ μ1 μ2

convertObj l γ (TSelf m1) (TSelf m2) 
  = convertObj l γ m1 m2


convertObj l γ t1@(TRef x1 (m1:t1s) _) t2@(TRef x2 (m2:t2s) _)
  --
  -- * Incompatible mutabilities
  --
  | not (isSubtype γ m1 m2) 
  = Left $ errorIncompMutTy l t1 t2
  --  
  -- * Both immutable, same name, non arrays: co-variant subtyping on arguments
  --
  | x1 == x2 && isImmutable m2 && not (isArr t1) 
  = do c1    <- convert' l γ m1 m2
       c2    <- zipWithM (convert' l γ) t1s t2s
       return $ mconcat $ c1 : c2
  -- 
  -- * Non-immutable, same name: co- and contra-variant subtyping on arguments
  --
  | x1 == x2 
  = do c1    <- convert' l γ m1 m2
       c2    <- zipWithM (convert' l γ) t1s t2s
       c3    <- zipWithM (convert' l γ) t2s t1s
       return $ mconcat $ [c1] ++ c2 ++ c3
  -- 
  -- * Compatible mutabilities, differenet names:
  --
  | isAncestor γ x1 x2 || isAncestor γ x2 x1
  = case (weaken γ (x1,m1:t1s) x2, weaken γ (x2,m2:t2s) x1) of
  -- 
  -- * Adjusting `t1` to reach `t2` moving upward in the type 
  --   hierarchy -- this is equivalent to Upcast
  --
      (Just (_,m1':t1s'), _) -> mconcat . (CDUp:) <$> zipWithM (convert' l γ) (m1':t1s') (m2:t2s)
  -- 
  -- * Adjusting `t2` to reach `t1` moving upward in the type 
  --   hierarchy -- this is equivalent to DownCast
  --
      (_, Just (_,m2':t2s')) -> mconcat . (CDDn:) <$> zipWithM (convert' l γ) (m1:t1s) (m2':t2s')
      (_, _) -> Left $ bugWeakenAncestors (srcPos l) x1 x2

convertObj l γ (TClass  c1) (TClass  c2) = convertTClass  l γ c1 c2

convertObj l γ (TModule m1) (TModule m2) = convertTModule l γ m1 m2

convertObj l γ (TEnum e1) (TEnum e2) = convertTEnum l γ e1 e2

-- convertObj l γ (TRef x1 _ _) (TRef x2 _ _) | isEnumValue γ x1 && isEnumValue γ x2 = return CDUp
-- convertObj l γ (TRef x1 _ _) t2            | isEnumValue γ x1 && isTNum t2        = return CDUp
-- convertObj l γ t1            (TRef x2 _ _) | isTNum t1        && isEnumValue γ x2 = return CDUp

-- 
-- * Fall back to structural subtyping
--
convertObj l γ t1 t2 =
  case (flattenType γ t1, flattenType γ t2) of 
    (Just ft1, Just ft2) -> convertObj l γ ft1 ft2
    (Nothing , Nothing ) -> Left $ errorUnresolvedTypes l t1 t2
    (Nothing , _       ) -> Left $ errorUnresolvedType l t1 
    (_       , Nothing ) -> Left $ errorUnresolvedType l t2


covariantConvertObj l γ (μ1,e1s) (μ2,e2s)
  | M.null uq1s && M.null uq2s = mconcat           <$> subEs  -- {x1:t1,..,xn:tn}          ?? {x1:t1',..,xn:tn'}
  |                M.null uq2s = mconcat . (CDUp:) <$> subEs  -- {x1:t1,..,xn:tn,..,xm:tm} ?? {x1:t1',..,xn:tn'}
  | M.null uq1s                = mconcat . (CDDn:) <$> subEs  -- {x1:t1,..,xn:tn}          ?? {x1:t1',..,xn:tn',..,xm:tm'}
  | hasStringIndexer l2s       = mconcat . (CDUp:) <$> subStringIndexer
  | otherwise                  = Left $ errorIncompatCovFields l e1s e2s
  where
    l2s                        = M.elems e2s
    e1s'                       = (M.map (combMutInField μ1) . M.filter subtypeable) e1s
    e2s'                       = (M.map (combMutInField μ2) . M.filter subtypeable) e2s
    -- Optional fields should not take part in width subtyping
    (e1s'', e2s'')             = mapPair (M.filter requiredField) (e1s',e2s')
    -- Subtyping equivalent type-members
    subEs                      = mapM (uncurry $ convertElt l γ True) es
    -- Type-members unique in the 1st group
    uq1s                       = e1s'' `M.difference` e2s''
   -- Type-members unique in the 2nd group
    uq2s                       = e2s'' `M.difference` e1s'' 
    -- Pairs of equivalent type-members in `e1s` and `e2s`
    es                         = M.elems $ M.intersectionWith (,) e1s' e2s'
    -- String Indexer -- checks all field elements 
    subStringIndexer           = mapM convStrIdx (fieldSigs $ M.elems e1s)
    convStrIdx e1              = convertStringIndexer l γ True e1 (getStringIndexer l2s)

-- | `invariantConvertObj l γ e1s e2s` determines if an object type containing
--   members @e1s@ can be converted (used as) an object type with members @e2s@. 
--
invariantConvertObj l γ (μ1,e1s) (μ2,e2s)
  | M.null uq1s && M.null uq2s = mconcat           <$> subEs  -- {x1:t1,..,xn:tn}          == {x1:t1',..,xn:tn'}
  |                M.null uq2s = mconcat . (CDUp:) <$> subEs  -- {x1:t1,..,xn:tn,..,xm:tm} UP {x1:t1',..,xn:tn'}
  -- FIXME: Add case for string indexer
  | hasStringIndexer l2s       = mconcat . (CDUp:) <$> subStringIndexer
  -- {x1:t1,..,xn:tn, y1:...} DD {x1:t1',..,xn:tn',..,xm:tm'} 
  | otherwise                  = Left $ errorIncompatInvFields l e1s e2s
  where
    l2s                        = M.elems e2s
    e1s'                       = (M.map (combMutInField μ1) . M.filter subtypeable) e1s
    e2s'                       = (M.map (combMutInField μ2) . M.filter subtypeable) e2s
    -- Optional fields should not take part in width subtyping
    (e1s'', e2s'')             = mapPair (M.filter requiredField) (e1s', e2s')
    -- Subtyping equivalent type-members
    subEs                      = mapM (uncurry $ convertElt l γ False) $ es ++ es'
    es'                        = swap <$> es
    -- Type-members unique in the 1st group
    uq1s                       = e1s'' `M.difference` e2s''
   -- Type-members unique in the 2nd group
    uq2s                       = e2s'' `M.difference` e1s'' 
    -- Pairs of equivalent type-members in `e1s` and `e2s`
    es                         = M.elems $ M.intersectionWith (,) e1s' e2s'
    -- String Indexer -- checks all field elements 
    subStringIndexer           = mapM convStrIdx (fieldSigs $ M.elems e1s)
    convStrIdx e1              = convertStringIndexer l γ False e1 (getStringIndexer l2s)


-- | `convertElt l γ mut e1 e2` performs a subtyping check between elements @e1@ and
--   @e2@ given that they belong to:
--
--    * an immutable structure if @mut@ is True, 
--    * a mutable one if @mut@ is False.
--
convertElt l γ _ (CallSig t1) (CallSig t2) 
  = convert' l γ t1 t2 

convertElt l γ False f1@(FieldSig _ _ m1 t1) f2@(FieldSig _ _ m2 t2)          -- mutable container
  -- | o1 /= o2
  -- = Left $ errorOptionalElt (srcPos l) f1 f2 
  | isSubtype γ t1 t2 && isSubtype γ m2 m1 
  = convert' l γ t1 t2 `mappendM` convert' l γ t2 t1
  | otherwise                           
  = Left $ errorIncompMutElt (srcPos l) f1 f2

convertElt l γ True f1@(FieldSig _ _ m1 t1) f2@(FieldSig _ _ m2 t2)   -- immutable container
  -- | o1 == Optional && o2 == Mandatory 
  -- = Left $ errorIncompatOptional (srcPos l) f1 f2 
  | isSubtype γ m1 m2 && isImmutable m2 
  = convert' l γ t1 t2
  | isSubtype γ m1 m2
  = convert' l γ t1 t2 `mappendM` convert' l γ t2 t1
  | otherwise                           
  = Left $ errorIncompMutElt (srcPos l) f1 f2

convertElt l γ _ (MethSig _ t1) (MethSig _ t2) 
  = convert' l γ t1 t2 
 
convertElt l γ _ (ConsSig t1) (ConsSig t2) 
  = convert' l γ t1 t2 

convertElt l γ _ (IndexSig _ _ t1) (IndexSig _ _ t2)
  = convert' l γ t1 t2 `mappendM` convert' l γ t2 t1

-- | otherwise fail
convertElt l _ _ f1 f2 = Left $ bugEltSubt (srcPos l) f1 f2


convertStringIndexer l γ True e1 e2 
  = convert' l γ (eltType e1) (eltType e2) 
convertStringIndexer l γ False e1 e2 
  = liftM2 mappend (convert' l γ (eltType e1) (eltType e2)) 
                   (convert' l γ (eltType e2) (eltType e1))


-- | `convertFun`
--  
--   * (t1,t2,..,tk) => t <: (t1,t2,..,tk,..,tl) => t
--
--       forall i . ti' <: ti   t <: t'
--   * ----------------------------------
--       (ti,..) => t <: (ti',..) => t'
--
--------------------------------------------------------------------------------
convertFun :: (Functor g, EnvLike () g)
           => SourceSpan -> g () -> Type -> Type -> Either Error CastDirection
--------------------------------------------------------------------------------
convertFun l γ (TFun s1 b1s o1 _) (TFun s2 b2s o2 _) 
  = mappend lengthSub <$> 
    mconcat           <$> liftM2 (:) (convert' l γ o1 o2) 
                            (zipWithM (convert' l γ) args2 args1)
  where
    lengthSub | length b1s == length b2s = CDNo
              | length b1s >  length b2s = CDDn
              | otherwise                = CDUp
    s1'       = fromMaybe tTop s1 
    s2'       = fromMaybe tTop s2
    args1     = s1' : map b_type b1s
    args2     = s2' : map b_type b2s

convertFun l γ t1@(TAnd _) t2@(TAnd t2s) = 
  if and $ isSubtype γ t1 <$> t2s then Right CDUp
                                  else Left  $ errorFuncSubtype l t1 t2

convertFun l γ t1@(TAnd t1s) t2 = 
  let f t1 = isSubtype γ t1 t2 in 
  if or $ f <$> t1s then Right CDUp
                    else Left  $ errorFuncSubtype l t1 t2
 
convertFun l _ t1 t2 = Left $ unsupportedConvFun l t1 t2


-- | `convertTClass`
--------------------------------------------------------------------------------
convertTClass :: (Functor g, EnvLike () g)
              => SourceSpan -> g () -> AbsName -> AbsName -> Either Error CastDirection
--------------------------------------------------------------------------------
convertTClass l _ c1 c2 | c1 == c2  = Right CDNo  
                        | otherwise = Left  $ errorTClassSubtype l c1 c2

-- | `convertTModule`
--------------------------------------------------------------------------------
convertTModule :: (Functor g, EnvLike () g)
              => SourceSpan -> g () -> AbsPath -> AbsPath -> Either Error CastDirection
--------------------------------------------------------------------------------
convertTModule l _ c1 c2 | c1 == c2  = Right CDNo
                         | otherwise = Left $ errorTModule l c1 c2

-- | `convertTEnum`
--------------------------------------------------------------------------------
convertTEnum :: (Functor g, EnvLike () g)
              => SourceSpan -> g () -> AbsName -> AbsName -> Either Error CastDirection
--------------------------------------------------------------------------------
convertTEnum l _ e1 e2 | e1 == e2  = Right CDNo  
                       | otherwise = Left  $ errorTEnumSubtype l e1 e2

-- | `convertSimple`
--------------------------------------------------------------------------------
convertSimple :: (Functor g, EnvLike () g)
              => SourceSpan -> g r -> Type -> Type -> Either Error CastDirection
--------------------------------------------------------------------------------
convertSimple _ _ t1 t2 | t1 == t2  = Right CDNo
                        | otherwise = Right CDDead 

-- | `convertUnion`
--------------------------------------------------------------------------------
convertUnion :: (Functor g, EnvLike () g)
             => SourceSpan -> g () -> Type -> Type -> Either Error CastDirection
--------------------------------------------------------------------------------
convertUnion _ γ t1 t2  
  | upcast    = Right CDUp 
  | downcast  = Right CDDn
  | otherwise = Right CDDead
  where 
    upcast        = all (\t1 -> any (\t2 -> isSubtype     γ t1 t2) t2s) t1s
    downcast      = any (\t1 -> any (\t2 -> isConvertible γ t1 t2) t2s) t1s 
    (t1s, t2s)    = chk $ mapPair bkUnion (t1, t2)
    chk ([ ],[ ]) = errorstar "unionParts', called on too small input"
    chk ([_],[ ]) = errorstar "unionParts', called on too small input"
    chk ([ ],[_]) = errorstar "unionParts', called on too small input"
    chk ([_],[_]) = errorstar "unionParts', called on too small input"
    chk p         = p


-- | Related types ( ~~ ) -- Cannot be parts of the same union. 
--  
--   Reflexive, symmetric, transitive
--   
class Related t where
  related :: (Functor g, EnvLike r g, PPR r) 
          => g r -> t r -> t r -> Bool

instance Related RType where
  related _ (TSelf _)    (TSelf _)              = True

  related γ (TRef x _ _) (TRef y _ _)           = isAncestor γ x y || isAncestor γ y x

  related _ TRef{}       TCons{}                = True
  related _ TCons{}      TRef{}                 = True

  related _ TCons{}      TCons{}                = True

  related _ t t' | all isPrimitive [t,t']       = toType t == toType t'
                 | all isTFun      [t,t']       = True
                 | toType t == toType t'        = True
                 | otherwise                    = False

