{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Nano.Typecheck.Unify ( 
  
  -- * Unification 
  unifys

  ) where 

import           Control.Applicative                ((<$>), (<*>))
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Errors 
import           Language.Nano.Errors 
import           Language.Nano.Locations
import           Language.Nano.Types
import           Language.Nano.Typecheck.Environment
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Resolve
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Typecheck.Sub


import           Data.Generics
import qualified Data.List as L
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as HM 
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Control.Monad  (foldM)
import           Data.Function                  (on)

-- import           Debug.Trace

-----------------------------------------------------------------------------
-- | Unification
-----------------------------------------------------------------------------

-- | Unify types @t@ and @t'@, in substitution environment @θ@ and type
-- definition environment @γ@.
-----------------------------------------------------------------------------
unify :: (Data r, PPR r) 
      => SourceSpan 
      -> TCEnv r
      -> RSubst r 
      -> RType r 
      -> RType r 
      -> Either Error (RSubst r)
-----------------------------------------------------------------------------
unify l _ θ (TVar α _) (TVar β _) = varEql l θ α β 
unify l _ θ (TVar α _) t' = varAsn l θ α t'
unify l _ θ t (TVar α _)  = varAsn l θ α t

-- XXX: ORDERING IMPORTANT HERE
-- Keep the union case before unfolding, but after type variables

unify l γ θ t t' | any isUnion [t,t'] = unifyUnions l γ θ t t'

unify l γ θ (TFun (Just s1) t1s o1 _) (TFun (Just s2) t2s o2 _)
  = unifys l γ θ (s1 : o1 : map b_type t1s') (s2 : o2 : map b_type t2s')
  where 
    (t1s',t2s') = unzip $ zip t1s t2s -- get their common parts

unify l γ θ (TFun _ t1s o1 _) (TFun _ t2s o2 _)
  = unifys l γ θ (o1 : map b_type t1s') (o2 : map b_type t2s')
  where 
    (t1s',t2s') = unzip $ zip t1s t2s -- get their common parts

unify l γ θ (TCons m1 e1s _) (TCons m2 e2s _)
  = unifys l γ θ (ofType m1 : t1s) (ofType m2 : t2s)
  where 
    (t1s , t2s ) = mapPair (concatMap allEltType)
                 $ unzip 
                 $ M.elems 
                 $ M.intersectionWith (,) e1s e2s

unify l γ θ (TRef x1 t1s _) (TRef x2 t2s _) 
  | x1 == x2
  = unifys l γ θ t1s t2s

  | isAncestor γ x1 x2 || isAncestor γ x2 x1       
  = case (weaken γ x1 x2 t1s, weaken γ x2 x1 t2s) of
  -- 
  -- * Adjusting `t1` to reach `t2` moving upward in the type 
  --   hierarchy -- this is equivalent to Upcast
  --
      (Just (_, t1s'), _) -> unifys l γ θ t1s' t2s
  -- 
  -- * Adjusting `t2` to reach `t1` moving upward in the type 
  --   hierarchy -- this is equivalent to DownCast
  --
      (_, Just (_, t2s')) -> unifys l γ θ t1s t2s'
      (_, _) -> Left $ bugWeakenAncestors (srcPos l) x1 x2

unify _ γ θ (TClass  c1) (TClass  c2) | c1 == c2 = return θ 
unify _ γ θ (TModule m1) (TModule m2) | m1 == m2 = return θ 
unify _ γ θ (TEnum   e1) (TEnum   e2) | e1 == e2 = return θ

unify _ _ θ t1 t2 | all isPrimitive [t1,t2] = return θ

-- "Object"-ify types that can be flattened to an object literal type
unify l γ θ t1 t2 | all isFlattenable [t1,t2]
  = case (flattenType γ t1, flattenType γ t2) of 
      (Just ft1, Just ft2) -> unify l γ θ ft1 ft2
      (Nothing , Nothing ) -> Left $ errorUnresolvedTypes l t1 t2
      (Nothing , _       ) -> Left $ errorUnresolvedType l t1 
      (_       , Nothing ) -> Left $ errorUnresolvedType l t2
   
-- The rest of the cases do not cause any unification.
unify _ _ θ _  _ = return θ

-----------------------------------------------------------------------------
unifyUnions :: PPR r
            => SourceSpan
            -> TCEnv r
            -> RSubst r
            -> RType r
            -> RType r
            -> Either Error (RSubst r)
-----------------------------------------------------------------------------
unifyUnions l γ θ t1 t2
  | length v1s > 1 = Left $ unsupportedUnionTVar (srcPos l) t1  
  | length v2s > 1 = Left $ unsupportedUnionTVar (srcPos l) t2 
  | otherwise      = do θ1 <- unifys   l γ θ cmn1 cmn2
                        θ2 <- unifyVar l γ θ1 v1s dif2
                        θ3 <- unifyVar l γ θ2 v2s dif1
                        return θ3
  where
    (t1s , t2s ) = mapPair bkUnion (t1, t2)
    (v1s, t1s')  = L.partition isTVar t1s
    (v2s, t2s')  = L.partition isTVar t2s
    (cmn1, cmn2) = unzip [ (t1, t2) | t1 <- t1s', t2 <- t2s', related γ t1 t2 ]
    (dif1,dif2)  = (rem cmn1 t1s', rem cmn2 t2s')
    rem xs ys    = [ y | y <- ys, not (toType y `elem` map toType xs) ]

unifyVar _ _ θ [ ] _  = return θ
unifyVar _ _ θ [_] [] = return θ
unifyVar l γ θ [v] ts = unify l γ θ v $ mkUnion ts
unifyVar l _ _ ts  _  = Left $ unsupportedUnionTVar (srcPos l) $ mkUnion ts

   
-----------------------------------------------------------------------------
unifys :: PPR r
       => SourceSpan 
       -> TCEnv r 
       -> RSubst r 
       -> [RType r] -> [RType r] 
       -> Either Error (RSubst r)
-----------------------------------------------------------------------------
unifys l γ θ ts ts'  
  | nTs == nTs'   = foldM foo θ $ zip ts ts'
  | otherwise     = Left $ errorUnification l ts ts'
  where 
    (nTs, nTs')   = mapPair length (ts, ts')
    foo θ (t, t') = unify l γ θ (apply θ t) (apply θ t')


-----------------------------------------------------------------------------
varEql :: PPR r => SourceSpan -> RSubst r -> TVar -> TVar -> Either Error (RSubst r)
-----------------------------------------------------------------------------
varEql l θ α β =  
  case varAsn l θ α $ tVar β of
    Right θ' -> Right θ'
    Left  s1 -> 
      case varAsn l θ β $ tVar α of
        Right θ'' -> Right θ''
        Left  s2  -> Left $ catMessage s1 (errMsg s2) 


-----------------------------------------------------------------------------
varAsn ::  PPR r => SourceSpan -> RSubst r -> TVar -> RType r -> Either Error (RSubst r)
-----------------------------------------------------------------------------
varAsn l θ α t 
  | on (==) toType t (apply θ (tVar α))       = Right $ θ 
  | any (on (==) toType (tVar α)) (bkUnion t) = Right $ θ 
  | α `S.member` free t                       = Left  $ errorOccursCheck l α t 
  | unassigned α θ                            = Right $ θ `mappend` (Su $ HM.singleton α t)
  | otherwise                                 = Left  $ errorRigidUnify l α (toType t)
  
unassigned α (Su m) = HM.lookup α m == Just (tVar α)

