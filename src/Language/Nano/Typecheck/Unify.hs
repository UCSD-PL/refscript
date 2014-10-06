{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Nano.Typecheck.Unify ( 
  
  -- * Unification 
  unifys

  ) where 

import           Control.Applicative                ((<$>))
import           Language.ECMAScript3.PrettyPrint
import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Types as F
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
import           Data.List (nub, find)
import           Data.Maybe (catMaybes)
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as HM 
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Control.Monad  (foldM)
import           Data.Function                  (on)

-- import           Debug.Trace

type PPR r = (PP r, F.Reftable r)

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
unify l γ θ (TFun (Just s1) t1s o1 _) (TFun (Just s2) t2s o2 _)
  = unifys l γ θ (s1 : o1 : map b_type t1s') (s2 : o2 : map b_type t2s')
  where 
    (t1s',t2s') = unzip $ zip t1s t2s -- get their common parts

unify l γ θ (TFun _ t1s o1 _) (TFun _ t2s o2 _)
  = unifys l γ θ (o1 : map b_type t1s') (o2 : map b_type t2s')
  where 
    (t1s',t2s') = unzip $ zip t1s t2s -- get their common parts

unify l _ θ (TVar α _) (TVar β _) = varEql l θ α β 
unify l _ θ (TVar α _) t' = varAsn l θ α t'
unify l _ θ t (TVar α _)  = varAsn l θ α t

-- XXX: ORDERING IMPORTANT HERE
-- Keep the union case before unfolding, but after type variables

unify l γ θ t t' | any isUnion [t,t'] = unifys l γ θ t1s' t2s'
  where
    (t1s', t2s') = unzip [ (t1, t2) | t1 <- t1s, t2 <- t2s, related γ t1 t2]
    (t1s , t2s ) = mapPair bkUnion (t,t')

unify l γ θ (TApp (TRef x) ts _) (TApp (TRef x') ts' _) 
  | x == x' = unifys l γ θ ts ts'

-- FIXME: fill in the "otherwise case"

unify l γ θ t1@(TApp (TRef _) _ _) t2 
  = case flattenType γ t1 of 
      Just ft1 -> unify l γ θ ft1 t2
      Nothing  -> Left $ errorUnfoldType l t1

unify l γ θ t1 t2@(TApp (TRef _) _ _)
  = case flattenType γ t2 of
      Just ft2 -> unify l γ θ t1 ft2
      Nothing  -> Left $ errorUnfoldType l t2

unify l γ θ t1@(TClass _) t2
  = case flattenType γ t1 of 
      Just ft1 -> unify l γ θ ft1 t2
      Nothing  -> Left $ errorUnfoldType l t1

unify l γ θ t1 t2@(TClass _)
  = case flattenType γ t2 of 
      Just ft2 -> unify l γ θ t1 ft2
      Nothing  -> Left $ errorUnfoldType l t2

unify l γ θ t@(TCons m1 e1s _) t'@(TCons m2 e2s _)
  = unifys l γ θ (ofType m1 : t1s) (ofType m2 : t2s)
  where 
    (t1s , t2s ) = mapPair (concatMap allEltType)
                 $ unzip 
                 $ M.elems 
                 $ M.intersectionWith (,) e1s e2s
   
-- The rest of the cases do not cause any unification.
unify _ _ θ _  _ = return θ


   
-----------------------------------------------------------------------------
unifys :: (Data r, PPR r) 
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

