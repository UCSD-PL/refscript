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
import qualified Data.HashMap.Strict as M 
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
unify :: (Data r, PPR r) => SourceSpan -> TCEnv r
  -> RSubst r -> RType r -> RType r -> Either Error (RSubst r)
-----------------------------------------------------------------------------

unify _ _ θ t t' | any isTop [t,t'] = Right θ

-- UNDEFINED
-- unify _ _ θ t t' | any isUndef [t,t'] = Right θ

unify l γ θ (TFun (Just s) xts t _) (TFun (Just s') xts' t' _)
  = unifys l γ θ (s : t : map b_type xts) (s' : t' : map b_type xts')

unify l γ θ (TFun _ xts t _) (TFun _ xts' t' _)
  = unifys l γ θ (t : map b_type xts) (t' : map b_type xts')

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

unify l γ θ (TCons e1s m1 _) (TCons e2s m2 _)
  = unifys l γ θ (ofType m1 : t1s') (ofType m2 : t2s')
  where 
    -- | Find common binders 
    cmnBnds    = nub [ F.symbol e1 | e1 <- e1s, e2 <- e2s, e1 `sameBinder` e2 ]
    -- | Types and mutabilities for each binder
    (t1s, m1s)    = unzip $ boundTy cmnBnds e1s
    t1s'          = t1s ++ (ofType <$> catMaybes m1s)
    (t2s, m2s)    = unzip $ boundTy cmnBnds e2s
    t2s'          = t2s ++ (ofType <$> catMaybes m2s)
    boundTy bs es = [ (eltType e, mutability e) 
                      | e <- catMaybes $ map (\b -> find ((==) b . F.symbol) es) bs ]

-- The rest of the cases do not cause any unification.
unify _ _ θ _  _ = return θ

   
-----------------------------------------------------------------------------
unifys :: (Data r, PPR r) => SourceSpan -> TCEnv r -> RSubst r -> [RType r] 
                 -> [RType r] -> Either Error (RSubst r)
-----------------------------------------------------------------------------
unifys l γ θ ts ts'  
  | nTs == nTs' = foldM foo θ $ zip ts ts'
  | otherwise   = Left $ errorUnification l ts ts'
  where 
    (nTs, nTs') = mapPair length (ts, ts')
    foo θ       = uncurry $ on (unify l γ θ) (apply θ)


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
  | unassigned α θ                            = Right $ θ `mappend` (Su $ M.singleton α t)
  | otherwise                                 = Left  $ errorRigidUnify l α (toType t)
  
unassigned α (Su m) = M.lookup α m == Just (tVar α)

