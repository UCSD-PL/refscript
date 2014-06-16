{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Nano.Typecheck.Unify ( 
  
  -- * Unification 
  unifys

  ) where 

-- import           Text.PrettyPrint.HughesPJ
import           Language.ECMAScript3.PrettyPrint
import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Errors 
import           Language.Nano.Errors 
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Subst


import           Language.ECMAScript3.Parser.Type    (SourceSpan (..))
import           Control.Applicative ((<$>))
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
-- definition environment @δ@.
-----------------------------------------------------------------------------
unify :: PPR r => SourceSpan -> TDefEnv (RType r) 
  -> RSubst r -> RType r -> RType r -> Either Error (RSubst r)
-----------------------------------------------------------------------------

unify _ _ θ t@(TApp _ _ _) t'@(TApp _ _ _) | any isTop [t,t'] = Right $ θ

unify l δ θ (TFun xts t _) (TFun xts' t' _)
  = unifys l δ θ (t: map b_type xts) (t': map b_type xts')

unify l _ θ (TVar α _) (TVar β _) = varEql l θ α β 
unify l _ θ (TVar α _) t' = varAsn l θ α t'
unify l _ θ t (TVar α _)  = varAsn l θ α t

-- XXX: ORDERING IMPORTANT HERE
-- Keep the union case before unfolding, but after type variables

-- FIXME
unify l δ θ t t' | any isUnion [t,t'] = unifys l δ θ t1s t2s
  where
    (ts, _, _) = error "unify-union" -- unionParts' unifEquiv t t' in
    (t1s, t2s) = unzip ts

unify l δ θ (TApp (TRef x s) ts _) (TApp (TRef x' s') ts' _) 
  | (x,s) == (x',s')   = unifys l δ θ ts ts'

unify l δ θ t1@(TApp (TRef _ _) _ _) t2
  = unify l δ θ (flattenType δ t1) t2

unify l δ θ t1 t2@(TApp (TRef _ _) _ _)
  = unify l δ θ t1 (flattenType δ t2)

unify l δ θ (TCons e1s m1 _) (TCons e2s m2 _)
  = unifys l δ θ (ofType m1:t1s) (ofType m2:t2s)
  where 
    (t1s, t2s) = unzip $ map tt es ++ concatMap ττ es ++ concatMap mm es
    es         = [ (e1, e2) | e1 <- e1s
                            , e2 <- e2s
                            , nonStaticElt e1
                            , nonStaticElt e2
                            , e1 `sameBinder` e2]
    tt         = mapPair eltType
    ττ (e1,e2) = case (baseType e1, baseType e2) of 
                   (Just τ1, Just τ2) -> [(τ1, τ2)]
                   _                  -> []
    mm (e1,e2) = case (mutability e1, mutability e2) of 
                   (Just m1, Just m2) -> [(ofType m1, ofType m2)]
                   _                  -> []

-- The rest of the cases do not cause any unification.
unify _ _ θ _  _ = return θ


unifEquiv t t' | toType t == toType t' 
               = True
unifEquiv t t' | any isUnion [t,t'] 
               = error "unifEquiv: no nested unions"
-- FIXME: TApp TRef ... 
unifEquiv (TApp c _ _ ) (TApp c' _ _  ) = c == c'       -- Interfaces appear once only on top-level unions
unifEquiv (TCons _ _ _) (TCons _ _ _  ) = True
unifEquiv (TVar v _   ) (TVar v' _    ) = v == v'
unifEquiv (TFun _ _ _ ) (TFun _ _ _   ) = True          -- Functions as well ... 
unifEquiv (TAll _ _   ) (TAll _ _     ) = error "unifEquiv-tall"
unifEquiv (TExp _     ) (TExp   _     ) = error "unifEquiv-texp"
unifEquiv _             _               = False


-----------------------------------------------------------------------------
unifys ::  PPR r => SourceSpan -> TDefEnv (RType r) 
            -> RSubst r -> [RType r] -> [RType r] -> Either Error (RSubst r)
-----------------------------------------------------------------------------
unifys loc γ θ ts ts'  
  | nTs == nTs' = foldM foo θ $ zip ts ts'
  | otherwise   = Left $ errorUnification loc ts ts'
  where 
    (nTs, nTs') = mapPair length (ts, ts')
    foo θ       = uncurry $ on (unify loc γ θ) (apply θ)


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
  | otherwise                                 = Left  $ errorRigidUnify l α t
  
unassigned α (Su m) = M.lookup α m == Just (tVar α)

