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
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M 
import           Data.Monoid
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
unify l δ θ t t' | any isUnion [t,t']
  = let (ts, _, _) = unionParts' unifEquiv t t' in
    let (t1s, t2s) = unzip ts in
    unifys l δ θ t1s t2s

unify l δ θ (TApp (TRef i) ts _) (TApp (TRef i') ts' _) 
  | i == i'   = unifys l δ θ ts ts'

unify l δ θ t1@(TApp (TRef _) _ _) t2
  = unify l δ θ (flattenType δ t1) t2

unify l δ θ t1 t2@(TApp (TRef _) _ _)
  = unify l δ θ t1 (flattenType δ t2)

unify l δ θ (TArr t _) (TArr t' _) = unify l δ θ t t'

unify l δ θ (TCons e1s _) (TCons e2s _)
  = unifys l δ θ t1s t2s
  where 
    (t1s, t2s) = unzip [ (t1, t2) | TE s1 _ t1 <- e1s
                                  , TE s2 _ t2 <- e2s
                                  , s1 == s2 ]

-- The rest of the cases do not cause any unification.
unify _ _ θ _  _ = return θ


unifEquiv t t' | toType t == toType t' 
               = True
unifEquiv t t' | any isUnion [t,t'] 
               = error "No nested unions"
unifEquiv (TApp c _ _ ) (TApp c' _ _  ) = c `equiv` c'  -- Interfaces appear once only on top-level unions
unifEquiv (TArr _ _   ) (TArr _ _     ) = True          -- Arrays are really interfaces
unifEquiv (TCons _ _  ) (TCons _ _    ) = True
unifEquiv (TVar v _   ) (TVar v' _    ) = v == v'
unifEquiv (TFun _ _ _ ) (TFun _ _ _   ) = True          -- Functions as well ... 
unifEquiv (TAll _ _   ) (TAll _ _     ) = error "unifEquiv-tall"
unifEquiv (TExp _     ) (TExp   _     ) = error "unifEquiv-texp"
unifEquiv _             _               = False


-----------------------------------------------------------------------------
unifys ::  PPR r => SourceSpan -> TDefEnv (RType r) 
            -> RSubst r -> [RType r] -> [RType r] -> Either Error (RSubst r)
-----------------------------------------------------------------------------
unifys loc env θ ts ts'  | nTs == nTs'         
                         = go env θ ts ts'
                         | otherwise           
                         = Left $ errorUnification loc ts ts'
  where 
    (nTs, nTs')          = mapPair length (ts, ts')
    go γ θ (s:ss) (t:tt) = either Left (\θ' -> go γ θ' ss tt) $ unify loc γ θ s t
    go _ θ []     []     = Right θ
    go _ _ _      _      = Left $ errorUnification loc ts ts'
--     safeJoin (Right θ@(Su m)) (Right θ'@(Su m'))
--       | check m m'       = Right $ mappend θ θ'
--       | otherwise        = Left  $ errorJoinSubsts loc θ θ' 
--     safeJoin (Left l) _  = Left l
--     safeJoin _ (Left l)  = Left l
                               
-- 
-- check m m' = vs == vs'
--   where vs  = map (toType <$>) $ (`M.lookup` m ) <$> ks
--         vs' = map (toType <$>) $ (`M.lookup` m') <$> ks
--         ks  = M.keys $ M.intersection (clr m) (clr m')
--         clr = M.filterWithKey (\k v -> tVar k /= v)


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
  | on (==) toType t (apply θ (tVar α)) = Right $ θ 
  | on (==) toType t (tVar α)           = Right $ θ 
  | α `S.member` free t                 = Left  $ errorOccursCheck l α t 
  | unassigned α θ                      = Right $ θ `mappend` (Su $ M.singleton α t)
  | otherwise                           = Left  $ errorRigidUnify l α t θ
  
unassigned α (Su m) = M.lookup α m == Just (tVar α)

