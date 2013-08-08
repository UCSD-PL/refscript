{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Nano.Typecheck.Unify ( 
  
  -- * Unification 
  unify, unifys

  ) where 

-- import           Text.PrettyPrint.HughesPJ
-- import           Language.ECMAScript3.PrettyPrint
import           Language.Fixpoint.Misc
-- import qualified Language.Fixpoint.Types as F
import           Language.Nano.Errors 
import           Language.Nano.Env
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Typecheck.Compare

import           Control.Applicative ((<$>))
-- import           Control.Monad
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M 
import           Data.Monoid
import           Text.Printf 
import           Debug.Trace
-- import           Language.Nano.Misc (mkEither)


-----------------------------------------------------------------------------
-- Unification --------------------------------------------------------------
-----------------------------------------------------------------------------

-- | Unify types @t@ and @t'@, using @θ@ as the current substitution and @env@
-- as the current type definition environment.
-----------------------------------------------------------------------------
unify :: Env Type -> Subst -> Type -> Type -> Either String Subst
-----------------------------------------------------------------------------

-- TODO: is this right??
unify _ θ t@(TApp _ _ _) t'@(TApp _ _ _) 
  | any isTop [t,t']                    = Right $ θ

unify env θ (TFun xts t _) (TFun xts' t' _) = 
  unifys env θ (t: (b_type <$> xts)) (t': (b_type <$> xts'))

unify env θ t@(TApp d@(TDef _) ts _) t'@(TApp d'@(TDef _) ts' _)
  | d == d'                             = unifys env θ ts ts'
  | otherwise                           = Left $ errorUnification t t'

unify env θ t@(TApp (TDef _) _ _) t'    = unify env θ (unfoldSafe env t) t'

unify env θ t t'@(TApp (TDef _) _ _)    = unify env θ t (unfoldSafe env t')

unify _  θ (TVar α _)     (TVar β _)    = varEql θ α β 
unify _  θ (TVar α _)     t             = varAsn θ α t 
unify _  θ t              (TVar α _)    = varAsn θ α t

-- List[A] + Null `unif` List[T0] + Null => A `unif` T0
-- TODO: make sure other nothing weird is going on with TVars,
-- e.g.  List[A] + B `unif` ... => this should not even be allowed!!!
unify γ θ t t' | any isUnion [t,t']     = 
  (uncurry $ unifys γ θ) $ unzip $ fst3 $ unionPartsWithEq unifEq γ t t'

unify _ _ (TBd _) _   = error $ bugTBodiesOccur "unify"
unify _ _ _ (TBd _)   = error $ bugTBodiesOccur "unify"

-- For the rest of the cases, blindly return the subst
-- Defer all other checks for later
unify _ θ _ _         = Right $ θ  


{-unify' γ θ t t' = unify γ θ (trace (printf "unify: %s - %s" (show t) (show t')) t) t' -}


unifEq (TApp d@(TDef _) _ _) (TApp d'@(TDef _) _ _) = d == d'
unifEq t t' = equiv t t'
  


-----------------------------------------------------------------------------
unifys ::  Env Type -> Subst -> [Type] -> [Type] -> Either String Subst
-----------------------------------------------------------------------------
unifys env θ xs ys = {-  tracePP msg $ -} unifys' env θ xs ys 
   {-where -}
   {-  msg      = printf "unifys: [xs = %s] [ys = %s]"  (ppshow xs) (ppshow ys)-}

unifys' env θ ts ts' 
  | nTs == nTs' = go env θ ts ts'
  | otherwise   = Left $ errorUnification ts ts'
  where 
    nTs                      = length ts
    nTs'                     = length ts'
    go γ θ ts ts' = foldl safeJoin (Right $ θ) $ zipWith (unify γ θ) ts ts'
    -- Only allow joining unifications where the common keys map to identical
    -- types
    safeJoin (Right θ@(Su m)) (Right θ'@(Su m'))
      | check m m' = Right $ mappend θ θ'
      | otherwise  = Left  $ printf "Cannot join substs: %s\nand\n%s"
                               (ppshow θ) (ppshow θ')
    safeJoin (Left l        ) _                  = Left l
    safeJoin _                (Left l        )   = Left l
                               

check m m' = vs == vs'
  where vs  = (`M.lookup` m ) <$> ks
        vs' = (`M.lookup` m') <$> ks
        ks  = M.keys $ M.intersection (clr m) (clr m')
        clr = M.filterWithKey (\k v -> tVar k /= v)


-----------------------------------------------------------------------------
varEql :: Subst -> TVar -> TVar -> Either String Subst
-----------------------------------------------------------------------------
varEql θ α β =  
  case varAsn θ α $ tVar β of
    Right θ' -> Right θ'
    Left  s1 -> 
      case varAsn θ β $ tVar α of
        Right θ'' -> Right θ''
        Left  s2  -> Left (s1 ++ "\n OR \n" ++ s2)


-----------------------------------------------------------------------------
varAsn :: Subst -> TVar -> Type -> Either String Subst
-----------------------------------------------------------------------------
varAsn θ α t 
  -- Check if previous substs are sufficient 
  | t == apply θ (tVar α)  = Right $ θ 
  -- We are not even checking if t is a subtype of `tVar α`, i.e.:
  -- unifying A with A + B will fail!
  | t == tVar α            = Right $ θ 
  | α `S.member` free t    = Left  $ errorOccursCheck α t 
  | unassigned α θ         = Right $ θ `mappend` (Su $ M.singleton α t)
  | otherwise              = Left  $ errorRigidUnify α t
  
unassigned α (Su m) = M.lookup α m == Just (tVar α)


