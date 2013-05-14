module Language.Nano.Liquid.Substitution ( 
  
  -- * Substitutions
    Subst
  , toList

  -- * Type-class with operations
  , Substitutable (..)

  -- * Unification
  , unify
  , unifys

  ) where 

import           Language.Fixpoint.Misc
import           Language.Nano.Errors 
import           Language.Nano.Liquid.Types

import           Control.Applicative ((<$>))
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M 
import           Data.Monoid
import           Text.Printf 

---------------------------------------------------------------------------
-- | Substitutions --------------------------------------------------------
---------------------------------------------------------------------------

-- | Type alias for Map from @TVar@ to @Type@. Hidden

newtype Subst = Su (M.HashMap TVar Type)

toList :: Subst -> [(TVar, Type)]
toList (Su m) =  M.toList m 

-- | Substitutions form a monoid; not commutative

instance Monoid Subst where 
  mempty                      = Su M.empty
  mappend θ@(Su m) θ'@(Su m') = Su $ (apply θ' <$> m) `M.union` m'

---------------------------------------------------------------------------
-- | Substitutions --------------------------------------------------------
---------------------------------------------------------------------------

class Substitutable a where 
  apply :: Subst -> a -> a 
  free  :: a -> S.HashSet TVar

instance Substitutable a => Substitutable [a] where 
  apply = map . apply 
  free  = S.unions . map free 

instance Substitutable Type where 
  apply θ (TApp c ts z)      = TApp c (apply θ ts) z 
  apply (Su m) t@(TVar α _)  = M.lookupDefault t α m  
  apply θ (TFun ts t)        = TFun  (apply θ ts) (apply θ t)
  apply (Su m) (TAll α t)    = apply (Su $ M.delete α m) t 

  free (TApp _ ts _)    = S.unions   $ free <$> ts
  free (TVar α _)       = S.singleton α 
  free (TFun ts t)      = S.unions   $ free <$> t:ts
  free (TAll α t)       = S.delete α $ free t 


-----------------------------------------------------------------------------
unify :: Type -> Type -> Either String Subst
-----------------------------------------------------------------------------

unify (TFun ts t) (TFun ts' t')     = unifys (t:ts) (t':ts')
unify (TVar α _) t                  = varAsgn α t 
unify t (TVar α _)                  = varAsgn α t
unify (TApp c ts _) (TApp c' ts' _) 
  | c == c'                         = unifys ts ts'
unify t t' 
  | t == t'                         = return mempty 
  | otherwise                       = Left $ errorUnification t t'             

unifys ts ts' 
  | nTs == nTs' = go (ts, ts') 
  | otherwise   = Left $ errorUnification ts ts'
  where 
    nTs                = length ts
    nTs'               = length ts'
    go (t:ts , t':ts') = unify t t' >>= \θ -> go (mapPair (apply θ) (ts, ts'))
    go ([]   , []    ) = return mempty


varAsgn α t 
  -- | t == TVar a      =  return empSubst
  | α `S.member` free t =  Left  $ printf "Occur check fails: %s in %s" (ppshow α) (ppshow t)
  | otherwise           =  Right $ Su $ M.singleton α t

