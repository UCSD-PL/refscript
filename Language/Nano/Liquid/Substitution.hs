module Language.Nano.Liquid.Substitution ( 
  
  -- * Substitutions
    Subst
  , toList

  -- * Type-class with operations
  , Substitutable (..)

  -- * Unification
  , unify

  ) where 

import           Control.Applicative ((<$>))

import           Language.Nano.Errors 
import           Language.Nano.Liquid.Types
import qualified Data.HashSet.Strict as S
import qualified Data.HashMap.Strict as M 

---------------------------------------------------------------------------
-- | Substitutions --------------------------------------------------------
---------------------------------------------------------------------------

-- | Type alias for Map from @TVar@ to @Type@. Hidden

type Subst = M.HashMap TVar Type

toList :: Subst -> [(TVar, Type)]
toList =  M.toList 

-- | Substitutions form a monoid; not commutative

instance Monoid Subst where 
  mempty       = M.empty
  mappend θ θ' = (apply θ' <$> θ) `M.union` θ' 

---------------------------------------------------------------------------
-- | Substitutions --------------------------------------------------------
---------------------------------------------------------------------------

class Substitutable a where 
  apply :: Subst -> a -> a 
  free  :: a -> S.Set TVar

class Substitutable a => Substitutable [a] where 
  apply = map apply 
  free  = S.unions . map free 

instance Substitutable Type where 
  apply θ (TApp c ts z) = TApp c (apply θ ts) z 
  apply θ t@(TVar α _)  = M.lookupDefault t α θ  
  apply θ (TFun ts t)   = TFun  (apply θ ts) (apply θ t)
  apply θ (TAll α t)    = apply (M.delete α θ) t 

  free (TApp _ ts _)    = S.unions   $ free ts
  free (TVar α _)       = S.singleton α 
  free (TFun ts t)      = S.unions   $ free (t:ts)
  free (TAll α t)       = S.delete α $ free t 


-----------------------------------------------------------------------------
unify :: SourcePos -> Type -> Type -> Either String Subst
-----------------------------------------------------------------------------

unify (TFun ts t) (TFun ts' t')     = unifys l (t:ts) (t':ts')
unify (TVar α _) t                  = varAsgn α t 
unify t (TVar α _)                  = varAsgn α t
unify (TApp c ts _) (TApp c' ts' _) 
  | c == c'                         = unifys l ts ts'
unify t t'                          = throwError $ errorUnification t t'             


unifys ts ts' 
  | nTs == nTs' = go ts ts' 
  | otherwise   = throwError $ errorUnification ts ts'
  where 
    go (t:ts) (t':ts') = do θ <- unify t t'
                            unifys (apply θ <$> ts) (apply θ <$> ts') 
    go [] []           = return emptySubst

mapPair (apply
