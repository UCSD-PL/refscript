module Language.Nano.Typecheck.Subst ( 
  
  -- * Substitutions
    RSubst ()
  , Subst 
  , toList
  , fromList

  -- * Type-class with operations
  , Substitutable (..)

  -- * Unification
  , unifys

  ) where 

import           Text.PrettyPrint.HughesPJ
import           Language.ECMAScript3.PrettyPrint
import           Language.Fixpoint.Misc
import           Language.Nano.Errors 
import           Language.Nano.Typecheck.Types

import           Control.Applicative ((<$>))
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M 
import           Data.Monoid
import           Text.Printf 

---------------------------------------------------------------------------
-- | Substitutions --------------------------------------------------------
---------------------------------------------------------------------------

-- | Type alias for Map from @TVar@ to @Type@. Hidden

data RSubst r = Su (M.HashMap TVar (RType r))
type Subst    = RSubst ()

toList        :: RSubst r -> [(TVar, RType r)]
toList (Su m) =  M.toList m 

fromList      :: [(TVar, RType r)] -> RSubst r
fromList      = Su . M.fromList 

-- | Substitutions form a monoid; not commutative

instance (F.Reftable r) => Monoid (RSubst r) where 
  mempty                    = Su M.empty
  mappend (Su m) θ'@(Su m') = Su $ (apply θ' <$> m) `M.union` m'

instance PP r => PP (Subst r) where 
  pp (Su m) = if M.null m then text "empty" else vcat $ (ppBind <$>) $ M.toList m 

ppBind (x, t) = pp x <+> text ":=" <+> pp t

---------------------------------------------------------------------------
-- | Substitutions --------------------------------------------------------
---------------------------------------------------------------------------

class Substitutable r a where 
  apply :: (RSubst r) -> a -> a 
  free  :: a -> S.HashSet TVar

instance Substitutable r a => Substitutable r [a] where 
  apply = map . apply 
  free  = S.unions . map free 

instance Substitutable () Type where 
  apply θ t = appTy θ t
    where 
      msg   = printf "apply [θ = %s] [t = %s]" (ppshow θ) (ppshow t)

  free (TApp _ ts _)    = S.unions   $ free <$> ts
  free (TVar α _)       = S.singleton α 
  free (TFun ts t)      = S.unions   $ free <$> t:ts
  free (TAll α t)       = S.delete α $ free t 

instance Substitutable () Fact where
  apply θ x@(PhiVar _)  = x
  apply θ (TypInst ts)  = TypInst $ apply θ ts

  free (PhiVar _)       = S.empty
  free (TypInst ts)     = free ts

-- type GType r = RType r without any TAll
-- appTy :: Subst -> GType r -> GType r

------------------------------------------------------------------------
appTy :: RSubst r -> RType r -> RType r
------------------------------------------------------------------------
appTy θ (TApp c ts z)      = TApp c (apply θ ts) z 
appTy (Su m) t@(TVar α r)  = (M.lookupDefault t α m) `meet` r
appTy θ (TFun ts t)        = TFun  (apply θ ts) (apply θ t)
appTy (Su m) (TAll α t)    = apply (Su $ M.delete α m) t 


-----------------------------------------------------------------------------
unify :: Subst () -> Type -> Type -> Either String Subst
-----------------------------------------------------------------------------

unify θ (TFun ts t) (TFun ts' t')     = unifys  θ (t:ts) (t':ts')
unify θ (TVar α _) t                  = varAsgn θ α t 
unify θ t (TVar α _)                  = varAsgn θ α t
unify θ (TApp c ts _) (TApp c' ts' _) 
  | c == c'                           = unifys  θ ts ts'
unify θ t t' 
  | t == t'                           = return θ
  | otherwise                         = Left $ errorUnification t t'             

unifys θ xs ys = {- tracePP msg $ -} unifys' θ xs ys 
  where 
    msg      = printf "unifys: [xs = %s] [ys = %s]"  (ppshow xs) (ppshow ys)

unifys' θ ts ts' 
  | nTs == nTs' = go θ (ts, ts') 
  | otherwise   = Left $ errorUnification ts ts'
  where 
    nTs                  = length ts
    nTs'                 = length ts'
    go θ (t:ts , t':ts') = unify θ t t' >>= \θ' -> go θ' (mapPair (apply θ') (ts, ts'))
    go θ ([]   , []    ) = return θ 

varAsgn θ α t 
  | t == tVar α         = Right $ θ 
  | α `S.member` free t = Left  $ printf "Occur check fails: %s in %s" (ppshow α) (ppshow t)
  | unassigned α θ      = Right $ θ `mappend` (Su $ M.singleton α t) 
  | otherwise           = Left  $ printf "Cannot unify rigid variable %s with %s" (ppshow α) (ppshow t) 
  
unassigned α (Su m) = M.lookup α m == Just (tVar α)



