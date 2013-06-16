{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Nano.Typecheck.Subst ( 
  
  -- * Substitutions
    RSubst (..)
  , Subst 
  , toList
  , fromList

  -- * Free Type Variables
  , Free (..)

  -- * Type-class with operations
  , Substitutable (..)

  -- * Unification
  , unifys

  -- * Subtyping
  , subtys

  ) where 

import           Text.PrettyPrint.HughesPJ
import           Language.ECMAScript3.PrettyPrint
import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Types as F
import           Language.Nano.Errors 
import           Language.Nano.Typecheck.Types

import           Control.Applicative ((<$>))
import qualified Data.HashSet as S
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as M 
import           Data.Monoid
import           Data.List           (sort, partition)
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

instance (F.Reftable r, Substitutable r (RType r)) => Monoid (RSubst r) where 
  mempty                    = Su M.empty
  mappend (Su m) θ'@(Su m') = Su $ (apply θ' <$> m) `M.union` m'

instance (F.Reftable r, PP r) => PP (RSubst r) where 
  pp (Su m) = if M.null m then text "empty" else vcat $ (ppBind <$>) $ M.toList m 

ppBind (x, t) = pp x <+> text ":=" <+> pp t

---------------------------------------------------------------------------
-- | Substitutions --------------------------------------------------------
---------------------------------------------------------------------------

class Free a where 
  free  :: a -> S.HashSet TVar

class Substitutable r a where 
  apply :: (RSubst r) -> a -> a 

instance Free a => Free [a] where 
  free = S.unions . map free

instance Substitutable r a => Substitutable r [a] where 
  apply = map . apply 

instance (PP r, F.Reftable r) => Substitutable r (RType r) where 
  apply θ t = appTy θ t
--     where 
--       msg   = printf "apply [θ = %s] [t = %s]" (ppshow θ) (ppshow t)

instance (PP r, F.Reftable r) => Substitutable r (Bind r) where 
  apply θ (B z t) = B z $ appTy θ t

instance Free (RType r) where
  free (TApp _ ts _)    = S.unions   $ free <$> ts
  free (TVar α _)       = S.singleton α 
  free (TFun xts t _)   = S.unions   $ free <$> t:ts where ts = b_type <$> xts
  free (TAll α t)       = S.delete α $ free t 

instance Substitutable () Fact where
  apply _ x@(PhiVar _)  = x
  apply θ (TypInst ts)  = TypInst $ apply θ ts

instance Free Fact where
  free (PhiVar _)       = S.empty
  free (TypInst ts)     = free ts

------------------------------------------------------------------------
-- appTy :: RSubst r -> RType r -> RType r
------------------------------------------------------------------------
appTy θ (TApp c ts z)      = TApp c (apply θ ts) z 
appTy (Su m) t@(TVar α r)  = (M.lookupDefault t α m) `strengthen` r
appTy θ (TFun ts t r)      = TFun  (apply θ ts) (apply θ t) r
appTy (Su m) (TAll α t)    = apply (Su $ M.delete α m) t 



applys f err θ ts ts' 
  | nTs == nTs' = go θ (ts, ts') 
  | otherwise   = Left $ err ts ts'
  where 
    nTs                  = length ts
    nTs'                 = length ts'
    go θ (t:ts , t':ts') = f θ t t' >>= \θ' -> go θ' (mapPair (apply θ') (ts, ts'))
    go θ (_    , _    )  = return θ 

-----------------------------------------------------------------------------
unify :: Subst -> Type -> Type -> Either String Subst
-----------------------------------------------------------------------------
unify θ (TFun xts t _) (TFun xts' t' _) = unifys θ (t: (b_type <$> xts)) (t': (b_type <$> xts'))
unify θ (TVar α _) (TVar β _)           = varEql θ α β 
unify θ (TVar α _) t                    = varAsn θ α t 
unify θ t (TVar α _)                    = varAsn θ α t

unify θ (TApp c ts _) (TApp c' ts' _)
  | c == c'                             = unifys  θ ts ts'

unify θ t t' 
  | t == t'                             = return θ
  | otherwise                           = Left $ errorUnification t t'

unifys         ::  Subst -> [Type] -> [Type] -> Either String Subst
unifys θ xs ys =  tracePP msg $  unifys' θ xs ys 
   where 
     msg      = printf "unifys: [xs = %s] [ys = %s]"  (ppshow xs) (ppshow ys)

unifys' = applys unify errorUnification

varEql θ α β = case varAsn θ α (tVar β) of 
                 z@(Right _) -> z
                 (Left e1)   -> case varAsn θ β (tVar α) of
                                  z@(Right _) -> z
                                  (Left e2)   -> Left (e1 ++ "\n OR \n" ++ e2) 
 
varAsn θ α t 
  | t == tVar α         = Right $ θ 
  | α `S.member` free t = Left  $ errorOccursCheck α t 
  | unassigned α θ      = Right $ θ `mappend` (Su $ M.singleton α t) 
  | otherwise           = Left  $ errorRigidUnify α t
  
unassigned α (Su m) = M.lookup α m == Just (tVar α)
 

-----------------------------------------------------------------------------
subty :: Subst -> Type -> Type -> Either String Subst
-----------------------------------------------------------------------------
subty θ (TApp TUn ts _ ) (TApp TUn ts' _) | not $ any var ts = subset ts ts' θ
subty θ t@(TApp TUn xs  _) t' = 
  case tvs of
    [ ]  -> subset xs [t'] θ
    [v]  -> unify θ v t' >>= subset ts [t']
    _    -> Left $ errorSubType t t'
  where 
    (tvs, ts) = partition var xs

subty θ t t'@(TApp TUn ts _ ) = 
  case t of 
    TVar _ _ -> unify θ t t'
    _        -> subset [t] ts θ

subty θ t t' = unify θ t t'

-- subty θ (TApp TUn ts _ ) t  = subtys ts [t]
-- subty θ t (TApp TUn ts' _ ) = subtys [t] ts'
-- subty θ t t'                = subtys [t] [t']

var (TVar _ _) = True
var _ = False

-----------------------------------------------------------------------------
subtys ::  Subst -> [Type] -> [Type] -> Either String Subst
-----------------------------------------------------------------------------
subtys θ xs ys =  tracePP msg $  subtys' θ xs ys 
   where 
     msg      = printf "unifys: [xs = %s] [ys = %s]"  (ppshow xs) (ppshow ys)


subtys' = applys subty errorSubType


-----------------------------------------------------------------------------
-- At this point the lists contain flat types (no unions)
-----------------------------------------------------------------------------
subset ::  [Type] -> [Type] -> Subst -> Either String Subst
-----------------------------------------------------------------------------
subset xs ys θ = 
  if Set.isSubsetOf (Set.fromList xs) (Set.fromList ys)
    then Right $ θ
    else Left  $ errorSubType xs ys

