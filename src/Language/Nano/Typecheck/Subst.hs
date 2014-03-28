{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Nano.Typecheck.Subst ( 
  
  -- * Substitutions
    RSubst (..)
  , Subst 
  , toList
  , fromList
  , toSubst

  -- * Free Type Variables
  , Free (..)

  -- * Type-class with operations
  , Substitutable (..)

  -- * Flatten a type definition applying subs
  , flatten, flattenTRef, flattenType
  ) where 

import           Text.PrettyPrint.HughesPJ
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.PrettyPrint
import qualified Language.Fixpoint.Types as F
import           Language.Nano.Env
import           Language.Nano.Typecheck.Types
import           Language.Fixpoint.Misc (mapSnd)

import           Control.Applicative ((<$>))
import qualified Data.HashSet as S
import qualified Data.List as L
import qualified Data.HashMap.Strict as M 
import           Data.Monoid
import           Data.Maybe (fromMaybe)

-- import           Debug.Trace

type PPR r = (PP r, F.Reftable r)

---------------------------------------------------------------------------
-- | Substitutions
---------------------------------------------------------------------------

-- | Type alias for Map from @TVar@ to @Type@. Hidden

data RSubst r = Su (M.HashMap TVar (RType r))
type Subst    = RSubst ()

toSubst :: RSubst r -> Subst
toSubst (Su m) = Su $ M.map toType m

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
-- | Substitutions
---------------------------------------------------------------------------

class Free a where 
  free  :: a -> S.HashSet TVar

class Substitutable r a where 
  apply :: (RSubst r) -> a -> a 

instance Free a => Free [a] where 
  free = S.unions . map free

instance Substitutable r a => Substitutable r [a] where 
  apply = map . apply 

instance (Substitutable r a, Substitutable r b) => Substitutable r (a,b) where 
  apply f (x,y) = (apply f x, apply f y)

instance (PP r, F.Reftable r) => Substitutable r (RType r) where 
  apply θ t = appTy θ t
--     where 
--       msg   = printf "apply [θ = %s] [t = %s]" (ppshow θ) (ppshow t)

instance (PP r, F.Reftable r) => Substitutable r (Bind r) where 
  apply θ (B z t) = B z $ appTy θ t

instance (PP r, F.Reftable r, Substitutable r t) => Substitutable r (Env t) where 
  apply = envMap . apply

instance (PP r, F.Reftable r, Substitutable r t) => Substitutable r (TElt t) where 
  apply θ (TE s b t) = TE s b $ apply θ t

instance Free (RType r) where
  free (TApp _ ts _)        = S.unions   $ free <$> ts
  free (TArr t _)           = free t
  free (TVar α _)           = S.singleton α 
  free (TFun xts t _)       = S.unions   $ free <$> t:ts where ts = b_type <$> xts
  free (TAll α t)           = S.delete α $ free t 
  free (TAnd ts)            = S.unions   $ free <$> ts 
  free (TExp _)             = error "free should not be applied to TExp"
  free (TCons _ _)          = error "free should not be applied to TCons"

instance (PP r, F.Reftable r) => Substitutable r (Cast r) where
  apply _ CNo        = CNo
  apply _ CDead      = CDead
  apply θ (CUp t t') = CUp (apply θ t) (apply θ t')
  apply θ (CDn t t') = CDn (apply θ t) (apply θ t')
  apply θ (CFn cs c) = CFn (mapSnd (apply θ) <$> cs) (apply θ c)
  apply θ (CCs cs  ) = CCs $ mapSnd (apply θ) <$> cs

instance (PP r, F.Reftable r) => Substitutable r (Fact r) where
  apply _ x@(PhiVar _)   = x
  apply θ (TypInst ξ ts) = TypInst ξ $ apply θ ts
  apply θ (Overload t)   = Overload (apply θ <$> t)
  apply θ (TCast   ξ c)  = TCast   ξ $ apply θ c
  apply θ (TAnnot t)     = TAnnot  $ apply θ t
  apply θ (CAnnot (c, t))= CAnnot  (c, apply θ t)

instance (PP r, F.Reftable r, Substitutable r a) => Substitutable r (Maybe a) where
  apply θ (Just a)       = Just $ apply θ a
  apply _ Nothing        = Nothing

instance (PP r, F.Reftable r) => Substitutable r (Id a) where
  apply _ i              = i

instance (PP r, F.Reftable r) => Substitutable r (Annot (Fact r) z) where
  apply θ (Ann z fs)     = Ann z $ apply θ fs

instance Free (Cast r) where
  free CNo        = S.empty
  free CDead      = S.empty
  free (CUp t t') = S.union (free t) (free t')
  free (CDn t t') = S.union (free t) (free t')
  free (CFn cs c) = S.unions $ free <$> c:cs' where cs' = snd <$> cs
  free (CCs cs  ) = S.unions $ (free . snd) <$> cs

instance (PPR r) => Substitutable r (TDef (RType r)) where
  apply θ (TD n v p e)   = TD n v p $ apply θ e


instance Free (Fact r) where
  free (PhiVar _)        = S.empty
  free (TypInst _ ts)    = free ts
  free (Overload t)      = free t
  free (TCast _ c)       = free c
  free (TAnnot t)        = free t
  free (CAnnot (vs,m))   = foldr S.delete (free m) vs

instance Free a => Free (Id b, a) where
  free (_, a)            = free a
 
instance Free a => Free (Maybe a) where
  free Nothing  = S.empty
  free (Just a) = free a
 
------------------------------------------------------------------------
appTy :: (PP r, F.Reftable r) => RSubst r -> RType r -> RType r
------------------------------------------------------------------------
appTy θ        (TApp c ts r) = TApp c (apply θ ts) r
appTy θ        (TAnd ts)     = TAnd (apply θ ts) 
appTy (Su m) t@(TVar α r)    = (M.lookupDefault t α m) `strengthen` r
appTy θ        (TFun ts t r) = TFun  (apply θ ts) (apply θ t) r
appTy (Su m)   (TAll α t)    = TAll α $ apply (Su $ M.delete α m) t
appTy θ        (TArr t r)    = TArr (apply θ t) r
appTy _        (TExp _)      = error "appTy should not be applied to TExp"
appTy _        (TCons _ _)   = error "appTy should not be applied to TCons"


------------------------------------------------------------------------
flatten :: PPR r => TDefEnv (RType r) -> TDef (RType r) -> [TElt (RType r)]
------------------------------------------------------------------------
flatten δ (TD _ vs pro elts) = 
  -- NOTE: unionBy favors elts (as it should)
  L.unionBy name elts $ fromMaybe [] base 
  where
    name e1 e2 = f_sym e1 == f_sym e2
    base       = pro >>= act >>= return . t_elts
    act (n,ts) = apply (fromList $ zip vs ts) (findTySym n δ) 

------------------------------------------------------------------------
flattenTRef :: PPR r => TDefEnv (RType r) -> RType r -> [TElt (RType r)]
------------------------------------------------------------------------
flattenTRef δ (TApp (TRef n) ts _) 
                            = apply θ (flatten δ d)
  where d@(TD _ vs _ _)     = findTyIdOrDie n δ
        θ                   = fromList $ zip vs ts
flattenTRef _ _ = error "Applying flattenTRef on non-tref"


-- | Flatten types that contain references to types with flat objects
------------------------------------------------------------------------
flattenType :: PPR r => TDefEnv (RType r) -> RType r -> RType r
------------------------------------------------------------------------
flattenType δ t@(TApp (TRef n) ts r) 
                            = TCons (bind <$> flattenTRef δ t) r
  where bind (TE s _ t)     = B s $ flattenType δ t
flattenType δ (TApp c ts r) = TApp c (flattenType δ <$> ts) r
flattenType _ (TVar v r)    = TVar v r
flattenType δ (TFun bs t r) = TFun (f <$> bs) (flattenType δ t) r
  where f (B s t)           = B s $ flattenType δ t
flattenType δ (TArr t r)    = TArr (flattenType δ t) r
flattenType δ (TAll v t)    = TAll v $ flattenType δ t
flattenType δ (TAnd ts)     = TAnd $ flattenType δ <$> ts
flattenType δ (TCons ts r)  = TCons (flattenBind δ <$> ts) r
flattenType _ _             = error "TExp should not appear here"

flattenBind δ (B s t)         = B s $ flattenType δ t 
