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

  -- * Print
  , pp'
  ) where 

import           Text.PrettyPrint.HughesPJ
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.PrettyPrint
import qualified Language.Fixpoint.Types as F
import           Language.Nano.Env
import           Language.Nano.Typecheck.Types
import           Language.Fixpoint.Misc (intersperse)

import           Language.Fixpoint.PrettyPrint
import           Control.Applicative ((<$>))
import qualified Data.HashSet as S
import qualified Data.List as L
import qualified Data.HashMap.Strict as M 
import           Data.Monoid hiding ((<>))
import           Data.Function (fix, on)
import           Data.Maybe (fromJust)

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
  apply θ (CDead t)  = CDead (apply θ t)
  apply θ (CUp t t') = CUp (apply θ t) (apply θ t')
  apply θ (CDn t t') = CDn (apply θ t) (apply θ t')

instance (PP r, F.Reftable r) => Substitutable r (Fact r) where
  apply _ x@(PhiVar _)      = x
  apply θ (TypInst ξ ts)    = TypInst ξ $ apply θ ts
  apply θ (Overload t)      = Overload (apply θ <$> t)
  apply θ (TCast   ξ c)     = TCast ξ $ apply θ c
  apply θ (VarAnn t)        = VarAnn $ apply θ t
  apply θ (FieldAnn (m,t))  = FieldAnn (m,apply θ t)
  apply θ (MethAnn t)       = MethAnn $ apply θ t
  apply θ (ConsAnn t)       = ConsAnn $ apply θ t
  apply θ (ClassAnn (c, t)) = ClassAnn (c, apply θ t)

instance (PP r, F.Reftable r, Substitutable r a) => Substitutable r (Maybe a) where
  apply θ (Just a)       = Just $ apply θ a
  apply _ Nothing        = Nothing

instance (PP r, F.Reftable r) => Substitutable r (Id a) where
  apply _ i              = i

instance (PP r, F.Reftable r) => Substitutable r (Annot (Fact r) z) where
  apply θ (Ann z fs)     = Ann z $ apply θ fs

instance Free (Cast r) where
  free CNo        = S.empty
  free (CDead t)  = free t
  free (CUp t t') = S.union (free t) (free t')
  free (CDn t t') = S.union (free t) (free t')

instance (PPR r) => Substitutable r (TDef (RType r)) where
  apply θ (TD n v p e)   = TD n v (apply θ p) (apply θ e)


instance Free (Fact r) where
  free (PhiVar _)        = S.empty
  free (TypInst _ ts)    = free ts
  free (Overload t)      = free t
  free (TCast _ c)       = free c
  free (VarAnn t)        = free t
  free (FieldAnn (_,t))  = free t
  free (MethAnn t)       = free t
  free (ConsAnn t)       = free t
  free (ClassAnn (vs,m)) = foldr S.delete (free m) vs

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
appTy θ        (TCons ts r)  = TCons (apply θ ts) r
appTy _        (TExp _)      = error "appTy should not be applied to TExp"


-- | `flatten`: Unfolds one level of the input type, flattening all the fields
-- inherited by possible parent classes.

------------------------------------------------------------------------
flatten :: PPR r => TDefEnv (RType r) -> TDef (RType r) -> [TElt (RType r)]
------------------------------------------------------------------------
flatten = flatten' False

------------------------------------------------------------------------
flattenType :: PPR r => TDefEnv (RType r) -> RType r -> RType r
------------------------------------------------------------------------
flattenType = flattenType' False

------------------------------------------------------------------------
flattenTRef :: PPR r => TDefEnv (RType r) -> RType r -> [TElt (RType r)]
------------------------------------------------------------------------
flattenTRef = flattenTRef' False


-- WARNING: Calling the following with True for @b@ may cause infinite loop for
-- recursive types.

flatten' b δ = fix ff
  where
    ff r (TD _ vs (Just (i, ts)) es)
      = L.unionBy nm (ee es) 
      $ r 
      $ fromJust 
      $ apply (fromList $ zip vs (tt ts)) (findTySym i δ)

    ff _ (TD _ _ _ es) = ee es

    nm = (==) `on` f_sym

    ee es | b         = flattenElt' b δ <$> es
          | otherwise = es

    tt ts | b         = flattenType' b δ <$> ts
          | otherwise = ts


flattenElt' b δ (TE s m t) = TE s m $ flattenType' b δ t

flattenTRef' b δ (TApp (TRef n) ts _) 
                            = apply θ (flatten' b δ d)
  where d@(TD _ vs _ _)     = findTyIdOrDie n δ
        θ                   = fromList $ zip vs ts
flattenTRef' _ _ _ = error "Applying flattenTRef on non-tref"


-- | Single level of flattenning types that contain references to types 
-- with flat objects
flattenType' b δ t@(TApp (TRef _) [] r) 
                                = TCons (bind <$> flattenTRef' b δ t) r
                                    where bind (TE s _ t) = B s t
flattenType' _ _  (TApp (TRef _) _ _) 
                                = error "[BUG] flattenType'"
flattenType' b δ (TApp c ts r)  = TApp c (flattenType' b δ <$> ts) r
flattenType' _ _ (TVar v r)     = TVar v r
flattenType' b δ (TFun ts to r) = TFun (f <$> ts) (flattenType' b δ to) r
                                    where f (B s t) = B s $ flattenType' b δ t
flattenType' b δ (TArr t r)     = TArr (flattenType' b δ t) r
flattenType' b δ (TAll v t)     = TAll v $ flattenType' b δ t
flattenType' b δ (TAnd ts)      = TAnd $ flattenType' b δ <$> ts
flattenType' b δ (TCons ts r)   = TCons (flattenBind' b δ <$> ts) r
flattenType' _ _ _              = error "TExp should not appear here"

flattenBind' b δ (B s t)        = B s $ flattenType' b δ t 



-- | pp': Print expanded types
-----------------------------------------------------------------------------
pp' :: (F.Reftable r, PP r) => TDefEnv (RType r) -> RType r -> Doc
-----------------------------------------------------------------------------
pp' _   (TVar α r)           = F.ppTy r $ pp α 
pp' δ   (TFun xts t' _)      = ppBinds' δ parens comma xts <+> text "=>" <+> pp' δ t' 
pp' δ t@(TAll _ _)           = text "forall" <+> ppArgs id space αs <> text "." <+> pp' δ t' where (αs, t') = bkAll t
pp' δ   (TAnd ts)            = vcat [text "/\\" <+> pp' δ t | t <- ts]
pp' _   (TExp e)             = pprint e
pp' δ   (TApp TUn ts r)      = F.ppTy r $ ppArgs' δ id (text " +") ts 
pp' δ t@(TApp (TRef i) ts r) = F.ppTy r $ maybe (anonym t) named (getTDefName δ i)
  where anonym t             = pp $ flattenType' True δ t
        named  t             = pp t <+> ppArgs' δ brackets comma ts
pp' _   (TApp c [] r)        = F.ppTy r $ pp c 
pp' δ   (TApp c ts r)        = F.ppTy r $ parens (pp c <+> ppArgs' δ id space ts)  
pp' δ   (TArr t' r)          = F.ppTy r $ brackets (pp' δ t')
pp' δ   (TCons bs r)         = F.ppTy r $ ppBinds' δ braces comma bs

ppArgs' δ p sep              = p . intersperse sep . map (pp' δ)
ppBinds' δ p sep             = p . intersperse sep . map (ppBind' δ)

ppBind' δ (B x t)            = pp x <> colon <> pp' δ t 

