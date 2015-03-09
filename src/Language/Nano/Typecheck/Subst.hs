{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Nano.Typecheck.Subst ( 
  
  -- * Substitutions
    RSubst
  , RSubstQ (..)
  , Subst 
  , toList
  , fromList
  , toSubst

  -- * Free Type Variables
  , Free (..)

  -- * Type-class with operations
  , Substitutable
  , SubstitutableQ (..)

  ) where 

import           Data.Maybe (maybeToList)
import           Text.PrettyPrint.HughesPJ
import           Language.Nano.Syntax
import           Language.Nano.Syntax.PrettyPrint
import qualified Language.Fixpoint.Types as F
import           Language.Nano.Annots
import           Language.Nano.Env
import           Language.Nano.Names
import           Language.Nano.Types
import           Language.Nano.Typecheck.Types
import           Language.Fixpoint.Misc (intersperse)

import           Control.Applicative ((<$>))
import qualified Data.HashSet as S
import qualified Data.Map.Strict as M 
import qualified Data.HashMap.Strict as HM 
import           Data.Monoid hiding ((<>))

-- import           Debug.Trace

---------------------------------------------------------------------------
-- | Substitutions
---------------------------------------------------------------------------

-- | Type alias for Map from @TVar@ to @Type@. Hidden

data RSubstQ q r = Su (HM.HashMap TVar (RTypeQ q r))

type RSubst r = RSubstQ AK r
type Subst    = RSubst ()

toSubst :: RSubst r -> Subst
toSubst (Su m) = Su $ HM.map toType m

toSubstQ :: RSubstQ q r -> RSubstQ q ()
toSubstQ (Su m) = Su $ HM.map toType m

toList        :: RSubst r -> [(TVar, RType r)]
toList (Su m) =  HM.toList m 

fromList      :: [(TVar, RTypeQ q r)] -> RSubstQ q r
fromList      = Su . HM.fromList 


-- | Substitutions form a monoid; not commutative

instance (F.Reftable r, SubstitutableQ q r (RType r)) => Monoid (RSubstQ q r) where 
  mempty                    = Su HM.empty
  mappend (Su m) θ'@(Su m') = Su $ (apply θ' <$> m) `HM.union` m'

instance (F.Reftable r, PP r) => PP (RSubst r) where 
  pp (Su m) = if HM.null m then text "empty" 
              else if HM.size m < 10 then intersperse comma $ (ppBind <$>) $ HM.toList m 
              else vcat $ (ppBind <$>) $ HM.toList m 

ppBind (x, t) = pp x <+> text ":=" <+> pp t


class Free a where 
  free  :: a -> S.HashSet TVar

instance Free (RType r) where
  free (TApp _ ts _)        = free ts
  free (TVar α _)           = S.singleton α 
  free (TFun s xts t _)     = free $ [t] ++ maybeToList s ++ (b_type <$> xts)
  free (TAll α t)           = S.delete α $ free t 
  free (TAnd ts)            = free ts 
  free (TExp _)             = error "free should not be applied to TExp"
  free (TCons m xts _)      = free (snd <$> M.toList xts) `mappend` free m
  free _                    = S.empty

instance Free a => Free [a] where 
  free                      = S.unions . map free

instance Free (Cast r) where
  free CNo         = S.empty
  free (CDead _ t) = free t
  free (CUp t t')  = free [t,t']
  free (CDn t t')  = free [t,t']

instance Free (Fact r) where
  free (PhiVar _)           = S.empty
  free (PhiVarTy t)         = free (snd t)
  free (PhiVarTC _)         = S.empty
  free (PhiPost _ )         = S.empty
  free (TypInst _ _ ts)     = free ts
  free (Overload _ t)       = free t
  free (EltOverload _ t)    = free t
  free (TCast _ c)          = free c
  free (VarAnn t)           = free t
  free (AmbVarAnn t)        = free t
  free (FieldAnn f)         = free f
  free (MethAnn m)          = free m
  free (StatAnn m)          = free m
  free (ConsAnn c)          = free c
  free (FuncAnn c)          = free c
  free (ReadOnlyVar)        = S.empty
  free (ClassAnn (vs,e,i))  = foldr S.delete (free $ e ++ i) vs
  free (UserCast t)         = free t
  free (IfaceAnn _)         = S.empty
  free (ExportedElt)        = S.empty
  free (ModuleAnn _)        = S.empty
  free (EnumAnn _)          = S.empty

instance Free (TypeMember r) where
  free (FieldSig _ o m t)   = free o `mappend` free m `mappend` free t
  free (MethSig  _ t)       = free t
  free (CallSig t)          = free t
  free (ConsSig t)          = free t
  free (IndexSig _ _ t)     = free t

instance Free a => Free (RelName, a) where
  free (_, a)               = free a

instance Free a => Free (Maybe a) where
  free Nothing              = S.empty
  free (Just a)             = free a

instance (Free a, Free b) => Free (a,b) where
  free (a,b)                = free a `S.union` free b

instance Free (QN l) where
  free _                    = S.empty

type Substitutable = SubstitutableQ AK

class SubstitutableQ q r a where 
  apply                     :: (RSubstQ q r) -> a -> a 

instance SubstitutableQ q r a => SubstitutableQ q r [a] where 
  apply                     = map . apply 

instance (SubstitutableQ q r a, SubstitutableQ q r b) => SubstitutableQ q r (a,b) where 
  apply f (x,y)             = (apply f x, apply f y)

instance F.Reftable r => SubstitutableQ q r (RTypeQ q r) where 
  apply θ t                 = appTy θ t

instance F.Reftable r => SubstitutableQ q r (BindQ q r) where 
  apply θ (B z t)           = B z $ appTy θ t

instance (SubstitutableQ q r t) => SubstitutableQ q r (Env t) where 
  apply                     = envMap . apply

instance F.Reftable r => SubstitutableQ q r (TypeMemberQ q r) where 
  apply θ (FieldSig x o m t) = FieldSig x   (appTy (toSubstQ θ) o) (appTy (toSubstQ θ) m) (apply θ t)
  apply θ (MethSig  x t)     = MethSig  x   (apply θ t)
  apply θ (CallSig t)        = CallSig      (apply θ t)
  apply θ (ConsSig t)        = ConsSig      (apply θ t)
  apply θ (IndexSig x b t)   = IndexSig x b (apply θ t)

instance F.Reftable r => SubstitutableQ q r (CastQ q r) where
  apply _ CNo         = CNo
  apply θ (CDead z t) = CDead z         (apply θ t)
  apply θ (CUp t t')  = CUp (apply θ t) (apply θ t')
  apply θ (CDn t t')  = CDn (apply θ t) (apply θ t')

instance F.Reftable r => SubstitutableQ q r (FactQ q r) where
  apply _ (PhiVar φ)         = PhiVar φ
  apply θ (TypInst i ξ ts)   = TypInst i ξ   $ apply θ ts
  apply θ (Overload ξ t)     = Overload ξ    $ apply θ t
  apply θ (EltOverload ξ t)  = EltOverload ξ $ apply θ t
  apply θ (TCast   ξ c)      = TCast ξ       $ apply θ c
  apply θ (VarAnn t)         = VarAnn        $ apply θ t
  apply θ (FieldAnn f)       = FieldAnn      $ apply θ f
  apply θ (MethAnn t)        = MethAnn       $ apply θ t
  apply θ (StatAnn t)        = StatAnn       $ apply θ t
  apply θ (ConsAnn t)        = ConsAnn       $ apply θ t
  apply θ (FuncAnn t)        = FuncAnn       $ apply θ t
  apply θ (ClassAnn (c,e,i)) = ClassAnn      $ (c, apply θ e, apply θ i)
  apply θ (UserCast t)       = UserCast      $ apply θ t
  apply _ a                  = a

instance SubstitutableQ q r a => SubstitutableQ q r (Maybe a) where
  apply θ (Just a)          = Just $ apply θ a
  apply _ Nothing           = Nothing

instance SubstitutableQ q r (Id a) where
  apply _ i                 = i

instance F.Reftable r => SubstitutableQ q r (Annot (FactQ q r) z) where
  apply θ (Ann i z fs)      = Ann i z $ apply θ fs

instance SubstitutableQ q r F.Symbol  where
  apply _ s                 = s 

instance SubstitutableQ q r (QN l) where
  apply _ s                 = s 

instance SubstitutableQ q r (QP l) where
  apply _ s                 = s 

instance F.Reftable r => SubstitutableQ q r (IfaceDefQ q r) where
  apply θ (ID n c v p e)    = ID n c v (apply θ p) (M.map (apply θ) e)

instance (F.Reftable r, SubstitutableQ q r a) => SubstitutableQ q r (Statement a) where
  apply θ s                 = fmap (apply θ) s

instance (F.Reftable r, SubstitutableQ q r t) => SubstitutableQ q r (FuncInputs t) where
  apply θ (FI a b)          = FI (apply θ a) (apply θ b)

instance SubstitutableQ q r Assignability where
  apply _ s                 = s

instance SubstitutableQ q r Initialization where
  apply _ s                 = s

instance (SubstitutableQ q r a, SubstitutableQ q r b, SubstitutableQ q r c) => SubstitutableQ q r (a,b,c) where
  apply θ (a,b,c)           = (apply θ a, apply θ b, apply θ c)

 
---------------------------------------------------------------------------------
appTy :: F.Reftable r => RSubstQ q r -> RTypeQ q r -> RTypeQ q r
---------------------------------------------------------------------------------
appTy θ        (TApp c ts r)   = flattenUnions $ TApp c (apply θ ts) r
appTy θ        (TRef x ts r)   = TRef x (apply θ ts) r
appTy θ        (TSelf m)       = TSelf (apply θ m)
appTy θ        (TAnd ts)       = TAnd (apply θ ts) 
appTy (Su m) t@(TVar α r)      = (HM.lookupDefault t α m) `strengthen` r
appTy θ        (TFun s ts t r) = TFun (apply θ <$> s) (apply θ ts) (apply θ t) r
appTy (Su m)   (TAll α t)      = TAll α $ apply (Su $ HM.delete α m) t
appTy θ        (TCons m es r)  = TCons (appTy (toSubstQ θ) m) (M.map (apply θ) es) r
appTy _        (TClass c)      = TClass c
appTy _        (TModule m)     = TModule m
appTy _        (TEnum e)       = TEnum e
appTy _        (TExp _)        = error "appTy should not be applied to TExp"


