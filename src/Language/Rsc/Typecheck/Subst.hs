{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Rsc.Typecheck.Subst (

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

import           Control.Applicative          ((<$>))
import qualified Data.HashMap.Strict          as HM
import qualified Data.HashSet                 as S
import           Data.Monoid                  hiding ((<>))
import           Language.Fixpoint.Misc       (intersperse)
import qualified Language.Fixpoint.Types      as F
import           Language.Rsc.Annotations
import           Language.Rsc.AST
import           Language.Rsc.Core.Env
import           Language.Rsc.Locations
import           Language.Rsc.Misc            (mapSnd)
import           Language.Rsc.Names
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
import           Text.PrettyPrint.HughesPJ

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

toList        :: RSubst r -> [(TVar, RType r)]
toList (Su m) =  HM.toList m

fromList      :: [(TVar, RTypeQ q r)] -> RSubstQ q r
fromList      = Su . HM.fromList


-- | Substitutions form a monoid; not commutative

instance (F.Reftable r, SubstitutableQ q r (RType r)) => Monoid (RSubstQ q r) where
  mempty                    = Su HM.empty
  mappend (Su m) θ'@(Su m') = Su $ (apply θ' <$> m) `HM.union` m'


class Free a where
  free  :: a -> S.HashSet TVar

instance Free (RType r) where
  free (TPrim _ _)          = S.empty
  free (TVar α _)           = S.singleton α
  free (TOr ts)             = free ts
  free (TAnd ts)            = free $ snd <$> ts
  free (TRef n _)           = free n
  free (TObj m es _)        = free m `mappend` free es
  free (TClass t)           = free t
  free (TMod _)             = S.empty
  free (TAll α t)           = S.delete (btvToTV α) $ free t
  free (TFun xts t _)       = free $ [t] ++ (b_type <$> xts)
  free (TExp _)             = error "free should not be applied to TExp"

instance Free (TGen r) where
  free (Gen _ ts)           = free ts

instance Free (TypeMembers r) where
  free (TM ms sms cl ct s n)
                            = S.unions [free ms, free sms,
                                        free cl, free ct, free s, free n]

instance Free t => Free (F.SEnv t) where
  free                      = free . map snd . F.toListSEnv

instance Free (TypeMember r) where
  free (FI _ _ t)           = free t
  free (MI _ mts)           = free $ map snd mts

instance Free a => Free [a] where
  free                      = S.unions . map free

instance Free (Cast r) where
  free CNo                  = S.empty
  free (CDead _ t)          = free t
  free (CUp t t')           = free [t,t']
  free (CDn t t')           = free [t,t']

instance Free (Fact r) where
  free (PhiVarTy (_,t))     = free t
  free (TypInst _ _ ts)     = free ts
  free (EltOverload _ t t') = free [t, t']
  free (VarAnn _ _ t )      = free t
  free (MemberAnn f)        = free f
  free (CtorAnn c)          = free c
  free (UserCast t)         = free t
  free (SigAnn _ c)         = free c
  free (TCast _ c)          = free c
  free (ClassAnn _ t)       = free t --  foldr S.delete (free $ e ++ i) vs
  free (InterfaceAnn t)     = free t --  foldr S.delete (free $ e ++ i) vs
  free _                    = S.empty

instance Free (TypeSig r) where
  free (TS _ n h)           = S.unions [free n, free h]

instance Free (TypeDecl r) where
  free (TD s m)             = S.unions [free s, free m]

instance Free (BTGen r) where
  free (BGen n ts)          = S.unions [free n, free ts]

instance Free (BTVar r) where
  free (BTV _ _ t)          = free t

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

instance SubstitutableQ q r t => SubstitutableQ q r (Located t) where
  apply θ (Loc s v)         = Loc s $ apply θ v

instance F.Reftable r => SubstitutableQ q r (VarInfoQ q r) where
  apply θ (VI l a i t)      = VI l a i $ apply θ t

instance F.Reftable r => SubstitutableQ q r (CastQ q r) where
  apply _ CNo         = CNo
  apply θ (CDead z t) = CDead z         (apply θ t)
  apply θ (CUp t t')  = CUp (apply θ t) (apply θ t')
  apply θ (CDn t t')  = CDn (apply θ t) (apply θ t')

instance F.Reftable r => SubstitutableQ q r (FactQ q r) where
  apply θ (PhiVarTy (v,t))     = PhiVarTy . (v,) $ apply θ t
  apply θ (TypInst i ξ ts)     = TypInst i ξ     $ apply θ ts
  apply θ (EltOverload ξ t t') = EltOverload ξ (apply θ t) (apply θ t')
  apply θ (VarAnn l a t)       = VarAnn l a      $ apply θ t
  apply θ (MemberAnn f)        = MemberAnn       $ apply θ f
  apply θ (CtorAnn t)          = CtorAnn         $ apply θ t
  apply θ (UserCast t)         = UserCast        $ apply θ t
  apply θ (SigAnn l t)         = SigAnn l        $ apply θ t
  apply θ (TCast ξ t)          = TCast ξ         $ apply θ t
  apply θ (ClassAnn l t)       = ClassAnn l      $ apply θ t
  apply θ (InterfaceAnn t)     = InterfaceAnn    $ apply θ t
  apply _ a                    = a

instance F.Reftable r => SubstitutableQ q r (TypeMemberQ q r) where
  apply θ (FI ms m t)       = FI ms m (apply θ t)
  apply θ (MI o mts)        = MI o (mapSnd (apply θ) <$> mts)

instance SubstitutableQ q r a => SubstitutableQ q r (Maybe a) where
  apply θ (Just a)          = Just $ apply θ a
  apply _ Nothing           = Nothing

instance F.Reftable r => SubstitutableQ q r (TGenQ q r) where
  apply θ (Gen n ts)        = Gen n $ apply θ ts

instance F.Reftable r => SubstitutableQ q r (BTGenQ q r) where
  apply θ (BGen n ts)       = BGen n $ apply θ ts

instance F.Reftable r => SubstitutableQ q r (BTVarQ q r) where
  apply θ (BTV s l c)       = BTV s l $ apply θ c

instance F.Reftable r => SubstitutableQ q r (TypeMembersQ q r) where
  apply θ (TM ms sms cl ct s n)
                            = TM (apply θ ms) (apply θ sms)
                                 (apply θ cl) (apply θ ct)
                                 (apply θ s) (apply θ n)

instance SubstitutableQ q r a => SubstitutableQ q r (F.SEnv a) where
  apply                     = fmap . apply

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

instance F.Reftable r => SubstitutableQ q r (TypeSigQ q r) where
  apply θ (TS k n h)        = TS k n (apply θ h)

instance F.Reftable r => SubstitutableQ q r (TypeDeclQ q r) where
  apply θ (TD s m)          = TD (apply θ s) (apply θ m)

instance (F.Reftable r, SubstitutableQ q r a) => SubstitutableQ q r (Statement a) where
  apply θ s                 = fmap (apply θ) s

instance SubstitutableQ q r Assignability where
  apply _ s                 = s

instance SubstitutableQ q r Locality where
  apply _ s                 = s

instance SubstitutableQ q r Initialization where
  apply _ s                 = s

instance (SubstitutableQ q r a, SubstitutableQ q r b, SubstitutableQ q r c) => SubstitutableQ q r (a,b,c) where
  apply θ (a,b,c)           = (apply θ a, apply θ b, apply θ c)

instance F.Reftable r => SubstitutableQ q r (FAnnQ q r) where
  apply θ (FA i s fs) = FA i s $ apply θ fs


---------------------------------------------------------------------------------
appTy :: F.Reftable r => RSubstQ q r -> RTypeQ q r -> RTypeQ q r
---------------------------------------------------------------------------------
appTy _        (TPrim p r)   = TPrim p r
appTy (Su m) t@(TVar α r)    = (HM.lookupDefault t α m) `strengthen` r
appTy θ        (TOr ts)      = TOr (apply θ ts)
appTy θ        (TAnd ts)     = TAnd (mapSnd (apply θ) <$> ts)
appTy θ        (TRef n r)    = TRef (apply θ n) r
appTy θ        (TObj m es r) = TObj (apply θ m) (apply θ es) r
appTy θ        (TClass t)    = TClass (apply θ t)
appTy _        (TMod n)      = TMod n
appTy (Su m)   (TAll α t)    = TAll α $ apply (Su $ HM.delete (btvToTV α) m) t
appTy θ        (TFun ts t r) = TFun (apply θ ts) (apply θ t) r
appTy _        (TExp _)      = error "appTy should not be applied to TExp"


---------------------------------------------------------------------------------
-- | Type equality (module α-renaming)
---------------------------------------------------------------------------------

instance (F.Reftable r) => Eq (TGen r) where
  Gen n ts    == Gen n' ts'    = n == n' && ts == ts'

instance (F.Reftable r) => Eq (TypeMembers r) where
  TM m sm c k s n == TM m' sm' c' k' s' n' =
    m == m' && sm == sm' && c == c' && k == k' && s == s' && n == n'

instance (F.Reftable r) => Eq (TypeMember r) where
  FI k t1 t2 == FI k' t1' t2' = k == k' && t1 == t1' && t2 == t2'
  MI k mts   == MI k' mts'    = k == k' && mts == mts'

instance (F.Reftable r) => Eq (Bind r) where
  B x t == B x' t' = x == x' && t == t'

instance F.Reftable r => Eq (BTGen r) where
  BGen n1 b1s == BGen n2 b2s = n1 == n2 && b1s == b2s

instance F.Reftable r => Eq (BTVar r) where
  BTV _ _ c1 == BTV _ _ c2 = c1 == c2

instance (F.Reftable r) => Eq (RType r) where
  TPrim p _    == TPrim p' _    = p == p'
  TVar α _     == TVar α' _     = α == α'
  TOr ts       == TOr ts'       = ts == ts'
  TAnd ts      == TAnd ts'      = ts == ts'
  TRef g _     == TRef g' _     = g == g'
  TObj m ms _  == TObj m' ms' _ = m == m && ms == ms'
  TClass g     == TClass g'     = g == g'
  TMod n       == TMod n'       = n == n'
  TAll v@(BTV _ b _) t == TAll v'@(BTV _ b' _) t'
                                = b == b' && t == appTy θ t'
    where
      θ = fromList [(btvToTV v', tVar $ btvToTV v)]
  TFun bs o _ == TFun bs' o' _ = bs == bs' && o == o'
  _           == _             = False

