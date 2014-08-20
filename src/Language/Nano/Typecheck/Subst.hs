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
  , flatten, flatten', flattenType

  -- * Ancestors
  , weaken, lineage

  -- * Constructors
  , Constructor, toConstructor, isConstSubtype, sameTypeof, getTypeof

  ) where 

import           Data.Default
import           Text.PrettyPrint.HughesPJ
import           Data.Function                  (on)
-- import           Text.Printf
import           Language.Nano.Errors
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.PrettyPrint
import qualified Language.Fixpoint.Types as F
import           Language.Nano.Env
import           Language.Nano.Typecheck.Types
import           Language.Fixpoint.Misc (intersperse)

import           Control.Applicative ((<$>))
import qualified Data.HashSet as S
import qualified Data.List as L
import qualified Data.HashMap.Strict as M 
import           Data.Monoid hiding ((<>))
import           Data.Function (fix)

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
  pp (Su m) = if M.null m then text "empty" 
              else if M.size m < 10 then intersperse comma $ (ppBind <$>) $ M.toList m 
              else vcat $ (ppBind <$>) $ M.toList m 

ppBind (x, t) = pp x <+> text ":=" <+> pp t


class Free a where 
  free  :: a -> S.HashSet TVar

instance Free (RType r) where
  free (TApp _ ts _)        = free ts
  free (TVar α _)           = S.singleton α 
  free (TFun xts t _)       = free $ t:(b_type <$> xts)
  free (TAll α t)           = S.delete α $ free t 
  free (TAnd ts)            = free ts 
  free (TExp _)             = error "free should not be applied to TExp"
  free (TCons xts m _)      = free xts `mappend` free m

instance Free a => Free [a] where 
  free = S.unions . map free

instance Free (Cast r) where
  free CNo         = S.empty
  free (CDead _ t) = free t
  free (CUp t t')  = free [t,t']
  free (CDn t t')  = free [t,t']

instance Free (Fact r) where
  free (PhiVar _)        = S.empty
  free (TypInst _ ts)    = free ts
  free (Overload _ t)    = free t
  free (EltOverload _ t) = free t
  free (TCast _ c)       = free c
  free (VarAnn t)        = free t
  free (FieldAnn f)      = free f
  free (MethAnn m)       = free m
  free (StatAnn m)       = free m
  free (ConsAnn c)       = free c
  free (FuncAnn c)       = free c
  free (ClassAnn (vs,m)) = foldr S.delete (free m) vs

instance Free (TElt r) where
  free (FieldSig _ m t)   = free m `mappend` free t
  free (MethSig  _ m t)   = free m `mappend` free t
  free (StatSig _ m t)    = free m `mappend` free t
  free (CallSig t)        = free t
  free (ConsSig t)        = free t
  free (IndexSig _ _ t)   = free t

instance Free a => Free (Id b, a) where
  free (_, a)            = free a

instance Free a => Free (Maybe a) where
  free Nothing  = S.empty
  free (Just a) = free a


class Substitutable r a where 
  apply :: (RSubst r) -> a -> a 

instance Substitutable r a => Substitutable r [a] where 
  apply = map . apply 

instance (Substitutable r a, Substitutable r b) => Substitutable r (a,b) where 
  apply f (x,y) = (apply f x, apply f y)

instance F.Reftable r => Substitutable r (RType r) where 
  apply θ t = appTy θ t

instance F.Reftable r => Substitutable r (Bind r) where 
  apply θ (B z t) = B z $ appTy θ t

instance (Substitutable r t) => Substitutable r (Env t) where 
  apply = envMap . apply

instance F.Reftable r => Substitutable r (TElt r) where 
  apply θ (FieldSig x m t)   = FieldSig x   (appTy (toSubst θ) m) (apply θ t)
  apply θ (StatSig x m t)    = StatSig  x   (appTy (toSubst θ) m) (apply θ t)
  apply θ (MethSig  x m t)   = MethSig  x   (appTy (toSubst θ) m) (apply θ t)
  apply θ (CallSig t)        = CallSig      (apply θ t)
  apply θ (ConsSig t)        = ConsSig      (apply θ t)
  apply θ (IndexSig x b t)   = IndexSig x b (apply θ t)

instance F.Reftable r => Substitutable r (Cast r) where
  apply _ CNo         = CNo
  apply θ (CDead z t) = CDead z         (apply θ t)
  apply θ (CUp t t')  = CUp (apply θ t) (apply θ t')
  apply θ (CDn t t')  = CDn (apply θ t) (apply θ t')

instance F.Reftable r => Substitutable r (Fact r) where
  apply _ x@(PhiVar _)      = x
  apply θ (TypInst ξ ts)    = TypInst ξ     $ apply θ ts
  apply θ (Overload ξ t)    = Overload ξ    $ apply θ t
  apply θ (EltOverload ξ t) = EltOverload ξ $ apply θ t
  apply θ (TCast   ξ c)     = TCast ξ       $ apply θ c
  apply θ (VarAnn t)        = VarAnn        $ apply θ t
  apply θ (FieldAnn f)      = FieldAnn      $ apply θ f
  apply θ (MethAnn t)       = MethAnn       $ apply θ t
  apply θ (StatAnn t)       = StatAnn       $ apply θ t
  apply θ (ConsAnn t)       = ConsAnn       $ apply θ t
  apply θ (FuncAnn t)       = FuncAnn       $ apply θ t
  apply θ (ClassAnn (c, t)) = ClassAnn      $ (c, apply θ t)

instance Substitutable r a => Substitutable r (Maybe a) where
  apply θ (Just a)       = Just $ apply θ a
  apply _ Nothing        = Nothing

instance Substitutable r (Id a) where
  apply _ i              = i

instance F.Reftable r => Substitutable r (Annot (Fact r) z) where
  apply θ (Ann z fs)     = Ann z $ apply θ fs

instance F.Reftable r => Substitutable r (TDef r) where
  apply θ (TD c n v p e) = TD c n v (apply θ p) (apply θ e)

 
---------------------------------------------------------------------------------
appTy :: F.Reftable r => RSubst r -> RType r -> RType r
---------------------------------------------------------------------------------
appTy θ        (TApp c ts r)  = TApp c (apply θ ts) r
appTy θ        (TAnd ts)      = TAnd (apply θ ts) 
appTy (Su m) t@(TVar α r)     = (M.lookupDefault t α m) `strengthen` r
appTy θ        (TFun ts t r)  = TFun  (apply θ ts) (apply θ t) r
appTy (Su m)   (TAll α t)     = TAll α $ apply (Su $ M.delete α m) t
appTy θ        (TCons es m r) = TCons (apply θ es) (appTy (toSubst θ) m) r
appTy _        (TExp _)       = error "appTy should not be applied to TExp"


-- | flattening type to include all fields inherited by ancestors
---------------------------------------------------------------------------
flatten :: PPR r => Bool -> TDefEnv r -> (TDef r, [RType r]) -> [TElt r]
---------------------------------------------------------------------------
flatten True  = fix . ff isStaticSig
flatten False = fix . ff nonStaticSig

ff flt δ r (TD _ _ vs (Just (i, ts')) es, ts) = 
    apply θ  . L.unionBy sameBinder (filter flt es) $ r (findSymOrDie i δ, ts')
  where 
    θ   = fromList $ zip vs ts

ff flt _ _ (TD _ _ vs _ es, ts)  = apply θ $ filter flt es
  where 
    θ = fromList $ zip vs ts

-- | flatten' does not apply the top-level type substitution
flatten' st δ d@(TD _ _ vs _ _) = flatten st δ (d, tVar <$> vs)


flattenType δ t@(TApp (TRef x) ts r) = TCons es mut r
  where 
    es      = flatten False δ (findSymOrDie x δ, ts)
    -- Be careful with the mutability classes themselves
    -- Do not set this to another mutability type cause, or you'll
    -- end up with infinite recursion
    mut     | isMutabilityType t = tTop
            | otherwise          
            = case ts of 
                -- FIXME there should always be a head element
                t:_ -> toType t
                _   -> def

flattenType δ (TApp (TTyOf x) _ r) = TCons es anyMutability r
  where 
    es      = flatten' True δ $ findSymOrDie x δ

flattenType _ t = t


-- | Weaken a named type, by moving upwards in the class hierarchy. This
-- function does the necessary type argument substitutions. 
--
-- FIXME: Works for classes, but interfaces could have multiple ancestors.
-- FIXME: What about common elements in parent class?
---------------------------------------------------------------------------
weaken :: PPR r => TDefEnv r -> (TDef r, [RType r]) -> F.Symbol -> Maybe (TDef r, [RType r])
---------------------------------------------------------------------------
weaken δ dt@(TD _ s vs (Just (p,ps)) _, ts) t 
  | F.symbol s /= t = weaken δ (apply θ $ findSymOrDie p δ, apply θ ps) t
  | otherwise       = Just dt
  where θ   = fromList $ zip vs ts

weaken _ dt@(TD _ s _ Nothing _, _) t
  | F.symbol s /= t = Nothing
  | otherwise       = Just dt


---------------------------------------------------------------------------
lineage :: TDefEnv t -> TDef t -> [F.Symbol]
---------------------------------------------------------------------------
lineage δ (TD _ s _ (Just (p,_)) _) = (F.symbol s):lineage δ (findSymOrDie p δ)
lineage _ (TD _ s _ Nothing      _) = [F.symbol s]

lineageSymbol δ s = 
  case findSym s δ of
    Just d  -> lineage δ d
    Nothing -> []



-----------------------------------------------------------------------
-- Constructors
-----------------------------------------------------------------------

type Constructor = Type 

funcConstr :: Constructor
funcConstr = TApp (TRef $ F.symbol "Function") [] ()

isFunctionConst (TApp (TRef s) [] ()) | s == F.symbol "Function" = True
isFunctionConst _                                                = False

objectConstr :: Constructor
objectConstr = TApp (TRef $ F.symbol "Object") [] ()

isObjectConstr (TApp (TRef s) [] ()) | s == F.symbol "Object" = True
isObjectConstr _                                              = False

-- Primitive types don't have constructor
toConstructor :: RType r -> Maybe Constructor
toConstructor  (TApp (TRef  x) _ _) = Just $ TApp (TRef  x) [] ()
toConstructor  (TApp (TTyOf x) _ _) = Just $ funcConstr
toConstructor  (TFun _ _ _ )        = Just $ funcConstr
toConstructor  (TCons _ _ _)        = Just $ objectConstr
toConstructor  (TAnd _)             = Just $ funcConstr 
toConstructor  _                    = Nothing

instance F.Symbolic Constructor where
  symbol (TApp (TRef  x) _ _) = x
  symbol (TApp (TTyOf x) _ _) = F.symbol "Function"
  symbol (TFun _ _ _ )        = F.symbol "Function"
  symbol (TCons _ _ _)        = F.symbol "Object"
  symbol (TAnd _)             = F.symbol "Function"
  symbol _                    = F.symbol "ConstructorERROR"

instance F.Expression Constructor where
  expr = F.expr . F.symbol



isConstSubtype δ c1 c2 
  | isObjectConstr c2 = True
  | otherwise         = 
        case (c1, c2) of
          (TApp (TRef s1) _ _, TApp (TRef s2) _ _) ->  s2 `elem` lineageSymbol δ s1
          _                                        -> False

sameTag (TApp TInt _ _   ) (TApp TInt _ _   ) = True
sameTag (TApp TBool _ _  ) (TApp TBool _ _  ) = True
sameTag (TApp TString _ _) (TApp TString _ _) = True
sameTag (TApp TUndef _ _ ) (TApp TUndef _ _ ) = True
sameTag (TFun _ _ _      ) (TFun _ _ _      ) = True
sameTag (TCons _ _ _     ) (TCons _ _ _     ) = True
sameTag (TApp (TRef _) _ _) (TApp (TRef _) _ _) = True
sameTag (TCons _ _ _) (TApp (TRef _) _ _) = True

getTypeof (TApp TInt _ _     ) = Just "number"
getTypeof (TApp TBool _ _    ) = Just "boolean"
getTypeof (TApp TString _ _  ) = Just "string"
getTypeof (TApp TUndef _ _   ) = Just "undefined"
getTypeof (TApp TNull  _ _   ) = Just "undefined"
getTypeof (TFun _ _ _        ) = Just "function"
getTypeof (TCons _ _ _       ) = Just "object"
getTypeof (TApp (TRef _) _ _ ) = Just "object"
getTypeof (TApp (TTyOf _) _ _) = Just "function"
getTypeof _                    = Nothing

sameTypeof = (==) `on` getTypeof

