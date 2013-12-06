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
  , toSubst

  -- * Free Type Variables
  , Free (..)

  -- * Type-class with operations
  , Substitutable (..)

  -- * Unfolding
  , unfoldFirst, unfoldMaybe, unfoldSafe
  
  -- * Accessing fields
  , getProp
  , getIdx
  

  ) where 

import           Text.PrettyPrint.HughesPJ
import           Language.ECMAScript3.PrettyPrint
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Parse as P
import           Language.Nano.Types
import           Language.Nano.Errors 
import           Language.Nano.Env
import           Language.Nano.Typecheck.Types

import           Control.Exception   (throw)
import           Control.Applicative ((<$>))
import qualified Data.HashSet as S
import           Data.List                      (find)
import qualified Data.HashMap.Strict as M 
import           Data.Monoid

import Text.Parsec
-- import Text.Parsec.Expr
-- import Text.Parsec.Language
-- import Text.Parsec.String hiding (Parser, parseFromFile)
-- import Text.Printf  (printf)

import           Text.Printf 
-- import           Debug.Trace
-- import           Language.Nano.Misc (mkEither)

---------------------------------------------------------------------------
-- | Substitutions --------------------------------------------------------
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

instance (Substitutable r a, Substitutable r b) => Substitutable r (a,b) where 
  apply f (x,y) = (apply f x, apply f y)

instance (PP r, F.Reftable r) => Substitutable r (RType r) where 
  apply θ t = appTy θ t
--     where 
--       msg   = printf "apply [θ = %s] [t = %s]" (ppshow θ) (ppshow t)

instance (PP r, F.Reftable r) => Substitutable r (Bind r) where 
  apply θ (B z t) = B z $ appTy θ t

instance (PP r, F.Reftable r) => Substitutable r (Env (RType r)) where 
  apply = envMap . apply


instance Free (RType r) where
  free (TApp _ ts _)        = S.unions   $ free <$> ts
  free (TArr t _)           = free t
  free (TVar α _)           = S.singleton α 
  free (TFun xts t _)       = S.unions   $ free <$> t:ts where ts = b_type <$> xts
  free (TAll α t)           = S.delete α $ free t 
  free (TObj bs _)          = S.unions   $ free <$> b_type <$> bs
  free (TBd (TD _ α t _ ))  = foldr S.delete (free t) α
  free (TAnd ts)            = S.unions   $ free <$> ts 

instance (PP r, F.Reftable r) => Substitutable r (Cast (RType r)) where
  apply θ c = c { castTarget = apply θ (castTarget c) }
  -- apply θ (UCST t) = UCST (apply θ t)
  -- apply θ (DCST t) = DCST (apply θ t)
  -- apply θ (DC   t) = DC   (apply θ t)

instance (PP r, F.Reftable r) => Substitutable r (Fact r) where
  apply _ x@(PhiVar _)     = x
  apply θ (TypInst ts)     = TypInst $ apply θ ts
  apply θ (Cast x  y)      = Cast x  $ apply θ y
  apply _ x@(LoopPhiVar _) = x
  apply θ (TAnnot t)       = TAnnot  $ apply θ t

instance Free (Cast (RType r)) where
  free = free . castTarget 

instance Free (Fact r) where
  free (PhiVar _)       = S.empty
  free (TypInst ts)     = free ts
  free (Cast _ c)       = free c
  free (LoopPhiVar _)   = S.empty
  free (TAnnot t)       = free t
 
 
------------------------------------------------------------------------
appTy :: (PP r, F.Reftable r) => RSubst r -> RType r -> RType r
------------------------------------------------------------------------
appTy θ (TApp c ts z)            = TApp c (apply θ ts) z 
appTy θ (TAnd ts)                = TAnd (apply θ ts) 
appTy θ (TObj bs z)              = TObj ((\b -> b { b_type = appTy θ $ b_type b}) <$> bs) z
appTy (Su m) t@(TVar α r)        = (M.lookupDefault t α m) `strengthen` r
appTy θ (TFun ts t r)            = TFun  (apply θ ts) (apply θ t) r
appTy (Su m) (TAll α t)          = apply (Su $ M.delete α m) t 
appTy θ (TArr t r)               = TArr (apply θ t) r
appTy (Su m) (TBd (TD c α t s))  = TBd $ TD c α (apply (Su $ foldr M.delete m α) t) s



-----------------------------------------------------------------------------
-- Unfolding ----------------------------------------------------------------
-----------------------------------------------------------------------------

-- | Unfold the FIRST TDef at any part of the type @t@.
-------------------------------------------------------------------------------
unfoldFirst :: (PP r, F.Reftable r) => Env (RType r) -> RType r -> RType r
-------------------------------------------------------------------------------
unfoldFirst env t = go t
  where 
    go (TFun its ot r)         = TFun (appTBi go <$> its) (go ot) r
    go (TObj bs r)             = TObj (appTBi go <$> bs) r
    go (TBd  _)                = errorstar "BUG: unfoldTDefDeep: there should not be a TBody here"
    go (TAnd _)                = errorstar "BUG: unfoldFirst: cannot unfold intersection"
    go (TAll v t)              = TAll v $ go t
    go (TApp (TDef id) acts _) = 
      case envFindTy (F.symbol id) env of
        Just (TBd (TD _ vs bd _ )) -> apply (fromList $ zip vs acts) bd
        _                          -> throw $ errorUnboundId (srcPos id) id
    go (TApp c a r)            = TApp c (go <$> a) r
    go (TArr t r)              = TArr (go t) r
    go t@(TVar _ _ )           = t
    appTBi f (B s t)           = B s $ f t


-- | Unfold a top-level type definition once. 
-- Return @Right t@, where @t@ is the unfolded type if the unfolding is succesful.
-- This includes the case where the input type @t@ is not a type definition in
-- which case the same type is returned.
-- If it is a type literal for which no definition exists return 
-- @Left "<Error message>".
--
-- TODO: Make sure toplevel refinements are the same.
-------------------------------------------------------------------------------
unfoldMaybe :: (PP r, F.Reftable r) => Env (RType r) -> RType r -> Either String (RType r)
-------------------------------------------------------------------------------
unfoldMaybe env t@(TApp (TDef id) acts _) =
      case envFindTy (F.symbol id) env of
        Just (TBd (TD _ vs bd _ )) -> Right $ apply (fromList $ zip vs acts) bd
        _                          -> Left  $ (printf "Failed unfolding: %s" $ ppshow t)
-- The only thing that is unfoldable is a TDef.
-- The rest are just returned as they are.
unfoldMaybe _ t                           = Right t


-- | Force a successful unfolding
-------------------------------------------------------------------------------
unfoldSafe :: (PP r, F.Reftable r) => Env (RType r) -> RType r -> RType r
-------------------------------------------------------------------------------
unfoldSafe env = either error id . unfoldMaybe env


-- Given an environment @γ@, a (string) field @s@ and a type @t@, `getProp` 
-- returns a tupple with elements:
-- ∙ The subtype of @t@ for which the access does not throw an error.
-- ∙ The type the corresponds to the access of exactly that type that does not
--   throw an error.
--
-------------------------------------------------------------------------------
getProp ::  (Ord r, PP r, F.Reftable r) => 
  Env (RType r) -> String -> RType r -> Maybe (RType r, RType r)
-------------------------------------------------------------------------------
getProp _ s t@(TObj bs _) = 
  do  case find (match $ F.symbol s) bs of
        Just b -> Just (t, b_type b)
        _      -> case find (match $ F.stringSymbol "*") bs of
                    Just b' -> Just (t, b_type b')
                    _       -> Just (t, tUndef)
  where match s (B f _)  = s == f

getProp γ s t@(TApp c ts _ ) = go c
  where  go TUn      = getPropUnion γ s ts
         go TInt     = Just (t, tUndef)
         go TBool    = Just (t, tUndef)
         go TString  = Just (t, tUndef)
         go TUndef   = Nothing
         go TNull    = Nothing
         go (TDef _) = getProp γ s $ unfoldSafe γ t
         go TTop     = error "getProp top"
         go TVoid    = error "getProp void"

getProp _ _ t@(TFun _ _ _ ) = Just (t, tUndef)

getProp γ s a@(TArr _ _)    = 
  case s of
    -- TODO: make more specific, add refinements
    "length" -> Just (a, tInt) 
    _        -> case stringToInt s of
    -- Implicit coersion of numieric strings:
    -- x["0"] = x[0], x["1"] = x[1], etc.
                  Just i  -> getIdx γ i a 
    -- The rest of the cases are undefined
                  Nothing -> Just (a, tUndef) 

getProp _ _ t               = error $ "getProp " ++ (ppshow t) 


-------------------------------------------------------------------------------
stringToInt :: String -> Maybe Int
-------------------------------------------------------------------------------
stringToInt s = 
  case runParser P.integer 0 "" s of
    Right i -> Just $ fromInteger i
    Left _  -> Nothing


-- Accessing the @x@ field of the union type with @ts@ as its parts, returns
-- "Nothing" if accessing all parts return error, or "Just (ts, tfs)" if
-- accessing @ts@ returns type @tfs@. @ts@ is useful for adding casts later on.
-------------------------------------------------------------------------------
getPropUnion ::  (Ord r, PP r, F.Reftable r) => 
  Env (RType r) -> String -> [RType r] -> Maybe (RType r, RType r)
-------------------------------------------------------------------------------
getPropUnion γ f ts = 
  -- Gather all the types that do not throw errors, and the type of 
  -- the accessed expression that yields them
  case [tts | Just tts <- getProp γ f <$> ts] of
    [] -> Nothing
    ts -> Just $ mapPair mkUnion $ unzip ts


-------------------------------------------------------------------------------
getIdx ::  (Ord r, PP r, F.Reftable r) => 
  Env (RType r) -> Int -> RType r -> Maybe (RType r, RType r)
-------------------------------------------------------------------------------
getIdx _ _ a@(TArr t _)  = Just (a,t)
getIdx γ i t             = getProp γ (show i) t 
--error $ "Unimplemented: getIdx on" ++ (ppshow t) 


