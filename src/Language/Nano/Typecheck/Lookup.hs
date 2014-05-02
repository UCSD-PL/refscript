{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Nano.Typecheck.Lookup (getProp, getPropTDef) where 

import           Data.Maybe (listToMaybe)
import           Language.ECMAScript3.PrettyPrint
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc
import           Language.Nano.Types
import           Language.Nano.Errors 
import           Language.Nano.Env
import           Language.Nano.Typecheck.Types
import qualified Language.Nano.Typecheck.Subst as S
import           Control.Applicative ((<$>))

-- import           Debug.Trace

type PPR r = (PP r, F.Reftable r)

type TDR r = TDefEnv (RType r)
type TER r = Env (RType r)

-- Naming convention for the following:
--
--  α: the variable declaration environment
--
--  γ: the type definition environment
--

-- Given an environment @γ@, a (string) field @s@ and a type @t@, `getProp` 
-- returns a tuple with elements:
-- * The subtype of @t@ for which the access is successful.
-- * The corresponding accessed type.
-------------------------------------------------------------------------------
getProp ::  (IsLocated l, PPR r) => 
  l -> TER r -> TDR r -> F.Symbol -> RType r -> Maybe (RType r, RType r)
-------------------------------------------------------------------------------
getProp l α γ s t@(TApp _ _ _)  = getPropApp l α γ s t 
getProp _ _ _ _   (TFun _ _ _ ) = Nothing
getProp l _ γ s a@(TArr _ _)    = (a,) <$> getPropArr l γ s a
getProp _ _ _ s a@(TCons _ _)   = (a,) <$> getPropCons False s a
getProp l _ _ _ t               = die $ bug (srcPos l) 
                                      $ "Using getProp on type: " ++ ppshow t 


-------------------------------------------------------------------------------
getPropApp :: (PPR r, IsLocated a) 
           => a -> TER r -> TDR r -> F.Symbol -> RType r -> Maybe (RType r, RType r)
-------------------------------------------------------------------------------
getPropApp l α γ s t@(TApp c ts _) = 
  case c of 
    TBool      -> Nothing
    TUndef     -> Nothing
    TNull      -> Nothing
    TUn        -> getPropUnion l α γ s ts
    TInt       -> lookupAmbientVar l α γ s "Number" t
    TString    -> lookupAmbientVar l α γ s "String" t
    TRef (i,b) -> findSym i γ >>= getPropTDef b l γ s ts >>= return . (t,)
    TTop       -> die $ bug (srcPos l) "getProp top"
    TVoid      -> die $ bug (srcPos l) "getProp void"

getPropApp _ _ _ _ _ = error "getPropArr should only be applied to TApp"

-- FIXME: IndexSig access could return null.
getPropCons b s t@(TCons es _) 
  | isIndSig t  = listToMaybe   [ {- orNull -} t | IndexSig _ _ t <- es]   
  | otherwise   = listToMaybe $ [ eltType e | e <- es, isStaticElt e == b, eltSym e == s ]

getPropCons _ _ _ = error "BUG: Cannot call getPropCons on non TCons"

-- Access the property from the relevant ambient object but return the 
-- original accessed type instead of the type of the ambient object. 
-------------------------------------------------------------------------------
lookupAmbientVar :: (PPR r, IsLocated l) => 
  l -> TER r -> TDR r -> F.Symbol -> String -> RType r -> Maybe (RType r, RType r)
-------------------------------------------------------------------------------
lookupAmbientVar l α γ s amb t = 
  envFindTy amb α >>= getProp l α γ s >>= return . (t,) . snd


-------------------------------------------------------------------------------
getPropTDef :: (PPR r) =>
  Bool -> t -> TDR r -> F.Symbol -> [RType r] -> TDef (RType r) -> Maybe (RType r)
-------------------------------------------------------------------------------
getPropTDef b _ γ f ts d = getPropCons b f t
  where
    t = TCons (S.flatten γ (d,ts)) fTop


-------------------------------------------------------------------------------
getPropArr :: (PPR r, IsLocated a) 
           => a -> TDR r -> F.Symbol -> RType r -> Maybe (RType r)
-------------------------------------------------------------------------------
getPropArr l γ s (TArr t _) =
-- NOTE: Array has been declared as a type declaration so 
-- it should reside in γ, and we can just getPropTDef on it,
-- using type t as teh single type parameter to it.
-- This Array reference is not static.
  findSym "Array" γ >>= getPropTDef False l γ s [t]

getPropArr _ _ _ _ = error "getPropArr should only be applied to arrays"


-- Accessing the @x@ field of the union type with @ts@ as its parts, returns
-- "Nothing" if accessing all parts return error, or "Just (ts, tfs)" if
-- accessing @ts@ returns type @tfs@. @ts@ is useful for adding casts later on.
-------------------------------------------------------------------------------
getPropUnion :: (IsLocated l, PPR r) => 
  l -> TER r -> TDR r -> F.Symbol -> [RType r] -> Maybe (RType r, RType r)
-------------------------------------------------------------------------------
getPropUnion l α γ f ts = 
  case [tts | Just tts <- getProp l α γ f <$> ts] of
    [] -> Nothing
    ts -> Just $ mapPair mkUnion $ unzip ts

