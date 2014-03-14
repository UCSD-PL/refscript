{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Nano.Typecheck.Lookup (getProp, getPropTDef) where 

import           Data.List (find)
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
getProp l α γ s a@(TArr _ _)    = (a,) <$> getPropArr l α γ s a
getProp l α γ s a@(TCons _ _)   = (a,) <$> getPropCons s a
getProp l _ _ _ t               = die $ bug (srcPos l) 
                                    $ "Using getProp on type: " 
                                      ++ (show $ toType t) 


-------------------------------------------------------------------------------
getPropApp :: (PPR r, IsLocated a) =>
  a -> TER r -> TDR r -> F.Symbol -> RType r -> Maybe (RType r, RType r)
-------------------------------------------------------------------------------
getPropApp l α γ s t@(TApp c ts _) = 
  case c of 
    TBool   -> Nothing
    TUndef  -> Nothing
    TNull   -> Nothing
    TUn     -> getPropUnion l α γ s ts
    TInt    -> lookupAmbientVar l α γ s "Number" t
    TString -> lookupAmbientVar l α γ s "String" t
    TRef i  -> findTyId i γ >>= getPropTDef l α γ s ts >>= return . (t,)
    TTop    -> die $ bug (srcPos l) "getProp top"
    TVoid   -> die $ bug (srcPos l) "getProp void"

getPropApp _ _ _ _ _ = error "getPropArr should only be applied to TApp"

getPropCons s t@(TCons bs _) = 
  b_type <$> find ((s ==) . b_sym) bs
getPropCons _ _ = error "BUG: Cannot call getPropCons on non TCons"

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
  t -> TER r -> TDR r -> F.Symbol -> [RType r] -> TDef (RType r) -> Maybe (RType r)
-------------------------------------------------------------------------------
getPropTDef l α γ f ts (TD _ vs pro elts) = 
    case [ p | TE s _ p <- elts, s == f ] of
      [ ] -> do (psy, pts) <- pro
                ptd        <- findTySym psy γ
                t          <- getPropTDef l α γ f pts ptd
                return      $ S.apply θ t
      [p] -> Just $ S.apply θ p                               -- found binding
      _   -> error $ "BUG: multiple field binding in TDef"
  where
    θ = S.fromList $ zip vs ts


-------------------------------------------------------------------------------
getPropArr :: (PPR r, IsLocated a) => 
  a -> TER r -> TDR r -> F.Symbol -> RType r -> Maybe (RType r)
-------------------------------------------------------------------------------
getPropArr l α γ s (TArr t _) =
-- NOTE: Array has been declared as a type declaration so 
-- it should reside in γ, and we can just getPropTDef on it,
-- using type t as teh single type parameter to it.
  findTySym (F.symbol "Array") γ >>= getPropTDef l α γ s [t]

getPropArr _ _ _ _ _ = error "getPropArr should only be applied to arrays"


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

