{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Nano.Typecheck.Unfold (unfoldFirst, unfoldMaybe, unfoldSafe) where 

import           Language.ECMAScript3.PrettyPrint
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Misc
import           Language.Nano.Types
import           Language.Nano.Errors 
import           Language.Nano.Env
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Typecheck.Types

import           Control.Exception   (throw)
import           Control.Applicative ((<$>))

import           Text.Printf 

-----------------------------------------------------------------------------
-- Unfolding ----------------------------------------------------------------
-----------------------------------------------------------------------------

-- | Unfold the FIRST TDef at any part of the type @t@.
-------------------------------------------------------------------------------
unfoldFirst :: (PP r, F.Reftable r) => TDefEnv (RType r) -> RType r -> RType r
-------------------------------------------------------------------------------
unfoldFirst env = go
  where 
    go (TFun its ot r)         = TFun (appTBi go <$> its) (go ot) r
    go (TAnd _)                = errorstar "BUG: unfoldFirst: cannot unfold intersection"
    go (TAll v t)              = TAll v $ go t
    go (TApp (TRef i) ts _)    = undefined 
    -- TODO: This is not ready !!!
      {-case envFindTy (F.symbol i) env of-}
      {-  Just (TD n vs es)     -> apply (fromList $ zip vs ts) es    -}
      {-  _                     -> throw $ errorUnboundId (srcPos id) id-}
    go (TApp c a r)            = TApp c (go <$> a) r
    go (TArr t r)              = TArr (go t) r
    go t@(TVar _ _ )           = t
    go (TExp _)                = error "unfoldFirst should not be applied to TExp" 
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
unfoldMaybe :: (PP r, F.Reftable r) => TDefEnv (RType r) -> RType r -> Either String (RType r)
-------------------------------------------------------------------------------
unfoldMaybe env t@(TApp (TRef id) acts _) = undefined 
  {-case envFindTy (F.symbol id) env of-}
  {-  Just (TD _ vs bd _ ) -> Right $ apply (fromList $ zip vs acts) bd-}
  {-  _                    -> Left  $ (printf "Failed unfolding: %s" $ ppshow t)-}
-- The only thing that is unfoldable is a TRef.
-- The rest are just returned as they are.
unfoldMaybe _ t                           = Right t


-- | Force a successful unfolding
-------------------------------------------------------------------------------
unfoldSafe :: (PP r, F.Reftable r) => TDefEnv (RType r) -> RType r -> RType r
-------------------------------------------------------------------------------
unfoldSafe env = either error id . unfoldMaybe env

