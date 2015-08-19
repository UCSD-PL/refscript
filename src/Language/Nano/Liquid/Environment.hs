{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}

module Language.Nano.Liquid.Environment where

import           Language.Nano.Annots         ()
import           Language.Nano.ClassHierarchy
import           Language.Nano.Core.Env
import           Language.Nano.Environment
import           Language.Nano.Names
import           Language.Nano.Pretty
import           Language.Nano.Types
import           Text.PrettyPrint.HughesPJ

import qualified Language.Fixpoint.Types      as F

-- import           Debug.Trace                        (trace)
--
--
-- TODO: keep ID for each environment: the pair (env_id, symbol) should be
--       unique ...

-------------------------------------------------------------------------------------
-- | Constraint Generation Environment
-------------------------------------------------------------------------------------

data CGEnvR r = CGE {
  --
  -- | Bindings in scope
  --
    cge_names  :: !(Env (EnvEntry r))
  --
  -- | Bounds for type variables
  --
  , cge_bounds :: !(Env (RType r))
  --
  -- ^ fixpoint bindings
  --
  , cge_fenv   :: F.SEnv F.IBindEnv
  --
  -- ^ branch target conditions
  --
  , cge_guards :: ![F.Pred]
  --
  -- ^ intersection-type context
  --
  , cge_ctx    :: !IContext
  --
  -- ^ ClassHierarchy
  --
  , cge_cha    :: ClassHierarchy r
  --
  -- ^ Namespace absolute path
  --
  , cge_path   :: AbsPath
  --
  -- ^ Constants
  --
  , cge_consts :: Env (RType r)

--   --
--   -- ^ Parent namespace environment
--   --
--   , cge_parent  :: Maybe (CGEnvR r)

  } deriving (Functor)

type CGEnv = CGEnvR F.Reft

type CGEnvEntry = EnvEntry F.Reft

instance EnvLike r CGEnvR where
  names     = cge_names
  bounds    = cge_bounds
  cha       = cge_cha
  absPath   = cge_path
  context   = cge_ctx
--   parent    = cge_parent


instance (PP r, F.Reftable r) => PP (CGEnvR r) where
  pp = ppTCEnv


ppTCEnv :: (PP r, F.Reftable r) => CGEnvR r -> Doc
ppTCEnv g
  =   text "******************** Environment ************************"
  $+$ pp (names g)
  $+$ text "******************** Absolute path **********************"
  $+$ pp (absPath g)

