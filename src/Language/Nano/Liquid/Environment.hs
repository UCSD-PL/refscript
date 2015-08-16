{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveDataTypeable         #-}

module Language.Nano.Liquid.Environment where

import           Text.PrettyPrint.HughesPJ

import           Language.Nano.Annots()
import           Language.Nano.ClassHierarchy
import           Language.Nano.Env
import           Language.Nano.Names
import           Language.Nano.Pretty
import           Language.Nano.Types
import           Language.Nano.Environment
import           Language.Nano.Program

import qualified Language.Fixpoint.Types as F
  
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
  -- ^ bindings in scope 
  --
    cge_names   :: !(Env (EnvEntry r))
  -- 
  -- ^ fixpoint bindings
  --
  , cge_fenv    :: F.SEnv F.IBindEnv
  -- 
  -- ^ branch target conditions  
  --
  , cge_guards  :: ![F.Pred]
  -- 
  -- ^ intersection-type context 
  --
  , cge_ctx     :: !IContext
  -- 
  -- ^ Modules in scope (exported API)
  --
  , cge_mod     :: QEnv (ModuleDef r)
  -- 
  -- ^ CHA
  -- 
  , cge_cha     :: ClassHierarchy r
  -- 
  -- ^ Namespace absolute path
  --
  , cge_path    :: AbsPath
  -- 
  -- ^ Constants
  --
  , cge_consts  :: Env (RType r)

--   -- 
--   -- ^ Parent namespace environment
--   --
--   , cge_parent  :: Maybe (CGEnvR r)
  
  } deriving (Functor)

type CGEnv = CGEnvR F.Reft

type CGEnvEntry = EnvEntry F.Reft

instance EnvLike r CGEnvR where
  names     = cge_names
  modules   = cge_mod
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
  $+$ text "******************** Modules ****************************"
  $+$ pp (modules g)
  $+$ text "******************** Absolute path **********************"
  $+$ pp (absPath g)

