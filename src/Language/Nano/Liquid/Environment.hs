{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveDataTypeable         #-}

module Language.Nano.Liquid.Environment where

import           Data.Data                          (Data)
import           Data.Typeable                      (Typeable)
import           Text.PrettyPrint.HughesPJ

import           Language.ECMAScript3.PrettyPrint

import           Language.Nano.Annots()
import           Language.Nano.Env
import           Language.Nano.Names
import           Language.Nano.Types
import           Language.Nano.Environment

import qualified Language.Fixpoint.Types as F
  
-- import           Debug.Trace                        (trace)

-------------------------------------------------------------------------------------
-- | Constraint Generation Environment 
-------------------------------------------------------------------------------------

data CGEnvR r = CGE { 
  -- 
  -- ^ bindings in scope 
  --
    cge_names   :: !(Env (RType r, Assignability))
  -- 
  -- ^ fixpoint bindings
  --
  , cge_fenv    :: F.IBindEnv
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
  -- ^ Namespace absolute path
  --
  , cge_path    :: AbsPath
  -- 
  -- ^ Parent namespace environment
  --
  , cge_parent  :: Maybe (CGEnvR r)
  
  } deriving (Functor, Data, Typeable)

type CGEnv = CGEnvR F.Reft
 

instance EnvLike r CGEnvR where
  names     = cge_names
  modules   = cge_mod
  absPath   = cge_path
  context   = cge_ctx
  parent    = cge_parent
 
 
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

