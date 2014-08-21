{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}

-- | Module pertaining to Refinement Type descriptions and conversions
--   Likely mergeable with @Language.Nano.Typecheck.Types@

module Language.Nano.Liquid.Environment ( 
  
  -- * Constraint Environments
    CGEnvR(..), CGEnv

  ) where

import           Data.Maybe             (fromMaybe, catMaybes)
import qualified Data.List               as L
import qualified Data.HashMap.Strict     as M
-- import qualified Data.HashSet            as S
import           Data.Monoid                        (mconcat)
import           Text.PrettyPrint.HughesPJ
import           Text.Printf 
import           Control.Applicative 
import           Control.Monad          (zipWithM)

import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.PrettyPrint

import           Language.Nano.Annots
import           Language.Nano.Env
import           Language.Nano.Errors
import           Language.Nano.Locations
import           Language.Nano.Misc
import           Language.Nano.Names
import           Language.Nano.Types
import           Language.Nano.Program
import           Language.Nano.Environment
import           Language.Nano.Typecheck.Resolve
import           Language.Nano.Typecheck.Sub
import           Language.Nano.Typecheck.Types

import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.PrettyPrint
  
-- import           Debug.Trace                        (trace)

type PPR r = (PP r, F.Reftable r)

-------------------------------------------------------------------------------------
-- | Refinement Types and Annotations
-------------------------------------------------------------------------------------

type RefType     = RType F.Reft

type AnnTypeR    = AnnType F.Reft

-------------------------------------------------------------------------------------
-- | Constraint Generation Environment 
-------------------------------------------------------------------------------------

data CGEnvR r = CGE { 
  -- 
  -- ^ bindings in scope 
  --
    cge_names   :: !(Env (RType r))
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
  -- ^ Globals in scope, along with their fixpoint binders. We need to keep
  --   track of these in order to remove them as bindings from the environments
  --   where the globals appear in the LHS of a constraint.
  --
  --   FIXME: 
  --
  --      * do not allow globals to be shadows by other vars !!!
  --        
  --      * perhaps merge with cge_names
  --
  , cge_globs   :: M.HashMap F.Symbol [F.BindId]
  -- 
  -- ^ Parent namespace environment
  --
  , cge_parent  :: Maybe (CGEnvR r)
  
  } deriving (Functor)

type CGEnv = CGEnvR F.Reft

instance EnvLike F.Reft CGEnvR where
  names     = cge_names
  modules   = cge_mod
  absPath   = cge_path
  context   = cge_ctx
  parent    = cge_parent
  

instance EnvLike () CGEnvR where
  names     = cge_names
  modules   = cge_mod
  absPath   = cge_path
  context   = cge_ctx
  parent    = cge_parent
 
 
