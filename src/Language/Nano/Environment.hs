
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Language.Nano.Environment where

import           Language.Nano.Types
import           Language.Nano.Env
import           Language.Nano.Names

-------------------------------------------------------------------------------
-- | Typecheck Environment
-------------------------------------------------------------------------------

class EnvLike r t where
  -- 
  -- ^ Bindings in scope
  --   (values of the source language:
  --   variables, functions, classes)
  --
  names           :: t r -> Env (RType r, Assignability, Initialization)
  -- 
  -- ^ Modules in scope (exported API)
  --
  modules         :: t r -> QEnv (ModuleDef r)
  -- 
  -- ^ Namespace absolute path
  --
  absPath         :: t r -> AbsPath
  -- 
  -- ^ Calling context
  --
  context         :: t r -> IContext
  -- 
  -- ^ Parent environment
  --
  parent          :: t r -> Maybe (t r)

-------------------------------------------------------------------------------
currentModule :: EnvLike r t => t r -> Maybe (ModuleDef r)
-------------------------------------------------------------------------------
currentModule g = 
  qenvFindTy (absPath g) (modules g)

