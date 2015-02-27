
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Language.Nano.Environment where

import           Data.Maybe           (isJust)
import           Language.Nano.Types
import           Language.Nano.Env
import           Language.Nano.Names
import           Language.Nano.Program
import           Language.Fixpoint.Names

-------------------------------------------------------------------------------
-- | Typecheck Environment
-------------------------------------------------------------------------------

class EnvLike r t where
  -- 
  -- ^ Bindings in scope
  --   Includes : variables, functions, classes, interfaces
  --
  names           :: t r -> Env (RType r, Assignability, Initialization)
  -- 
  -- ^ Modules in scope (exported API)
  --
  modules         :: t r -> QEnv (ModuleDef r)
  -- 
  -- ^ ClassHierarchy
  -- 
  cha             :: t r -> ClassHierarchy r
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
envLikeFindTy :: (EnvLike r t, Symbolic a) => a -> t r -> Maybe (RType r)
-------------------------------------------------------------------------------
envLikeFindTy x γ = 
  case envFindTy x $ names γ of 
    Just (t,_,_) -> Just t
    Nothing -> case parent γ of 
                 Just γ' -> envLikeFindTy x γ'
                 Nothing -> Nothing

envLikeMember x = isJust . envLikeFindTy x

