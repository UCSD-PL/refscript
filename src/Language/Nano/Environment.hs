
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
import           Language.Fixpoint.Misc

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
envLikeFindTy' :: (EnvLike r t, Symbolic a) 
               => a -> t r -> Maybe (RType r, Assignability, Initialization)
-------------------------------------------------------------------------------
envLikeFindTy' x γ | Just t  <- envFindTy x $ names γ = Just t
                   | Just γ' <- parent γ              = envLikeFindTy' x γ'
                   | otherwise                        = Nothing

-------------------------------------------------------------------------------
envLikeFindTy :: (EnvLike r t, Symbolic a) => a -> t r -> Maybe (RType r)
-------------------------------------------------------------------------------
envLikeFindTy x = fmap fst3 . envLikeFindTy' x

envLikeMember x = isJust . envLikeFindTy x

