
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Language.Nano.Environment where

import           Language.Nano.Types
import           Language.Nano.Typecheck.Types
import           Language.Nano.Env
import           Language.Nano.Names

import           Language.ECMAScript3.PrettyPrint
import qualified Language.Fixpoint.Types        as F
import           Text.PrettyPrint.HughesPJ 

-------------------------------------------------------------------------------
-- | Typecheck Environment
-------------------------------------------------------------------------------

class EnvLike r t where
  -- 
  -- ^ Bindings in scope
  --   (values of the source language:
  --   variables, functions, classes)
  --
  names           :: t r -> Env (RType r)               
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


instance (PP r, F.Reftable r, EnvLike r g) => PP (g r) where
  pp = ppTCEnv


ppTCEnv g -- ( nms mod _ pth _ )
  =   text "******************** Environment ************************"
  $+$ pp (names g)
  $+$ text "******************** Modules ****************************"
  $+$ pp (modules g)
  $+$ text "******************** Absolute path **********************"
  $+$ pp (absPath g)


-------------------------------------------------------------------------------
currentModule :: EnvLike r t => t r -> Maybe (ModuleDef r)
-------------------------------------------------------------------------------
currentModule g = 
  qenvFindTy (absPath g) (modules g)

