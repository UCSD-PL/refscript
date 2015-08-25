{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Language.Nano.Environment (

    EnvLike (..)
  , EnvEntry
  , envLikeFindTy, envLikeFindTy'
  , envFindBound
  , resolveModuleInEnv, resolveTypeInEnv, resolveEnumInEnv

) where

import           Control.Applicative           ((<$>))
import           Control.Exception             (throw)
import           Language.Fixpoint.Names
import qualified Language.Fixpoint.Types       as F
import           Language.Nano.AST
import           Language.Nano.ClassHierarchy
import           Language.Nano.Core.Env
import           Language.Nano.Errors
import           Language.Nano.Locations
import           Language.Nano.Names
import           Language.Nano.Typecheck.Types
import           Language.Nano.Types

-------------------------------------------------------------------------------
-- | Typecheck Environment
-------------------------------------------------------------------------------

type EnvEntry r = VarInfo r

class EnvLike r t where
  --
  -- | Bindings in scope
  --
  names           :: t r -> Env (EnvEntry r)
  --
  -- | Bounds for type variables
  --
  bounds          :: t r -> Env (RType r)
  --
  -- | Calling context
  --
  context         :: t r -> IContext
  --
  -- | Namespace absolute path
  --
  absPath         :: t r -> AbsPath
  --
  -- | ClassHierarchy
  --
  cha             :: t r -> ClassHierarchy r

-------------------------------------------------------------------------------
envLikeFindTy' :: (EnvLike r t, Symbolic a) => a -> t r -> Maybe (EnvEntry r)
-------------------------------------------------------------------------------
envLikeFindTy' x (names -> γ) = envFindTy x γ

-- envLikeFindTy' x γ | Just t  <- envFindTy x $ names γ = Just t
--                    | Just γ' <- parent γ              = envLikeFindTy' x γ'
--                    | otherwise                        = Nothing

-------------------------------------------------------------------------------
envLikeFindTy :: (EnvLike r t, Symbolic a) => a -> t r -> Maybe (RType r)
-------------------------------------------------------------------------------
envLikeFindTy x = fmap v_type . envLikeFindTy' x

-------------------------------------------------------------------------------
envFindBound :: (EnvLike r t, Symbolic a) => a -> t r -> Maybe (RType r)
-------------------------------------------------------------------------------
envFindBound x (bounds -> b) = envFindTy x b



--------------------------------------------------------------------------------
resolveModuleInEnv  :: EnvLike r t => t r -> AbsPath -> Maybe (ModuleDef r)
resolveTypeInEnv    :: EnvLike r t => t r -> AbsName -> Maybe (TypeDecl r)
resolveEnumInEnv    :: EnvLike r t => t r -> AbsName -> Maybe EnumDef
--------------------------------------------------------------------------------
resolveModuleInEnv (cha -> c) = resolveModule c
resolveTypeInEnv   (cha -> c) = resolveType c
resolveEnumInEnv   (cha -> c) = resolveEnum c


-- isClassType :: EnvLike r g => g r -> RType r -> Bool
