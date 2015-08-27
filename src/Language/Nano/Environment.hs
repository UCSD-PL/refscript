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
import           Language.Nano.Pretty
import           Language.Nano.Typecheck.Types
import           Language.Nano.Types
import           Text.PrettyPrint.HughesPJ

-------------------------------------------------------------------------------
-- | Typecheck Environment
-------------------------------------------------------------------------------

type EnvEntry r = VarInfo r

class EnvLike r t where
  --
  -- | Bindings in scope
  --
  envNames  :: t r -> Env (EnvEntry r)
  --
  -- | Bounds for type variables
  --
  envBounds :: t r -> Env (RType r)
  --
  -- | Calling context
  --
  envCtx    :: t r -> IContext
  --
  -- | Namespace absolute path
  --
  envPath   :: t r -> AbsPath
  --
  -- | ClassHierarchy
  --
  envCHA    :: t r -> ClassHierarchy r

-------------------------------------------------------------------------------
envLikeFindTy' :: (EnvLike r t, Symbolic a) => a -> t r -> Maybe (EnvEntry r)
-------------------------------------------------------------------------------
envLikeFindTy' x (envNames -> γ) = envFindTy x γ

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
envFindBound x (envBounds -> b) = envFindTy x b

--------------------------------------------------------------------------------
resolveModuleInEnv  :: EnvLike r t => t r -> AbsPath -> Maybe (ModuleDef r)
resolveTypeInEnv    :: EnvLike r t => t r -> AbsName -> Maybe (TypeDecl r)
resolveEnumInEnv    :: EnvLike r t => t r -> AbsName -> Maybe EnumDef
--------------------------------------------------------------------------------
resolveModuleInEnv (envCHA -> c) = resolveModule c
resolveTypeInEnv   (envCHA -> c) = resolveType c
resolveEnumInEnv   (envCHA -> c) = resolveEnum c


instance (PP r, F.Reftable r, EnvLike r t) => PP (t r) where
  pp = ppTCEnv

--------------------------------------------------------------------------------
ppTCEnv :: (EnvLike r t, PP r, F.Reftable r) => t r -> Doc
--------------------------------------------------------------------------------
ppTCEnv g
  =   text "******************** Environment ************************"
  $+$ pp (envNames g)
  -- $+$ text "******************** Modules ****************************"
  -- $+$ pp (modules g)
  $+$ text "******************** Absolute path **********************"
  $+$ pp (envPath g)

