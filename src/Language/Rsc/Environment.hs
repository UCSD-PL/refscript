{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Language.Rsc.Environment (

    EnvLike (..)
  , EnvEntry
  , envLikeFindTy, envLikeFindTy'
  , safeEnvFindTy
  , envFindBound
  , resolveModuleInEnv, resolveTypeInEnv, resolveEnumInEnv
  , toFgn

  -- Global definitions
  , globalLengthType


) where

import           Control.Applicative          ((<$>))
import           Control.Exception            (throw)
import           Data.Default
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Names
import qualified Language.Fixpoint.Types      as F
import           Language.Rsc.AST
import           Language.Rsc.ClassHierarchy
import           Language.Rsc.Core.Env
import           Language.Rsc.Errors
import           Language.Rsc.Locations
import           Language.Rsc.Names
import           Language.Rsc.Pretty
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
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

  -- | Class related
  --
  --   * Method Mutability
  --
  envMut    :: t r -> Maybe MutabilityMod
  --
  --   * Type for this
  --
  envThis   :: t r -> Maybe (RType r)


-------------------------------------------------------------------------------
envLikeFindTy' :: (EnvLike r t, Symbolic a) => a -> t r -> Maybe (EnvEntry r)
-------------------------------------------------------------------------------
envLikeFindTy' x (envNames -> γ) = envFindTy x γ

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
ppTCEnv g = pp (envNames g)
--   =   text "******************** Environment ************************"
--   $+$ pp (envNames g)
--   $+$ text "******************** Absolute path **********************"
--   $+$ pp (envPath g)


--------------------------------------------------------------------------------
toFgn :: Env (VarInfoQ q r) -> Env (VarInfoQ q r)
--------------------------------------------------------------------------------
toFgn = envMap go
  where
    go (VI loc WriteLocal i t) = VI loc ForeignLocal i t
    go v = v

--------------------------------------------------------------------------------
safeEnvFindTy :: (EnvLike r t, IsLocated l, Symbolic x, Monad m) => l -> t r -> x -> m (RType r)
--------------------------------------------------------------------------------
safeEnvFindTy l γ x | Just t <- envLikeFindTy x γ = return t
                    | otherwise = die $ bugEnvFindTy (srcPos l) (F.symbol x)

globalLengthType γ = safeEnvFindTy (def::SrcSpan) γ "builtin_getLength"

