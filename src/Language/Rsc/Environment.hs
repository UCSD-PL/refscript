{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Language.Rsc.Environment (

    CheckingEnvironment (..)
  , EnvEntry
  , chkEnvFindTy, chkEnvFindTy'
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

class CheckingEnvironment r t where
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
  envMut    :: t r -> Maybe (Mutability r)
  --
  --   * Type for this
  --
  envThis   :: t r -> Maybe (RType r)
  --
  --   * Enclosing function's id
  --
  envFnId   :: t r -> Int


-------------------------------------------------------------------------------
chkEnvFindTy' :: (CheckingEnvironment r t, Symbolic a) => a -> t r -> Maybe (EnvEntry r)
-------------------------------------------------------------------------------
chkEnvFindTy' x (envNames -> γ) = envFindTy x γ

-------------------------------------------------------------------------------
chkEnvFindTy :: (CheckingEnvironment r t, Symbolic a) => a -> t r -> Maybe (RType r)
-------------------------------------------------------------------------------
chkEnvFindTy x = fmap v_type . chkEnvFindTy' x

-------------------------------------------------------------------------------
envFindBound :: (CheckingEnvironment r t, Symbolic a) => a -> t r -> Maybe (RType r)
-------------------------------------------------------------------------------
envFindBound x (envBounds -> b) = envFindTy x b

--------------------------------------------------------------------------------
resolveModuleInEnv  :: (PPR r, CheckingEnvironment r t) => t r -> AbsPath -> Maybe (ModuleDef r)
resolveTypeInEnv    :: (PPR r, CheckingEnvironment r t) => t r -> AbsName -> Maybe (TypeDecl r)
resolveEnumInEnv    :: (PPR r, CheckingEnvironment r t) => t r -> AbsName -> Maybe EnumDef
--------------------------------------------------------------------------------
resolveModuleInEnv (envCHA -> c) = resolveModule c
resolveTypeInEnv   (envCHA -> c) = resolveType c
resolveEnumInEnv   (envCHA -> c) = resolveEnum c


instance (PP r, F.Reftable r, CheckingEnvironment r t) => PP (t r) where
  pp = pp . envNames

--------------------------------------------------------------------------------
toFgn :: Env (VarInfoQ q r) -> Env (VarInfoQ q r)
--------------------------------------------------------------------------------
toFgn = envMap go
  where
    go (VI loc WriteLocal i t) = VI loc ForeignLocal i t
    go v = v

--------------------------------------------------------------------------------
safeEnvFindTy :: (CheckingEnvironment r t, IsLocated l, Symbolic x, Monad m) => l -> t r -> x -> m (RType r)
--------------------------------------------------------------------------------
safeEnvFindTy l γ x | Just t <- chkEnvFindTy x γ = return t
                    | otherwise = die $ bugEnvFindTy (srcPos l) (F.symbol x)

globalLengthType γ = safeEnvFindTy (def::SrcSpan) γ "builtin_getLength"

