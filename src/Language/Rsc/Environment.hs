{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Language.Rsc.Environment (

  -- * Environments
    CheckingEnvironment (..)
  , chkEnvFindTy, chkEnvFindTy'
  , safeEnvFindTy
  , envFindBound, envFindBoundOpt
  , resolveModuleInEnv, resolveTypeInEnv, resolveEnumInEnv
  , toFgn

  -- Global definitions
  , globalLengthType

) where

import           Control.Applicative            ((<$>))
import           Control.Exception              (throw)
import           Data.Default
import           Data.Generics
import qualified Data.List                      as L
import           Data.Maybe                     (fromMaybe)
import           Language.Fixpoint.Misc         (safeZip)
import qualified Language.Fixpoint.Types        as F
import           Language.Fixpoint.Types.Errors
import           Language.Fixpoint.Types.Names
import           Language.Rsc.Annotations
import           Language.Rsc.AST
import           Language.Rsc.ClassHierarchy
import           Language.Rsc.Core.Env
import           Language.Rsc.Errors
import           Language.Rsc.Locations
import           Language.Rsc.Module
import           Language.Rsc.Names
import           Language.Rsc.Pretty.Common
import           Language.Rsc.Symbols
import           Language.Rsc.Traversals
import           Language.Rsc.Typecheck.Subst
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
import           Text.PrettyPrint.HughesPJ


-------------------------------------------------------------------------------
-- | Typecheck Environment
-------------------------------------------------------------------------------

class CheckingEnvironment r t where
  --
  -- | Bindings in scope
  --
  envNames  :: t r -> Env (SymInfo r)
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
  envMut    :: t r -> Maybe (MutabilityR r)
  --
  --   * Type for this
  --
  envThis   :: t r -> Maybe (RType r)
  --
  --   * Enclosing function's id
  --
  envFnId   :: t r -> Int


-------------------------------------------------------------------------------
chkEnvFindTy' :: (CheckingEnvironment r t, Symbolic a) => a -> t r -> Maybe (SymInfo r)
-------------------------------------------------------------------------------
chkEnvFindTy' x (envNames -> γ) = envFindTy x γ

-------------------------------------------------------------------------------
chkEnvFindTy :: (CheckingEnvironment r t, Symbolic a) => a -> t r -> Maybe (RType r)
-------------------------------------------------------------------------------
chkEnvFindTy x = fmap v_type . chkEnvFindTy' x

-------------------------------------------------------------------------------
envFindBound :: (CheckingEnvironment r t, F.Reftable r) => t r -> RType r -> RType r
-------------------------------------------------------------------------------
envFindBound (envBounds -> b) t | TVar a _ <- t, Just s <- envFindTy a b = s
                                | otherwise = tTop

-------------------------------------------------------------------------------
envFindBoundOpt :: (CheckingEnvironment r t, F.Reftable r) => t r -> RType r -> Maybe (RType r)
-------------------------------------------------------------------------------
envFindBoundOpt (envBounds -> b) t | TVar a _ <- t = envFindTy a b
                                   | otherwise     = Nothing


--------------------------------------------------------------------------------
resolveModuleInEnv  :: (PPR r, CheckingEnvironment r t) => t r -> AbsPath -> Maybe (ModuleDef r)
resolveTypeInEnv    :: (PPR r, CheckingEnvironment r t) => t r -> AbsName -> Maybe (TypeDecl r)
resolveEnumInEnv    :: (PPR r, CheckingEnvironment r t) => t r -> AbsName -> Maybe EnumDef
--------------------------------------------------------------------------------
resolveModuleInEnv (envCHA -> c) = resolveModule c
resolveTypeInEnv   (envCHA -> c) = resolveType c
resolveEnumInEnv   (envCHA -> c) = resolveEnum c

--------------------------------------------------------------------------------
toFgn :: Env (SymInfoQ q r) -> Env (SymInfoQ q r)
--------------------------------------------------------------------------------
toFgn = envMap go
  where
    go (SI n loc WriteLocal i t) = SI n loc ForeignLocal i t
    go v = v

--------------------------------------------------------------------------------
safeEnvFindTy :: (CheckingEnvironment r t, IsLocated l, Symbolic x, Monad m) => l -> t r -> x -> m (RType r)
--------------------------------------------------------------------------------
safeEnvFindTy l γ x | Just t <- chkEnvFindTy x γ = return t
                    | otherwise = die $ bugEnvFindTy (srcPos l) (F.symbol x)

globalLengthType γ = safeEnvFindTy (def::SrcSpan) γ "builtin_getLength"

