{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Language.Rsc.Environment (

  -- * Environments
    CheckingEnvironment (..)

  -- * TC Env
  , TCEnv(..), TCEnvO


  -- * Liquid env
  , CGEnvR (..), CGEnv, CGEnvEntry


  -- * Operations
  , chkEnvFindTy
  , chkEnvFindTy'
  , safeEnvFindTy
  , safeEnvFindSI
  , envFindBound
  , envFindBoundOpt
  , resolveModuleInEnv
  , resolveTypeInEnv
  , resolveEnumInEnv
  , resolveTypeM
  , toFgn

  -- Global definitions
  , globalLengthType

) where

import           Data.Default
import qualified Language.Fixpoint.Types        as F
import           Language.Fixpoint.Types.Errors
import           Language.Fixpoint.Types.Names
import           Language.Rsc.ClassHierarchy
import           Language.Rsc.Core.Env
import           Language.Rsc.Errors
import           Language.Rsc.Locations
import           Language.Rsc.Module
import           Language.Rsc.Names
import           Language.Rsc.Pretty.Common
import           Language.Rsc.Symbols
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types



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

  -- | Class related      --- XXX: Why is this necessary ????
  --                      --- Just update the binding for `this`
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



--------------------------------------------------------------------------------
-- | TC Environment
--------------------------------------------------------------------------------

data TCEnv r  = TCE {
    tce_names  :: Env (SymInfo r)
  , tce_bounds :: Env (RType r)
  , tce_ctx    :: IContext
  , tce_path   :: AbsPath
  , tce_cha    :: ClassHierarchy r
  , tce_mut    :: Maybe (MutabilityR r)
  , tce_this   :: Maybe (RType r)
  , tce_fnid   :: Int
  }
  deriving (Functor)

--   We define this alias as the "output" type for typechecking any entity
--   that can create or affect binders (e.g. @VarDecl@ or @Statement@)
--   @Nothing@ means if we definitely hit a "return"
--   @Just γ'@ means environment extended with statement binders

type TCEnvO r = Maybe (TCEnv r)

instance CheckingEnvironment r TCEnv where
  envNames  = tce_names
  envBounds = tce_bounds
  envPath   = tce_path
  envCtx    = tce_ctx
  envCHA    = tce_cha
  envMut    = tce_mut
  envThis   = tce_this
  envFnId   = tce_fnid
--
-- instance PPR r => PP (TCEnv r) where
--   pp γ = pp (tce_names γ)
--


-------------------------------------------------------------------------------------
-- | Constraint Generation Environment
-------------------------------------------------------------------------------------

data CGEnvR r = CGE {
    cge_names  :: !(Env (SymInfo r))
  , cge_bounds :: !(Env (RType r))
  , cge_ctx    :: !IContext
  , cge_path   :: !AbsPath
  , cge_cha    :: !(ClassHierarchy r)
  , cge_fenv   :: !F.IBindEnv                   -- Fixpoint bindings (at this point)
  , cge_guards :: ![F.Expr]                     -- Branch target conditions
  , cge_consts :: !(Env (RType r))              -- Constants
  , cge_mut    :: !(Maybe (MutabilityR r))      -- Method mutability
  , cge_this   :: !(Maybe (RType r))            -- Method mutability
  , cge_fnid   :: !Int                          -- Enclosing fun's id
  } deriving (Functor)

instance CheckingEnvironment r CGEnvR where
  envNames  = cge_names
  envBounds = cge_bounds
  envCHA    = cge_cha
  envPath   = cge_path
  envCtx    = cge_ctx
  envMut    = cge_mut
  envThis   = cge_this
  envFnId   = cge_fnid

type CGEnv      = CGEnvR F.Reft
type CGEnvEntry = SymInfo F.Reft


-------------------------------------------------------------------------------
-- | Operations
-------------------------------------------------------------------------------

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
resolveTypeM :: (Monad m, IsLocated a, PPR r, CheckingEnvironment r t)
             => a -> t r -> AbsName -> m (TypeDecl r)
--------------------------------------------------------------------------------
resolveTypeM l γ x =
  case resolveTypeInEnv γ x of
    Just t  -> return t
    Nothing -> die $ bugClassDefNotFound (srcPos l) x


--------------------------------------------------------------------------------
toFgn :: Env (SymInfoQ q r) -> Env (SymInfoQ q r)
--------------------------------------------------------------------------------
toFgn = envMap go
  where
    go (SI n loc WriteLocal i t) = SI n loc ForeignLocal i t
    go v = v

--------------------------------------------------------------------------------
safeEnvFindTy :: (CheckingEnvironment r t, IsLocated l, Symbolic x, Monad m)
              => l -> t r -> x -> m (RType r)
--------------------------------------------------------------------------------
safeEnvFindTy l γ x | Just t <- chkEnvFindTy x γ = return t
                    | otherwise = die $ bugEnvFindTy (srcPos l) (F.symbol x)

--------------------------------------------------------------------------------
safeEnvFindSI :: (CheckingEnvironment r t, IsLocated l, Symbolic x, Monad m)
              => l -> t r -> x -> m (SymInfo r)
--------------------------------------------------------------------------------
safeEnvFindSI l γ x | Just t <- chkEnvFindTy' x γ = return t
                    | otherwise = die $ bugEnvFindTy (srcPos l) (F.symbol x)


globalLengthType γ = safeEnvFindTy (def::SrcSpan) γ "builtin_getLength"

