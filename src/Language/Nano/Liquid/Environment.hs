{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

module Language.Nano.Liquid.Environment where

import qualified Language.Fixpoint.Types       as F
import           Language.Nano.Annots          ()
import           Language.Nano.ClassHierarchy
import           Language.Nano.Core.Env
import           Language.Nano.Environment
import           Language.Nano.Liquid.Types
import           Language.Nano.Locations
import           Language.Nano.Names
import           Language.Nano.Pretty
import           Language.Nano.Typecheck.Types
import           Language.Nano.Types

-------------------------------------------------------------------------------------
-- | Constraint Generation Environment
-------------------------------------------------------------------------------------

data CGEnvR r = CGE {
    cge_names  :: !(Env (EnvEntry r))
  , cge_bounds :: !(Env (RType r))
  , cge_ctx    :: !IContext
  , cge_path   :: !AbsPath
  , cge_cha    :: !(ClassHierarchy r)
  , cge_fenv   :: !(F.SEnv F.IBindEnv)  -- Fixpoint bindings
  , cge_guards :: ![F.Pred]             -- Branch target conditions
  , cge_consts :: !(Env (RType r))      -- Constants
  } deriving (Functor)

type CGEnv = CGEnvR F.Reft

type CGEnvEntry = EnvEntry F.Reft

instance EnvLike r CGEnvR where
  envNames  = cge_names
  envBounds = cge_bounds
  envCHA    = cge_cha
  envPath   = cge_path
  envCtx    = cge_ctx

type EnvKey x = (IsLocated x, F.Symbolic x, PP x, F.Expression x)


-- Only include the "singleton" refinement in the case where Assignability is
-- either ReadOnly of WriteLocal (SSAed)
---------------------------------------------------------------------------------------
envFindTyWithAsgn :: (EnvKey x, F.Expression x) => x -> CGEnv -> Maybe CGEnvEntry
---------------------------------------------------------------------------------------
envFindTyWithAsgn x (envNames -> γ) = fmap singleton (envFindTy x γ)
  where
    singleton v@(VI WriteGlobal Initialized   _) = v
    singleton v@(VI WriteGlobal Uninitialized t) = v { v_type = orUndef t }
    singleton v = v { v_type = eSingleton (v_type v) x }

-- ---------------------------------------------------------------------------------------
-- envAddReturn :: (IsLocated f)  => f -> RefType -> CGEnv -> CGEnv
-- ---------------------------------------------------------------------------------------
-- envAddReturn f t g  = g { cge_names = E.envAddReturn f e $ cge_names g }
--   where
--     e = VI ReturnVar Initialized t

---------------------------------------------------------------------------------------
cgEnvFindReturn :: CGEnv -> RefType
---------------------------------------------------------------------------------------
cgEnvFindReturn = v_type . envFindReturn . cge_names


