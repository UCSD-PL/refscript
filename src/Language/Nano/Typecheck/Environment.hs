
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE DeriveFunctor             #-}

module Language.Nano.Typecheck.Environment where

import           Control.Applicative            hiding (empty)

import           Language.Nano.Types
import           Language.Nano.Typecheck.Types
import           Language.Nano.Env
import           Language.Nano.Misc
import           Language.Nano.Names

import           Language.ECMAScript3.PrettyPrint
import qualified Language.Fixpoint.Types        as F
import           Text.PrettyPrint.HughesPJ 

-------------------------------------------------------------------------------
-- | Typecheck Environment (TODO: move to src/Environment.hs) 
-------------------------------------------------------------------------------

class EnvLike r t where
  -- 
  -- ^ Bindings in scope
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
--   -- 
--   -- ^ Ambient bindings in the global module
--   --
--   get_common_ts   :: t r -> CommonTypes r


data TCEnv r  = TCE {
    tce_names     :: Env (RType r)
  , tce_mod       :: QEnv (ModuleDef r)
  , tce_ctx       :: !IContext                          -- ^ Calling context 
  , tce_path      :: AbsPath
  , tce_parent    :: Maybe (TCEnv r)
  }
  deriving (Functor)


--   We define this alias as the "output" type for typechecking any entity
--   that can create or affect binders (e.g. @VarDecl@ or @Statement@)
--   @Nothing@ means if we definitely hit a "return" 
--   @Just γ'@ means environment extended with statement binders

type TCEnvO r = Maybe (TCEnv r)


instance EnvLike r TCEnv where
  names           = tce_names
  modules         = tce_mod
  absPath         = tce_path
  context         = tce_ctx
  parent          = tce_parent
 

instance (PP r, F.Reftable r) => PP (TCEnv r) where
  pp = ppTCEnv

ppTCEnv (TCE nms mod _ pth _ )
  =   text "******************** Environment ************************"
  $+$ pp nms
  $+$ text "******************** Modules ****************************"
  $+$ pp mod
  $+$ text "******************** Absolute path **********************"
  $+$ pp pth
--   $+$ text "******************** Call Context ***********************"
--   $+$ pp ctx

