
{-# LANGUAGE MultiParamTypeClasses     #-}

module Language.Nano.Typecheck.Environment (

  TCEnv (..), TCEnvO, EnvLike (..)

  , IfaceEnv, mapIfaceEnvM

  ) where

import           Language.Nano.Types
import           Language.Nano.Typecheck.Types
import           Language.Nano.Errors
import           Language.Nano.Env
import           Language.ECMAScript3.PrettyPrint
import           Control.Applicative            hiding (empty)
import           Language.Fixpoint.Misc
import           Language.Nano.Misc
import qualified Language.Fixpoint.Types        as F
import           Text.PrettyPrint.HughesPJ 

-------------------------------------------------------------------------------
-- | Typecheck Environment (TODO: move to Environment.hs) 
-------------------------------------------------------------------------------

class EnvLike r t where
  -- 
  -- ^ Bindings in scope
  --
  get_env         :: t r -> Env (RType r)               
  -- 
  -- ^ Classs/Interfaces in scope
  --
  get_types       :: t r -> Env (IfaceDef r)      
  -- 
  -- ^ Modules in scope (exported API)
  --
  get_mod         :: t r -> QEnv (ModuleExports r)
  -- 
  -- ^ Namespace absolute path
  --
  get_nspace      :: t r -> NameSpacePath
  -- 
  -- ^ Calling context
  --
  get_ctx         :: t r -> IContext
--   -- 
--   -- ^ Ambient bindings in the global module
--   --
--   get_common_ts   :: t r -> CommonTypes r




data TCEnv r  = TCE {
    tce_env       :: Env (RType r)
  , tce_types     :: Env (IfaceDef r) 
  , tce_mod       :: QEnv (ModuleExports r)
  , tce_ctx       :: !IContext                          -- ^ Calling context 
  , tce_nspace    :: NameSpacePath                      
  }
  deriving (Functor)

--   We define this alias as the "output" type for typechecking any entity
--   that can create or affect binders (e.g. @VarDecl@ or @Statement@)
--   @Nothing@ means if we definitely hit a "return" 
--   @Just γ'@ means environment extended with statement binders

type TCEnvO r = Maybe (TCEnv r)


instance EnvLike r TCEnv where
  get_env         = tce_env
  get_types       = tce_types
  get_mod         = tce_mod
  get_nspace      = tce_nspace
  get_ctx         = tce_ctx

--   get_common_ts   = undefined -- TODO !!!!



type IfaceEnv r = Env (IfaceDef r) 
 

---------------------------------------------------------------------------------
-- | Environment operations
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
mapIfaceEnvM :: (Monad m, Applicative m) 
             => (RType t -> m (RType r)) -> IfaceEnv t -> m (IfaceEnv r)
---------------------------------------------------------------------------------
mapIfaceEnvM f e = envFromList <$> mapM (mapSndM (mapIfaceDefsM f)) (envToList e)
  

---------------------------------------------------------------------------------
mapIfaceDefsM :: (Monad m, Functor m) 
              => (RType t -> m (RType r)) -> IfaceDef t -> m (IfaceDef r)
---------------------------------------------------------------------------------
mapIfaceDefsM f (ID c n αs (Just (p,ps)) es) 
  = do  ps' <- mapM f ps 
        es' <- mapM (mapEltM f) es 
        return $ ID c n αs (Just (p,ps')) es'
mapIfaceDefsM f (ID c n αs Nothing es) = 
  ID c n αs Nothing <$> mapM (mapEltM f) es


instance (PP r, F.Reftable r) => PP (IfaceEnv r) where
  pp γ = text "Type definitions:"  $$ nest 2 (pp γ)

