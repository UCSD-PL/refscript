
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ViewPatterns              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Language.Nano.Environment (

    EnvLike (..)
  , EnvEntry
  , envLikeFindTy, envLikeFindTy'
  , envFindBound
  , fromListToEnv

) where

import           Control.Applicative                ((<$>), (<*>))
import           Control.Exception                  (throw)
import           Data.Maybe                         (isJust)
import qualified Data.Map.Strict                    as M

import           Language.Nano.AST
import           Language.Nano.ClassHierarchy
import           Language.Nano.Types
import           Language.Nano.Typecheck.Types
import           Language.Nano.Types
import           Language.Nano.Env
import           Language.Nano.Errors
import           Language.Nano.Locations
import           Language.Nano.Names
import           Language.Nano.Program
import           Language.Fixpoint.Names
import qualified Language.Fixpoint.Types            as F

-------------------------------------------------------------------------------
-- | Typecheck Environment
-------------------------------------------------------------------------------

type EnvEntry r = VarInfo r 

class EnvLike r t where
  -- 
  -- ^ Bindings in scope
  --   Includes : variables, functions, classes, interfaces
  --
  names           :: t r -> Env (EnvEntry r)
  -- 
  -- ^ Bounds for type variables
  --
  bounds          :: t r -> Env (RType r)
  -- 
  -- ^ Calling context
  --
  context         :: t r -> IContext
  -- 
  -- ^ Namespace absolute path
  --
  absPath         :: t r -> AbsPath
  -- 
  -- ^ Modules in scope (exported API)
  --
  modules         :: t r -> QEnv (ModuleDef r)
  -- 
  -- ^ ClassHierarchy
  -- 
  cha             :: t r -> ClassHierarchy r


--   -- 
--   -- ^ Parent environment
--   --
--   parent          :: t r -> Maybe (t r)
-- 

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

envLikeMember x = isJust . envLikeFindTy x

envFindBound x (bounds -> b) = envFindTy x b


---------------------------------------------------------------------------------------
fromListToEnv :: F.Symbolic s => [(s, (SyntaxKind, VarInfo r))] -> Env (SyntaxKind, EnvEntry r)
---------------------------------------------------------------------------------------
fromListToEnv = envFromListWithKey mergeVarInfo . concatMap f . M.toList . foldl merge M.empty
  where
    merge ms (x, (k,v)) = M.insertWith (++) (F.symbol x) [(k,v)] ms

    f (s, vs)   = [ (s, (k, g v [ v' | (FuncOverloadKind, v') <- vs ])) | (k@FuncDefKind, v) <- vs ] ++
                    ( (s,) . (FuncAmbientKind,) <$> amb [ v | (k@FuncAmbientKind, v) <- vs ] ) ++ 
                  [ (s, (k, v)) | (k@VarDeclKind, v) <- vs ] ++ 
                  [ (s, (k, v)) | (k@ClassDefKind, v) <- vs ] ++
                  [ (s, (k, v)) | (k@ModuleDefKind, v) <- vs ] ++
                  [ (s, (k, v)) | (k@EnumDefKind, v) <- vs ]

    g v []                = v
    g _ vs@(VI a i _ : _) = VI a i $ mkAnd $ v_type <$> vs

    amb [ ] = [ ] 
    amb [v] = [v]
    amb vs@((VI a i _) : _) = [ VI a i $ mkAnd $ v_type <$> vs ]

mergeVarInfo x _ _ = throw $ errorDuplicateKey (srcPos x) x

