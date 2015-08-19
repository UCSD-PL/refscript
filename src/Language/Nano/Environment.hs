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
  , fromListToEnv
  , resolveModuleInEnv, resolveTypeInEnv, resolveEnumInEnv

) where

import           Control.Applicative           ((<$>))
import           Control.Exception             (throw)
import qualified Data.Map.Strict               as M
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

-------------------------------------------------------------------------------
envFindBound :: (EnvLike r t, Symbolic a) => a -> t r -> Maybe (RType r)
-------------------------------------------------------------------------------
envFindBound x (bounds -> b) = envFindTy x b


---------------------------------------------------------------------------------------
fromListToEnv :: F.Symbolic s => [(s, (SyntaxKind, VarInfo r))] -> Env (SyntaxKind, EnvEntry r)
---------------------------------------------------------------------------------------
fromListToEnv = envFromListWithKey mergeVarInfo . concatMap f . M.toList . foldl merge M.empty
  where
    merge ms (x, (k,v)) = M.insertWith (++) (F.symbol x) [(k,v)] ms

    f (s, vs)   = [ (s, (k, g v [ v' | (FuncOverloadKind, v') <- vs ])) | (k@FuncDefKind, v) <- vs ] ++
                    ( (s,) . (FuncAmbientKind,) <$> amb [ v | (FuncAmbientKind, v) <- vs ] ) ++
                  [ (s, (k, v)) | (k@VarDeclKind, v) <- vs ] ++
                  [ (s, (k, v)) | (k@ClassDefKind, v) <- vs ] ++
                  [ (s, (k, v)) | (k@ModuleDefKind, v) <- vs ] ++
                  [ (s, (k, v)) | (k@EnumDefKind, v) <- vs ]

    g v []                = v
    g _ vs@(VI a i _ : _) = VI a i $ mkAnd $ v_type <$> vs

    amb [ ] = [ ]
    amb [v] = [v]
    amb vs@(VI a i _ : _) = [ VI a i $ mkAnd $ v_type <$> vs ]

    mergeVarInfo x _ _ = throw $ errorDuplicateKey (srcPos x) x


--------------------------------------------------------------------------------
resolveModuleInEnv  :: EnvLike r t => t r -> AbsPath -> Maybe (ModuleDef r)
resolveTypeInEnv    :: EnvLike r t => t r -> AbsName -> Maybe (TypeDecl r)
resolveEnumInEnv    :: EnvLike r t => t r -> AbsName -> Maybe EnumDef
--------------------------------------------------------------------------------
resolveModuleInEnv (cha -> c) = resolveModule c
resolveTypeInEnv   (cha -> c) = resolveType c
resolveEnumInEnv   (cha -> c) = resolveEnum c


-- isClassType :: EnvLike r g => g r -> RType r -> Bool
