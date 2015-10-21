{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Rsc.Typecheck.Environment
    ( TCEnv(..), TCEnvO
    , initModuleEnv, initGlobalEnv
    , initClassCtorEnv, initClassMethEnv
    , initCallableEnv
    , initClassInstanceEnv
    , tcEnvFindTy, resolveTypeM, tcEnvFindTyForAsgn
    , tcEnvFindReturn, tcEnvAdd, tcEnvAdds
    , tcEnvAddBounds
    , Unif
    ) where

import           Data.Data
import           Data.Function                (on)
import           Data.List                    (sortBy)
import           Data.Monoid
import           Data.Typeable
import           Language.Fixpoint.Errors     (die)
import           Language.Fixpoint.Misc       (fst3, safeZip, single)
import qualified Language.Fixpoint.Types      as F
import           Language.Rsc.Annotations
import           Language.Rsc.AST
import           Language.Rsc.ClassHierarchy
import           Language.Rsc.Core.Env
import           Language.Rsc.Environment
import           Language.Rsc.Errors
import           Language.Rsc.Locations
import           Language.Rsc.Misc
import           Language.Rsc.Names
import           Language.Rsc.Pretty
import           Language.Rsc.Program
import           Language.Rsc.Traversals
import           Language.Rsc.Typecheck.Subst
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
import           Text.PrettyPrint.HughesPJ

-- import           Debug.Trace


type Unif r = ( PP r
              , F.Reftable r
              , ExprReftable F.Expr r
              , ExprReftable Int r
              , ExprReftable F.Symbol r
              , Free (Fact r)
              , Typeable r
              , Data r
              )

--------------------------------------------------------------------------------
-- | Typecheck Environment
--------------------------------------------------------------------------------

data TCEnv r  = TCE {
    tce_names  :: Env (EnvEntry r)
  , tce_bounds :: Env (RType r)
  , tce_ctx    :: IContext
  , tce_path   :: AbsPath
  , tce_cha    :: ClassHierarchy r
  , tce_mut    :: Maybe (MutabilityMod)
  , tce_this   :: Maybe (RType r)
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


--------------------------------------------------------------------------------
-- | Environment initialization
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
initGlobalEnv :: Unif r => TcRsc r -> ClassHierarchy r -> TCEnv r
--------------------------------------------------------------------------------
initGlobalEnv pgm@(Rsc { code = Src ss }) cha
  = TCE nms bnds ctx pth cha mut tThis
  where
    nms   = mkVarEnv (accumVars ss)
    bnds  = mempty
    ctx   = emptyContext
    pth   = emptyPath
    mut   = Nothing
    tThis = Nothing

-- This will be called *last* on every contructor, method, function.
-- It is transparent to the incoming environment's: path, cha, mut, this
--------------------------------------------------------------------------------
initCallableEnv :: (IsLocated l, Unif r)
                => AnnTc r -> TCEnv r -> l
                -> IOverloadSig r
                -> [Id (AnnTc r)]
                -> [Statement (AnnTc r)]
                -> TCEnv r
--------------------------------------------------------------------------------
initCallableEnv l γ f fty xs s
  = TCE nms bnds ctx pth cha mut tThis
  & tcEnvAdds arg
  & tcEnvAdds varBs
  & tcEnvAdds tyBs
  where
    nms   = toFgn (envNames γ)
          & envUnion (mkVarEnv (accumVars s))
          & envAddReturn f (VI Local ReturnVar Initialized t)

    tyBs  = [(Loc (srcPos l) α, VI Local Ambient Initialized $ tVar α) | α <- αs]
    varBs = [(x, VI Local WriteLocal Initialized t) | (x, t) <- safeZip "initCallableEnv" xs ts]
    arg   = single (argId $ srcPos l, mkArgTy l ts)
    bnds  = envAdds [(s,t) | BTV s _ (Just t) <- bs] $ envBounds γ
    ctx   = pushContext i (envCtx γ)
    pth   = envPath γ
    cha   = envCHA γ
    mut   = envMut γ
    tThis = envThis γ
    (i, (bs,xts,t)) = fty
    ts    = map b_type xts
    αs    = map btvToTV bs

--------------------------------------------------------------------------------
initClassInstanceEnv :: TypeSig r -> TCEnv r -> TCEnv r
--------------------------------------------------------------------------------
initClassInstanceEnv (TS _ (BGen _ bs) _) γ =
  γ { tce_bounds = envAdds bts (tce_bounds γ) }
  where
    bts = [(s,t) | BTV s _ (Just t) <- bs]

--------------------------------------------------------------------------------
initClassMethEnv :: Unif r => MutabilityMod -> TypeSig r -> TCEnv r -> TCEnv r
--------------------------------------------------------------------------------
initClassMethEnv m (TS _ (BGen nm bs) _) γ
  = γ { tce_mut  = Just m
      , tce_this = Just $ TRef (Gen nm (map btVar bs)) fTop }

--------------------------------------------------------------------------------
initModuleEnv :: (Unif r, F.Symbolic n, PP n) => TCEnv r -> n -> [Statement (AnnTc r)] -> TCEnv r
--------------------------------------------------------------------------------
initModuleEnv γ n s = TCE nms bnds ctx pth cha mut tThis
  where
    nms   = mkVarEnv (accumVars s) `envUnion` toFgn (envNames γ)
    bnds  = envBounds γ
    ctx   = envCtx γ
    pth   = extendAbsPath (envPath γ) n
    cha   = envCHA γ
    mut   = Nothing -- 'this' gets out of scope when entering a module
    tThis = Nothing

-- initCallable will be called later on the result
--------------------------------------------------------------------------------
initClassCtorEnv :: Unif r => TypeSigQ AK r -> TCEnv r -> TCEnv r
--------------------------------------------------------------------------------
initClassCtorEnv (TS _ (BGen nm bs) _) γ
  = γ { tce_names = envNames γ
      , tce_mut   = Just AssignsFields
      , tce_this  = Just tThis
      }
  &  tcEnvAdd ctorExit ctorExitVI
  where
    ctorExitVI = VI Local Ambient Initialized exitTy
    ctorExit = builtinOpId BICtorExit
    -- XXX: * Keep the right order of fields
    --      * Make the return object immutable to avoid contra-variance
    --        checks at the return from the constructor.
    exitTy   = mkFun (bs, xts, tThis)
    xts      | Just (TObj _ ms _) <- expandType Coercive (envCHA γ) tThis
             = sortBy c_sym [ B x t | (x, FI _ _ t) <- F.toListSEnv $ tm_prop ms ]
             | otherwise
             = []
    c_sym    = on compare b_sym
    tThis    = TRef (Gen nm (map btVar bs)) fTop


--------------------------------------------------------------------------------
-- | Environment wrappers
--------------------------------------------------------------------------------

tcEnvAdds xs γ = γ { tce_names = envAdds xs $ tce_names γ }

tcEnvAdd x t γ = γ { tce_names = envAdd x t $ tce_names γ }

--------------------------------------------------------------------------------
tcEnvFindTy :: (Unif r, F.Symbolic x, IsLocated x) => x -> TCEnv r -> Maybe (RType r)
--------------------------------------------------------------------------------
tcEnvFindTy x γ = fmap v_type (tcEnvFindTyWithAgsn x γ)

--------------------------------------------------------------------------------
tcEnvFindTyWithAgsn :: (Unif r, F.Symbolic x) => x -> TCEnv r -> Maybe (EnvEntry r)
--------------------------------------------------------------------------------
tcEnvFindTyWithAgsn x γ | Just t <- envFindTy x $ tce_names γ
                        = Just $ adjustInit t
                        | otherwise
                        = Nothing
  where
    adjustInit s@(VI _ _ Initialized _) = s
    adjustInit (VI loc a _ t) = VI loc a Uninitialized $ orUndef t

-- This is a variant of the above that doesn't add the ' + undefined' for
-- non-initialized variables.
--------------------------------------------------------------------------------
tcEnvFindTyForAsgn    :: (Unif r, F.Symbolic x) => x -> TCEnv r -> Maybe (EnvEntry r)
--------------------------------------------------------------------------------
tcEnvFindTyForAsgn x γ = envFindTy x $ tce_names γ

tcEnvFindReturn = v_type . envFindReturn . tce_names

resolveTypeM l γ x
  = case resolveTypeInEnv γ x of
      Just t  -> return t
      Nothing -> die $ bugClassDefNotFound (srcPos l) x

--------------------------------------------------------------------------------
tcEnvAddBounds :: [BTVar r] -> TCEnv r -> TCEnv r
--------------------------------------------------------------------------------
tcEnvAddBounds = flip $ foldr go
  where
    go (BTV α _ (Just t)) γ = γ { tce_bounds = envAdd α t $ tce_bounds γ }

