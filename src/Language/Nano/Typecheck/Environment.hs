{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Nano.Typecheck.Environment
    ( TCEnv(..), TCEnvO
    , initModuleEnv, initGlobalEnv
    , initClassCtorEnv, initClassMethEnv
    , initCallableEnv
    , initClassInstanceEnv
    , tcEnvFindTy, tcEnvFindTypeDefM, tcEnvFindTyForAsgn
    , tcEnvFindReturn, tcEnvAdd, tcEnvAdds, safeTcEnvFindTy
    , tcEnvAddBounds
    ) where

import           Data.Function                 (on)
import           Data.List                     (sortBy)
import           Data.Monoid
import           Language.Fixpoint.Errors      (die)
import           Language.Fixpoint.Misc        (single)
import qualified Language.Fixpoint.Types       as F
import           Language.Nano.Annots
import           Language.Nano.AST
import           Language.Nano.ClassHierarchy
import           Language.Nano.Core.Env
import           Language.Nano.Environment
import           Language.Nano.Errors
import           Language.Nano.Locations
import           Language.Nano.Names
import           Language.Nano.Pretty
import           Language.Nano.Program
import           Language.Nano.Traversals
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Typecheck.Types
import           Language.Nano.Types
import           Text.PrettyPrint.HughesPJ

type Unif r = (PP r, F.Reftable r, ExprReftable Int r, ExprReftable F.Symbol r, Free (Fact r))

-------------------------------------------------------------------------------
-- | Typecheck Environment
-------------------------------------------------------------------------------

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

instance EnvLike r TCEnv where
  envNames  = tce_names
  envBounds = tce_bounds
  envPath   = tce_path
  envCtx    = tce_ctx
  envCHA    = tce_cha
  envThis   = tce_this


-------------------------------------------------------------------------------
-- | Environment initialization
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
initGlobalEnv :: Unif r => TcRsc r -> ClassHierarchy r -> TCEnv r
-------------------------------------------------------------------------------
initGlobalEnv pgm@(Rsc { code = Src ss }) cha
  = TCE nms bnds ctx pth cha mut tThis
  where
    nms   = mkVarEnv (accumVars ss)    -- modules ?
    bnds  = mempty
    ctx   = emptyContext
    pth   = emptyPath
    mut   = Nothing
    tThis = Nothing

-- This will be called *last* on every contructor, method, function.
-- It is transparent to the incoming environment's: path, cha, mut, this
-------------------------------------------------------------------------------
initCallableEnv :: (IsLocated l, Unif r)
                => AnnTc r -> TCEnv r -> l
                -> IOverloadSig r
                -> [Statement (AnnTc r)]
                -> TCEnv r
-------------------------------------------------------------------------------
initCallableEnv l γ f fty s
  = TCE nms bnds ctx pth cha mut tThis
  where
    nms   = envAddReturn f (VI ReturnVar Initialized t)
          $ envAdds tyBs
          $ envAdds varBs
          $ envAdds arg
          $ envUnion (mkVarEnv (accumVars s))
          $ toFgn (envNames γ)
    -- tyBs  = [(tVarId α, VI Ambient Initialized $ tVar α) | α <- αs]
    -- tVarId (TV a l) = Id l $ "TVAR$$" ++ F.symbolString a
    tyBs  = [(Loc (srcPos l) α, VI Ambient Initialized $ tVar α) | α <- αs]
    varBs = [(x, VI WriteLocal Initialized t) | B x t <- xts]
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

-------------------------------------------------------------------------------
initClassInstanceEnv :: TypeSig r -> TCEnv r -> TCEnv r
-------------------------------------------------------------------------------
initClassInstanceEnv (TS _ (BGen _ bs) _) γ =
  γ { tce_bounds = envAdds bts (tce_bounds γ) }
  where
    bts = [(s,t) | BTV s _ (Just t) <- bs]

-------------------------------------------------------------------------------
initClassMethEnv :: Unif r => MutabilityMod -> TypeSig r -> TCEnv r -> TCEnv r
-------------------------------------------------------------------------------
initClassMethEnv m (TS _ (BGen nm bs) _) γ
  = γ { tce_mut  = Just m
      , tce_this = Just $ TRef (Gen nm (map btVar bs)) fTop }

-------------------------------------------------------------------------------
initModuleEnv :: (Unif r, F.Symbolic n, PP n) => TCEnv r -> n -> [Statement (AnnTc r)] -> TCEnv r
-------------------------------------------------------------------------------
initModuleEnv γ n s = TCE nms bnds ctx pth cha mut tThis
  where
    nms   = mkVarEnv (accumVars s)
    bnds  = envBounds γ
    ctx   = envCtx γ
    pth   = extendAbsPath (envPath γ) n
    cha   = envCHA γ
    mut   = Nothing -- 'this' gets out of scope when entering a module
    tThis = Nothing

-- initCallable will be called later on the result
-------------------------------------------------------------------------------
initClassCtorEnv :: Unif r => TypeSigQ AK r -> TCEnv r -> TCEnv r
-------------------------------------------------------------------------------
initClassCtorEnv (TS _ (BGen nm bs) _) γ
  = γ { tce_names = addExit (envNames γ)
      , tce_mut   = Just AssignsFields
      , tce_this  = Just tThis
      }
  where
    -- addSuper | Just t <- getImmediateSuperclass sig
    --          = envAdd super (VI Ambient Initialized t)
    --          | otherwise
    --          = id
    -- super    = builtinOpId BISuper
    addExit  = envAdd ctorExit (VI Ambient Initialized exitTy)
    ctorExit = builtinOpId BICtorExit
    -- XXX: * Keep the right order of fields
    --      * Make the return object immutable to avoid contra-variance
    --        checks at the return from the constructor.
    exitTy   = mkFun (bs, xts, tThis)
    xts      | Just (TObj ms _) <- expandType Coercive (envCHA γ) tThis
             = sortBy c_sym [ B x t | (x, FI _ _ t) <- F.toListSEnv $ tm_prop ms ]
             | otherwise
             = []
    c_sym    = on compare b_sym
    tThis    = TRef (Gen nm (map btVar bs)) fTop


-------------------------------------------------------------------------------
-- | Environment wrappers
-------------------------------------------------------------------------------

tcEnvAdds xs γ = γ { tce_names = envAdds xs $ tce_names γ }

tcEnvAdd x t γ = γ { tce_names = envAdd x t $ tce_names γ }

-------------------------------------------------------------------------------
tcEnvFindTy :: (Unif r, F.Symbolic x, IsLocated x) => x -> TCEnv r -> Maybe (RType r)
-------------------------------------------------------------------------------
tcEnvFindTy x γ = fmap v_type (tcEnvFindTyWithAgsn x γ)

-------------------------------------------------------------------------------
tcEnvFindTyWithAgsn :: (Unif r, F.Symbolic x) => x -> TCEnv r -> Maybe (EnvEntry r)
-------------------------------------------------------------------------------
tcEnvFindTyWithAgsn x γ | Just t <- envFindTy x $ tce_names γ
                        = Just $ adjustInit t
                        | otherwise
                        = Nothing
  where
    adjustInit s@(VI _ Initialized _) = s
    adjustInit (VI a _ t) = VI a Uninitialized $ orUndef t

-- This is a variant of the above that doesn't add the ' + undefined' for
-- non-initialized variables.
-------------------------------------------------------------------------------
tcEnvFindTyForAsgn    :: (Unif r, F.Symbolic x) => x -> TCEnv r -> Maybe (EnvEntry r)
-------------------------------------------------------------------------------
tcEnvFindTyForAsgn x γ = envFindTy x $ tce_names γ

safeTcEnvFindTy l γ x | Just t <- tcEnvFindTy x γ = return t
                      | otherwise = die $ bugEnvFindTy (srcPos l) x

tcEnvFindReturn = v_type . envFindReturn . tce_names

tcEnvFindTypeDefM l γ x
  = case resolveTypeInEnv γ x of
      Just t  -> return t
      Nothing -> die $ bugClassDefNotFound (srcPos l) x

-------------------------------------------------------------------------------
tcEnvAddBounds :: [BTVar r] -> TCEnv r -> TCEnv r
-------------------------------------------------------------------------------
tcEnvAddBounds = flip $ foldr go
  where
    go (BTV α _ (Just t)) γ = γ { tce_bounds = envAdd α t $ tce_bounds γ }

