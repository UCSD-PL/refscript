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
    , tcEnvFindTy, resolveTypeM, tcEnvFindTyForAsgn
    , tcEnvFindReturn, tcEnvAdd, tcEnvAdds
    , tcEnvAddBounds
    , Unif
    ) where

import           Data.Data
import           Language.Fixpoint.Misc         (safeZip)
import qualified Language.Fixpoint.Types        as F
import           Language.Fixpoint.Types.Errors (die)
import           Language.Rsc.Annotations
import           Language.Rsc.AST
import           Language.Rsc.ClassHierarchy
import           Language.Rsc.Core.Env
import           Language.Rsc.Environment
import           Language.Rsc.Errors
import           Language.Rsc.Locations
import           Language.Rsc.Misc
import           Language.Rsc.Names
import           Language.Rsc.Pretty.Common
import           Language.Rsc.Program
import           Language.Rsc.Symbols
import           Language.Rsc.Typecheck.Subst
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types

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


--------------------------------------------------------------------------------
-- | Environment initialization
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
initGlobalEnv :: Unif r => TcRsc r -> ClassHierarchy r -> TCEnv r
--------------------------------------------------------------------------------
initGlobalEnv (Rsc { code = Src ss }) cha
  = TCE nms bnds ctx pth cha mut tThis (-1)
  where
    nms   = symEnv ss
    bnds  = mempty
    ctx   = emptyContext
    pth   = emptyPath
    mut   = Nothing
    tThis = Nothing

-- This will be called *last* on every contructor, method, function.
-- It is transparent to the incoming environment's: path, cha, mut, this
--
-- TODO: Shadow `this` binding (in case this is a class context)
--
--------------------------------------------------------------------------------
initCallableEnv :: (IsLocated l, Unif r)
                => AnnTc r -> TCEnv r -> l
                -> IOverloadSig r
                -> [Id (AnnTc r)]
                -> [Statement (AnnTc r)]
                -> TCEnv r
--------------------------------------------------------------------------------
initCallableEnv l γ f fty xs s
  = TCE nms bnds ctx pth cha mut tThis (fId l)
  & tcEnvAdds arg
  & tcEnvAdds varBs
  & tcEnvAdds tyBs
  where
    nms   = toFgn (envNames γ)
          & mappend (symEnv s)
          & envAddReturn f (SI rSym Local ReturnVar Initialized t)

    rSym  = F.symbol "return"
    tyBs  = [(Loc (srcPos l) α, SI (F.symbol α) Local Ambient Initialized $ tVar α) | α <- αs]
    varBs = [(x, SI (F.symbol x) Local WriteLocal Initialized t) | (x, t) <- safeZip "initCallableEnv" xs ts]
    arg   = [(getArgId (srcPos l), mkArgumentsSI l ts)]
    bnds  = envAdds [(s,t) | BTV s _ (Just t) <- bs] $ envBounds γ
    ctx   = pushContext i (envCtx γ)
    pth   = envPath γ
    cha   = envCHA γ
    mut   = envMut γ
    tThis = envThis γ
    (i, (bs,xts,t)) = fty
    ts    = map b_type xts
    αs    = map btvToTV bs


-- | `initClassCtorEnv` makes `this` a Unique & Uninitialized binding
--------------------------------------------------------------------------------
initClassCtorEnv :: Unif r => TypeSigQ AK r -> TCEnv r -> TCEnv r
--------------------------------------------------------------------------------
initClassCtorEnv (TS _ (BGen nm bs) _) γ =
    tcEnvAdd thisSym eThis γ'
  where
    γ'    = γ { tce_mut    = Just tUQ
              , tce_this   = Just tThis
              , tce_bounds = envAdds bts (tce_bounds γ)
              }
    bts   = [(s,t) | BTV s _ (Just t) <- bs]
    tThis = adjUQ (TRef (Gen nm (map btVar bs)) fTop)
    eThis = SI thisSym Local RdOnly Uninitialized tThis
    adjUQ (TRef (Gen n (_:ps)) r) = TRef (Gen n (tUQ:ps)) r
    adjUQ t                       = t


--------------------------------------------------------------------------------
initClassMethEnv :: Unif r => MutabilityR r -> TypeSig r -> TCEnv r -> TCEnv r
--------------------------------------------------------------------------------
initClassMethEnv m (TS _ (BGen nm bs) _) γ =
    tcEnvAdd thisSym eThis γ'
  where
    γ'    = γ { tce_bounds = envAdds bts (tce_bounds γ)
              , tce_mut    = Just m
              }
    bts   = [(s,t) | BTV s _ (Just t) <- bs]
    tThis = adjMut $ TRef (Gen nm (map btVar bs)) fTop
    eThis = SI thisSym Local RdOnly Initialized tThis

    adjMut (TRef (Gen n (_:ps)) r) = TRef (Gen n (m:ps)) r
    adjMut t                       = t


--------------------------------------------------------------------------------
initModuleEnv :: (Unif r, F.Symbolic n, PP n) => TCEnv r -> n -> [Statement (AnnTc r)] -> TCEnv r
--------------------------------------------------------------------------------
initModuleEnv γ n s = TCE nms bnds ctx pth cha mut tThis fnId
  where
    nms   = symEnv s `mappend` toFgn (envNames γ)
    bnds  = envBounds γ
    ctx   = envCtx γ
    pth   = extendAbsPath (envPath γ) n
    cha   = envCHA γ
    mut   = Nothing -- 'this' gets out of scope when entering a module
    tThis = Nothing
    fnId  = envFnId γ



--------------------------------------------------------------------------------
-- | Environment wrappers
--------------------------------------------------------------------------------

tcEnvAdds xs γ = γ { tce_names = envAdds xs $ tce_names γ }

--------------------------------------------------------------------------------
tcEnvAdd :: (F.Symbolic x, IsLocated x) => x -> SymInfo r -> TCEnv r -> TCEnv r
--------------------------------------------------------------------------------
tcEnvAdd x t γ = γ { tce_names = envAdd x t $ tce_names γ }

--------------------------------------------------------------------------------
tcEnvFindTy :: (Unif r, F.Symbolic x, IsLocated x) => x -> TCEnv r -> Maybe (RType r)
--------------------------------------------------------------------------------
tcEnvFindTy x γ = fmap v_type (tcEnvFindTyWithAgsn x γ)

--------------------------------------------------------------------------------
tcEnvFindTyWithAgsn :: (Unif r, F.Symbolic x) => x -> TCEnv r -> Maybe (SymInfo r)
--------------------------------------------------------------------------------
tcEnvFindTyWithAgsn x γ | Just t <- envFindTy x $ tce_names γ
                        = Just $ adjustInit t
                        | otherwise
                        = Nothing
  where
    adjustInit s@(SI _ _ _ Initialized _) = s
    adjustInit (SI n loc a _ t) = SI n loc a Uninitialized $ orUndef t

-- This is a variant of the above that doesn't add the ' + undefined' for
-- non-initialized variables.
--------------------------------------------------------------------------------
tcEnvFindTyForAsgn    :: (Unif r, F.Symbolic x) => x -> TCEnv r -> Maybe (SymInfo r)
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
    go (BTV _ _ _       ) γ = γ

