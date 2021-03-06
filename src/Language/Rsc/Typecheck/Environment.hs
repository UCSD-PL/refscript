{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Rsc.Typecheck.Environment (

    -- * Create env
      initModuleEnv
    , initGlobalEnv
    , initClassCtorEnv
    , initClassMethEnv
    , initCallableEnv

    -- * Search env
    , tcEnvFindTy
    , tcEnvFindSymInfo
    , tcEnvFindReturn
    , tcEnvAdd
    , tcEnvAdds
    , tcEnvAddBounds

    , Unif
    ) where

import           Language.Fixpoint.Misc         (safeZip)
import qualified Language.Fixpoint.Types        as F
import           Language.Rsc.Annotations
import           Language.Rsc.AST
import           Language.Rsc.ClassHierarchy
import           Language.Rsc.Core.Env
import           Language.Rsc.Environment
import           Language.Rsc.Locations
import           Language.Rsc.Names
import           Language.Rsc.Pretty
import           Language.Rsc.Program
import           Language.Rsc.Symbols
import           Language.Rsc.Typecheck.TCMonad
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Typecheck.Unify   (Unif)
import           Language.Rsc.Types

-- import           Debug.Trace


--------------------------------------------------------------------------------
-- | Environment initialization
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
initGlobalEnv :: Unif r => TcRsc r -> ClassHierarchy r -> TCM r (TCEnv r)
--------------------------------------------------------------------------------
initGlobalEnv (Rsc { code = Src ss }) cha = do
    nms   <- either fatal return (symEnv ss)
    return $ TCE nms bnds ctx pth cha mut tThis fId
  where
    bnds  = mempty
    ctx   = emptyContext
    pth   = emptyPath
    mut   = Nothing
    tThis = Nothing
    fId   = (-1)

-- This will be called *last* on every contructor, method, function.
-- It is transparent to the incoming environment's: path, cha, mut, this
--
-- TODO: Shadow `this` binding (in case this is a class context)
--
--------------------------------------------------------------------------------
initCallableEnv :: (PP f, IsLocated f, Unif r)
                => AnnTc r -> TCEnv r -> f
                -> IOverloadSig r
                -> [Id (AnnTc r)]
                -> [Statement (AnnTc r)]
                -> TCM r (TCEnv r)
--------------------------------------------------------------------------------
initCallableEnv l γ f fty xs s = do
    locs      <- either fatal return (symEnv s)
    let nms1   = locs `mappend` nms0  -- favors first
    let nms    = envAddReturn f siRet nms1

    return     $ tcEnvAdds arg
               $ tcEnvAdds varBs
               $ tcEnvAdds tyBs
               $ TCE nms bnds ctx pth cha mut tThis (fId l)
  where

    (i, sig)   = fty
    (bs,xts,t) = sig

    nms0       = toFgn (envNames γ)
    siRet      = SI rSym Local ReturnVar t

    rSym       = returnSymbol
    tyBs       = [lsia α | α <- αs]
    varBs      = [(x, siw x t) | (x, t) <- safeZip "initCallableEnv" xs ts]
    arg        = [(getArgId (srcPos l), mkArgumentsSI l ts)]
    bnds       = envAdds [(s,t) | BTV s _ (Just t) <- bs] $ envBounds γ
    ctx        = pushContext i (envCtx γ)
    pth        = envPath γ
    cha        = envCHA γ
    mut        = envMut γ
    tThis      = envThis γ
    ts         = map b_type xts
    αs         = map btvToTV bs

    lsia x     = (Loc (srcPos l) x, sia x (tVar x))
    sia  x t   = SI (F.symbol x) Local Ambient    t
    siw  x t   = SI (F.symbol x) Local WriteLocal t


-- | `initClassCtorEnv` makes `this` a Unique binding
--------------------------------------------------------------------------------
initClassCtorEnv :: Unif r => TypeSigQ AK r -> TCEnv r -> TCEnv r
--------------------------------------------------------------------------------
initClassCtorEnv (TS _ (BGen nm bs) _) γ = tcEnvAdd eThis γ'
  where
    γ'    = γ { tce_mut    = Just tUQ
              , tce_this   = Just tThis
              , tce_bounds = envAdds bts (tce_bounds γ)
              }
    bts   = [(s,t) | BTV s _ (Just t) <- bs]
    tThis = adjUQ (TRef (Gen nm (map btVar bs)) fTop)
    eThis = SI thisSym Local RdOnly tThis
    adjUQ (TRef (Gen n (_:ps)) r) = TRef (Gen n (tUQ:ps)) r
    adjUQ t                       = t


--------------------------------------------------------------------------------
initClassMethEnv :: Unif r => MutabilityR r -> TypeSig r -> TCEnv r -> TCEnv r
--------------------------------------------------------------------------------
initClassMethEnv m (TS _ (BGen nm bs) _) γ = tcEnvAdd eThis γ'
  where
    γ'    = γ { tce_bounds = envAdds bts (tce_bounds γ)
              , tce_mut    = Just m
              }
    bts   = [(s,t) | BTV s _ (Just t) <- bs]
    tThis = adjMut $ TRef (Gen nm (map btVar bs)) fTop
    eThis = SI thisSym Local RdOnly tThis

    adjMut (TRef (Gen n (_:ps)) r) = TRef (Gen n (m:ps)) r
    adjMut t                       = t


--------------------------------------------------------------------------------
initModuleEnv :: (Unif r, F.Symbolic n, PP n)
              => TCEnv r -> n -> [Statement (AnnTc r)] -> TCM r (TCEnv r)
--------------------------------------------------------------------------------
initModuleEnv γ n s = do
    nms1    <- either fatal return (symEnv s)
    let nms  = nms1 `mappend` nms0
    return   $ TCE nms bnds ctx pth cha mut tThis fnId
  where
    nms0     = toFgn (envNames γ)
    bnds     = envBounds γ
    ctx      = envCtx γ
    pth      = extendAbsPath (envPath γ) n
    cha      = envCHA γ
    mut      = Nothing -- 'this' gets out of scope when entering a module
    tThis    = Nothing
    fnId     = envFnId γ



--------------------------------------------------------------------------------
-- | Environment wrappers
--------------------------------------------------------------------------------

tcEnvAdds xs γ = γ { tce_names = envAdds xs $ tce_names γ }

--------------------------------------------------------------------------------
tcEnvAdd :: SymInfo r -> TCEnv r -> TCEnv r
--------------------------------------------------------------------------------
tcEnvAdd s γ = γ { tce_names = envAdd (F.symbol s) s $ tce_names γ }

--------------------------------------------------------------------------------
tcEnvFindTy :: (Unif r, F.Symbolic x, IsLocated x) => x -> TCEnv r -> Maybe (RType r)
--------------------------------------------------------------------------------
tcEnvFindTy x γ = fmap v_type (tcEnvFindSymInfo x γ)

--------------------------------------------------------------------------------
tcEnvFindSymInfo :: (Unif r, F.Symbolic x) => x -> TCEnv r -> Maybe (SymInfo r)
--------------------------------------------------------------------------------
tcEnvFindSymInfo x γ = envFindTy x (tce_names γ)

tcEnvFindReturn = v_type . envFindReturn . tce_names

--------------------------------------------------------------------------------
tcEnvAddBounds :: [BTVar r] -> TCEnv r -> TCEnv r
--------------------------------------------------------------------------------
tcEnvAddBounds = flip $ foldr go
  where
    go (BTV α _ (Just t)) γ = γ { tce_bounds = envAdd α t $ tce_bounds γ }
    go (BTV _ _ _       ) γ = γ

