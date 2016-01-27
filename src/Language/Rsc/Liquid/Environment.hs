{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

module Language.Rsc.Liquid.Environment where

import           Data.Maybe                     (catMaybes)
import qualified Language.Fixpoint.Types        as F
import           Language.Fixpoint.Types.Errors
import           Language.Rsc.Annotations       ()
import           Language.Rsc.ClassHierarchy
import           Language.Rsc.Core.Env
import           Language.Rsc.Environment
import           Language.Rsc.Errors
import           Language.Rsc.Liquid.Types
import           Language.Rsc.Locations
import           Language.Rsc.Names
import           Language.Rsc.Pretty
import           Language.Rsc.Symbols
import           Language.Rsc.Traversals
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
import           Text.PrettyPrint.HughesPJ

-------------------------------------------------------------------------------------
-- | Constraint Generation Environment
-------------------------------------------------------------------------------------

data CGEnvR r = CGE {
    cge_names  :: !(Env (SymInfo r))
  , cge_bounds :: !(Env (RType r))
  , cge_ctx    :: !IContext
  , cge_path   :: !AbsPath
  , cge_cha    :: !(ClassHierarchy r)
  , cge_fenv   :: !(F.SEnv F.IBindEnv)          -- Fixpoint bindings - XXX: Why not in monad? Remove?
  , cge_guards :: ![F.Pred]                     -- Branch target conditions
  , cge_consts :: !(Env (RType r))              -- Constants
  , cge_mut    :: !(Maybe (MutabilityR r))      -- Method mutability
  , cge_this   :: !(Maybe (RType r))            -- Method mutability
  , cge_fnid   :: !Int                          -- Enclosing fun's id
  } deriving (Functor)

type CGEnv = CGEnvR F.Reft

type CGEnvEntry = SymInfo F.Reft

instance CheckingEnvironment r CGEnvR where
  envNames  = cge_names
  envBounds = cge_bounds
  envCHA    = cge_cha
  envPath   = cge_path
  envCtx    = cge_ctx
  envMut    = cge_mut
  envThis   = cge_this
  envFnId   = cge_fnid

type EnvKey x = (IsLocated x, F.Symbolic x, PP x, F.Expression x)


-- Only include the "singleton" refinement in the case where Assignability is
-- either ReadOnly of WriteLocal (SSAed)
---------------------------------------------------------------------------------------
envFindTyWithAsgn :: (EnvKey x, F.Expression x) => x -> CGEnv -> Maybe CGEnvEntry
---------------------------------------------------------------------------------------
envFindTyWithAsgn x (envNames -> γ) = fmap singleton (envFindTy x γ)
  where
    singleton v@(SI _ WriteGlobal Initialized   _) = v
    singleton v@(SI _ WriteGlobal Uninitialized t) = v { v_type = orUndef t }
    singleton v = v { v_type = eSingleton (v_type v) x }

---------------------------------------------------------------------------------------
cgEnvFindReturn :: CGEnv -> RefType
cgEnvFindTy     :: F.Symbolic x => x -> CGEnvR r -> Maybe (RTypeQ AK r)
---------------------------------------------------------------------------------------
cgEnvFindReturn = v_type . envFindReturn . cge_names
cgEnvFindTy x   = fmap v_type . envFindTy x . cge_names


---------------------------------------------------------------------------------------
-- | Well-Formedness
---------------------------------------------------------------------------------------

-- | Valid symbols:
--
--   * The respective defined symbol @x@
--   * The value variable (v)
--   * Explicitly acceptable symbols (@ok@)
--   * Internal binders
--   * Constant measures
--   * Additional builtin symbols
--   * ReadOnly ... binders in the environment
--
---------------------------------------------------------------------------------------
checkSyms :: (IsLocated l, EnvKey a) => l -> String -> CGEnv -> [a] -> a -> RefType -> [Error]
---------------------------------------------------------------------------------------
checkSyms l m g ok x t = efoldRType h f F.emptySEnv [] t
  where
    h _        = ()
    f γ t' s   = let rt   = rTypeReft t'   in
                 let noKv = noKVars   rt   in
                 let ss   = F.syms    noKv in
                 s ++ catMaybes (fmap (chk γ t') ss)

    chk γ t' s | s `elem` biReserved
               = Just $ unimplementedReservedSyms l
               | s == x_sym
               = Nothing
               | s == rTypeValueVar t'
               = Nothing
               | s `elem` ok_syms
               = Nothing
               | s `F.memberSEnv` γ
               = Nothing
               | s `F.memberSEnv` cge_consts g
               = Nothing
               | s `elem` biExtra
               = Nothing
               | Just (SI _ a _ _) <- chkEnvFindTy' s g
               = if a `elem` validAsgn
                   then Nothing
                   else Just $ errorAsgnInRef l s t a
               | otherwise
               = Just $ errorUnboundSyms l (F.symbol x) t s m

    biReserved = map F.symbol ["func", "obj"]
    biExtra    = map F.symbol ["bvor", "bvand", "builtin_BINumArgs", "offset", "this"]
    x_sym      = F.symbol x
    ok_syms    = map F.symbol ok
    validAsgn  = [RdOnly, Ambient, WriteLocal]

    -- errMsg     = show $ pp "type" $+$ nest 4 (pp t') $+$
    --                                   pp "rt"   $+$ nest 4 (pp rt) $+$
    --                                   pp "noKV" $+$ nest 4 (pp noKv) $+$
    --                                   pp "syms" $+$ nest 4 (pp s) $+$
    --                                   pp "errors"


-------------------------------------------------------------------------------
initClassInstanceEnv :: TypeSig F.Reft -> CGEnv -> CGEnv
-------------------------------------------------------------------------------
initClassInstanceEnv sig@(TS _ (BGen nm bs) _) γ =
  γ { cge_bounds = envAdds bts (cge_bounds γ) }
  where
    bts = [(s,t) | BTV s _ (Just t) <- bs]

-- Initializes mutability and type of 'this'
-------------------------------------------------------------------------------
initClassCtorEnv :: TypeSig F.Reft -> CGEnv -> CGEnv
-------------------------------------------------------------------------------
initClassCtorEnv (TS _ (BGen nm bs) _) γ
  = γ { cge_mut   = Just tAF
      , cge_this  = Just $ TRef (Gen nm (map btVar bs)) fTop }

-------------------------------------------------------------------------------
initClassMethEnv :: Mutability -> TypeSig F.Reft -> CGEnv -> CGEnv
-------------------------------------------------------------------------------
initClassMethEnv m (TS _ (BGen nm bs) _) γ
  = γ { cge_mut  = Just m
      , cge_this = Just tThis
      }
  where
    tThis = TRef (Gen nm (map btVar bs)) fTop


