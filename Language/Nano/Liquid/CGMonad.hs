-- | Operations pertaining to Constraint Generation

module Language.Nano.Liquid.CGMonad (
    
  -- * Constraint Generation Monad
    CGM (..)

  -- * Execute Action and Get FInfo
  , getFInfo 

  -- * Throw Errors
  , cgError      

  -- * Fresh Templates for Unknown Refinement Types 
  , freshTyInst
  , freshTyPhis

  -- * Environment API
  , envAddFresh
  , envAdds
  , envAddReturn
  , envAddGuard
  , envFindTy
  , envFindReturn

  -- * Add Subtyping Constraints
  , subTypes
  , subType 
  ) where

import           Data.Maybe             (isJust)
import           Data.Monoid            hiding ((<>))            

import qualified Data.List               as L
import qualified Data.HashMap.Strict     as M

import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Misc
import           Language.Fixpoint.PrettyPrint
import           Text.PrettyPrint.HughesPJ

import           Language.Nano.Types
import           Language.Nano.Errors
import           Language.Nano.Typecheck.Types 
import           Language.Nano.Liquid.Types

import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Misc
import           Language.Fixpoint.PrettyPrint
import           Control.Applicative 

import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Error
import           Text.Printf 

import           Language.ECMAScript3.Syntax


-------------------------------------------------------------------------------
getFInfo :: NanoRefType -> CGM a -> F.FInfo Cinfo  
-------------------------------------------------------------------------------
getFInfo = error "TOBD"


-- getFInfo pgm act 
--   = case runState (runErrorT act) (initState pgm) of 
--       (Left err, _) -> errorstar err
--       (Right x, st) -> 
--       applyNonNull (Right x) Left (reverse $ tc_errs st)
--     
-- 
-- cgStateFInfo ((cs', ws'), cg)  
--   = F.FI { F.cm    = M.fromList $ F.addIds cs'  
--          , F.ws    = ws'
--          , F.bs    = binds cg
--          , F.gs    = builtinMeasureSEnv
--          , F.lits  = []
--          , F.kuts  = F.ksEmpty
--          , F.quals = [] }
-- 
-- getFixCs 
--   = do cs'   <- cs    <$> get
--        γbs   <- bbγs  <$> get
--        em    <- edgem <$> get 
--        ccs'  <- closeSubC em γbs cs'
--        fcs'  <- concatMapM splitC ccs' 
--        return $ fcs'
-- 
-- getFixWs
--   = do ws'   <- ws <$> get
--        fws'  <- concatMapM splitW ws'
--        return $ fws'
-- 
-- 
-- finState act  = do act
--                    fcs'  <- getFixCs
--                    fws'  <- getFixWs
--                    return $ (fcs', fws') 
-- 
-- initState pgm = error "TOBD"
-- 
-- cgStateFInfo ((cs', ws'), cg)  
--   = F.FI { F.cm    = M.fromList $ F.addIds cs'  
--          , F.ws    = ws'
--          , F.bs    = binds cg
--          , F.gs    = builtinMeasureSEnv
--          , F.lits  = []
--          , F.kuts  = F.ksEmpty
--          , F.quals = [] }



---------------------------------------------------------------------------------------
-- | Constraint Generation Monad ------------------------------------------------------
---------------------------------------------------------------------------------------

data CGState = CGS { cg_index :: !Int } 

type CGM     = ErrorT String (State CGState)

---------------------------------------------------------------------------------------
cgError :: (IsLocated l) => l -> String -> CGM a 
---------------------------------------------------------------------------------------
cgError l msg = throwError $ printf "CG-ERROR at %s : %s" (ppshow $ srcPos l) msg


---------------------------------------------------------------------------------------
-- | Environment API ------------------------------------------------------------------
---------------------------------------------------------------------------------------

envAddFresh  :: (IsLocated l) => l -> RefType -> CGEnv -> CGM (Id AnnType, CGEnv) 
envAddFresh  = error "TOBD" 

envAdds      :: (F.Symbolic x, IsLocated x) => [(x, RefType)] -> CGEnv -> CGM CGEnv
envAdds      = error "TOBD"
envAddReturn :: (IsLocated f)  => f -> RefType -> CGEnv -> CGM CGEnv 
envAddReturn = error "TOBD"

envAddGuard     :: (F.Symbolic x, IsLocated x) => x -> Bool -> CGEnv -> CGM CGEnv  
envAddGuard     = error "TOBD"
  where 
    guard True  = F.eProp 
    guard False = F.PNot . F.eProp



envFindTy     :: (F.Symbolic x) => x -> CGEnv -> RefType 
envFindTy     = error "TOBD"
envFindReturn :: CGEnv -> RefType 
envFindReturn = error "TOBD"

---------------------------------------------------------------------------------------
-- | Fresh Templates ------------------------------------------------------------------
---------------------------------------------------------------------------------------

-- | Instantiate Fresh Type (at Call-site)
freshTyInst :: CGEnv -> [TVar] -> [Type] -> RefType -> CGM RefType 
freshTyInst = error "TOBD"

-- | Instantiate Fresh Type (at Phi-site) 
freshTyPhis :: (IsLocated l) => CGEnv -> [(Id l, Type)] -> CGM (CGEnv, [RefType])  
freshTyPhis = error "TOBD"

---------------------------------------------------------------------------------------
-- | Subtyping Constraints ------------------------------------------------------------
---------------------------------------------------------------------------------------

subTypes :: (IsLocated l) => l -> CGEnv -> [RefType] -> [RefType] -> CGM F.Subst 
subTypes = error "TOBD"  

subType :: (IsLocated l) => l -> CGEnv -> RefType -> RefType -> CGM ()
subType l g t1 t2 = subTypes l g [t1] [t2] >> return ()
