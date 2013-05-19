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

import           Data.Maybe             (fromMaybe, isJust)
import           Data.Monoid            hiding ((<>))            

import qualified Data.List               as L
import qualified Data.HashMap.Strict     as M

import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Misc
import           Language.Fixpoint.PrettyPrint
import           Text.PrettyPrint.HughesPJ

import           Language.Nano.Types
import           Language.Nano.Errors
import qualified Language.Nano.Env       as E
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

data CGState 
  = CGS { binds :: F.BindEnv  -- ^ global list of fixpoint binders
        , cs    :: ![SubC]    -- ^ subtyping constraints
        , ws    :: ![WfC]     -- ^ well-formedness constraints
        , count :: !Integer   -- ^ freshness counter
        }


type CGM     = ErrorT String (State CGState)

---------------------------------------------------------------------------------------
cgError :: (IsLocated l) => l -> String -> CGM a 
---------------------------------------------------------------------------------------
cgError l msg = throwError $ printf "CG-ERROR at %s : %s" (ppshow $ srcPos l) msg


---------------------------------------------------------------------------------------
-- | Environment API ------------------------------------------------------------------
---------------------------------------------------------------------------------------

envAddFresh       :: (IsLocated l) => l -> RefType -> CGEnv -> CGM (Id l, CGEnv) 
envAddFresh l t g 
  = do x  <- freshId l
       g' <- envAdds [(x, t)] g
       return (x, g')

envAdds      :: (F.Symbolic x, IsLocated x) => [(x, RefType)] -> CGEnv -> CGM CGEnv
envAdds xts g
  = do is    <- mapM addFixpointBind xts
       return $ g { renv = E.envAdds xts        (renv g) } 
                  { fenv = F.insertsIBindEnv is (fenv g) }

addFixpointBind :: (F.Symbolic x) => (x, RefType) -> CGM F.BindId
addFixpointBind (x, t) 
  = do let s     = F.symbol x
       let r     = rTypeSortedReft t
       (i, bs') <- F.insertBindEnv s r . binds <$> get 
       modify    $ \st -> st { binds = bs' }
       return i 

envAddReturn        :: (IsLocated f)  => f -> RefType -> CGEnv -> CGEnv 
envAddReturn f t g  = g { renv = E.envAddReturn f t (renv g) } 

envAddGuard       :: (F.Symbolic x, IsLocated x) => x -> Bool -> CGEnv -> CGEnv  
envAddGuard x b g = g { guards = guard b x : guards g }
  where 
    guard True    = F.eProp 
    guard False   = F.PNot . F.eProp

envFindTy     :: (IsLocated x, F.Symbolic x) => x -> CGEnv -> RefType 
envFindTy x g = fromMaybe err $ E.envFindTy x $ renv g
  where 
    err       = errorstar $ bugUnboundVariable (srcPos x) (F.symbol x)

envFindReturn :: CGEnv -> RefType 
envFindReturn = E.envFindReturn . renv

---------------------------------------------------------------------------------------
-- | Fresh Templates ------------------------------------------------------------------
---------------------------------------------------------------------------------------

-- | Instantiate Fresh Type (at Call-site)
freshTyInst :: CGEnv -> [TVar] -> [Type] -> RefType -> CGM RefType 
freshTyInst g αs τs t = error "TOBD"

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

---------------------------------------------------------------------------------------
-- | Generating Fresh Values ----------------------------------------------------------
---------------------------------------------------------------------------------------

class Freshable a where
  fresh   :: CGM a
  true    :: a -> CGM a
  true    = return . id
  refresh :: a -> CGM a
  refresh = return . id

instance Freshable Integer where
  fresh = do modify $ \st -> st { count = 1 + (count st) }
             count <$> get 

instance Freshable F.Symbol where
  fresh = F.tempSymbol "nano" <$> fresh

instance Freshable String where
  fresh = F.symbolString <$> fresh

freshId   :: (IsLocated l) => l -> CGM (Id l)
freshId l = Id l <$> fresh


instance Freshable F.Refa where
  fresh = (`F.RKvar` F.emptySubst) <$> (F.intKvar <$> fresh)

instance Freshable [F.Refa] where
  fresh = single <$> fresh

instance Freshable F.Reft where
  fresh                  = errorstar "fresh Reft"
  true    (F.Reft (v,_)) = return $ F.Reft (v, []) 
  refresh (F.Reft (_,_)) = curry F.Reft <$> freshVV <*> fresh
    where freshVV        = F.vv . Just  <$> fresh

instance Freshable F.SortedReft where
  fresh                  = errorstar "fresh Reft"
  true    (F.RR so r)    = F.RR so <$> true r 
  refresh (F.RR so r)    = F.RR so <$> refresh r

instance Freshable RefType where
  fresh   = errorstar "fresh RefType"
  refresh = refreshRefType
  true    = trueRefType 

trueRefType    :: RefType -> CGM RefType
trueRefType    = mapReftM true

refreshRefType :: RefType -> CGM RefType
refreshRefType = mapReftM refresh

 
