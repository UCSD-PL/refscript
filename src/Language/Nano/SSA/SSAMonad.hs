{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-------------------------------------------------------------------------------------
-- | SSA Monad 
-------------------------------------------------------------------------------------


module Language.Nano.SSA.SSAMonad (
   
   -- * SSA Information
     SsaInfo (..)
   
   -- * SSA Monad
   , SSAM
   , ssaError
   , execute
   , tryAction
 
   -- * SSA Environment
   , SsaEnv
   , names
   , updSsaEnv 
   , findSsaEnv
   , extSsaEnv
   , setSsaEnv
   , getSsaEnv
 
   -- * Access Annotations
   , addAnn, getAnns
   , setGlobs, getGlobs
   , setMeas, getMeas

   -- * Tracking Assignability
   , getAssignability
   , withAssignability

   ) where 

import           Control.Applicative                ((<$>))
import           Control.Monad.State                
import           Control.Monad.Trans.Except

import           Data.Maybe                         (fromMaybe) 
import qualified Data.HashMap.Strict as M 
import qualified Data.HashSet as S
import qualified Language.Fixpoint.Types        as F
import           Language.Nano.Errors
import           Language.Nano.Env
import           Language.Nano.Types                
import           Language.Nano.SSA.Types
import           Language.Nano.Typecheck.Types
import           Language.ECMAScript3.PrettyPrint

import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc             

-- import           Debug.Trace                        (trace)

type SSAM r     = ExceptT Error (State (SsaState r))

data SsaState r = SsaST { assign      :: Env Assignability        -- ^ assignability status 
                        -- names to be deprecated
                        , names       :: SsaEnv                   -- ^ current SSA names 
                        
                        , count       :: !Int                     -- ^ fresh index
                        , anns        :: !(AnnInfo r)             -- ^ built up map of annots 
                        
                        , ssa_globs   :: S.HashSet SourceSpan     -- ^ Global definition locations
                        , ssa_meas    :: S.HashSet F.Symbol       -- ^ Measures
                        }

type SsaEnv     = Env SsaInfo 
newtype SsaInfo = SI (Var) deriving (Eq)

instance PP SsaInfo where
  pp (SI i) =  pp i


-- -------------------------------------------------------------------------------------
-- withExtSsaEnv    :: [Var] -> SSAM r a -> SSAM r a
-- -------------------------------------------------------------------------------------
-- withExtSsaEnv xs act 
--   = do θ      <- getSsaEnv 
--        let θ'  = envAdds [(x, SI x) | x <- xs] θ
--        modify  $ \st -> st { names = θ' } 
--        r      <- act
--        modify  $ \st -> st { names = θ }
--        return  r

-------------------------------------------------------------------------------------
extSsaEnv    :: [Var] -> SsaEnv -> SsaEnv 
-------------------------------------------------------------------------------------
extSsaEnv xs = envAdds [(x, SI x) | x <- xs]

-------------------------------------------------------------------------------------
getSsaEnv   :: SSAM r SsaEnv 
-------------------------------------------------------------------------------------
getSsaEnv   = names <$> get 

-------------------------------------------------------------------------------------
setSsaEnv    :: SsaEnv -> SSAM r () 
------------------------------------------------------------------------------------
setSsaEnv θ = modify $ \st -> st { names = θ } 

-------------------------------------------------------------------------------------
withAssignability :: Assignability -> [Var] -> SSAM r a -> SSAM r a 
-------------------------------------------------------------------------------------
withAssignability m xs act
  = do zOld  <- assign <$> get
       modify $ \st -> st { assign = zNew `envUnion` zOld } 
       ret   <- act
       modify $ \st -> st { assign = zOld }
       return $ ret
    where 
       zNew   = envFromList $ (, m) <$> xs 

-------------------------------------------------------------------------------------
getAssignability :: Var -> SSAM r Assignability 
-------------------------------------------------------------------------------------
getAssignability x = fromMaybe WriteLocal . envFindTy x . assign <$> get


-------------------------------------------------------------------------------------
updSsaEnv   :: SourceSpan -> Var -> SSAM r (Var) 
-------------------------------------------------------------------------------------
updSsaEnv l x 
  = do mut <- getAssignability x
       case mut of
         WriteLocal  -> updSsaEnvLocal l x
         WriteGlobal -> return x
         ReadOnly    -> ssaError $ errorWriteImmutable l x 

updSsaEnvLocal l x 
  = do n     <- count <$> get
       let x' = mkSSAId l x n
       modify $ \st -> st {names = envAdds [(x, SI x')] (names st)} {count = 1 + n}
       return x'

-------------------------------------------------------------------------------------
findSsaEnv   :: Var -> SSAM r (Maybe (Var))
-------------------------------------------------------------------------------------
findSsaEnv x 
  = do θ  <- names <$> get 
       case envFindTy x θ of 
         Just (SI i) -> return $ Just i 
         Nothing     -> return $ Nothing 


addAnn l f = modify $ \st -> st { anns = inserts l f (anns st) }
getAnns    = anns <$> get

setGlobs g =  modify $ \st -> st { ssa_globs = g } 
getGlobs   = ssa_globs <$> get

setMeas m =  modify $ \st -> st { ssa_meas= m } 
getMeas   = ssa_meas <$> get


-------------------------------------------------------------------------------------
ssaError :: Error -> SSAM r a
-------------------------------------------------------------------------------------
ssaError = throwE


-------------------------------------------------------------------------------------
execute         :: SSAM r a -> Either Error a 
-------------------------------------------------------------------------------------
execute act 
  = case runState (runExceptT act) initState of 
      (Left err, _) -> Left err
      (Right x, _)  -> Right x

-- Try the action @act@ in the current state. 
-- The state will be intact in the end. Just the result will be returned
tryAction act = get >>= return . runState (runExceptT act)

initState :: SsaState r
initState = SsaST envEmpty envEmpty 0 M.empty S.empty S.empty 

