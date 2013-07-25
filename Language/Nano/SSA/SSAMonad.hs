{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-------------------------------------------------------------------------------------
-- | SSA Monad ----------------------------------------------------------------------
-------------------------------------------------------------------------------------


module Language.Nano.SSA.SSAMonad (
   
   -- * SSA Information
     SsaInfo (..)
   
   -- * SSA Monad
   , SSAM
   , ssaError
   , execute
 
   -- * SSA Environment
   , SsaEnv
   , setSsaEnv
   , getSsaEnv
   , updSsaEnv 
   , extSsaEnv
   , findSsaEnv
 
   -- * Access Annotations
   , addAnn
   , getAnns

   -- * Immutable Variables 
   , isImmutable
   , getImmutables
   , setImmutables
   , addImmutables
   ) where 

import           Control.Applicative                ((<$>))
import           Control.Monad                
import           Control.Monad.State                
import           Control.Monad.Error

import qualified Data.HashMap.Strict as M 
import           Language.Nano.Errors
import           Language.Nano.Env
import           Language.Nano.Types                (dummySpan)
import           Language.Nano.Typecheck.Types
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Parser        (SourceSpan (..))

import           Language.Fixpoint.Misc             
import           Text.Printf                        (printf)

type SSAM     = ErrorT String (State SsaState)

data SsaState = SsaST { immutables :: Env ()   -- ^ globals
                      , names      :: SsaEnv   -- ^ current SSA names 
                      , count      :: !Int     -- ^ fresh index
                      , anns       :: !AnnInfo -- ^ built up map of annots 
                      }

type SsaEnv     = Env SsaInfo 
newtype SsaInfo = SI (Id SourceSpan) deriving (Eq)

-------------------------------------------------------------------------------------
extSsaEnv    :: [Id SourceSpan] -> SsaEnv -> SsaEnv 
-------------------------------------------------------------------------------------
extSsaEnv xs = envAdds [(x, SI x) | x <- xs]

-------------------------------------------------------------------------------------
getSsaEnv   :: SSAM SsaEnv 
-------------------------------------------------------------------------------------
getSsaEnv   = names <$> get 

-------------------------------------------------------------------------------------
addImmutables   :: Env () -> SSAM () 
-------------------------------------------------------------------------------------
addImmutables z = modify $ \st -> st { immutables = envExt z (immutables st) } 
  where
    envExt x y  = envFromList (envToList x ++ envToList y)

-------------------------------------------------------------------------------------
setImmutables   :: Env () -> SSAM () 
-------------------------------------------------------------------------------------
setImmutables z = modify $ \st -> st { immutables = z } 

-------------------------------------------------------------------------------------
getImmutables   :: SSAM (Env ()) 
-------------------------------------------------------------------------------------
getImmutables   = immutables <$> get




-------------------------------------------------------------------------------------
setSsaEnv    :: SsaEnv -> SSAM () 
-------------------------------------------------------------------------------------
setSsaEnv θ = modify $ \st -> st { names = θ } 


-------------------------------------------------------------------------------------
updSsaEnv   :: SourceSpan -> Id SourceSpan -> SSAM (Id SourceSpan) 
-------------------------------------------------------------------------------------
updSsaEnv l x 
  = do imm   <- isImmutable x 
       when imm $ ssaError l $ errorWriteImmutable x
       n     <- count <$> get
       let x' = newId l x n
       modify $ \st -> st {names = envAdds [(x, SI x')] (names st)} {count = 1 + n}
       return x'


---------------------------------------------------------------------------------
isImmutable   :: Id SourceSpan -> SSAM Bool 
---------------------------------------------------------------------------------
isImmutable x = envMem x . immutables <$> get

newId :: SourceSpan -> Id SourceSpan -> Int -> Id SourceSpan 
newId l (Id _ x) n = Id l (x ++ "_SSA_" ++ show n)  

-------------------------------------------------------------------------------
findSsaEnv   :: Id SourceSpan -> SSAM (Maybe (Id SourceSpan))
-------------------------------------------------------------------------------
findSsaEnv x 
  = do θ  <- names <$> get 
       case envFindTy x θ of 
         Just (SI i) -> return $ Just i 
         Nothing     -> return Nothing 

-- allNames = do xs <- map fst . envToList . names      <$> get
--               ys <- map fst . envToList . immutables <$> get
--               return $ xs ++ ys

-------------------------------------------------------------------------------
addAnn     :: SourceSpan -> Fact -> SSAM ()
-------------------------------------------------------------------------------
addAnn l f = modify $ \st -> st { anns = inserts l f (anns st) }


-------------------------------------------------------------------------------
getAnns    :: SSAM AnnInfo 
-------------------------------------------------------------------------------
getAnns    = anns <$> get


-------------------------------------------------------------------------------
ssaError       :: SourceSpan -> String -> SSAM a
-------------------------------------------------------------------------------
ssaError l msg = throwError $ printf "ERROR at %s : %s" (ppshow l) msg


-- inserts l xs m = M.insert l (xs ++ M.lookupDefault [] l m) m

-------------------------------------------------------------------------------
execute         :: SSAM a -> Either (SourceSpan, String) a 
-------------------------------------------------------------------------------
execute act 
  = case runState (runErrorT act) initState of 
      (Left err, _) -> Left  (dummySpan,  err)
      (Right x, _)  -> Right x

initState :: SsaState
initState = SsaST envEmpty envEmpty 0 M.empty


