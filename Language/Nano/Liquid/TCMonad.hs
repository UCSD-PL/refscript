{- LANGUAGE TypeSynonymInstances #-}
{- LANGUAGE FlexibleInstances    #-}
{- LANGUAGE NoMonomorphismRestriction #-}
{- LANGUAGE ScopedTypeVariables  #-}

-- | This module has the code for the Type-Checker Monad. 
--   You /need not/ modify the code here, just use the exported API.

module Language.Nano.Liquid.TCMonad (
  -- * TC Monad
    TCM
  , (>>>=)
 
  -- * Execute 
  , execute

  -- * Throw Errors
  , tcError

  -- * Log Errors
  , logError

  -- * Freshness
  , Freshable (..)
  )  where 

import           Text.Printf
import           Control.Applicative          ((<$>))
import           Control.Monad.State
import           Control.Monad.Error
import           Language.Fixpoint.Misc 
import qualified Language.Fixpoint.Types as F
import           Language.Nano.Types
import           Language.Nano.Liquid.Types
import           Language.Nano.Errors
import           Data.Maybe                   (fromMaybe)
import           Text.Parsec.Pos              

-- import           Text.PrettyPrint.HughesPJ
-- import           Language.Nano.Types

-------------------------------------------------------------------------------
-- | Typechecking monad -------------------------------------------------------
-------------------------------------------------------------------------------

data TCState = TCS { tc_errs :: ![(SourcePos, String)]
                   , tc_cnt  :: !Int
                   }

type TCM     = ErrorT String (State TCState)

-------------------------------------------------------------------------------
tcError :: SourcePos -> String -> TCM a
-------------------------------------------------------------------------------
tcError l msg = throwError $ printf "ERROR at %s : %s" (ppshow l) msg

-------------------------------------------------------------------------------
logError   :: a -> SourcePos -> String -> TCM a
-------------------------------------------------------------------------------
logError x l msg = (modify $ \st -> st { tc_errs = (l,msg):(tc_errs st)}) >> return x

-------------------------------------------------------------------------------
execute     :: TCM a -> Either [(SourcePos, String)] a
-------------------------------------------------------------------------------
execute act = case runState (runErrorT act) (TCS [] 0)of 
                (Left err, _) -> Left [(initialPos "" ,  err)]
                (Right x, st) ->  applyNonNull (Right x) Left (reverse $ tc_errs st)
 
-- execute act = applyNonNull (Right x) Left (reverse $ tc_errs st)
--   where 
--     st0     = TCS [] 0
--     (x, st) = runState act st0

-- instance Show a => Show (ESW a) where 
--   show m = "Log:\n"  ++ log ++ "\n" ++ 
--            "Count: " ++ show cnt ++ "\n" ++
--            result
--     where ((res, cnt), log) = runWriter (runStateT (runErrorT m) 0)
--           result   = case res of 
--                        Left s -> "Error: " ++ s
--                        Right v -> "Value: " ++ show v



(>>>=) :: TCM (Maybe a) -> (a -> TCM ()) -> TCM ()
z >>>= f = z >>= maybe (return ()) f


--------------------------------------------------------------------------
-- | Generating Fresh Values ---------------------------------------------
--------------------------------------------------------------------------

tick :: TCM Int
tick = do st    <- get 
          let n  = tc_cnt st
          put    $ st { tc_cnt = n + 1 }
          return n 

class Freshable a where 
  fresh :: a -> TCM a 

instance Freshable TVar where 
  fresh _ = TV . F.intSymbol "T" <$> tick

instance Freshable a => Freshable [a] where 
  fresh = mapM fresh
