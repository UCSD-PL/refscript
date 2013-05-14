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

  -- * Access return type
  -- , setReturn
  -- , getReturn

  -- * Log Errors
  , logError

  )  where 

import           Control.Applicative          ((<$>))
import           Control.Monad.State
import           Language.Fixpoint.Misc 
import qualified Language.Fixpoint.Types as F
import           Language.Nano.Types
import           Language.Nano.Liquid.Types
import           Data.Maybe                   (fromMaybe)
import           Language.ECMAScript3.Syntax  (SourcePos)

-- import           Text.PrettyPrint.HughesPJ
-- import           Language.Nano.Types

-------------------------------------------------------------------------------
-- | Typechecking monad -------------------------------------------------------
-------------------------------------------------------------------------------

data TCState = TCS { tc_errs :: ![(SourcePos, String)]
                   , tc_ret  :: !(Maybe Type) 
                   , tc_cnt  :: !Int
                   }

type TCM     = State TCState 

-- -------------------------------------------------------------------------------
-- setReturn :: Type -> TCM ()
-- -------------------------------------------------------------------------------
-- setReturn t = modify $ \st -> st { tc_ret = Just t }
-- 
-- -------------------------------------------------------------------------------
-- getReturn :: TCM Type 
-- -------------------------------------------------------------------------------
-- getReturn = fromMaybe tErr . tc_ret <$> get  

-------------------------------------------------------------------------------
logError   :: a -> SourcePos -> String -> TCM a
-------------------------------------------------------------------------------
logError x l msg 
  = do modify $ \st -> st { tc_errs = (l, msg) : (tc_errs st) } 
       return x

-------------------------------------------------------------------------------
execute     :: TCM a -> Either [(SourcePos, String)] a
-------------------------------------------------------------------------------
execute act = applyNonNull (Right x) Left (reverse $ tc_errs st)
  where 
    st0     = TCS [] Nothing 0
    (x, st) = runState act st0


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
  fresh (TV (Loc l _)) = TV . Loc l . F.intSymbol "T" <$> tick

