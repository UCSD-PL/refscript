{- LANGUAGE TypeSynonymInstances #-}
{- LANGUAGE FlexibleInstances    #-}
{- LANGUAGE ScopedTypeVariables  #-}

-- | This module has the code for the VC-Generation Monad. 
--   You /need not/ modify the code here, just use the exported API.

module Language.Nano.ESC.VCMonad (
  -- * VC Monad
    VCM
 
  -- * Execute 
  , execute

  -- * Side Conditions
  , addSideCond
  , getSideCond

  -- * Set/Lookup Current Function
  , setFunction
  , getFunctionPostcond

  -- * Lookup Specification for Callee
  , getCalleeSpec 

  -- * Generate a fresh Id
  , freshId 

  )  where 

import qualified Data.HashMap.Strict as M
import           Control.Applicative          ((<$>))
import           Control.Monad.State
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Parser.Type (SourceSpan (..))
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Misc 
-- import           Language.Nano.Types
import           Language.Nano.ESC.Types
import           Data.Monoid
import           Data.Maybe                   (fromMaybe)

--------------------------------------------------------------------
-- | `VCM` is a VCGen monad that logs the loop-inv "side conditions" 
--------------------------------------------------------------------

data VCState = VCS { vc_vc    :: !VCond 
                   , vc_spec  :: M.HashMap String (Fun SourceSpan)  
                   , vc_count :: !Int 
                   , vc_fun   :: Maybe (Fun SourceSpan) 
                   }

type VCM     = State VCState



-------------------------------------------------------------------
execute :: Nano -> VCM a -> a
-------------------------------------------------------------------
execute pgm act   = fst $ runState act init 
  where 
    init          = VCS mempty sp 0 Nothing
    sp            = M.fromList [(name $ fname f, f) | f <- pgm]
    name (Id _ x) = x


-------------------------------------------------------------------
addSideCond     :: VCond -> VCM ()
-------------------------------------------------------------------
addSideCond vc' = modify $ \st -> st { vc_vc = mappend vc' (vc_vc st) }


-------------------------------------------------------------------
getSideCond :: VCM VCond
-------------------------------------------------------------------
getSideCond = vc_vc <$> get


-------------------------------------------------------------------
setFunction :: Fun SourceSpan -> VCM () 
-------------------------------------------------------------------
setFunction fn = modify $ \st -> st { vc_fun = Just fn } 
                                    { vc_vc  = mempty }


-------------------------------------------------------------------
getFunctionPostcond :: VCM F.Pred
-------------------------------------------------------------------
getFunctionPostcond = maybe err fpost . vc_fun <$> get
  where 
    err             = errorstar "No Current Function: getPostcond"

-------------------------------------------------------------------
freshId      :: a -> VCM (Id a)
-------------------------------------------------------------------
freshId l    = do st    <- get 
                  put    $ st { vc_count = 1 + (vc_count st) } 
                  return $ Id l $ "nanoTmp" ++ show (vc_count st)

-------------------------------------------------------------------
getCalleeSpec :: String -> VCM (Fun SourceSpan)
-------------------------------------------------------------------
getCalleeSpec f = (fromMaybe err . M.lookup f . vc_spec) <$> get
  where 
    err         = errorstar $ "Unknown Function:" ++ f 


