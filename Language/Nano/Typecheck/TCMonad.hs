{- LANGUAGE TypeSynonymInstances #-}
{- LANGUAGE FlexibleInstances    #-}
{- LANGUAGE NoMonomorphismRestriction #-}
{- LANGUAGE ScopedTypeVariables  #-}

-- | This module has the code for the Type-Checker Monad. 
--   You /need not/ modify the code here, just use the exported API.

module Language.Nano.Typecheck.TCMonad (
  -- * TC Monad
    TCM
 
  -- * Execute 
  , execute

  -- * Log Errors
  , logError
  
  -- * Error Action
  , tcError

  -- * Freshness
  , freshTyArgs

  -- * Substitutions
  , getSubst, setSubst

  -- * Annotations
  , accumAnn
  , getAllAnns

  -- * Unification
  , unifyType
  , unifyTypes

  -- * Get Type Signature 
  , getDefType 
  )  where 

import           Text.Printf
import           Control.Applicative          ((<$>))
import           Control.Monad.State
import           Control.Monad.Error
import           Language.Fixpoint.Misc 
import qualified Language.Fixpoint.Types as F

import           Language.Nano.Env
import           Language.Nano.Types
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Errors
import           Data.Monoid                  
import qualified Data.HashMap.Strict     as M
import           Text.Parsec.Pos              
import           Language.ECMAScript3.Parser        (SourceSpan (..))

-------------------------------------------------------------------------------
-- | Typechecking monad -------------------------------------------------------
-------------------------------------------------------------------------------

data TCState = TCS { tc_errs  :: ![(SourceSpan, String)]
                   , tc_subst :: !Subst
                   , tc_cnt   :: !Int
                   , tc_anns  :: AnnInfo
                   , tc_annss :: [AnnInfo]
                   , tc_defs  :: !(Env Type) 
                   }

type TCM     = ErrorT String (State TCState)

-------------------------------------------------------------------------------
getSubst :: TCM Subst
-------------------------------------------------------------------------------
getSubst = tc_subst <$> get 

-------------------------------------------------------------------------------
setSubst   :: Subst -> TCM () 
-------------------------------------------------------------------------------
setSubst θ = modify $ \st -> st { tc_subst = θ }

-------------------------------------------------------------------------------
extSubst :: [TVar] -> TCM ()
-------------------------------------------------------------------------------
extSubst βs = getSubst >>= setSubst . (`mappend` θ')
  where 
    θ'      = fromList $ zip βs (tVar <$> βs)


-------------------------------------------------------------------------------
tcError :: (IsLocated l) => l -> String -> TCM a
-------------------------------------------------------------------------------
tcError l msg = throwError $ printf "TC-ERROR at %s : %s" (ppshow $ srcPos l) msg


-------------------------------------------------------------------------------
logError   :: a -> SourceSpan -> String -> TCM a
-------------------------------------------------------------------------------
logError x l msg = (modify $ \st -> st { tc_errs = (l,msg):(tc_errs st)}) >> return x


-------------------------------------------------------------------------------
freshTyArgs :: SourceSpan -> ([TVar], Type) -> TCM Type 
-------------------------------------------------------------------------------
freshTyArgs l (αs, t) 
  = (`apply` t) <$> freshSubst l αs

freshSubst :: SourceSpan -> [TVar] -> TCM Subst
freshSubst l αs
  = do βs <- mapM (freshTVar l) αs
       setTyArgs l βs
       extSubst βs 
       return $ fromList $ zip αs (tVar <$> βs)

setTyArgs l βs 
  = do m <- tc_anns <$> get 
       when (M.member l m) $ tcError l "Multiple Type Args"
       addAnn l $ TypInst (tVar <$> βs)



-------------------------------------------------------------------------------
-- | Managing Annotations: Type Instantiations --------------------------------
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
getAnns :: TCM AnnInfo  
-------------------------------------------------------------------------------
getAnns = do θ     <- tc_subst <$> get
             m     <- tc_anns  <$> get
             let m' = fmap (apply θ . sortNub) m
             _     <- modify $ \st -> st { tc_anns = m' }
             return m' 

-------------------------------------------------------------------------------
addAnn :: SourceSpan -> Fact -> TCM () 
-------------------------------------------------------------------------------
addAnn l f = modify $ \st -> st { tc_anns = inserts l f (tc_anns st) } 

-------------------------------------------------------------------------------
getAllAnns :: TCM [AnnInfo]  
-------------------------------------------------------------------------------
getAllAnns = tc_annss <$> get


-------------------------------------------------------------------------------
accumAnn :: (AnnInfo -> [(SourceSpan, String)]) -> TCM () -> TCM ()
-------------------------------------------------------------------------------
accumAnn check act 
  = do m     <- tc_anns <$> get 
       modify $ \st -> st {tc_anns = M.empty}
       act
       m'    <- getAnns
       forM_ (check m') $ \(l, s) -> tcError l s 
       modify $ \st -> st {tc_anns = m} {tc_annss = m' : tc_annss st}

-------------------------------------------------------------------------------
execute     :: Nano z (RType r) -> TCM a -> Either [(SourceSpan, String)] a
-------------------------------------------------------------------------------
execute pgm act 
  = case runState (runErrorT act) $ initState pgm of 
      (Left err, _) -> Left [(dummySpan,  err)]
      (Right x, st) ->  applyNonNull (Right x) Left (reverse $ tc_errs st)

initState :: Nano z (RType r) -> TCState
initState pgm = TCS [] mempty 0 M.empty [] (envMap toType $ defs pgm) 


getDefType f 
  = do m <- tc_defs <$> get
       maybe err return $ envFindTy f m 
    where 
       err = tcError l $ errorMissingSpec l f
       l   = srcPos f


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

-- instance Freshable TVar where 
--   fresh _ = TV . F.intSymbol "T" <$> tick

instance Freshable a => Freshable [a] where 
  fresh = mapM fresh

freshTVar l _ =  ((`TV` l). F.intSymbol "T") <$> tick
              

----------------------------------------------------------------------------------
unifyTypes :: (IsLocated l) => l -> String -> [Type] -> [Type] -> TCM Subst
----------------------------------------------------------------------------------
unifyTypes l msg t1s t2s
  | length t1s /= length t2s = tcError l errorArgMismatch 
  | otherwise                = do θ <- getSubst 
                                  case unifys θ t1s t2s of
                                    Left msg' -> tcError l $ msg ++ msg'
                                    Right θ'  -> setSubst θ' >> return θ' 

unifyType l m e t t' = unifyTypes l msg [t] [t'] >> return ()
  where 
    msg              = errorWrongType m e t t'


