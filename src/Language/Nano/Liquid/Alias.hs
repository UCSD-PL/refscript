module Language.Nano.Liquid.Alias (expandAliases) where

import           Data.Maybe
import           Data.Generics.Aliases
import           Data.Generics.Schemes

import           Control.Applicative ((<$>))
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Error hiding (Error)
import           Text.Printf 

import           Language.Fixpoint.Errors
import qualified Language.Fixpoint.Types as F

import           Language.Nano.Env
import           Language.Nano.Errors
import           Language.Nano.Types
import           Language.Nano.Typecheck.Types
import           Language.Nano.Liquid.Types

expandAliases :: NanoRefType -> NanoRefType
expandAliases = undefined

expandPAliasEnv :: PAliasEnv -> PAliasEnv 
expandPAliasEnv pe = solve pe support expandPAlias 
  where
    support        = filter isAlias . getApps . al_body
    isAlias x      = x `envMem` pe
    getApps        :: F.Pred -> [F.Symbol]
    getApps p      = everything (++) ([] `mkQ` fromP) p
    fromP (F.PBexp (F.EApp (F.Loc _ f) _)) = [f]
    fromP _                                = []

expandPAlias :: PAliasEnv -> PAlias -> PAlias
expandPAlias = undefined

expandTAliases :: PAliasEnv -> TAliasEnv RefType -> TAliasEnv RefType 
expandTAliases = undefined

expandRefType :: PAliasEnv -> TAliasEnv RefType -> RefType -> RefType  
expandRefType = undefined

---------------------------------------------------------------------------------------
-- | A Generic Solver for Expanding Definitions --------------------------------------- 
---------------------------------------------------------------------------------------

solve :: (IsLocated a)
      => Env a              -- ^ Input definitions
      -> (a -> [F.Symbol])  -- ^ Dependencies (each Symbol is in `defs`)
      -> (Env a -> a -> a)  -- ^ Expansion function
      -> Env a              -- ^ Output "closed" definitions

solve defs deps exp = ex_solved $ snd $ runState st0 $ forM_ xs $ solveM deps exp []
  where 
    st0             = ExS defs envEmpty
    xs              = [ Loc (srcPos d) x | (x, d) <- envToList defs] 

solveM deps exp stk x 
  | x `elem` stk    = die $ errorCyclicDefs (srcPos x) x stk
  | otherwise       = do xr <- getResult x
                         case xr of
                           Just d' -> return (x, d') 
                           Nothing -> do d     <- getDefinition x
                                         xds'  <- mapM (solveM deps exp (x:stk)) $ deps d
                                         setResult x $ exp (envFromList xds') d

type ExM a     = State (ExState a)

data ExState a = ExS { ex_defs   :: Env a
                     , ex_solved :: Env a 
                     }

-- getDefinition   :: F.Symbol -> ExM a a
getDefinition x = (fromMaybe (die $ bugUnknownAlias (srcPos x) x) . envFindTy (val x) . ex_defs) <$> get

-- getResult     :: F.Symbol -> ExM a (Maybe a)
getResult x   = (envFindTy (val x) . ex_solved) <$> get  

-- setResult     :: (IsLocated a) => Symbol -> a -> ExM a (Symbol, a)
setResult x d = do modify $ \st -> st { ex_solved = envAdd x' d (ex_solved st) } 
                   return (x', d)
  where 
    x'        = Loc (srcPos d) x

