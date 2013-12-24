module Language.Nano.Liquid.Alias (expandAliases) where

import Language.Fixpoint.Types
import Language.Nano.Env
import Language.Nano.Typecheck.Types
import Language.Nano.Liquid.Types

expandAliases :: NanoRefType -> NanoRefType
expandAliases = undefined

expandPAliases :: PAliasEnv -> PAliasEnv 
expandPAliases = undefined

expandTAliases :: PAliasEnv -> TAliasEnv RefType -> TAliasEnv RefType 
expandTAliases = undefined

expandRefType :: PAliasEnv -> TAliasEnv RefType -> RefType -> RefType  
expandRefType = undefined


solve :: (Symbolic x) 
      => Env a              -- ^ Input definitions
      -> (a -> [x])         -- ^ Dependencies
      -> (Env a -> a -> a)  -- ^ Expansion function
      -> Env a              -- ^ Output "closed" definitions

solve defs deps exp = undefined 
