module Language.Nano.Liquid.Alias (expandAliases) where

import Language.Nano.Typecheck.Types

expandAliases :: NanoRefType -> NanoRefType
expandAliases = undefined

expandPAliases :: PAliasEnv -> PAliasEnv 
expandPAliases = undefined

expandTAliases :: PAliasEnv -> TAliasEnv RefType -> TAliasEnv RefType 
expandTAliases = undefined

expandRefType :: PAliasEnv -> TAliasEnv RefType -> RefType -> RefType  
expandRefType = undefined
