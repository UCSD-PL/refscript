module Language.Nano.Liquid.Qualifiers (nanoQualifiers) where

import Language.Fixpoint.Errors
import Language.Fixpoint.Types hiding (quals) 
import Language.Nano.Typecheck.Types
import Language.Nano.Liquid.Types
import Language.Nano.Env
import Language.Nano.Errors
import Language.Nano.Types
import Language.ECMAScript3.Syntax
import Control.Applicative      ((<$>))
import Data.List                (delete, nub)
import Data.Maybe               (fromMaybe)


nanoQualifiers         :: NanoRefType -> [Qualifier]
nanoQualifiers p       = quals p ++ nanoQualifiers' p

nanoQualifiers'        :: NanoRefType -> [Qualifier]
nanoQualifiers' p      = concatMap (refTypeQualifiers γ0) $ envToList $ envs 
  where
    envs               = envUnionList [sigs p, tAnns p, specs p]
    γ0                 = envSEnv $ envMap rTypeSort $ specs p

refTypeQualifiers γ0 (l, t) = efoldRType rTypeSort addQs γ0 [] t 
  where
    addQs γ t qs            = (mkQuals l γ t) ++ qs

mkQuals l γ t      = [ mkQual l γ v so pa | let (RR so (Reft (v, ras))) = rTypeSortedReft t 
                                          , RConc p                    <- ras                 
                                          , pa                         <- atoms p
                     ]

mkQual l γ v so p = Q "Auto" ((v, so) : yts) $ subst θ p
  where 
    yts           = [(y, lookupSort l x γ) | (x, y) <- xys ]
    θ             = mkSubst [(x, eVar y)   | (x, y) <- xys]
    xys           = zipWith (\x i -> (x, stringSymbol ("~A" ++ show i))) xs [0..] 
    xs            = delete v $ orderedFreeVars γ p

lookupSort l  x γ = fromMaybe err $ lookupSEnv x γ 
  where 
    err           = die $ bug (srcPos l) $ "Unbound variable " ++ show x ++ " in specification for " ++ show (unId l)

orderedFreeVars γ = nub . filter (`memberSEnv` γ) . syms 

atoms (PAnd ps)   = concatMap atoms ps
atoms p           = [p]
