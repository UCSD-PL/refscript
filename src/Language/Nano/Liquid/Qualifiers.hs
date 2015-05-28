module Language.Nano.Liquid.Qualifiers (qualifiers) where

import Language.Fixpoint.Errors
import Language.Fixpoint.Types hiding (quals)
import Language.Nano.Liquid.Types
import Language.Nano.Env
import Language.Nano.Errors
import Language.Nano.Locations
import Language.Nano.Syntax
import Data.List                (delete, nub)
import Data.Maybe               (fromMaybe)


-- nanoQualifiers   :: NanoRefType -> [Qualifier]
-- nanoQualifiers p  = pQuals p ++ nanoQualifiers' p
--
-- nanoQualifiers'  :: NanoRefType -> [Qualifier]
-- nanoQualifiers' p = concatMap (refTypeQualifiers γ0) $ envToList env
--   where
--     γ0            = envSEnv $ envMap rTypeSort env
--     env           = tracePP "qualPool" $ qualPool p

qualifiers xts = concatMap (refTypeQualifiers γ0) xts
  where
     γ0        = envSEnv $ envMap rTypeSort $ envFromList xts



refTypeQualifiers γ0 (l, t) = efoldRType rTypeSort addQs γ0 [] t
  where
    addQs γ t qs  = mkQuals l γ t ++ qs

mkQuals l γ t     = [ mkQual l γ v so pa | let (RR so (Reft (v, ra))) = rTypeSortedReft t
                                         , pa                        <- conjuncts $ raPred ra
                    ]

mkQual l γ v so p = Q (symbol "Auto") ((v, so) : yts) (subst θ p) l0
  where
    yts           = [(y, lookupSort l x γ) | (x, y) <- xys ]
    θ             = mkSubst [(x, eVar y)   | (x, y) <- xys]
    xys           = zipWith (\x i -> (x, symbol ("~A" ++ show i))) xs [0..]
    xs            = delete v $ orderedFreeVars γ p
    l0            = dummyPos "RSC.Qualifiers.mkQual"

lookupSort l  x γ = fromMaybe err $ lookupSEnv x γ
  where
    err           = die $ bug (srcPos l) $ "Unbound variable " ++ show x ++ " in specification for " ++ show (unId l)

orderedFreeVars γ = nub . filter (`memberSEnv` γ) . syms

atoms (PAnd ps)   = concatMap atoms ps
atoms p           = [p]
