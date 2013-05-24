{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances         #-}

module Language.Nano.Errors where

import Debug.Trace
import Text.Printf 
import Text.PrettyPrint.HughesPJ
import Language.ECMAScript3.PrettyPrint

bugBadPhi l t1s t2s     = printf "BUG: Unbalanced Phi at %s \n %s \n %s" (ppshow l) (ppshow t1s) (ppshow t2s)
bugBadSubtypes x    = printf "BUG: Unexpected Subtyping Constraint \n %s" (ppshow x)
bugUnboundPhiVar x      = printf "BUG: Phi Variable %s is unbound" (ppshow x)
bugUnboundVariable l x  = printf "BUG: Variable %s is unbound in environment at %s" (ppshow x) (ppshow l)
bugMissingTypeArgs l    = printf "BUG: Missing Type Arguments at %s" (ppshow l)

errorArgName l x y      = printf "Wrong Parameter Name at %s: Saw %s but Expected %s" (ppshow l) (ppshow x) (ppshow y)  

errorMissingSpec l f    = printf "Missing Signature For %s defined at %s" (ppshow f) (ppshow l)
errorDuplicate i l l'   = printf "Duplicate Specification for %s:\n  %s \n  %s" (ppshow i) (ppshow l) (ppshow l')
errorArgMismatch        = printf "Mismatch in Number of Args in Call" 
errorNonFunction f t    = printf "Non-function type for %s :: %s" (ppshow f) (ppshow t)  
errorUnboundId x        = printf "Identifier %s unbound" (ppshow x) 
errorUnboundIdEnv x t   = printf "ZOGBERT Identifier %s unbound in %s" (ppshow x) (ppshow t)
errorWrongType m e t t' = printf "%s -- unexpected type for %s :: %s expected %s" m (ppshow e) (ppshow t) (ppshow t')
errorJoin x t t'        = printf "Conflicting join for %s \n   %s\n   %s" (ppshow x) (ppshow t) (ppshow t') 
errorUnification t t'   = printf "Cannot unify types: %s and %s" (ppshow t) (ppshow t')
errorBoundTyVar a t     = printf "Cannot unify bound type parameter %s with %s" (ppshow a) (ppshow t)
errorFreeTyVar t        = printf "Type not fully instantiated: %s" (ppshow t)
errorWriteImmutable x   = printf "Cannot write immutable: %s" (ppshow x)
errorInvalidTopStmt x   = printf "Invalid top-level statement: %s" (ppshow x) 
errorOccursCheck a t    = printf "Occurs check fails: %s in %s" (ppshow a) (ppshow t)
errorRigidUnify a t     = printf "Cannot unify rigid variable %s with %s" (ppshow a) (ppshow t) 
 
ppshow                  = render . pp

tracePP     ::  (PP a) => String -> a -> a
tracePP s x = trace (printf "\nTrace: [%s]: %s" s (ppshow x)) x

instance PP a => PP (Either String a) where 
  pp (Left s)  = text $ "ERROR!" ++ s
  pp (Right x) = pp x 
