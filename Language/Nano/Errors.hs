{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.Nano.Errors where

import Debug.Trace
import Text.Printf 
import Text.PrettyPrint.HughesPJ
import Language.ECMAScript3.PrettyPrint

errorDuplicate i l l'   = printf "Duplicate Specification for %s:\n  %s \n  %s" (ppshow i) (ppshow l) (ppshow l')
errorArgMismatch        = printf "Mismatch in Number of Args in Call" 
errorNonFunction f      = printf "Non-function type for %s" (ppshow f)  
errorUnboundId x        = printf "Identifier %s unbound" (ppshow x) 
errorWrongType m e t t' = printf "%s -- unexpected type for %s :: %s expected %s" m (ppshow e) (ppshow t) (ppshow t')
errorJoin x t t'        = printf "Conflicting join for %s \n   %s\n   %s" (ppshow x) (ppshow t) (ppshow t') 
errorUnification t t'   = printf "Cannot unify types: %s and %s" (ppshow t) (ppshow t')
errorBoundTyVar a t     = printf "Cannot unify bound type parameter %s with %s" (ppshow a) (ppshow t)
errorFreeTyVar t        = printf "Type not fully instantiated: %s" (ppshow t)

ppshow                  = render . pp


tracePP     ::  (PP a) => String -> a -> a
tracePP s x = trace (printf "\nTrace: [%s]: %s" s (ppshow x)) x

instance PP a => PP (Either String a) where 
  pp (Left s)  = text $ "ERROR!" ++ s
  pp (Right x) = pp x 
