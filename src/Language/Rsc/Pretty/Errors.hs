{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances      #-}

module Language.Rsc.Pretty.Errors where

import           Debug.Trace
import           Language.Rsc.Locations
import           Language.Rsc.Pretty.Common
import           Text.PrettyPrint.HughesPJ
import           Text.Printf

import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc
import           Language.Fixpoint.PrettyPrint


---------------------------------------------------------------------
tracePP     ::  (PP a) => String -> a -> a
---------------------------------------------------------------------
tracePP s x = trace (printf "\nTrace: [%s]: %s" s (ppshow x)) x

---------------------------------------------------------------------
ltracePP     ::  (PP a, IsLocated l) => l -> String -> a -> a
---------------------------------------------------------------------
ltracePP l s x = trace (printf "\nTrace: [%s: %s]: %s" (ppshow (srcPos l)) s (ppshow x)) x

instance PP Error where
  pp e = pp (errLoc e) <+> pp (errMsg e)

convertError tgt e  = errortext $ msg <+> pp e
  where
    msg             = text $ "Cannot convert to: " ++ tgt

---------------------------------------------------------------------
errortext :: Doc -> c
---------------------------------------------------------------------
errortext = errorstar . render


