{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.Rsc.Pretty.Errors where

import           Debug.Trace
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc
import           Language.Fixpoint.PrettyPrint
import           Language.Rsc.Locations
import           Language.Rsc.Pretty.Common
import           System.Console.ANSI
import           Text.PrettyPrint.HughesPJ
import           Text.Printf


boldBlue s = setSGRCode [SetConsoleIntensity BoldIntensity,
                         SetColor Foreground Vivid Magenta] ++ s ++
             setSGRCode [Reset]

---------------------------------------------------------------------
tracePP     ::  (PP a) => String -> a -> a
---------------------------------------------------------------------
tracePP s x = trace (boldBlue (printf "\nTrace: [%s]\n" s) ++  printf "%s" (ppshow x)) x

---------------------------------------------------------------------
ltracePP     ::  (PP a, IsLocated l) => l -> String -> a -> a
---------------------------------------------------------------------
ltracePP l s x = trace (boldBlue (printf "\nTrace: [%s: %s]\n" (ppshow (srcPos l)) s) ++ printf "%s" (ppshow x)) x

instance PP Error where
  pp e = pp (errLoc e) <+> pp (errMsg e)

convertError tgt e  = errortext $ msg <+> pp e
  where
    msg             = text $ "Cannot convert to: " ++ tgt

---------------------------------------------------------------------
errortext :: Doc -> c
---------------------------------------------------------------------
errortext = errorstar . render


