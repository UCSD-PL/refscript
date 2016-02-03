{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.Rsc.Pretty.Errors where

import           Debug.Trace
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Types.Errors
import           Language.Rsc.Locations
import           Language.Rsc.Pretty.Common
import           System.Console.ANSI
import           Text.PrettyPrint.HughesPJ
import           Text.Printf


boldBlue s = setSGRCode [SetConsoleIntensity BoldIntensity,
                         SetColor Foreground Vivid Magenta] ++ s ++
             setSGRCode [Reset]

---------------------------------------------------------------------
tracePP     ::  (PP a, PP b) => a -> b -> b
---------------------------------------------------------------------
tracePP s x = trace (boldBlue (printf "\nTrace: [%s]\n" (ppshow s)) ++  printf "%s" (ppshow x)) x

---------------------------------------------------------------------
ltracePP     ::  (PP a, PP b, IsLocated l) => l -> a -> b -> b
---------------------------------------------------------------------
ltracePP l s x =
  trace (boldBlue (printf "\nTrace: [%s: %s]\n" (ppshow (srcPos l)) (ppshow s)) ++
         printf "%s" (ppshow x)) x

instance PP Error where
  pp e = pp (errLoc e) $+$ pp (errMsg e)

convertError tgt e  = errortext $ msg <+> pp e
  where
    msg             = text $ "Cannot convert to: " ++ tgt

---------------------------------------------------------------------
errortext :: Doc -> c
---------------------------------------------------------------------
errortext = errorstar . render


