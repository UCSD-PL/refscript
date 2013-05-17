{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Language.Nano.CmdLine (getOpts) where

import Language.Nano.Types                      (Config (..))
import System.Console.CmdArgs                  

---------------------------------------------------------------------------------
-- | Parsing Command Line -------------------------------------------------------
---------------------------------------------------------------------------------

esc = Esc { 
   files   = def &= typ "TARGET" 
                 &= args 
                 &= typFile 
 
 , incdirs = def &= typDir 
                 &= help "Paths to Spec Include Directory " 
   
 } &= help    "Extended Static Checker for Nano" 

tc = TC { 
   files   = def &= typ "TARGET" 
                 &= args 
                 &= typFile 
 
 , incdirs = def &= typDir 
                 &= help "Paths to Spec Include Directory " 
   
 } &= help    "Type Checker for Nano" 


liquid = Liquid { 
   files   = def &= typ "TARGET" 
                 &= args 
                 &= typFile 
 
 , incdirs = def &= typDir 
                 &= help "Paths to Spec Include Directory " 
   
 } &= help    "Refinement Type Checker for Nano" 

config = modes [esc, tc, liquid] 
            &= help    "nano-js is a suite of toy program verifiers"
            &= program "nano-js" 
            &= summary "nano-js © Copyright 2013 Regents of the University of California." 
            &= verbosity
            &= verbosity
   
getOpts :: IO Config 
getOpts = do md <- cmdArgs config 
             putStrLn $ banner md
             return   $ md

banner args =  "nano-js © Copyright 2013 Regents of the University of California.\n" 
            ++ "All Rights Reserved.\n"
            ++ "nano-js" ++ show args ++ "\n" 
