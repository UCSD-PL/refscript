{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Language.Nano.CmdLine (getOpts) where

import Language.Nano.Types                      (Config (..))
import System.Console.CmdArgs                  

---------------------------------------------------------------------------------
-- | Parsing Command Line -------------------------------------------------------
---------------------------------------------------------------------------------

esc = Esc { 
   files   = def  &= typ "TARGET"
                  &= args
                  &= typFile

 , incdirs = def  &= typDir
                  &= help "Paths to Spec Include Directory "

 } &= help    "Extended Static Checker for Nano" 

tc = TC { 
   files        = def   &= typ "TARGET"
                        &= args
                        &= typFile

 , incdirs      = def   &= typDir
                        &= help "Paths to Spec Include Directory "
 , noFailCasts  = def   &= help "Do not fail typecheck when casts are added"

 } &= help    "Type Checker for Nano" 


liquid = Liquid { 
   files    = def  &= typ "TARGET"
                   &= args
                   &= typFile

 , incdirs  = def  &= typDir
                   &= help "Paths to Spec Include Directory "

 , kVarInst = True &= help "Enable k-var toplevel function type instantiation [default = True]"
   
 } &= help    "Refinement Type Checker for Nano" 


visit = Visit { 
   files   = def &= typ "TARGET" 
                 &= args 
                 &= typFile 
 
 , incdirs = def &= typDir 
                 &= help "Paths to Spec Include Directory " 
   
 } &= help    "Visitor testing" 



config = modes [esc, tc, liquid, visit] 
            &= help    "nanojs is a suite of toy program verifiers"
            &= program "nanojs" 
            &= summary "nanojs © Copyright 2013 Regents of the University of California." 
            &= verbosity
            &= verbosity
   
getOpts :: IO Config 
getOpts = do md <- cmdArgs config 
             whenLoud $ putStrLn $ banner md
             return   $ md

banner args =  "nanojs © Copyright 2013 Regents of the University of California.\n" 
            ++ "All Rights Reserved.\n"
            ++ "nanojs" ++ show args ++ "\n" 
