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

 , noKVarInst = def &= help "Disable k-var toplevel function type instantiation"
                    &= name "no-kvar-inst"
   
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
             putStrLn $ banner md
             return   $ md

banner args =  "nanojs © Copyright 2013 Regents of the University of California.\n" 
            ++ "All Rights Reserved.\n"
            ++ "nanojs" ++ show args ++ "\n" 
