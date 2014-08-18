{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Language.Nano.CmdLine (Config(..), getOpts) where

import System.Console.CmdArgs 

---------------------------------------------------------------------
-- | Command Line Configuration Options
---------------------------------------------------------------------

data Config 
  = TC     { files       :: [FilePath]     -- ^ source files to check
           , incdirs     :: [FilePath]     -- ^ path to directory for include specs
           , noFailCasts :: Bool           -- ^ fail typecheck when casts are inserted
           }
  | Liquid { files       :: [FilePath]     -- ^ source files to check
           , incdirs     :: [FilePath]     -- ^ path to directory for include specs
           , kVarInst    :: Bool           -- ^ instantiate function types with k-vars
           }
  deriving (Data, Typeable, Show, Eq)


---------------------------------------------------------------------------------
-- | Parsing Command Line -------------------------------------------------------
---------------------------------------------------------------------------------

tc = TC { 
   files        = def   &= typ "TARGET"
                        &= args
                        &= typFile

 , incdirs      = def   &= typDir
                        &= help "Paths to Spec Include Directory "
 , noFailCasts  = def   &= help "Do not fail typecheck when casts are added"

 } &= help    "RefScript Type Checker" 


liquid = Liquid { 
   files    = def  &= typ "TARGET"
                   &= args
                   &= typFile

 , incdirs  = def  &= typDir
                   &= help "Paths to Spec Include Directory "

 , kVarInst = True &= help "Enable k-var toplevel function type instantiation [default = True]"
   
 } &= help    "RefScript Refinement Type Checker" 




config = modes [ tc
               , liquid &= auto 
               ] 
            &= help    "rsc is an optional refinement type checker for TypeScript"
            &= program "rsc" 
            &= summary "rsc © Copyright 2013-14 Regents of the University of California." 
            &= verbosity
   
getOpts :: IO Config 
getOpts = do md <- cmdArgs config 
             whenLoud $ putStrLn $ banner md
             return   $ md

banner args =  "nanojs © Copyright 2013-14 Regents of the University of California.\n" 
            ++ "All Rights Reserved.\n"
            ++ "nanojs" ++ show args ++ "\n" 
