{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Language.Rsc.CmdLine (Config(..), config) where

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
           , extraInvs   :: Bool           -- ^ add extra invariants to object types
           , renderAnns  :: Bool           -- ^ render annotations
           , prelude     :: Maybe FilePath -- ^ use this prelude file
           }
  deriving (Data, Typeable, Show, Eq)


---------------------------------------------------------------------------------
-- | Parsing Command Line -------------------------------------------------------
---------------------------------------------------------------------------------
tc :: Config
tc = TC {
   files        = def   &= typ "TARGET"
                        &= args
                        &= typFile

 , incdirs      = def   &= typDir
                        &= help "Paths to Spec Include Directory "

 , noFailCasts  = def   &= help "Do not fail typecheck when casts are added"

 } &= help    "RefScript Type Checker"

liquid :: Config
liquid = Liquid {
   files        = def  &= typ "TARGET"
                       &= args
                       &= typFile

 , incdirs      = def  &= typDir
                       &= help "Paths to Spec Include Directory "

 , extraInvs    = def  &= help "Add extra invariants (e.g. 'keyIn' for object types)"

 , renderAnns   = def  &= help "Render annotations"

 , prelude      = def  &= help "Use given prelude.ts file (debug)"

 } &= help    "RefScript Refinement Type Checker"



config :: Config
config = modes [
                 liquid &= auto
               , tc
               ]
            &= help    "rsc is an optional refinement type checker for TypeScript"
            &= program "rsc"
            &= summary "rsc © Copyright 2013-14 Regents of the University of California."
            &= verbosity

-- getOpts :: IO Config
-- getOpts = do md <- cmdArgs config
--              whenLoud $ putStrLn $ banner md
--              return   $ md

-- banner args =  "rsc © Copyright 2013-14 Regents of the University of California.\n"
--             ++ "All Rights Reserved.\n"
--             ++ "rsc" ++ show args ++ "\n"
