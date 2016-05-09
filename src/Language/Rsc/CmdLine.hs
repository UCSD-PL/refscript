{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections      #-}

module Language.Rsc.CmdLine (Config(..), config, withPragmas) where

import           Control.Monad                   (foldM)
import           Language.Rsc.Locations
import           System.Console.CmdArgs
import           System.Console.CmdArgs.Explicit (modeValue)
import           System.Environment              (withArgs)

---------------------------------------------------------------------
-- | Command Line Configuration Options
---------------------------------------------------------------------

data Config
  = TC     { files       :: [FilePath]     -- ^ source files to check
           , incdirs     :: [FilePath]     -- ^ path to directory for include specs
           , noFailCasts :: Bool           -- ^ fail typecheck when casts are inserted
           }
  | Liquid { files      :: [FilePath]     -- ^ source files to check
           , incdirs    :: [FilePath]     -- ^ path to directory for include specs
           , extraInvs  :: Bool           -- ^ add extra invariants to object types
           , renderAnns :: Bool           -- ^ render annotations
           , prelude    :: Maybe FilePath -- ^ use this prelude file
           , real       :: Bool           -- ^ use real-valued SMT arithmetic
           , extSolver  :: Bool           -- ^ use external (Ocaml) fixpoint solver (deprecated)
           , dumpDebug  :: Bool           -- ^ emit .fq, .fqout, .out files
           , dumpJson   :: Bool           -- ^ dump result in JSON form in error stream
           }
  deriving (Data, Typeable, Show, Eq)


instance Default Config where
  def = Liquid [] [] False False Nothing False False False False


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

 , real         = def  &= help "Use real-valued SMT logic (slow!)"

 , extSolver    = def  &= help "Use external (Ocaml) fixpoint solver (deprecated)"

 , dumpDebug    = def  &= help "Dump debug files (e.g. .fq, .fqout, .out)"

 , dumpJson     = def  &= help "Dump result in JSON format in error stream"

 } &= help    "RefScript Refinement Type Checker"

config :: Config
config = modes [ liquid &= auto
               , tc
               ]
            &= help    "rsc is an optional refinement type checker for TypeScript"
            &= program "rsc"
            &= summary "rsc © Copyright 2013-15 Regents of the University of California."
            &= verbosity

---------------------------------------------------------------------------------------
withPragmas :: Config -> [Located String] -> IO Config
---------------------------------------------------------------------------------------
withPragmas = foldM withPragma

withPragma :: Config -> Located String -> IO Config
withPragma c s = withArgs [val s] $ cmdArgsRun
                    cfg0 { modeValue = (modeValue cfg0) { cmdArgsValue = c } }
  where
    cfg0       = cmdArgsMode liquid



-- getOpts :: IO Config
-- getOpts = do md <- cmdArgs config
--              whenLoud $ putStrLn $ banner md
--              return   $ md

-- banner args =  "rsc © Copyright 2013-14 Regents of the University of California.\n"
--             ++ "All Rights Reserved.\n"
--             ++ "rsc" ++ show args ++ "\n"
