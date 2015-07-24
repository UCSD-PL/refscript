{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Language.Nano.CmdLine (Config(..), config, parseConfig) where

import Language.Nano.Errors
import Language.Nano.Locations
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit hiding (mode)
-- import System.Console.CmdArgs.Implicit     hiding (Loud)
-- import System.Console.CmdArgs.Text
import Control.Exception              (throw)
import System.Environment (withArgs)
import System.IO.Unsafe   (unsafePerformIO)

---------------------------------------------------------------------
-- | Command Line Configuration Options
---------------------------------------------------------------------

data Config
  = Config { files       :: [FilePath]     -- ^ source files to check
           , incdirs     :: [FilePath]     -- ^ path to directory for include specs
           , extraInvs   :: Bool           -- ^ add extra invariants to object types
           , renderAnns  :: Bool           -- ^ render annotations
           , prelude     :: Maybe FilePath -- ^ use this prelude file
           , real        :: Bool           -- ^ use real-arithmetic
           , noFailCasts :: Bool           -- ^ fail typecheck when casts are inserted
           , typeCheck   :: Bool           -- ^ only run TypeChecker (ignore Refinements)
           }
  deriving (Data, Typeable, Show, Eq)


---------------------------------------------------------------------------------
-- | Parsing Command Line -------------------------------------------------------
---------------------------------------------------------------------------------
config :: Config
config = Config {
   files        = def  &= typ "TARGET"
                       &= args
                       &= typFile

 , incdirs      = def  &= typDir
                       &= help "Paths to Spec Include Directory "

 , extraInvs    = def  &= help "Add extra invariants (e.g. 'keyIn' for object types)"

 , renderAnns   = def  &= help "Render annotations"

 , prelude      = def  &= help "Use given prelude.ts file (debug)"

 , real         = def  &= help "Use real arithmetic in SMT solver (slow!)"

 , noFailCasts  = def   &= help "Do not fail typecheck when casts are added"

 , typeCheck    = def   &= help "Only run type checker, not refinement checker"

 } &= help    "rsc is an optional refinement type checker for TypeScript"
   &= program "rsc"
   &= summary "rsc © Copyright 2013-15 Regents of the University of California."
   &= verbosity



-- parseConfigs :: [(SrcSpan, String)] -> Config
-- parseConfigs = mconcat . fmap parseConfig
-- parseConfig s = withArgs [s] $ cmdArgsRun (cmdArgsMode config)


parseConfig        :: (SrcSpan, String) -> Config
parseConfig (_, s) = unsafePerformIO $ withArgs [s] $ cmdArgsRun cfgMode
  where
    cfgMode        = cmdArgsMode config

{-
parseConfig :: (SrcSpan, String) -> Config
parseConfig (sp, s)
  = case process (modeEmpty config) [s] of
      Left err -> throw $ errorMalformedConfig sp err
      Right c  -> c
-}

instance Monoid Config where
  mempty        = config
  mappend c1 c2 = Config { files       = files c1       ++  files c2
                         , incdirs     = incdirs c1     ++  incdirs c2
                         , extraInvs   = extraInvs c1   ||  extraInvs c2
                         , renderAnns  = renderAnns c1  ||  renderAnns c2
                         , prelude     = prelude c1   `ma`  prelude c2
                         , real        = real    c1     ||  real c2
                         , noFailCasts = noFailCasts c1 ||  noFailCasts c2
                         , typeCheck   = typeCheck c1   ||  typeCheck c2}

ma :: Maybe a -> Maybe a -> Maybe a
ma Nothing x = x
ma x Nothing = x
ma _ x       = x
