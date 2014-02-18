import Control.Monad
import Data.Maybe
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import System.Posix.Env
import System.Process
import System.Exit

main         = defaultMainWithHooks fixHooks 
  where 
    fixHooks = simpleUserHooks { postInst = buildAndCopyFixpoint } 
   
buildAndCopyFixpoint _ _ pkg lbi 
  = do putStrLn $ "Post Install - copy TS binary into PATH: " ++ show binDir
       executeShellCommand   "chmod a+x ../typescript/bin/tsc"
       executeShellCommand $ "cp ../typescript/bin/tsc "            ++ binDir
       executeShellCommand $ "cp ../typescript/built/local/tsc.js " ++ binDir
  where 
    allDirs     = absoluteInstallDirs pkg lbi NoCopyDest
    binDir      = bindir allDirs ++ "/"
    tscPath     = "../typescript/bin/tsc"
    tscJsPath   = "../typescript/bin/tsc.js"
    flags       = configConfigurationsFlags $ configFlags lbi
    z3mem       = fromJust $ lookup (FlagName "z3mem") flags

executeShellCommand cmd   = putStrLn ("EXEC: " ++ cmd) >> system cmd >>= check
  where 
    check (ExitSuccess)   = return ()
    check (ExitFailure n) = error $ "cmd: " ++ cmd ++ " failure code " ++ show n 

