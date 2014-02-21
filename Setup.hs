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
  = do putStrLn $ "Post Install - copy TS binaries into PATH: " ++ show binDir
  -- Compile and copy TypeScript executables
       executeShellCommand   "chmod a+x ../typescript/bin/tsc"
       mapM_ executeShellCommand (map cpFile tscBins)
  -- Compile and copy TypeScript the prelude
       executeShellCommand $ "tsc --nano " ++ preludeSrc
       executeShellCommand ("cp " ++ preludeTgt ++ " " ++ dataDir ++ preludeTgt)
  where 
    allDirs     = absoluteInstallDirs pkg lbi NoCopyDest
    withSpaces  = concatMap $ (++) " "
    binDir      = bindir allDirs ++ "/"
    dataDir     = datadir allDirs ++ "/"

    cpFile f    = withSpaces ["cp", f, binDir]
    tscBins     = [ "../typescript/bin/tsc",
                    "../typescript/built/local/tsc.js",
                    "../typescript/typings/lib.d.ts",
                    "../typescript/typings/jquery.d.ts",
                    "../typescript/typings/winjs.d.ts",
                    "../typescript/typings/winrt.d.ts" ]
    preludeSrc  = "include/prelude.ts"
    preludeTgt  = "include/prelude.json"
    flags       = configConfigurationsFlags $ configFlags lbi
    z3mem       = fromJust $ lookup (FlagName "z3mem") flags

executeShellCommand cmd   = putStrLn ("EXEC: " ++ cmd) >> system cmd >>= check
  where 
    check (ExitSuccess)   = return ()
    check (ExitFailure n) = error $ "cmd: " ++ cmd ++ " failure code " ++ show n 

