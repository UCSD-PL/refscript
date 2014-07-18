import Control.Monad
import Data.Maybe
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import System.Posix.Env           hiding  (getEnvironment)
import System.Environment                 (getEnvironment)
import System.Process
import System.Exit
import Data.List                          (find)

main         = defaultMainWithHooks fixHooks 
  where 
    fixHooks = simpleUserHooks { postCopy = copyExts 
                               , postInst = copyExts }


copyExts _ _ pkg lbi 
  = do putStrLn $ "Post Install - copy TS binaries into PATH: " ++ show binDir
       env     <- getEnvironment 
       let tscR = fromMaybe lTscRoot $ fmap snd $ find ((== "TSC_ROOT") . fst) env
       putStrLn $ "Using '" ++ tscR ++ "' for `tsc` sources." 
       -- Compile and copy TypeScript executables
       executeShellCommand $ "chmod a+x " ++ tscR ++ "/bin/tsc"
       mapM_ executeShellCommand (map cpFile $ tscBins tscR)
       -- Compile and copy TypeScript's prelude
       executeShellCommand $ "tsc --refscript " ++ preludeSrc
       executeShellCommand $ "cp " ++ preludeTgt ++ " " ++ dataDir ++ preludeTgt
  where 
    allDirs     = absoluteInstallDirs pkg lbi NoCopyDest
    withSpaces  = concatMap $ (++) " "
    binDir      = bindir allDirs ++ "/"
    dataDir     = datadir allDirs ++ "/"

    cpFile f    = withSpaces ["cp", f, binDir]
    lTscRoot    = "ext/tsc-bin"
    tscBins r   = map (r ++) ["/bin/tsc",
                              "/built/local/tsc.js",
                              "/built/local/lib.d.ts",
                              "/typings/jquery.d.ts",
                              "/typings/winjs.d.ts",
                              "/typings/winrt.d.ts" ]

    preludeSrc  = "include/prelude.ts"
    preludeTgt  = "include/prelude.json"
    flags       = configConfigurationsFlags $ configFlags lbi
    z3mem       = fromJust $ lookup (FlagName "z3mem") flags

executeShellCommand cmd   = putStrLn ("EXEC: " ++ cmd) >> system cmd >>= check
  where 
    check (ExitSuccess)   = return ()
    check (ExitFailure n) = error $ "cmd: " ++ cmd ++ " failure code " ++ show n 

