import           Control.Monad
import           Data.List                          (find, intercalate)
import           Data.Maybe
import           Distribution.PackageDescription
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.Setup
import           System.Environment                 (getEnvironment)
import           System.Exit
import           System.Posix.Env                   hiding (getEnvironment)
import           System.Process

main         = defaultMainWithHooks fixHooks
  where
    fixHooks = simpleUserHooks { preBuild = buildPrelude
                               , postCopy = copyExts
                               , postInst = copyExts }

buildPrelude _ _
  = do putStrLn            $ "Building prelude"
       executeShellCommand $ buildScriptCmd
       executeShellCommand $ runScriptCmd
       return              $ emptyHookedBuildInfo
  where
    buildScriptCmd         = withSpaces ["tsc", "-p", "./scripts" ]
    runScriptCmd           = withSpaces ["node", "./scripts/build-prelude.js" ]


copyExts _ _ pkg lbi
  = do putStrLn            $ "Post installation"
       env                <- getEnvironment
       let tscR            = fromMaybe lTscRoot $ fmap snd $ find ((== "TSC_ROOT") . fst) env

       putStrLn            $ "Using '" ++ tscR ++ "' for `tsc` sources."

       putStrLn            $ "Copying tsc binaries to " ++ show binDir
       executeShellCommand $ withSpaces ["chmod", "a+x", tscR ++ "/bin/tsc-refscript" ]

       mapM_ executeShellCommand (map cpFile $ tscBins tscR)
  where
    allDirs     = absoluteInstallDirs pkg lbi NoCopyDest
    binDir      = bindir allDirs ++ "/"

    cpFile f    = withSpaces ["cp", f, binDir]
    lTscRoot    = "ext/tsc-bin"
    tscBins r   = map (r ++) [ "/bin/tsc-refscript"
                             , "/built/local/tsc-refscript.js"
                             ]
    flags       = configConfigurationsFlags $ configFlags lbi
    z3mem       = fromJust $ lookup (FlagName "z3mem") flags

executeShellCommand cmd   = putStrLn ("EXEC: " ++ cmd) >> system cmd >>= check
  where
    check (ExitSuccess)   = return ()
    check (ExitFailure n) = error $ withSpaces ["cmd:", cmd, "failure code", show n]

withSpaces  = intercalate " "
