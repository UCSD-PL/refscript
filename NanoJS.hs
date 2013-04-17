
import System.Environment           (getArgs)
import Text.Printf                  (printf)

-- import Language.Nano.SMT.Misc       (errorstar)
-- import Language.Nano.SMT.Types      (Formula)
-- import Language.Nano.SMT.Tests      (runTests)
-- import Language.Nano.SMT.SMT        (smt_solver)
import Control.Monad                (forM_)
import Language.Nano.JS.CmdLine         (getOpts)


main = do cfg <- getOpts  
          putStrLn $ "nano-js: " ++ show cfg

-- main 
--   = do files <- getArgs
--        case files of
--          []  -> errorstar "No Input Files!"
--          fs  -> forM fs solveFile
--
-- main 
--   = runTests 
-- 
-- solveFile f 
--   = parseQuery f >>= solveQuery f
-- 
-- solveQuery name q
--   = putStrLn $ printf "Result: %s is %s" name (show $ smt_solver q)
-- 
-- parseQuery :: FilePath -> IO Formula
-- parseQuery = undefined
-- 
-- tests :: [(String, Formula)]
-- tests = []  
