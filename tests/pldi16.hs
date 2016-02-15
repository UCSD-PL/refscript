{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Control.Applicative
import           Data.List           (isPrefixOf)
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.IO.Error
import           System.Process
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Printf

main :: IO ()
main = defaultMain =<< group "Tests" [unitTests]

unitTests = group "Unit"
  [
    testGroup "pldi-d3"           <$> dirTests rscCmd "tests/pos/pldi16/d3"                  []                ExitSuccess,
    testGroup "pldi-octane"       <$> dirTests rscCmd "tests/pos/pldi16/octane"              ["extra", "libs"] ExitSuccess,
    testGroup "pldi-transducers"  <$> dirTests rscCmd "tests/pos/pldi16/transducers"         []                ExitSuccess,
    testGroup "pldi-tsc"          <$> dirTests rscCmd "tests/pos/pldi16/typescript-1.0.1.0"  ["extra", "libs"] ExitSuccess
  ]

isTest   :: FilePath -> Bool
isTest f = takeExtension f `elem` [".ts"]

testBinary :: String
testBinary = "rsc"

---------------------------------------------------------------------------
type TestCmd = FilePath -> FilePath -> FilePath -> String
---------------------------------------------------------------------------
rscCmd :: TestCmd
rscCmd bin dir file = printf "cd %s && %s %s" dir bin file

-- extra invariants
eCmd :: TestCmd
eCmd bin dir file   = printf "cd %s && %s --extrainvs %s" dir bin file

-- reals
rCmd :: TestCmd
rCmd bin dir file   = printf "cd %s && %s --real %s" dir bin file


---------------------------------------------------------------------------
dirTests :: TestCmd -> FilePath -> [FilePath] -> ExitCode -> IO [TestTree]
---------------------------------------------------------------------------
dirTests testCmd root ignored code
  = do files    <- walkDirectory root
       let tests = [ rel | f <- files
                         , isTest f
                         , let rel = makeRelative root f
                         , all (not . (`isPrefixOf` rel)) ignored ]
       -- putStrLn  $ "Tests found in " ++ root ++ " are = " ++ show tests
       return    $ mkTest testCmd code root <$> tests







---------------------------------------------------------------------------
mkTest :: TestCmd -> ExitCode -> FilePath -> FilePath -> TestTree
---------------------------------------------------------------------------
mkTest testCmd code dir file
  = testCase file $
      if test `elem` knownToFail
      then do
        printf "%s is known to fail: SKIPPING" test
        assertEqual "" True True
      else do
        createDirectoryIfMissing True $ takeDirectory log
        bin <- binPath testBinary
        withFile log WriteMode $ \h -> do
          let cmd     = testCmd bin dir file
          (_,_,_,ph) <- createProcess $ (shell cmd) {std_out = UseHandle h, std_err = UseHandle h}
          c          <- waitForProcess ph
          assertEqual "Wrong exit code" code c
  where
    test = dir </> file
    log  = let (d,f) = splitFileName file in dir </> d </> ".liquid" </> f <.> "log"

binPath pkgName = do
  testPath <- getExecutablePath
  return    $ (takeDirectory $ takeDirectory testPath) </> pkgName </> pkgName

knownToFail = []

----------------------------------------------------------------------------------------
-- Generic Helpers
----------------------------------------------------------------------------------------

group n xs = testGroup n <$> sequence xs

----------------------------------------------------------------------------------------
walkDirectory :: FilePath -> IO [FilePath]
----------------------------------------------------------------------------------------
walkDirectory root
  = do (ds,fs) <- partitionM doesDirectoryExist . candidates =<< (getDirectoryContents root `catchIOError` const (return []))
       (fs++) <$> concatMapM walkDirectory ds
  where
    candidates fs = [root </> f | f <- fs, not (isExtSeparator (head f))]

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM f = go [] []
  where
    go ls rs []     = return (ls,rs)
    go ls rs (x:xs) = do b <- f x
                         if b then go (x:ls) rs xs
                              else go ls (x:rs) xs

concatMapM :: Applicative m => (a -> m [b]) -> [a] -> m [b]
concatMapM _ []     = pure []
concatMapM f (x:xs) = (++) <$> f x <*> concatMapM f xs
