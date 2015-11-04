{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Applicative
import System.Directory
import System.Exit
import System.FilePath
import System.Environment
import System.IO
import System.IO.Error
import System.Process
import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf

main :: IO ()
main = defaultMain =<< group "Tests" [unitTests]

unitTests = group "Unit"
  [ testGroup "pos-object" <$> dirTests rscCmd "tests/pos/objects"    [] ExitSuccess
  , testGroup "pos-array"  <$> dirTests rscCmd "tests/pos/arrays"     [] ExitSuccess
  , testGroup "pos-class"  <$> dirTests rscCmd "tests/pos/classes"    [] ExitSuccess
  , testGroup "pos-loop"   <$> dirTests rscCmd "tests/pos/loops"      [] ExitSuccess
  , testGroup "pos-misc"   <$> dirTests rscCmd "tests/pos/misc"       [] ExitSuccess
  , testGroup "pos-ops"    <$> dirTests rscCmd "tests/pos/operators"  [] ExitSuccess
  , testGroup "pos-simple" <$> dirTests rscCmd "tests/pos/simple"     [] ExitSuccess
  , testGroup "pos-union"  <$> dirTests rscCmd "tests/pos/unions"     [] ExitSuccess
  , testGroup "pos-alias"  <$> dirTests rscCmd "tests/pos/typealias"  [] ExitSuccess
  , testGroup "pos-fb"     <$> dirTests rscCmd "tests/pos/fb"         [] ExitSuccess
  , testGroup "pos-incl"   <$> dirTests eCmd   "tests/pos/inclusion"  [] ExitSuccess
  , testGroup "neg-object" <$> dirTests rscCmd "tests/neg/objects"    [] (ExitFailure 1)
  , testGroup "neg-array"  <$> dirTests rscCmd "tests/neg/arrays"     [] (ExitFailure 1)
  , testGroup "neg-class"  <$> dirTests rscCmd "tests/neg/classes"    [] (ExitFailure 1)
  , testGroup "neg-loop"   <$> dirTests rscCmd "tests/neg/loops"      [] (ExitFailure 1)
  , testGroup "neg-misc"   <$> dirTests rscCmd "tests/neg/misc"       [] (ExitFailure 1)
  , testGroup "neg-ops"    <$> dirTests rscCmd "tests/neg/operators"  [] (ExitFailure 1)
  , testGroup "neg-simple" <$> dirTests rscCmd "tests/neg/simple"     [] (ExitFailure 1)
  , testGroup "neg-union"  <$> dirTests rscCmd "tests/neg/unions"     [] (ExitFailure 1)
  , testGroup "neg-alias"  <$> dirTests rscCmd "tests/neg/typealias"  [] (ExitFailure 1)
  , testGroup "neg-fb"     <$> dirTests rscCmd "tests/neg/fb"         [] (ExitFailure 1)
  , testGroup "neg-incl"   <$> dirTests eCmd   "tests/neg/inclusion"  [] (ExitFailure 1)
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

eCmd :: TestCmd
eCmd bin dir file   = printf "cd %s && %s --extrainvs %s" dir bin file


---------------------------------------------------------------------------
dirTests :: TestCmd -> FilePath -> [FilePath] -> ExitCode -> IO [TestTree]
---------------------------------------------------------------------------
dirTests testCmd root ignored code
  = do files    <- walkDirectory root
       let tests = [ rel | f <- files, isTest f, let rel = makeRelative root f, rel `notElem` ignored ]
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
