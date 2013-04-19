{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Language.Nano.CmdLine (getOpts) where

-- import Control.Applicative                      ((<$>))
-- import System.FilePath                          (dropFileName)
-- import Language.Fixpoint.Misc                   (single, sortNub) 
import Language.Nano.Types                      (Config (..))
import System.Console.CmdArgs                  

---------------------------------------------------------------------------------
-- | Parsing Command Line -------------------------------------------------------
---------------------------------------------------------------------------------

config = Config { 
   files   = def &= typ "TARGET" 
                 &= args 
                 &= typFile 
 
 , incdirs = def &= typDir 
                 &= help "Paths to Spec Include Directory " 
   
 } &= verbosity
   &= program "nano-js" 
   &= help    "The Nano Software Verification System" 
   &= summary "nano-js © Copyright 2013 Regents of the University of California." 
   &= details [ "nano-js is suite of toy program verifiers"
              , ""
              , "To check a file foo.js, type:"
              , "  nano-js foo.hs "
              ]

getOpts :: IO Config 
getOpts = do md <- cmdArgs config 
             putStrLn $ banner md
             return   $ md

banner args =  "nano-js © Copyright 2013 Regents of the University of California.\n" 
            ++ "All Rights Reserved.\n"
            ++ "nano-js" ++ show args ++ "\n" 

-- mkOpts :: Config -> IO Config
-- mkOpts = return

-- mkOpts md  
--   = do files' <- sortNub . concat <$> mapM getHsTargets (files md) 
--        idirs' <- if null (idirs md) then single <$> getIncludePath else return (idirs md) 
--        return  $ md { files = files' } { idirs = map dropFileName files' ++ idirs' }

