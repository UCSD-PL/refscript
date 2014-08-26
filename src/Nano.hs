{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE DeriveGeneric             #-}

import qualified Language.Nano.Typecheck.Typecheck  as TC
import qualified Language.Nano.Liquid.Liquid        as LQ
import qualified Language.Nano.Liquid.Types         as L

import           Data.Aeson                         (eitherDecode)
import           Data.Aeson.Types            hiding (Parser, Error, parse)
import           Language.Nano.CmdLine
import           Language.Nano.Errors
import           Language.Nano.Files
import           Language.Nano.SystemUtils
import           Control.Exception                  (catch)
import           Control.Monad
import           Data.Monoid
import           Data.List                          (sort, nub)
import           System.Exit
import           System.Directory                   (createDirectoryIfMissing, doesFileExist)
import           System.Process
import           System.FilePath.Posix
import           Language.Fixpoint.Interface        (resultExit)
import qualified Language.Fixpoint.Types      as    F
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Files
import           Text.PrettyPrint.HughesPJ
import           Language.ECMAScript3.PrettyPrint
import qualified Data.ByteString.Lazy.Char8   as    B

-- import           Debug.Trace                        (trace)

main = do cfg  <- getOpts
          run (verifier cfg) cfg

-------------------------------------------------------------------------------
verifier           :: Config -> FilePath -> IO (UAnnSol L.RefType, F.FixResult Error)
-------------------------------------------------------------------------------
verifier cfg f 
  = do  z             <- json f
        case z of
          Left  e     -> return (NoAnn, e)
          Right jsons -> case cfg of
                        TC     {} -> TC.verifyFile   jsons
                        Liquid {} -> LQ.verifyFile f jsons

-------------------------------------------------------------------------------
json :: FilePath -> IO (Either (F.FixResult Error) [FilePath])
-------------------------------------------------------------------------------
json f = do fileExists <- doesFileExist f
            if fileExists then withExistingFile f
                          else return $ Left $ F.Crash [] $ "File does not exist: " ++ f

withExistingFile f 
  | ext `elem` oks 
  = do  preludeTSPath     <- getPreludeTSPath 
        (code, stdOut, _) <- readProcessWithExitCode tsCmd (mkArgs preludeTSPath) ""
        case code of 
          ExitSuccess     -> case eitherDecode (B.pack stdOut) :: Either String [String] of
                                Left  s  -> return $ Left  $ F.UnknownError s
                                Right fs -> return $ Right $ fs
          ExitFailure _   -> case eitherDecode (B.pack stdOut) :: Either String (F.FixResult Error) of
                                Left  s  -> return $ Left $ F.UnknownError s
                                Right e  -> return $ Left $ e
  | otherwise
  = return $ Left $ F.Crash [] $ "Unsupported input file format: " ++ ext
  where 
    ext            = takeExtension f
    tsCmd          = "tsc" 
    oks            = [".ts", ".js"]
    mkArgs pre     = [ "--outDir", tempDirectory f
                     , "--refscript"
                     , "--lib", pre
                     , f ]


instance FromJSON (F.FixResult Error)
instance ToJSON (F.FixResult Error)

instance FromJSON Error
instance ToJSON Error 

instance FromJSON SrcSpan
instance ToJSON SrcSpan


run verifyFile cfg 
  = do mapM_ (createDirectoryIfMissing False. tmpDir) (files cfg)
       rs   <- mapM (runOne verifyFile) $ files cfg
       let r = mconcat rs
       writeResult r
       exitWith (resultExit r)
    where
       tmpDir    = tempDirectory

runOne verifyFile f
  = do createDirectoryIfMissing False tmpDir
       (u, r) <- verifyFile f `catch` handler
       renderAnnotations f r u
       return r
    where
       handler e = return (NoAnn, F.Unsafe [e])
       tmpDir    = tempDirectory f


-------------------------------------------------------------------------------
writeResult :: (Ord a, PP a) => F.FixResult a -> IO ()
-------------------------------------------------------------------------------
writeResult r            = mapM_ (writeDoc c) $ zip [0..] $ resDocs r
  where 
    c                    = F.colorResult r
    writeDoc c (i, d)    = writeBlock c i $ lines $ render d
    writeBlock _ _ []    = return ()
    writeBlock c 0 ss    = forM_ ss (colorPhaseLn c "")
    writeBlock _ _ ss    = forM_ ("\n" : ss) putStrLn

resDocs F.Safe             = [text "SAFE"]
resDocs (F.Crash xs s)     = text ("CRASH: " ++ s) : pprManyOrdered xs
resDocs (F.Unsafe xs)      = text "UNSAFE"         : pprManyOrdered (nub xs)
resDocs (F.UnknownError d) = [text "PANIC: Unexpected Error: " <+> text d, reportUrl]
reportUrl                  = text "Please submit a bug report at: https://github.com/ucsd-pl/RefScript"

pprManyOrdered = map pp . sort


----------------------------------------------------------------------------------
renderAnnotations :: PP t => FilePath -> F.FixResult Error -> UAnnSol t -> IO ()
----------------------------------------------------------------------------------
renderAnnotations srcFile res (NoAnn :: UAnnSol t)
  = do B.writeFile jsonFile    $ annotByteString res (mempty :: UAnnInfo t)
       writeFile   vimFile     $ annotVimString res (mempty :: UAnnInfo t)
    where
       jsonFile = extFileName Json  srcFile
       vimFile  = extFileName Annot (srcFile ++ ".vim")

renderAnnotations srcFile res (SomeAnn ann sol)
  = do writeFile   annFile  $ wrapStarsWithOptStars False "Constraint Templates" ++ "\n"
       appendFile  annFile  $ ppshow ann
       appendFile  annFile  $ wrapStarsWithOptStars False "Inferred Types"       ++ "\n"
       appendFile  annFile  $ ppshow ann'
       B.writeFile jsonFile $ annotByteString res ann'
       writeFile   vimFile  $ annotVimString res ann'
       donePhaseWithOptStars False Loud "Written Inferred Annotations"
    where
       jsonFile = extFileName Json  srcFile
       vimFile  = extFileName Annot (srcFile ++ ".vim")
       annFile  = extFileName Annot srcFile
       ann'     = sol ann

-- Local Variables:
-- flycheck-disabled-checkers: (haskell-liquid)
-- End:
