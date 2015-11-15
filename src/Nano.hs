{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}

import qualified Language.Nano.Liquid.Liquid       as LQ
import qualified Language.Nano.Liquid.Types        as L
import qualified Language.Nano.Typecheck.Typecheck as TC

import           System.Console.CmdArgs            hiding (Loud)

import           Control.Exception                 (catch)
import           Control.Monad
import           Data.Aeson                        (eitherDecode)
import           Data.Aeson.Types                  hiding (Error, Parser, parse)
import qualified Data.ByteString.Lazy.Char8        as B
import           Data.List                         (nub, sort)
--import           Language.Fixpoint.Config          ()
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Files
import           Language.Fixpoint.Interface       (resultExit)
import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Types           as F
import           Language.Nano.CmdLine
import           Language.Nano.Errors
import           Language.Nano.Files
import           Language.Nano.Misc                (mapi, single)
import           Language.Nano.Syntax.PrettyPrint
import           Language.Nano.SystemUtils
import           System.Directory                  (createDirectoryIfMissing, doesFileExist)
import           System.Exit
import           System.FilePath.Posix
import           System.Process
import           Text.PrettyPrint.HughesPJ

main :: IO a
main = do cfg  <- cmdArgs config
          run (verifier cfg) cfg

top s invs = run (verifier cfg) cfg
  where
    cfg = def { extraInvs = invs
              , files     = [s]
              }

-------------------------------------------------------------------------------
verifier           :: Config -> FilePath -> IO (UAnnSol L.RefType, F.FixResult Error)
-------------------------------------------------------------------------------
verifier cfg f
  = json cfg f >>= \case
      Left  e     -> return (NoAnn, e)
      Right jsons -> case cfg of
                       TC     {} -> TC.verifyFile cfg   jsons
                       Liquid {} -> LQ.verifyFile cfg f jsons

-------------------------------------------------------------------------------
json :: Config -> FilePath -> IO (Either (F.FixResult Error) [FilePath])
-------------------------------------------------------------------------------
json cfg f = do
  fileExists <- doesFileExist f
  if fileExists then withExistingFile cfg f
                else return $ Left $ F.Crash [] $ "File does not exist: " ++ f


withExistingFile :: Config -> FilePath -> IO (Either (F.FixResult Error) [FilePath])
withExistingFile cfg f
  | ext `elem` oks
  = do  libs              <- getIncludeLibs cfg
        (code, stdOut, _) <- readProcessWithExitCode tsCmd (mkArgs libs) ""
        case code of
          ExitSuccess     -> case eitherDecode (B.pack stdOut) :: Either String [String] of
                                Left  s  -> return $ Left  $ F.UnknownError $ "withExistingFile1: " ++ s
                                Right fs -> return $ Right fs
          ExitFailure _   -> case eitherDecode (B.pack stdOut) :: Either String (F.FixResult Error) of
                                Left  s  -> return $ Left $ F.UnknownError $ "withExistingFile2: " ++ s
                                Right e  -> return $ Left e
  | otherwise
  = return $ Left $ F.Crash [] $ "Unsupported input file format: " ++ ext
  where
    ext            = takeExtension f
    tsCmd          = "tsc"
    oks            = [".ts", ".js"]
    mkArgs libs    = [ "--outDir", tempDirectory f
                     , "--refscript"] ++
                     concatMap (("--lib":) . single) libs ++
                     [ f ]

getIncludeLibs :: Config -> IO [FilePath]
getIncludeLibs cfg = case prelude cfg of
  Nothing -> (\p1 p2 -> [p1, p2]) <$> getPreludeTSPath <*> getDomTSPath
  Just p  -> (: [])               <$> getIncludePath p

instance FromJSON (F.FixResult Error)
instance ToJSON (F.FixResult Error)

instance FromJSON Error
instance ToJSON Error


run verifyFile cfg
  = do mapM_ (createDirectoryIfMissing False. tmpDir) (files cfg)
       rs   <- mapM (runOne cfg verifyFile) $ files cfg
       let r = mconcat rs
       writeResult r
       exitWith (resultExit r)
    where
       tmpDir    = tempDirectory

runOne cfg verifyFile f
  = do createDirectoryIfMissing False tmpDir
       (u, r) <- verifyFile f `catch` handler
       when (renderAnns cfg) $ renderAnnotations f r u
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

writeDoc c (i, d)    = writeBlock c i $ procDoc d
writeBlock _ _ []    = return ()
writeBlock c 0 ss    = forM_ ss (colorPhaseLn c "")
writeBlock _ _ ss    = forM_ ("\n" : ss) putStrLn

procDoc              = mapi pad . filter (not . null . words) . lines . render
  where
    pad 0 x          = x
    pad _ x          = "  " ++ x

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
  = do -- writeFile   annFile  $ wrapStarsWithOptStars False "Constraint Templates" ++ "\n"
       -- appendFile  annFile  $ ppshow ann
       -- appendFile  annFile  $ wrapStarsWithOptStars False "Inferred Types"       ++ "\n"
       -- appendFile  annFile  $ ppshow ann'
       B.writeFile jsonFile $ annotByteString res ann'
       writeFile   vimFile  $ annotVimString res ann'
       donePhase Loud "Written Inferred Annotations"
    where
       jsonFile = extFileName Json  srcFile
       vimFile  = extFileName Annot (srcFile ++ ".vim")
       annFile  = extFileName Annot srcFile
       ann'     = sol ann
