{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}

import           Control.Exception              (catch)
import           Control.Monad
import           Data.Aeson                     (eitherDecode, encode)
import           Data.Aeson.Types               hiding (Error, Parser, parse)
import qualified Data.ByteString.Lazy.Char8     as B
import           Data.List                      (nub, sort)
import           GHC.Generics                   (Generic)
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Solver       (resultExit)
import qualified Language.Fixpoint.Types        as F
import           Language.Fixpoint.Types.Errors
import           Language.Fixpoint.Utils.Files
import           Language.Rsc.CmdLine
import           Language.Rsc.Core.Files
import qualified Language.Rsc.Liquid.Checker    as LQ
import qualified Language.Rsc.Liquid.Types      as L
import           Language.Rsc.Misc              (mapi, single, (<//>))
import           Language.Rsc.Pretty
import           Language.Rsc.SystemUtils
import qualified Language.Rsc.Typecheck.Checker as TC
import           System.Console.CmdArgs         hiding (Loud)
import           System.Directory               (createDirectoryIfMissing,
                                                 doesFileExist)
import           System.IO                      (hPutStrLn, stderr)

import           System.Exit
import           System.FilePath.Posix
import           System.Process
import           Text.PrettyPrint.HughesPJ

main :: IO a
main = do cfg  <- cmdArgs config
          run (verifier cfg) cfg

top dbg s = run (verifier cfg) cfg
  where
    cfg = def { files     = [s]
              , dumpDebug = dbg
              }


-------------------------------------------------------------------------------
verifier :: Config -> FilePath -> IO (UAnnSol L.RefType, F.FixResult Error)
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
        tsBin             <- getTSBindPath
        (code, stdOut, _) <- readProcessWithExitCode "node" (mkArgs tsBin libs) ""
        case code of
          ExitSuccess     -> case eitherDecode (B.pack stdOut) :: Either String [String] of
                                Left  s  -> return $ Left $ F.Crash [] $ s <//> stdOut
                                Right fs -> return $ Right fs
          ExitFailure _   -> case eitherDecode (B.pack stdOut) :: Either String (F.FixResult SError) of
                                Left  s  -> return $ Left $ F.Crash [] $ s <//> stdOut
                                Right e  -> return $ Left $ toErr e
  | otherwise
  = return $ Left $ F.Crash [] $ "Unsupported input file format: " ++ ext
  where
    ext            = takeExtension f
    oks            = [".ts", ".js"]
    mkArgs ts libs = [ ts
                     , "--outDir", tempDirectory f
                     , "--module", moduleKind
                     , "--refscript", "cmdline"
                     ] ++
                     concat [ ["--lib", lib] | lib <- libs ] ++
                     [ f ]
    moduleKind      = "commonjs" -- also 'amd', 'system', 'umd'


getIncludeLibs :: Config -> IO [FilePath]
getIncludeLibs cfg = case prelude cfg of
  Nothing -> single <$> getPreludeTSPath
  Just p  -> return [p]


data SError = SError { errLoc :: SrcSpan
                     , errMsg :: String }
          deriving (Eq, Ord, Show, Generic)

toErr :: F.FixResult SError -> F.FixResult Error
toErr = fmap (\(SError l s) -> err l (pp s))


instance FromJSON SError
instance ToJSON SError where
  toJSON = genericToJSON defaultOptions

instance FromJSON (F.FixResult SError)
instance ToJSON (F.FixResult SError) where
  toJSON = genericToJSON defaultOptions

instance ToJSON F.Error where
  toJSON = genericToJSON defaultOptions

instance ToJSON F.Error1 where
  toJSON = genericToJSON defaultOptions

instance ToJSON Doc where
  toJSON = genericToJSON defaultOptions . show

-- instance ToJSON TextDetails where
--   toJSON = genericToJSON defaultOptions



run verifyFile cfg
  = do mapM_ (createDirectoryIfMissing False. tmpDir) (files cfg)
       rs   <- mapM (runOne cfg verifyFile) $ files cfg
       let r = mconcat rs
       writeResult r
       when (dumpJson cfg) (writeJSONResult r)
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

-- Write the JSON in the error stream
writeJSONResult       = hPutStrLn stderr . B.unpack . encode

-------------------------------------------------------------------------------
writeResult :: (Ord a, PP a) => F.FixResult a -> IO ()
-------------------------------------------------------------------------------
writeResult r         = mapM_ (writeDoc c) $ zip [0..] $ resDocs r
  where
    c                 = F.colorResult r

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
reportUrl                  = text "Please submit a bug report at: https://github.com/ucsd-pl/refscript"

pprManyOrdered = map pp . sort

----------------------------------------------------------------------------------
renderAnnotations :: PP t => FilePath -> F.FixResult Error -> UAnnSol t -> IO ()
----------------------------------------------------------------------------------
renderAnnotations srcFile res (NoAnn :: UAnnSol t)
  = do B.writeFile jsonFile $ annotByteString res (mempty :: UAnnInfo t)
       writeFile   vimFile  $ annotVimString res (mempty :: UAnnInfo t)
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
       -- annFile  = extFileName Annot srcFile
       ann'     = sol ann
