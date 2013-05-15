module Language.Nano.Liquid.Liquid (main) where 

-- import           Control.Applicative                ((<$>))
-- import           Control.Monad                
-- import qualified Data.HashSet as S 
-- import qualified Data.List as L
import           Data.Monoid
import           Language.Nano.Files
import           Language.Nano.Types
import           Language.Nano.Liquid.Types
import           Language.Nano.Liquid.Parse 
import           Language.Nano.Liquid.Typecheck

import           Language.ECMAScript3.Syntax
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Interface        (resultExit)
import           Language.Fixpoint.Misc             
import           Text.PrettyPrint.HughesPJ          (text, render, ($+$) {- , Doc, (<+>) -})
-- import           Text.Printf                        (printf)
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Parser        (parseJavaScriptFromFile)
import           System.Exit                        (exitWith)


main cfg 
  = do rs   <- mapM verifyFile $ files cfg
       let r = mconcat rs
       donePhase (F.colorResult r) (render $ pp r) 
       exitWith (resultExit r)

--------------------------------------------------------------------------------
-- | Top-level Verifier 
--------------------------------------------------------------------------------
verifyFile :: FilePath -> IO (F.FixResult SourcePos)
--------------------------------------------------------------------------------
verifyFile f 
  = do nano <- parseNanoFromFile f 
       putStrLn . render . pp $ nano
       r    <- typeCheck nano
       return r

-------------------------------------------------------------------------------
-- | Parse File and Type Signatures -------------------------------------------
-------------------------------------------------------------------------------

parseNanoFromFile :: FilePath -> IO NanoBare
parseNanoFromFile f 
  = do src   <- parseJavaScriptFromFile f
       spec  <- parseSpecFromFile f
       ispec <- parseSpecFromFile =<< getPreludePath
       return $ either err id (mkNano src (spec `mappend` ispec))
    where 
       err m  = errortext $ text ("Invalid Input file: " ++ f) $+$ m

