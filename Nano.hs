
import qualified Language.Nano.ESC.ESC              as ESC
import qualified Language.Nano.Typecheck.Typecheck  as TC 
import qualified Language.Nano.Liquid.Liquid        as Liquid 
import           Language.Nano.CmdLine              (getOpts)
import           Language.Nano.Errors
import           Language.Nano.Types
import           Language.Nano.Annots
import           Control.Exception                  (catch)
import           Data.Monoid
import           System.Exit                        (exitWith)
import           Language.Fixpoint.Interface        (resultExit)
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Misc             
import           Language.Fixpoint.Files
-- import           Text.PrettyPrint.HughesPJ          (Doc, text, render, ($+$), (<+>))
import           Text.PrettyPrint.HughesPJ          (render)
import           Language.ECMAScript3.PrettyPrint
import qualified Data.ByteString.Lazy               as B

main = do cfg  <- getOpts
          run (verifier cfg) cfg
    
-------------------------------------------------------------------------------
-- verifier              :: Config -> FilePath -> IO (F.FixResult Error)  
-------------------------------------------------------------------------------
verifier (Esc    {} ) = ESC.verifyFile 
verifier (TC     {} ) = TC.verifyFile
verifier (Liquid {} ) = Liquid.verifyFile

run verifyFile cfg 
  = do rs   <- mapM (runOne verifyFile) $ files cfg
       let r = mconcat rs
       donePhase (F.colorResult r) (render $ pp r) 
       exitWith (resultExit r)

runOne verifyFile f 
  = do (u, r) <- verifyFile f `catch` handler 
       renderAnnotations f r u
       return r
    where
       handler e = return (NoAnn, F.Crash [e] "") 

       -- renderAnnotations f sol r' $ cgi_annot cgi
       -- donePhase (F.colorResult r) (F.showFix r) 
       -- return r'

renderAnnotations _       _   NoAnn 
  = return () 
renderAnnotations srcFile res (SomeAnn ann sol)
  = do writeFile   annFile  $ wrapStars "Constraint Templates" ++ "\n" 
       appendFile  annFile  $ ppshow ann
       appendFile  annFile  $ wrapStars "Inferred Types"       ++ "\n" 
       appendFile  annFile  $ ppshow ann'
       B.writeFile jsonFile $ annotByteString res ann' 
       donePhase Loud "Written Inferred Types"
    where 
       jsonFile = extFileName Json  srcFile
       annFile  = extFileName Annot srcFile
       ann'     = sol ann

