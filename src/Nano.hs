
import qualified Language.Nano.ESC.ESC              as ESC
import qualified Language.Nano.Typecheck.Typecheck  as TC 
import qualified Language.Nano.Liquid.Liquid        as Liquid 
-- import qualified Language.TypeScript.Parse          as TS 

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
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Files
-- import           Text.PrettyPrint.HughesPJ          (Doc, text, render, ($+$), (<+>))
import           Text.PrettyPrint.HughesPJ          (render)
import           Language.ECMAScript3.PrettyPrint
import qualified Data.ByteString.Lazy               as B

main = do cfg  <- getOpts
          run (verifier cfg) cfg
    
-------------------------------------------------------------------------------
-- verifier           :: Config -> FilePath -> IO (F.FixResult Error)  
-------------------------------------------------------------------------------
verifier (Esc    {} ) = ESC.verifyFile 
verifier (TC     {} ) = TC.verifyFile
verifier (Liquid {} ) = Liquid.verifyFile
-- verifier (TS {})      = tsVerifyFile


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

----------------------------------------------------------------------------------
renderAnnotations :: (PP t) => FilePath -> F.FixResult Error -> UAnnSol t -> IO () 
----------------------------------------------------------------------------------

renderAnnotations srcFile res (NoAnn :: UAnnSol t)
  = B.writeFile jsonFile    $ annotByteString res (mempty :: UAnnInfo t) 
    where 
       jsonFile = extFileName Json  srcFile
    
renderAnnotations srcFile res (SomeAnn ann sol)
  = do writeFile   annFile  $ wrapStars "Constraint Templates" ++ "\n" 
       appendFile  annFile  $ ppshow ann
       appendFile  annFile  $ wrapStars "Inferred Types"       ++ "\n" 
       appendFile  annFile  $ ppshow ann'
       B.writeFile jsonFile $ annotByteString res ann' 
       donePhase Loud "Written Inferred Annotations"
    where 
       jsonFile = extFileName Json  srcFile
       annFile  = extFileName Annot srcFile
       ann'     = sol ann

----------------------------------------------------------------------------------
-- tsVerifyFile :: Config -> FilePath -> IO (F.FixResult Error)  
----------------------------------------------------------------------------------
-- tsVerifyFile f 
--   = do pgm <- TS.parseTypeScript f
--        putStrLn $ "******************* Converted TS TO ***************************"
--        putStrLn $ render $ javaScript pgm
--        putStrLn $ "***************************************************************"
--        return (NoAnn, F.Safe)
