
import qualified Language.Nano.ESC.ESC              as ESC
import qualified Language.Nano.Typecheck.Typecheck  as TC 
import qualified Language.Nano.Liquid.Liquid        as Liquid 
import qualified Language.Nano.Visitor.Visit        as Visit
import           Language.Nano.CmdLine              (getOpts)
import           Language.Nano.Types
import           Language.Nano.Misc
import           Data.Monoid
import           System.Exit                        (exitWith)
import           Language.Fixpoint.Interface        (resultExit)
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Misc             
-- import           Text.PrettyPrint.HughesPJ          (Doc, text, render, ($+$), (<+>))
import           Text.PrettyPrint.HughesPJ          (render)
import           Language.ECMAScript3.PrettyPrint
import           System.Console.CmdArgs.Verbosity as V

main = do cfg  <- getOpts
          run (verifier cfg) cfg
      
verifier c@(Esc    {} ) = ESC.verifyFile
verifier c@(TC     {} ) = TC.verifyFile c
verifier c@(Liquid {} ) = Liquid.verifyFile
verifier c@(Visit  {} ) = Visit.verifyFile

run verifyFile cfg 
  = do rs   <- mapM verifyFile $ files cfg
       let r = mconcat rs
       donePhase (F.colorResult r) (render $ pp r) 
       exitWith (resultExit r)
       
