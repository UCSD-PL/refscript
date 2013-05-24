
import qualified Language.Nano.ESC.ESC              as ESC
import qualified Language.Nano.Typecheck.Typecheck  as TC 
import qualified Language.Nano.Liquid.Liquid        as Liquid 
import           Language.Nano.CmdLine              (getOpts)
import           Language.Nano.Types
import           Data.Monoid
import           System.Exit                        (exitWith)
import           Language.Fixpoint.Interface        (resultExit)
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Misc             
-- import           Text.PrettyPrint.HughesPJ          (Doc, text, render, ($+$), (<+>))
import           Text.PrettyPrint.HughesPJ          (render)
import           Language.ECMAScript3.PrettyPrint

main = do cfg  <- getOpts
          run (verifier cfg) cfg
      
verifier (Esc {})    = ESC.verifyFile
verifier (TC {})     = TC.verifyFile
verifier (Liquid {}) = Liquid.verifyFile

run verifyFile cfg 
  = do rs   <- mapM verifyFile $ files cfg
       let r = mconcat rs
       donePhase (F.colorResult r) (render $ pp r) 
       exitWith (resultExit r)
