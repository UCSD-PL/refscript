import Control.Monad                    (mapM)
import Language.Nano.CmdLine            (getOpts)
import Language.Fixpoint.Types
import Language.Fixpoint.Misc
import Language.Fixpoint.Interface      (resultExit)
import Language.ECMAScript3.PrettyPrint (pp)
import Text.PrettyPrint.HughesPJ        (render)
import Language.Nano.Types          
import Language.Nano.ESC                (verifyFile)
import Data.Monoid                      (mconcat)
import Control.Applicative              ((<$>))
import System.Exit                      (exitWith)

main = do cfg <-  getOpts  
          rs   <- mapM verifyFile $ files cfg
          let r = mconcat rs
          donePhase (colorResult r) (render $ pp r) 
          exitWith (resultExit r)
