import Control.Monad                (mapM)
import Language.Nano.CmdLine        (getOpts)
import Language.Fixpoint.Types
import Language.Fixpoint.Misc
import Language.Nano.ESC            (verifyFile)
import Data.Monoid                  (mconcat)

main = do cfg <- getOpts  
          r   <- mconcat <$> mapM verifyFile $ files cfg
          donePhase (colorResult r) (showFix r) 
          exitWith (resultExit r)
