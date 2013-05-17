
import qualified Language.Nano.ESC.ESC              as ESC
import qualified Language.Nano.Typecheck.Typecheck  as TC 
import           Language.Nano.CmdLine            (getOpts)
import           Language.Nano.Types

main = do cfg <-  getOpts
          case cfg of 
            Esc {}    -> ESC.main cfg
            Liquid {} -> TC.main cfg

