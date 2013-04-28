
import qualified Language.Nano.ESC.ESC       as E
import qualified Language.Nano.Liquid.Liquid as L
import           Language.Nano.CmdLine            (getOpts)
import           Language.Nano.Types

main = do cfg <-  getOpts
          case cfg of 
            Esc {}    -> E.main cfg
            Liquid {} -> L.main cfg

