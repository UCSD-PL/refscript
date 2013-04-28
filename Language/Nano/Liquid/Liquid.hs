module Language.Nano.Liquid.Liquid (main) where 

import           Control.Monad                (mapM)
import           Data.Monoid
import           Language.Nano.Types
import           Language.ECMAScript3.Syntax
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Interface  (checkValid, resultExit)
import           Language.Fixpoint.Misc       (safeZip, sortNub, donePhase)
import           Text.PrettyPrint.HughesPJ    (text, render, (<+>))
import           Language.ECMAScript3.PrettyPrint
import           System.Exit                  (exitWith)

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
  = do nano <- parseLiquidFromFile f 
       forM_ nano (putStrLn . render . pp)
       return F.Safe 



