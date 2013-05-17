-- | Top Level for Refinement Type checker

module Language.Nano.Liquid.Liquid (verifyFile) where


import           Text.PrettyPrint.HughesPJ          (Doc, text, render, ($+$), (<+>))
import           Language.Fixpoint.Interface        (resultExit)
import qualified Language.Fixpoint.Types as F
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.PrettyPrint

--------------------------------------------------------------------------------
verifyFile :: FilePath -> IO (F.FixResult SourcePos)
--------------------------------------------------------------------------------
verifyFile = undefined
