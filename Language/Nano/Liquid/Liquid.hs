-- | Top Level for Refinement Type checker

module Language.Nano.Liquid.Liquid (verifyFile) where


import           Text.Parsec.Pos    (initialPos)
import           Text.PrettyPrint.HughesPJ          (Doc, text, render, ($+$), (<+>))

import           Control.Monad
import           Control.Applicative ((<$>), (<*>))
import           Data.Maybe             (fromMaybe, isJust)
import           Data.Monoid            hiding ((<>))            
import           Data.Ord               (comparing) 
import qualified Data.List               as L
import qualified Data.HashMap.Strict     as M
import           Data.Generics.Aliases
import           Data.Generics.Schemes

import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations
import           Language.ECMAScript3.PrettyPrint

import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Misc
import           Language.Fixpoint.PrettyPrint
import           Language.Fixpoint.Interface        (resultExit, solve)

import           Language.Nano.Errors
import           Language.Nano.Types
import           Language.Nano.Typecheck.Types
import           Language.Nano.Liquid.Types


--------------------------------------------------------------------------------
verifyFile :: FilePath -> IO (F.FixResult SourcePos)
--------------------------------------------------------------------------------
verifyFile f = solveConstraints f . generateConstraints =<< mkNano f


--------------------------------------------------------------------------------
mkNano :: FilePath -> IO NanoRefType 
--------------------------------------------------------------------------------
mkNano = error "TOBD"



--------------------------------------------------------------------------------
solveConstraints :: FilePath -> F.FInfo Cinfo -> IO (F.FixResult SourcePos) 
--------------------------------------------------------------------------------
solveConstraints f ci 
  = do (r, sol) <- solve f [] ci
       let r'    = fmap (srcPos . F.sinfo) r
       donePhase (F.colorResult r) (F.showFix r) 
       return r'


--------------------------------------------------------------------------------
generateConstraints :: NanoRefType -> F.FInfo Cinfo 
--------------------------------------------------------------------------------
generateConstraints = error "TOBD"  


