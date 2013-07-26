module Language.Nano.Visitor.Visit (verifyFile) where 

import           Text.PrettyPrint.HughesPJ          (render)

import           Language.Nano.Types
import           Language.Nano.Typecheck.Parse 
import           Language.Nano.SSA.SSA
import           Language.Nano.Visitor.Visitor

import qualified Language.Fixpoint.Types as F
-- import           Language.Fixpoint.Interface        (resultExit)
import           Language.Fixpoint.Misc             
-- import           System.Exit                        (exitWith)
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Parser        (SourceSpan (..))

--------------------------------------------------------------------------------
-- | Top-level Visitor 
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
verifyFile :: FilePath -> IO (F.FixResult (SourceSpan, String))
--------------------------------------------------------------------------------
verifyFile f 
   = do nano <- parseNanoFromFile f 
        {-donePhase Loud "Parse"-}
        {-putStrLn . render . pp $ nano-}
        let nanoSsa = ssaTransform nano
        donePhase Loud "SSA Transform"
        putStrLn . render . pp $ nanoSsa

        nanoVisited <- runVisitProgramM skipVisitorM nanoSsa 
        
        donePhase Loud "After Visit"
        putStrLn . render . pp $ nanoVisited

        return F.Safe 


-- | Define visitor
--skipVisitorM :: NanoVisitorM (IO (F.FixResult SourceSpan)) AnnSSA (RType r)
skipVisitorM = NVM {
     vProgramM       = const SkipChildrenM  
  ,  vStatementM     = const SkipChildrenM
  ,  vJavaScriptM    = const SkipChildrenM
  ,  vIdM            = const SkipChildrenM
  ,  vInfixOpM       = const SkipChildrenM
  ,  vAssignOpM      = const SkipChildrenM
  ,  vUnaryAssignOpM = const SkipChildrenM
  ,  vPrefixOpM      = const SkipChildrenM
  ,  vPropM          = const SkipChildrenM
  ,  vLValueM        = const SkipChildrenM
  ,  vExpressionM    = const SkipChildrenM
  ,  vCaseClauseM    = const SkipChildrenM
  ,  vCatchClauseM   = const SkipChildrenM
  ,  vVarDeclM       = const SkipChildrenM 
  ,  vForInitM       = const SkipChildrenM 
  ,  vForInInitM     = const SkipChildrenM 
  }

