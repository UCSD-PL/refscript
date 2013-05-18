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
generateConstraints = getFInfo pgm $ consNano pgm

--------------------------------------------------------------------------------
consNano     :: NanoRefType -> CGM ()
--------------------------------------------------------------------------------
consNano pgm@(Nano {code = Src fs}) 
  = initCGEnv pgm >>= (forM_ fs . consFun)

initCGEnv    :: NanoRefType -> CGM CGEnv
initEnv pgm  = error "TOBD" -- (\renv -> CGE renv F.emptyIBindEnv [] Nothing) <$> globalEnv info


--------------------------------------------------------------------------------
consFun :: CGEnv -> FunctionStatement AnnType -> CGM ()
--------------------------------------------------------------------------------
consFun γ (FunctionStmt l f xs body) 
  = do (αs, ts, t)    <- funTy l γ f xs
       let γ'          = envAddFun l f αs xs ts t γ 
       γo             <- consStmts γ' body
       case γo of 
         Just γ'      -> subType l "Missing return" γ' rVoid t
         Nothing      -> return ()

--------------------------------------------------------------------------------
consExpr :: CGEnv -> Expression AnnType -> CGM (CGEnv, Id AnnType) 
--------------------------------------------------------------------------------

-- | @consExpr γ e@ returns a pair (γ', x') where
--   x' is a fresh, temporary (A-Normalized) holding the value of `e`,
--   γ' is γ extended with a binding for x' (and other temps required for `e`)

consExpr γ e = error "TOBD"

--------------------------------------------------------------------------------
consStmts :: CGEnv -> [Statement AnnType]  -> CGM (Maybe CGEnv) 
--------------------------------------------------------------------------------
consStmts = consSeq consStmt

--------------------------------------------------------------------------------
consStmt :: CGEnv -> Statement AnnType -> CGM (Maybe CGEnv) 
--------------------------------------------------------------------------------

-- | @consStmt γ s@ returns the environment extended with binders that are
-- due to the execution of statement s. @Nothing@ is returned if the
-- statement has (definitely) hit a `return` along the way.

consStmt γ s = error "TOBD"

-- skip
tcStmt γ (EmptyStmt _) 
  = return $ Just γ

-- x = e
tcStmt γ (ExprStmt _ (AssignExpr l OpAssign (LVar lx x) e))   
  = tcAsgn γ l (Id lx x) e

-- e
tcStmt γ (ExprStmt _ e)   
  = tcExpr γ e >> return (Just γ) 

-- s1;s2;...;sn
tcStmt γ (BlockStmt _ stmts) 
  = tcStmts γ stmts 

-- if b { s1 }
tcStmt γ (IfSingleStmt l b s)
  = tcStmt γ (IfStmt l b s (EmptyStmt l))

-- if b { s1 } else { s2 }
tcStmt γ (IfStmt l e s1 s2)
  = do t     <- tcExpr γ e
       unifyType l "If condition" e t tBool
       γ1    <- tcStmt γ s1
       γ2    <- tcStmt γ s2
       envJoin l γ1 γ2

-- var x1 [ = e1 ]; ... ; var xn [= en];
tcStmt γ (VarDeclStmt _ ds)
  = tcSeq tcVarDecl γ ds

-- return e 
tcStmt γ (ReturnStmt l eo) 
  = do t <- maybe (return tVoid) (tcExpr γ) eo 
       unifyType l "Return" eo t $ envFindReturn γ 
       return Nothing

-- OTHER (Not handled)
tcStmt γ s 
  = convertError "tcStmt" s

-------------------------------------------------------------------------------
tcVarDecl :: Env Type -> VarDecl SourcePos -> TCM TCEnv  
-------------------------------------------------------------------------------

tcVarDecl γ (VarDecl l x (Just e)) 
  = tcAsgn γ l x e  
tcVarDecl γ (VarDecl l x Nothing)  
  = return $ Just γ

------------------------------------------------------------------------------------
tcAsgn :: Env Type -> SourcePos -> Id SourcePos -> Expression SourcePos -> TCM TCEnv
------------------------------------------------------------------------------------

tcAsgn γ l x e 
  = do t <- tcExpr γ e
       return $ Just $ envAdd x t γ

-------------------------------------------------------------------------------
tcExpr :: Env Type -> Expression SourcePos -> TCM Type
-------------------------------------------------------------------------------

tcExpr _ (IntLit _ _)               
  = return tInt 

tcExpr _ (BoolLit _ _)
  = return tBool

tcExpr γ (VarRef l x)
  = maybe (tcError l $ errorUnboundId x) return $ envFindTy x γ

tcExpr γ (PrefixExpr l o e)
  = tcCall γ l o [e] (prefixOpTy o)

tcExpr γ (InfixExpr l o e1 e2)        
  = tcCall γ l o  [e1, e2] (infixOpTy o)

tcExpr γ (CallExpr l e es)
  = tcCall γ l e es =<< tcExpr γ e 

tcExpr γ e 
  = convertError "tcExpr" e

----------------------------------------------------------------------------------
-- tcCall :: Env Type -> SourcePos -> Type -> [Expression SourcePos] -> TCM Type
----------------------------------------------------------------------------------

tcCall γ l z es ft 
  = do (_,its,ot) <- instantiate l ft
       ets        <- mapM (tcExpr γ) es
       θ'         <- unifyTypes l "" its ets
       return      $ apply θ' ot

instantiate l ft 
  = do t' <- freshTyArgs l $ bkAll ft 
       maybe err return   $ bkFun t'
    where
       err = tcError l $ errorNonFunction ft

--------------------------------------------------------------------------------
consSeq :: (CGEnv -> a -> Maybe CGEnv) -> CGEnv -> [a] -> CGEnv (Maybe CGEnv) 
--------------------------------------------------------------------------------
consSeq f             = foldM step . Just 
  where 
    step Nothing _  = return Nothing
    step (Just γ) x = f γ x

------------------------------------------------------------------------------------
-- | This is where subtyping constraints get added ---------------------------------
------------------------------------------------------------------------------------

subTypes :: SourcePos -> String -> CGEnv -> [RefType] -> [RefType] -> CGM () 
subTypes = error "TOBD"

subType  :: SourcePos -> String -> CGEnv -> RefType -> RefType -> CGM ()
subType  = error "TOBD" -- unifyTypes l (errorWrongType m e t t') [t] [t'] >> return ()

