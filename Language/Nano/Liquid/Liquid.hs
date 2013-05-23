{-# LANGUAGE OverlappingInstances #-}

-- | Top Level for Refinement Type checker

module Language.Nano.Liquid.Liquid (verifyFile) where

import           Text.Printf                        (printf)
import           Text.Parsec.Pos                    (initialPos)
import           Text.PrettyPrint.HughesPJ          (Doc, text, render, ($+$), (<+>))
import           Control.Monad
import           Control.Applicative                ((<$>), (<*>))
import           Data.Maybe                         (fromMaybe, fromJust, isJust)
import           Data.Monoid                 hiding ((<>))            
import           Data.Ord                           (comparing) 
import qualified Data.List                   as L
import qualified Data.HashMap.Strict         as M
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
import           Language.Nano.Typecheck.Parse
import           Language.Nano.Typecheck.Typecheck  (typeCheck) 
import           Language.Nano.Typecheck.SSA

import qualified Language.Nano.Env as E 
import           Language.Nano.Liquid.Types
import           Language.Nano.Liquid.CGMonad

--------------------------------------------------------------------------------
verifyFile     :: FilePath -> IO (F.FixResult SourcePos)
--------------------------------------------------------------------------------
verifyFile f   = reftypeCheck f . typeCheck . ssaTransform' =<< parseNanoFromFile f

ssaTransform' x = {- tracePP "SSATX" $ -} ssaTransform x 

reftypeCheck   :: FilePath -> Nano AnnType RefType -> IO (F.FixResult SourcePos)
reftypeCheck f = solveConstraints f . generateConstraints  

--------------------------------------------------------------------------------
solveConstraints :: FilePath -> F.FInfo Cinfo -> IO (F.FixResult SourcePos) 
--------------------------------------------------------------------------------
solveConstraints f ci 
  = do (r, sol) <- solve f [] ci
       let r'    = fmap (srcPos . F.sinfo) r
       donePhase (F.colorResult r) (F.showFix r) 
       return r'

--------------------------------------------------------------------------------
generateConstraints     :: NanoRefType -> F.FInfo Cinfo 
--------------------------------------------------------------------------------
generateConstraints pgm = getFInfo pgm $ consNano pgm

--------------------------------------------------------------------------------
consNano     :: NanoRefType -> CGM ()
--------------------------------------------------------------------------------
consNano pgm@(Nano {code = Src fs}) 
  = forM_ fs . consFun =<< initCGEnv pgm

initCGEnv     :: NanoRefType -> CGM CGEnv
initCGEnv pgm@(Nano {code = Src fs}) 
  = do ts'    <- forM fts (uncurry $ freshTyFun g0)
       return  $ g0 { renv = E.envAdds (zip fns ts') γ} 
    where 
       g0      = CGE γ F.emptyIBindEnv []
       γ       = env pgm
       fns     = [fn | (FunctionStmt _ fn _ _) <- fs]
       fts     = [(fn, initFunTy fn γ) | fn <- fns]

initFunTy fn γ = fromMaybe err $ E.envFindTy fn γ 
  where 
    err        = errorstar $ bugUnboundVariable (srcPos fn) fn


--------------------------------------------------------------------------------
consFun :: CGEnv -> FunctionStatement AnnType -> CGM ()
--------------------------------------------------------------------------------
consFun g (FunctionStmt l f xs body) 
  = do ft             <- {- TODO: NESTED FUNCTIONS freshTyFun g l $ -} return $ envFindTy f g
       let (αs, ts, t) = fromJust $ bkFun ft
       g'             <- envAddFun l g f αs xs ts t
       gm             <- consStmts g' body
       case gm of 
         Just g'      -> subType l g' tVoid t
         Nothing      -> return ()

envAddFun l g f αs xs ts t = envAdds tyBinds =<< envAdds (varBinds xs ts') =<< (return $ envAddReturn f t' g) 
  where  
    tyBinds                = [(Loc (srcPos l) α, tVar α) | α <- αs]
    varBinds               = safeZip "envAddFun"
    (ts', t')              = renameArgs xs ts t 

renameArgs xs ts t = (ts', t')
  where 
    (su, ts')      = shiftVVs ts xs
    t'             = F.subst su t

checkFormal x t 
  | xsym == tsym = (x, t)
  | otherwise    = errorstar $ errorArgName (srcPos x) xsym tsym
  where 
    xsym         = F.symbol x
    tsym         = F.symbol t

--------------------------------------------------------------------------------
consStmts :: CGEnv -> [Statement AnnType]  -> CGM (Maybe CGEnv) 
--------------------------------------------------------------------------------
consStmts = consSeq consStmt

--------------------------------------------------------------------------------
consStmt :: CGEnv -> Statement AnnType -> CGM (Maybe CGEnv) 
--------------------------------------------------------------------------------

-- | @consStmt g s@ returns the environment extended with binders that are
-- due to the execution of statement s. @Nothing@ is returned if the
-- statement has (definitely) hit a `return` along the way.

-- skip
consStmt g (EmptyStmt _) 
  = return $ Just g

-- x = e
consStmt g (ExprStmt _ (AssignExpr l OpAssign (LVar lx x) e))   
  = consAsgn g (Id lx x) e

-- e
consStmt g (ExprStmt _ e)   
  = consExpr g e >> return (Just g) 

-- s1;s2;...;sn
consStmt g (BlockStmt _ stmts) 
  = consStmts g stmts 

-- if b { s1 }
consStmt g (IfSingleStmt l b s)
  = consStmt g (IfStmt l b s (EmptyStmt l))

-- HINT: 1. Use @envAddGuard True@ and @envAddGuard False@ to add the binder 
--          from the condition expression @e@ into @g@ to obtain the @CGEnv@ 
--          for the "then" and "else" statements @s1@ and @s2 respectively. 
--       2. Recursively constrain @s1@ and @s2@ under the respective environments.
--       3. Combine the resulting environments with @envJoin@ 

-- if e { s1 } else { s2 }
consStmt g (IfStmt l e s1 s2)
  = do (xe, ge) <- consExpr g e
       g1'      <- (`consStmt` s1) $ envAddGuard xe True  ge 
       g2'      <- (`consStmt` s2) $ envAddGuard xe False ge 
       envJoin l g g1' g2'

-- var x1 [ = e1 ]; ... ; var xn [= en];
consStmt g (VarDeclStmt _ ds)
  = consSeq consVarDecl g ds

-- return e 
consStmt g (ReturnStmt l (Just e))
  = do (xe, g') <- consExpr g e 
       subType l g' (envFindTy xe g') $ envFindReturn g' 
       return Nothing

-- return
consStmt g (ReturnStmt l Nothing)
  = return Nothing 

-- OTHER (Not handled)
consStmt g s 
  = errorstar $ "consStmt: not handled " ++ ppshow s

----------------------------------------------------------------------------------
envJoin :: AnnType -> CGEnv -> Maybe CGEnv -> Maybe CGEnv -> CGM (Maybe CGEnv)
----------------------------------------------------------------------------------
envJoin _ _ Nothing x           = return x
envJoin _ _ x Nothing           = return x
envJoin l g (Just g1) (Just g2) = Just <$> envJoin' l g g1 g2 

----------------------------------------------------------------------------------
envJoin' :: AnnType -> CGEnv -> CGEnv -> CGEnv -> CGM CGEnv
----------------------------------------------------------------------------------

-- HINT: 1. use @envFindTy@ to get types for the phi-var x in environments g1 AND g2
--       2. use @freshTy@ to generate fresh types (and an extended environment with 
--          the fresh-type bindings) for all the phi-vars using the un-refined types 
--          from step 1.
--       3. generate subtyping constraints between the types from step 1 and the fresh types
--       4. return the extended environment.

envJoin' l g g1 g2
  = do let xs   = [x | PhiVar x <- ann_fact l] 
       let t1s  = (`envFindTy` g1) <$> xs 
       let t2s  = (`envFindTy` g2) <$> xs
       (g',ts) <- freshTyPhis (srcPos l) g xs $ map toType t1s -- SHOULD BE SAME as t2s 
       subTypes l g1 xs ts
       subTypes l g2 xs ts
       return g'

------------------------------------------------------------------------------------
consVarDecl :: CGEnv -> VarDecl AnnType -> CGM (Maybe CGEnv) 
------------------------------------------------------------------------------------

consVarDecl g (VarDecl l x (Just e)) 
  = consAsgn g x e  

consVarDecl g (VarDecl l x Nothing)  
  = return $ Just g

------------------------------------------------------------------------------------
consAsgn :: CGEnv -> Id AnnType -> Expression AnnType -> CGM (Maybe CGEnv) 
------------------------------------------------------------------------------------
consAsgn g x e 
  = do (x', g') <- consExpr g e
       Just <$> envAdds [(x, envFindTy x' g')] g'

------------------------------------------------------------------------------------
consExpr :: CGEnv -> Expression AnnType -> CGM (Id AnnType, CGEnv) 
------------------------------------------------------------------------------------

-- | @consExpr g e@ returns a pair (g', x') where
--   x' is a fresh, temporary (A-Normalized) holding the value of `e`,
--   g' is g extended with a binding for x' (and other temps required for `e`)

consExpr g (IntLit l i)               
  = envAddFresh l (eSingleton tInt i) g

consExpr g (BoolLit l b)
  = envAddFresh l (pSingleton tBool b) g 

consExpr g (VarRef l x)
  = return (x, g) 

consExpr g (PrefixExpr l o e)
  = do (x', g') <- consCall g l o [e] (prefixOpTy o $ renv g)
       return (x', g')

consExpr g (InfixExpr l o e1 e2)        
  = do (x', g') <- consCall g l o [e1, e2] (infixOpTy o $ renv g)
       return (x', g')

consExpr g (CallExpr l e es)
  = do (x, g') <- consExpr g e 
       consCall g' l e es $ envFindTy x g'

consExpr g e 
  = errorstar "consExpr: not handled" (pp e)


----------------------------------------------------------------------------------
-- consCall :: Env Type -> SourcePos -> Type -> [Expression SourcePos] -> TCM Type
----------------------------------------------------------------------------------

-- HINT: This code is almost isomorphic to the version in 
--   @Liquid.Nano.Typecheck.Typecheck@ except we use subtyping
--   instead of unification.
--
--   1. Fill in @instantiate@ to get a monomorphic instance of @ft@ 
--      i.e. the callee's RefType, at this call-site
--   2. Use @consExpr@ to determine types for arguments @es@
--   3. Use @subTypes@ to add constraints between the types from (step 2) and (step 1)
--   4. Use the @F.subst@ returned in 3. to substitute formals with actuals in output type of callee.

consCall g l z es ft 
  = do (_,its,ot) <- instantiate l g ft
       (xes, g')  <- consScan consExpr g es 
       θ          <- subTypes l g' xes its 
       envAddFresh l (F.subst ({- F.traceFix (printf "consCall-SUBST %s %s" (ppshow xes) (ppshow its)) -} θ) ot) g'

instantiate l g t = fromJust . bkFun <$> freshTyInst l g αs τs tbody 
  where 
    (αs, tbody)   = bkAll t
    τs            = getTypArgs l αs 

getTypArgs :: AnnType -> [TVar] -> [Type] 
getTypArgs l αs
  = case [i | TypInst i <- ann_fact l] of 
      [i] | length i == length αs -> i 
      _                           -> errorstar $ bugMissingTypeArgs $ srcPos l

---------------------------------------------------------------------------------
consScan :: (CGEnv -> a -> CGM (b, CGEnv)) -> CGEnv -> [a] -> CGM ([b], CGEnv)
---------------------------------------------------------------------------------
consScan step g xs  = go g [] xs 
  where 
    go g acc []     = return (reverse acc, g)
    go g acc (x:xs) = do (y, g') <- step g x
                         go g' (y:acc) xs

---------------------------------------------------------------------------------
consSeq  :: (CGEnv -> a -> CGM (Maybe CGEnv)) -> CGEnv -> [a] -> CGM (Maybe CGEnv) 
---------------------------------------------------------------------------------
consSeq f           = foldM step . Just 
  where 
    step Nothing _  = return Nothing
    step (Just g) x = f g x

