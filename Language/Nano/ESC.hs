{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

-- | Extended Static Checker - Nano

module Language.Nano.ESC (verifyFile) where

import           Text.PrettyPrint.HughesPJ    (text, render, (<+>))
import           System.FilePath              (addExtension)
import           Control.Monad.State
import           Control.Applicative          ((<$>))
import qualified Control.Exception as Ex

import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Syntax
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Interface  (checkValid)
import           Language.Fixpoint.Misc       (sortNub, errorstar)
import           Language.Nano.Types
import           Data.Monoid
import           Data.Maybe                   (isJust) -- fromMaybe, maybe)

--------------------------------------------------------------------------------
-- | Top-level Verifier 
--------------------------------------------------------------------------------
verifyFile :: FilePath -> IO (F.FixResult SourcePos)
--------------------------------------------------------------------------------

verifyFile f 
  = do nano   <- parseNanoFromFile f
       let vc  = genVC nano 
       writeFile (f `addExtension` ".vc") (render $ pp vc)
       rs     <- mapM checkVC $ obligationsVCond vc
       forM rs $ (putStrLn . render . pp)  
       return  $ mconcat rs

--------------------------------------------------------------------------------
-- | Top-level VC Generator 
--------------------------------------------------------------------------------
genVC :: Nano -> VCond
--------------------------------------------------------------------------------
genVC = mconcat . map generateFunVC 


-----------------------------------------------------------------------------------
-- | Top-level SMT Interface 
-----------------------------------------------------------------------------------
checkVC :: (SourcePos, F.Pred) -> IO (F.FixResult SourcePos)
-----------------------------------------------------------------------------------
checkVC z@(loc, _) 
  = do r <- checkVC' z
       putStrLn $ render $ text "checkVC" <+> pp loc <+> pp r
       return r

checkVC' (loc, p) 
  = Ex.catch (checkValid loc xts p) $ \(e :: Ex.IOException) ->
      return $ F.Crash [loc] ("VC crashes fixpoint: " ++ show e )
    where 
      xts = [ (x, F.FInt) | x <- sortNub $ F.syms p ] 

-----------------------------------------------------------------------------------
-- | Verification Condition Generator 
-----------------------------------------------------------------------------------

class IsVerifiable a where 
  generateVC :: a -> VCond -> VCM VCond 

instance IsVerifiable (Fun SourcePos) where 
  generateVC fn _   = return $ generateFunVC fn

instance IsVerifiable a => IsVerifiable [a] where 
  generateVC xs vc  = foldM (\vc s -> generateVC s vc) vc (reverse xs)

instance IsVerifiable (Statement SourcePos) where 
  generateVC        = generateStmtVC

instance IsVerifiable (VarDecl SourcePos) where 
  generateVC (VarDecl _ x (Just e)) vc = return $ generateAsgnVC x e vc
  generateVC (VarDecl _ _ Nothing)  vc = return vc


-----------------------------------------------------------------------------------
generateFunVC :: Fun SourcePos -> VCond 
-----------------------------------------------------------------------------------
generateFunVC fn    = vcTop <> vcSide 
  where 
    (vcTop, vcSide) = runState (generateVC ss vc0) vc0 
    vc0             = mempty
    ss              = fbody fn

-----------------------------------------------------------------------------------
generateStmtVC :: Statement SourcePos -> VCond -> VCM VCond 
-----------------------------------------------------------------------------------


generateStmtVC (EmptyStmt _) vc 
  = return vc

generateStmtVC (ExprStmt _ (AssignExpr _ OpAssign x e)) vc  
  = return $ generateAsgnVC x e vc 

generateStmtVC (BlockStmt _ ss) vc
  = generateVC ss vc

generateStmtVC (IfStmt _ b s1 s2) vc 
  = do vc1     <- generateVC s1 vc 
       vc2     <- generateVC s2 vc
       return   $ ((bp `F.PImp`) <$> vc1) <> ((bp' `F.PImp`) <$> vc2)
    where 
       bp       = F.prop b
       bp'      = F.PNot bp

generateStmtVC (IfSingleStmt l b s) vc
  = generateVC (IfStmt l b s (EmptyStmt l)) vc

generateStmtVC w@(WhileStmt l cond s) vc 
  = do vci'    <- generateVC s vci 
       sideCond $ ((i `pAnd` b)        `F.PImp`) <$> vci' -- require i is inductive 
       sideCond $ ((i `pAnd` F.PNot b) `F.PImp`) <$> vc   -- establish vc at exit 
       return vci                                         -- require i holds on entry
    where 
       b        = F.prop cond
       i        = getInvariant s
       vci      = newVCond l i

generateStmtVC e@(VarDeclStmt l ds) vc
  = generateVC ds vc

generateStmtVC e@(ExprStmt _ (CallExpr _ _ _)) vc
  | isJust $ getAssume e
  = return $ (p `F.PImp`) <$> vc
    where Just p = getAssume e
  
generateStmtVC e@(ExprStmt l (CallExpr _ _ _)) vc
  | isJust $ getAssert e
  = return $ newVCond l p <> vc
    where Just p = getAssert e

generateStmtVC e@(ExprStmt l (CallExpr _ _ _)) vc
  | isSpecification e -- Ignore 
  = return vc

generateStmtVC w _ 
  = convertError "generateStmtVC" w

-----------------------------------------------------------------------------------
-- | `VCM` is a VCGen monad that logs the loop-inv "side conditions" 
-----------------------------------------------------------------------------------

type VCM = State VCond  

-----------------------------------------------------------------------------------
-- | `sideCond vc` adds the goal `vc` to the side-conditions to be checked.
-------------------------------------------------------------------
sideCond     :: VCond -> VCM ()
-------------------------------------------------------------------

sideCond vc' = modify $ mappend vc' 

-------------------------------------------------------------------

generateAsgnVC :: (F.Symbolic x, F.Expression e) => x -> e -> VCond -> VCond 
generateAsgnVC x e vc = (`F.subst1` (F.symbol x, F.expr e)) <$> vc
