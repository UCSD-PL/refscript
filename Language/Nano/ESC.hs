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
genVC = mconcat . map generateFunVC . functions 


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
generateFunVC :: Fun SourcePos -> VCond 
-----------------------------------------------------------------------------------

generateFunVC fn    = vcTop <> vcSide 
  where 
    (vcTop, vcSide) = runState (generateBlockVC ss vc0) vc0 
    vc0             = mempty
    ss              = fbody fn

-----------------------------------------------------------------------------------
generateBlockVC :: [Statement SourcePos] -> VCond -> VCM VCond 
-----------------------------------------------------------------------------------

generateBlockVC ss vc
  = foldM (\vc s -> generateVC s vc) vc (reverse ss)

-----------------------------------------------------------------------------------
generateVC :: Statement SourcePos -> VCond -> VCM VCond 
-----------------------------------------------------------------------------------

generateVC (EmptyStmt _) vc 
  = return vc

generateVC (ExprStmt _ (AssignExpr _ OpAssign x e)) vc  
  = return $ (`F.subst1` (F.symbol x, F.expr e)) <$> vc

generateVC (BlockStmt _ ss) vc
  = generateBlockVC ss vc

generateVC (IfStmt _ b s1 s2) vc 
  = do vc1     <- generateVC s1 vc 
       vc2     <- generateVC s2 vc
       return   $ ((bp `F.PImp`) <$> vc1) <> ((bp' `F.PImp`) <$> vc2)
    where 
       bp       = F.prop b
       bp'      = F.PNot bp

generateVC (IfSingleStmt l b s) vc
  = generateVC (IfStmt l b s (EmptyStmt l)) vc

generateVC w@(WhileStmt l _ _) vc 
  = do vci'     <- generateVC s vci 
       sideCond $ ((i `pAnd` b)        `F.PImp`) <$> vci' -- require i is inductive 
       sideCond $ ((i `pAnd` F.PNot b) `F.PImp`) <$> vc   -- establish vc at exit 
       return vci                                         -- require i holds on entry
    where 
       (b, i, s) = splitWhileStmt w
       vci       = newVCond l i

generateVC e@(ExprStmt _ (CallExpr _ _ _)) vc
  | isJust $ getAssume e
  = return $ (p `F.PImp`) <$> vc
    where Just p = getAssume e
  
generateVC e@(ExprStmt l (CallExpr _ _ _)) vc
  | isJust $ getAssert e
  = return $ newVCond l p <> vc
    where Just p = getAssert e

generateVC w _ 
  = convertError "generateVC" w

-----------------------------------------------------------------------------------
-- | Helpers for extracting specifications from @ECMAScript3@ @Statement@ 
-----------------------------------------------------------------------------------

-- Ideally, a la JML, we'd modify the parser to take in annotations for 
-- 
--   * assert(p)
--   * assume(p)
--   * invariant(p) 
--
-- For now, we hack them with function calls.

splitWhileStmt :: (Statement a) -> (F.Pred, F.Pred, Statement a)
splitWhileStmt (WhileStmt _ b s) = (cond, invariant, body)
  where 
    cond                         = F.prop b
    (invariant, body)            = splitWhileBody s

splitWhileStmt _                 = errorstar "Unexpected call to splitWhileStmt"

splitWhileBody :: Statement a   -> (F.Pred, Statement a)
splitWhileBody s@(BlockStmt l (si : ss)) 
  = case getInvariant si of 
      Just i  -> (F.prop i, BlockStmt l ss)
      Nothing -> (F.PTrue , s             )

splitWhileBody s
  = (F.PTrue, s)

getAssume    = getStatementPred "assume"
getAssert    = getStatementPred "assert"
getInvariant = getStatementPred "invariant"

getStatementPred :: String -> Statement a -> Maybe F.Pred 
getStatementPred name (ExprStmt _ (CallExpr _ (VarRef _ (Id _ f)) [p]))
  | name == f 
  = Just $ F.prop p
getStatementPred _ _ 
  = Nothing 


-----------------------------------------------------------------------------------
-- | `VCM` is a VCGen monad that logs the loop-inv "side conditions" 
-----------------------------------------------------------------------------------

type VCM = State VCond  

-----------------------------------------------------------------------------------
-- | `sideCond vc` adds the goal `vc` to the side-conditions to be checked.
-----------------------------------------------------------------------------------
sideCond     :: VCond -> VCM ()
-----------------------------------------------------------------------------------

sideCond vc' = modify $ mappend vc' 


