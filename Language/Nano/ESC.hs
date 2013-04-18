{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

-- | Extended Static Checker - Nano

module Language.Nano.ESC (verifyFile) where


import           Control.Monad.State
import           Control.Applicative          ((<$>))
import qualified Control.Exception as Ex
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Parser  (parseJavaScriptFromFile)
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Interface  (checkValid)
import           Language.Fixpoint.Misc       (errorstar)
import           Language.Nano.Types
import           Data.Monoid
-- import           Data.Maybe                   (fromMaybe, maybe)

------------------------------------------------------------------
-- | Top-level Verifier ------------------------------------------
------------------------------------------------------------------

verifyFile :: FilePath -> IO (F.FixResult SourcePos)
verifyFile f 
  = do s <- parseJavaScriptFromFile f
       if isNano s 
         then verifyNano s
         else return $ F.Crash [] ("Invalid Input File: " ++ f) 

verifyNano :: Nano -> IO (F.FixResult SourcePos)
verifyNano = fmap mconcat . mapM checkVC . genVC


genVC :: Nano -> [(SourcePos, F.Pred)] 
genVC _ = undefined 
  -- runState (generateVC mempty p)


checkVC :: (a, F.Pred) -> IO (F.FixResult a)
checkVC (x, p) = Ex.catch (checkValid x undefined p) $ \(e :: Ex.IOException) ->
                   return $ F.Crash [x] ("VC crashes fixpoint: " ++ show e )
------------------------------------------------------------------
-- | We will use the State monad to log all the individual "side" 
--   queries that arise due to checking of loop invariants.
------------------------------------------------------------------

type VCM = State VCond  

-- | `sideCond vc` adds the goal `vc` to the side-conditions to be checked.

------------------------------------------------------------------
sideCond     :: VCond -> VCM ()
------------------------------------------------------------------

sideCond vc' = modify $ mappend vc' 

------------------------------------------------------------------
-- | Verification Condition Generator ----------------------------
------------------------------------------------------------------

------------------------------------------------------------------
generateVC :: Statement SourcePos -> VCond -> VCM VCond 
------------------------------------------------------------------

generateVC (EmptyStmt _) vc 
  = return vc

generateVC (ExprStmt _ (AssignExpr _ OpAssign x e)) vc  
  = return $ (`F.subst1` (F.symbol x, F.expr e)) <$> vc

generateVC (BlockStmt _ ss) vc
  = foldM (\vc s -> generateVC s vc) vc (reverse ss)

generateVC (IfStmt _ b s1 s2) vc 
  = do vc1     <- generateVC s1 vc 
       vc2     <- generateVC s2 vc
       return   $ (pAnd bp <$> vc1) <> (pAnd bp' <$> vc2)
    where 
       bp       = F.prop b
       bp'      = F.PNot bp

generateVC (IfSingleStmt l b s) vc
  = generateVC (IfStmt l b s (EmptyStmt l)) vc

generateVC w@(WhileStmt l _ _) vc 
  = do vci'     <- generateVC s vci 
       sideCond $ ((i `pAnd` b)        `F.PImp`) <$> vci' -- require i is inductive 
       sideCond $ ((i `pAnd` F.PNot b) `F.PImp`) <$> vc   -- establish vc at exit 
       return vci                                          -- require i holds on entry
    where 
       (b, i, s) = splitWhileStmt w
       vci       = newVCond l i

generateVC w _ 
  = convertError "generateVC" w


----------------------------------------------------------------
-- Helpers for extracting specification from Statements --------
----------------------------------------------------------------

-- Ideally, we'd modify the parser to take in annotations for 
-- 
--   - assert(p)
--   - assume(p)
--   - invariant(p) 
--
-- a la JML, but for now, we hack them with function calls.

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



