{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Extended Static Checker - Nano

module Language.Nano.ESC (verifyFile) where

import           Control.Monad.State
import           Control.Applicative          ((<$>))
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Parser  (parseJavaScriptFromFile)
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Interface  (checkValid)
import           Language.Nano.Types
import           Control.Exception            (try)
import           Data.Monoid
import           Data.Maybe                   (fromMaybe, maybe)
import qualified Data.HashMap.Strict as M

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
genVC p = undefined 
  -- runState (generateVC mempty p)


checkVC :: (a, F.Pred) -> IO (F.FixResult a)
checkVC (x, p) = catch (checkValid x undefined p) $ \e ->
                   return (F.Crash [x] "VC crashes fixpoint")

------------------------------------------------------------------
-- | Verification Conditions -------------------------------------
------------------------------------------------------------------

-- | `VC` are formulas indexed by source-position from which the 
--   obligation arises.

type VCond = M.HashMap SourcePos F.Pred

instance Monoid VCond where 
  mempty  = M.empty
  mappend = M.unionWith pAnd 

vcond l p = M.singleton l p

------------------------------------------------------------------
-- | We will use the State monad to log all the individual "side" 
--   queries that arise due to checking of loop invariants.
------------------------------------------------------------------

type VCM = State VCond  

-- | `validAt l p` adds `p` to the list of side-conditions that 
--   must be checked.

------------------------------------------------------------------
validAt       :: SourcePos -> F.Pred -> VCM ()
------------------------------------------------------------------
validAt loc p 
  = do vc    <- get 
       let p' = fromMaybe p (pAnd p <$> M.lookup loc vc)
       put    $ M.insert loc p' vc 

------------------------------------------------------------------
-- | Verification Condition Generator ----------------------------
------------------------------------------------------------------

------------------------------------------------------------------
generateVC            :: VCond -> Nano -> VCM VCond 
------------------------------------------------------------------

generateVC (EmptyStmt _) vc 
  = return vc

generateVC (ExprStmt _ (AssignExpr _ OpAssign x e)) vc  
  = return $ (`F.subst` (F.symbol x, F.expr e)) <$> vc

generateVC (BlockStmt ss) vc
  = foldM (\vc s -> generateVC s vc) vc (reverse ss)

generateVC (IfStmt _ b s1 s2) vc 
  = do vc1     <- generateVC s1 vc 
       vc2     <- generateVC s2 vc
       return   $ (pAnd bp <$> vc1) <> (pAnd bp' <$> vc2)
    where 
       bp       = F.prop b
       bp'      = F.PNot bp

generateVC (IfSingleStmt l b s) vc
  = generateVC (IfStmt l b s (EmptyStmt l))

generateVC w@(WhileStmt l _ _) vc 
  = do vci'     <- generateVC s vci 
       validAt l $ ((i `pAnd` b)      `F.PImp`) <$> vci' -- require i is inductive 
       validAt l $ ((i `pAnd` F.PNot b) `F.PImp`) <$> vc   -- establish vc at exit 
       return vci                                        -- require i holds on entry
    where 
       (b, i, s) = splitWhileBody
       vci       = vcond l i

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

splitWhileBody :: Statement a   -> (F.Pred, Statement a)
splitWhileBody (BlockStmt _ (si : ss)) 
  = case getInvariant si of 
      Just i  -> (F.prop i,  ss)
      Nothing -> (F.PTrue, si : ss)

splitWhileBody s
  = (F.PTrue, s)

getAssume    = getStatementPred "assume"
getAssert    = getStatementPred "assert"
getInvariant = getStatementPred "invariant"

getStatementPred :: String -> Statement a -> Maybe F.Pred 
getStatementPred name (ExprStmt _ (CallExpr (VarRef _ (Id _ f)) [p]))
  | name == f 
  = Just $ F.prop p
getStatementPred _ _ 
  = Nothing 



