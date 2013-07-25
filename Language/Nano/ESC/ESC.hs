{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

-- | Extended Static Checker - Nano

module Language.Nano.ESC.ESC (verifyFile) where

import           Text.PrettyPrint.HughesPJ    (text, render, (<+>))
import           System.FilePath              (addExtension)
import           Control.Monad.State
import           Control.Applicative          ((<$>))
-- import           Control.Monad                (mapM)
import qualified Control.Exception as Ex
import           Data.Monoid
import           Data.Maybe                   (isJust, fromJust) -- fromMaybe, maybe)
-- import           System.Exit                  (exitWith)

import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Parser        (SourceSpan (..))

import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Interface  (checkValid {-, resultExit-} )
import           Language.Fixpoint.Misc       (safeZip, sortNub {-, donePhase -} )

import           Language.Nano.Types
import           Language.Nano.ESC.Types
import           Language.Nano.ESC.VCMonad

--------------------------------------------------------------------------------

-- main cfg 
--   = do rs   <- mapM verifyFile $ files cfg
--        let r = mconcat rs
--        donePhase (F.colorResult r) (render $ pp r) 
--        exitWith (resultExit r)

--------------------------------------------------------------------------------
-- | Top-level Verifier 
--------------------------------------------------------------------------------
verifyFile :: OptionConf -> FilePath -> IO (F.FixResult SourceSpan)
--------------------------------------------------------------------------------

verifyFile _ f 
  = do nano   <- parseNanoFromFile f
       forM_ nano (putStrLn . render . pp)  
       let vc  = genVC nano 
       writeFile (f `addExtension` ".vc") (render $ pp vc)
       rs     <- mapM checkVC $ obligationsVCond vc
       forM rs $ (putStrLn . render . pp)  
       return  $ mconcat rs

--------------------------------------------------------------------------------
-- | Top-level VC Generator 
--------------------------------------------------------------------------------
genVC     :: Nano -> VCond
--------------------------------------------------------------------------------
genVC pgm = execute pgm act 
  where 
    act   = mconcat <$> forM pgm (`generateVC` mempty)



-----------------------------------------------------------------------------------
-- | Top-level SMT Interface 
-----------------------------------------------------------------------------------
checkVC :: (SourceSpan, F.Pred) -> IO (F.FixResult SourceSpan)
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

instance IsVerifiable (Fun SourceSpan) where 
  generateVC fn _   = generateFunVC fn

instance IsVerifiable a => IsVerifiable [a] where 
  generateVC xs vc  = foldM (\vc s -> generateVC s vc) vc (reverse xs)

instance IsVerifiable (Statement SourceSpan) where 
  generateVC        = generateStmtVC

instance IsVerifiable (VarDecl SourceSpan) where 
  generateVC (VarDecl l x (Just e)) vc = generateAsgnVC l x e vc
  generateVC (VarDecl _ _ Nothing)  vc = return vc


-----------------------------------------------------------------------------------
generateFunVC    :: Fun SourceSpan -> VCM VCond 
-----------------------------------------------------------------------------------
generateFunVC fn 
  = do _        <- setFunction fn
       let sts   = fbody fn ++ [retStmt (floc fn)] 
       vc       <- (generateAssumeVC (fpre fn) <=< generateVC sts) mempty
       vc'      <- getSideCond 
       return    $ vc <> vc'
      
retStmt l = ReturnStmt l (Just $ IntLit l 0)
-----------------------------------------------------------------------------------
generateStmtVC :: Statement SourceSpan -> VCond -> VCM VCond 
-----------------------------------------------------------------------------------

-- skip
generateStmtVC (EmptyStmt _) vc 
  = return vc

-- x = e
generateStmtVC (ExprStmt _ (AssignExpr l OpAssign x e)) vc  
  = generateAsgnVC l x e vc 

-- s1;s2;...;sn
generateStmtVC (BlockStmt _ ss) vc
  = generateVC ss vc

-- if b { s1 } else { s2 }
generateStmtVC (IfStmt _ b s1 s2) vc 
  = do vc1     <- generateVC s1 vc 
       vc2     <- generateVC s2 vc
       return   $ ((bp `F.PImp`) <$> vc1) <> ((bp' `F.PImp`) <$> vc2)
    where 
       bp       = F.prop b
       bp'      = F.PNot bp

-- if b { s1 }
generateStmtVC (IfSingleStmt l b s) vc
  = generateVC (IfStmt l b s (EmptyStmt l)) vc

-- while (cond) { s }
generateStmtVC (WhileStmt l cond s) vc 
  = do vci'       <- generateVC s vci 
       addSideCond $ ((i `pAnd` b)        `F.PImp`) <$> vci' -- require i is inductive 
       addSideCond $ ((i `pAnd` F.PNot b) `F.PImp`) <$> vc   -- establish vc at exit 
       return vci                                            -- require i holds on entry
    where 
       b           = F.prop cond
       i           = getInvariant s
       vci         = newVCond l i

-- var x1 [ = e1 ]; ... ; var xn [= en];
generateStmtVC (VarDeclStmt _ ds) vc
  = generateVC ds vc

-- assume(e)
generateStmtVC e@(ExprStmt _ (CallExpr _ _ _)) vc
  | isJust ep = generateAssumeVC (fromJust ep) vc
  where 
    ep        = getAssume e

-- assert(e)
generateStmtVC e@(ExprStmt l (CallExpr _ _ _)) vc
  | isJust ep = generateAssertVC l (fromJust ep) vc
  where 
    ep        = getAssert e

-- ignore other specification statements
generateStmtVC e@(ExprStmt _ (CallExpr _ _ _)) vc
  | isSpecification e
  = return vc

-- return e 
generateStmtVC (ReturnStmt l (Just e)) vc
  = generateReturnVC l e vc


generateStmtVC w _ 
  = convertError "generateStmtVC" w

-----------------------------------------------------------------------------------
generateAsgnVC :: F.Symbolic x => SourceSpan -> x -> Expression a -> VCond -> VCM VCond
-----------------------------------------------------------------------------------

generateAsgnVC l x (CallExpr _ (VarRef _ (Id _ f)) es) vc
  = generateFunAsgnVC l x f es vc

generateAsgnVC _ x e  vc
  = generateExprAsgnVC x e vc

-----------------------------------------------------------------------------------
--
-- HIDE
generateAssumeVC :: F.Pred -> VCond -> VCM VCond 
generateAssumeVC   p vc = return $ (p `F.PImp`) <$> vc

generateAssertVC :: SourceSpan -> F.Pred -> VCond -> VCM VCond 
generateAssertVC l p vc = return $ newVCond l p <> vc

-- x = e; // where e is not a function call
generateExprAsgnVC :: (F.Symbolic x, F.Expression e) => x -> e -> VCond -> VCM VCond 
generateExprAsgnVC x e vc = return   $ (`F.subst1` (F.symbol x, F.expr e)) <$> vc

-- x = f(e1,...,en)
generateFunAsgnVC :: (F.Symbolic x, F.Expression e) 
                  => SourceSpan -> x -> String -> [e] -> VCond -> VCM VCond 
generateFunAsgnVC l x f es vc
  = do z       <- freshId l
       sp      <- getCalleeSpec f
       let su   = F.mkSubst $ safeZip "funCall" (F.symbol <$> fargs sp) (F.expr <$> es)
       let su'  = F.mkSubst [(returnSymbol, F.eVar z)]
       let pre  = F.subst su          (fpre sp) 
       let post = F.subst (su <> su') (fpost sp)
       (generateAssertVC l pre <=< generateAssumeVC post <=< generateExprAsgnVC x z) vc 

-- return e
generateReturnVC :: SourceSpan-> Expression SourceSpan -> VCond-> VCM VCond
generateReturnVC l e vc 
  = do p <- getFunctionPostcond
       (generateAsgnVC l returnSymbol e <=<  generateAssertVC l p <=< generateAssumeVC F.PFalse) $ vc



