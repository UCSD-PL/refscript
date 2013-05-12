module Language.Nano.Liquid.Liquid (main) where 

import           Control.Applicative          ((<$>))
import           Control.Monad                
import           Data.Monoid
import           Language.Nano.Types
import           Language.Nano.Liquid.Types
import           Language.Nano.Liquid.Parse 
import           Language.ECMAScript3.Syntax
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Interface        (checkValid, resultExit)
import           Language.Fixpoint.Misc             -- (safeZip, sortNub, donePhase)
import           Text.PrettyPrint.HughesPJ          (Doc, text, render, ($+$), (<+>))
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Parser        (parseJavaScriptFromFile)
import           System.Exit                        (exitWith)

main cfg 
  = do rs   <- mapM verifyFile $ files cfg
       let r = mconcat rs
       donePhase (F.colorResult r) (render $ pp r) 
       exitWith (resultExit r)

--------------------------------------------------------------------------------
-- | Top-level Verifier 
--------------------------------------------------------------------------------
verifyFile :: FilePath -> IO (F.FixResult SourcePos)
--------------------------------------------------------------------------------
verifyFile f 
  = do nano <- parseNanoFromFile f 
       putStrLn . render . pp $ nano
       r    <- verifyNano nano
       return r

-------------------------------------------------------------------------------
-- | Parse File and Type Signatures -------------------------------------------
-------------------------------------------------------------------------------

parseNanoFromFile :: FilePath -> IO Nano
parseNanoFromFile f 
  = do src   <- parseJavaScriptFromFile f
       spec  <- parseSpecFromFile f
       return $ either err id (mkNano src spec)
    where 
       err m  = errortext $ text ("Invalid Input file: " ++ f) $+$ m

-------------------------------------------------------------------------------
-- | Execute Type Checker -----------------------------------------------------
-------------------------------------------------------------------------------

verifyNano  :: Nano -> IO (F.FixResult SourcePos)
verifyNano  = either unsafe safe . tcExecute . tcNano 
    
unsafe errs = do forM_ errs $ \(loc, err) -> putDocLn $ text "Error at" <+> pp loc <+> err
                 return $ F.Unsafe (fst <$> errs)
    
safe _      = return Safe 

-------------------------------------------------------------------------------
-- | Type Check Environment ---------------------------------------------------
-------------------------------------------------------------------------------

--   We define this alias as the "output" type for typechecking any entity
--   that can create or affect binders (e.g. @VarDecl@ or @Statement@)
--   @Nothing@ means if we definitely hits a "return" 
--   @Just Γ'@ means environment extended with statement binders

type TCEnv = Maybe (Env Type)

-------------------------------------------------------------------------------
-- | TypeCheck Nano Program ---------------------------------------------------
-------------------------------------------------------------------------------

tcNano     :: Nano -> TCM () 
tcNano pgm = sequence_ (tcFun Γ0) fs
  where
    Γ0     = env pgm
    Src fs = code pgm

-------------------------------------------------------------------------------
-- | TypeCheck Function -------------------------------------------------------
-------------------------------------------------------------------------------

tcFun    :: Env Type -> FunctionStatement -> TCM ()
tcFun Γ (FunctionStmt l f xs body) 
  = case getFunTy Γ f of 
      Just (_,xts,t) -> do let Γ' = envAdds Γ ((returnId l, t) : xts)
                           z     <- tcStmts Γ' body
                           when (isJust z) $ tcError (errorNoReturn f l)
      Nothing        -> tcError $ errorNonFunction f l 

getFunTy Γ f = bkFun =<< envFind f Γ


--------------------------------------------------------------------------------
tcSeq               :: (a -> TCM TCEnv) -> Env Type -> [a] -> TCM TCEnv
--------------------------------------------------------------------------------

tcSeq tc            = foldM step . Just 
  where 
    step Nothing _  = return Nothing
    step (Just Γ) x = tc Γ x

--------------------------------------------------------------------------------
tcStmts :: Env Type -> [Statement SourcePos]  -> TCM TCEnv
--------------------------------------------------------------------------------

tcStmts = tcSeq tcStmt

-------------------------------------------------------------------------------
tcStmt :: Env Type -> Statement SourcePos  -> TCM TCEnv  
-------------------------------------------------------------------------------

-- skip
tcStmt Γ (EmptyStmt _) 
  = return Γ

-- x = e
tcStmt Γ (ExprStmt _ (AssignExpr l OpAssign x e))   
  = tcAsgn Γ l x e

-- s1;s2;...;sn
tcStmt Γ (BlockStmt _ stmts) 
  = tcStmts Γ stmts 

-- if b { s1 } else { s2 }
tcStmt Γ (IfStmt _ b s1 s2)
  = error "TODO tcStmt IfStmt"
 
-- if b { s1 }
tcStmt Γ (IfSingleStmt l b s)
  = tcStmt Γ (IfStmt l b s (EmptyStmt l))

-- var x1 [ = e1 ]; ... ; var xn [= en];
tcStmt Γ (VarDeclStmt _ ds)
  = tcSeq tcVarDecl Γ ds

-- return e 
tcStmt Γ (ReturnStmt l (Just e)) 
  = tcReturn l e vc

-- OTHER (Not handled)
tcStmt Γ s 
  = convertError "tcStmt" s

-------------------------------------------------------------------------------
tcVarDecl :: Env Type -> VarDecl SourcePos -> TCM TCEnv  
-------------------------------------------------------------------------------

tcVarDecl Γ (VarDecl l x (Just e)) 
  = tcAsgn Γ l x e  
tcVarDecl Γ (VarDecl l x Nothing)  
  = return Γ

------------------------------------------------------------------------------------
tcAsgn :: Env Type -> SourcePos -> Id SourcePos -> Expression SourcePos -> TCM TCEnv
------------------------------------------------------------------------------------

tcAsgn Γ l x e = error "TODO: tcAsgn"

-------------------------------------------------------------------------------
tcExpr :: Env Type -> Expression SourcePos -> TCM Type
-------------------------------------------------------------------------------

tcExpr = error "TODO: tcExpr"

instance F.Symbolic   (Id a) where
  symbol (Id _ x)   = F.symbol x 

instance F.Symbolic (LValue a) where
  symbol (LVar _ x) = F.symbol x
  symbol lv         = convertError "F.Symbol" lv

instance F.Expression (Id a) where
  expr = F.eVar

instance F.Expression (LValue a) where
  expr = F.eVar

instance F.Expression (Expression a) where
  expr (IntLit _ i)                 = F.expr i
  expr (VarRef _ x)                 = F.expr x
  expr (InfixExpr _ o e1 e2)        = F.EBin (bop o) (F.expr e1) (F.expr e2)
  expr (PrefixExpr _ PrefixMinus e) = F.EBin F.Minus (F.expr (0 :: Int)) (F.expr e)  
  expr e                            = convertError "F.Expr" e

instance F.Predicate  (Expression a) where 
  prop (BoolLit _ True)            = F.PTrue
  prop (BoolLit _ False)           = F.PFalse
  prop (PrefixExpr _ PrefixLNot e) = F.PNot (F.prop e)
  prop e@(InfixExpr _ _ _ _ )      = eProp e
  prop e                           = convertError "F.Pred" e  


OpLT   
OpLEq  
OpGT   
OpGEq  
OpEq   
OpNEq  
       
OpLAnd 
OpLOr  
       
OpSub  
OpAdd  
OpMul  -------------------------------------------------------------------------------
OpDiv  -- | Typechecking monad -------------------------------------------------------
OpMod  -------------------------------------------------------------------------------

data TCM a

tcError :: Doc -> TCM ()
tcError = error "TODO: tcError" 

tcExecute :: TCM a -> Either [(SourcePos, Doc)] a
tcExecute = error "TODO: tcExecute" 

---------------------------------------------------------------------------------------
-- | Error Messages -------------------------------------------------------------------
---------------------------------------------------------------------------------------

errorNonFunction f l = text "Bad function type for" <+> pp f <+> text "defined at" <+> pp l  
errorNoReturn f l    = text "Function" <+> pp f <+> text "defined at" <+> pp l <+> text "does not return"


