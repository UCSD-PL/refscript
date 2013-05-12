module Language.Nano.Liquid.Liquid (main) where 

import           Control.Applicative                ((<$>), (<*>))
import           Control.Monad                
import qualified Data.List as L
import           Data.Monoid
import           Data.Maybe                         (isJust, maybeToList)
import           Language.Nano.Types
import           Language.Nano.Liquid.Types
import           Language.Nano.Liquid.Parse 
import           Language.Nano.Liquid.TCMonad
import           Language.ECMAScript3.Syntax
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Interface        ({- checkValid,-} resultExit)
-- import           Language.Fixpoint.PrettyPrint      (showpp)
import           Language.Fixpoint.Misc             
import           Text.PrettyPrint.HughesPJ          (Doc, text, render, ($+$), (<+>))
import           Text.Printf                        (printf)
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
verifyNano  = either unsafe safe . execute . tcNano 
    
unsafe errs = do forM_ errs $ \(loc, err) -> putStrLn $ printf "Error at %s : %s" (ppshow loc) err
                 return $ F.Unsafe (fst <$> errs)
    
safe _      = return F.Safe 

-------------------------------------------------------------------------------
-- | Type Check Environment ---------------------------------------------------
-------------------------------------------------------------------------------

--   We define this alias as the "output" type for typechecking any entity
--   that can create or affect binders (e.g. @VarDecl@ or @Statement@)
--   @Nothing@ means if we definitely hits a "return" 
--   @Just γ'@ means environment extended with statement binders

type TCEnv = Maybe (F.SEnv Type)

-------------------------------------------------------------------------------
-- | TypeCheck Nano Program ---------------------------------------------------
-------------------------------------------------------------------------------

tcNano     :: Nano -> TCM () 
tcNano pgm = forM_ fs $ tcFun γ0
  where
    γ0     = env pgm
    Src fs = code pgm

-------------------------------------------------------------------------------
-- | TypeCheck Function -------------------------------------------------------
-------------------------------------------------------------------------------

tcFun    :: F.SEnv Type -> FunctionStatement -> TCM ()
tcFun γ (FunctionStmt l f xs body) 
  = do z <- funEnv l γ f xs
       forM_ (maybeToList z) $ \(t, γ') ->
         do _  <- setReturn t
            ex <- tcStmts γ' body
            maybe (return ()) (\_ -> assertTy l f t tVoid) ex 

funEnv l γ f xs
  = case bkFun =<< envFind f γ of
      Nothing       -> logError Nothing l $ errorNonFunction f
      Just (_,ts,t) -> if length xs /= length ts 
                         then logError Nothing l $ errorArgMismatch  
                         else return $ Just (t, L.foldl' (\γ (x,t) -> envAdd x t γ) γ $ zip xs ts)

--------------------------------------------------------------------------------
tcSeq :: (F.SEnv Type -> a -> TCM TCEnv) -> F.SEnv Type -> [a] -> TCM TCEnv
--------------------------------------------------------------------------------

tcSeq tc            = foldM step . Just 
  where 
    step Nothing _  = return Nothing
    step (Just γ) x = tc γ x

--------------------------------------------------------------------------------
tcStmts :: F.SEnv Type -> [Statement SourcePos]  -> TCM TCEnv
--------------------------------------------------------------------------------

tcStmts = tcSeq tcStmt

-------------------------------------------------------------------------------
tcStmt :: F.SEnv Type -> Statement SourcePos  -> TCM TCEnv  
-------------------------------------------------------------------------------

-- skip
tcStmt γ (EmptyStmt _) 
  = return $ Just γ

-- x = e
tcStmt γ (ExprStmt _ (AssignExpr l OpAssign (LVar lx x) e))   
  = tcAsgn γ l (Id lx x) e

-- s1;s2;...;sn
tcStmt γ (BlockStmt _ stmts) 
  = tcStmts γ stmts 

-- if b { s1 }
tcStmt γ (IfSingleStmt l b s)
  = tcStmt γ (IfStmt l b s (EmptyStmt l))

-- if b { s1 } else { s2 }
tcStmt γ (IfStmt l e s1 s2)
  = do t     <- tcExpr γ e
       assertTy l e t tBool
       γ1    <- tcStmt γ s1
       γ2    <- tcStmt γ s2
       envJoin l γ1 γ2

-- var x1 [ = e1 ]; ... ; var xn [= en];
tcStmt γ (VarDeclStmt _ ds)
  = tcSeq tcVarDecl γ ds

-- return e 
tcStmt γ (ReturnStmt l (Just e)) 
  = do t  <- tcExpr γ e 
       t' <- getReturn 
       assertTy l e t t' 
       return Nothing

-- OTHER (Not handled)
tcStmt γ s 
  = convertError "tcStmt" s

-------------------------------------------------------------------------------
tcVarDecl :: F.SEnv Type -> VarDecl SourcePos -> TCM TCEnv  
-------------------------------------------------------------------------------

tcVarDecl γ (VarDecl l x (Just e)) 
  = tcAsgn γ l x e  
tcVarDecl γ (VarDecl l x Nothing)  
  = return $ Just γ

------------------------------------------------------------------------------------
tcAsgn :: F.SEnv Type -> SourcePos -> Id SourcePos -> Expression SourcePos -> TCM TCEnv
------------------------------------------------------------------------------------

tcAsgn γ l x e 
  = do t <- tcExpr γ e
       return $ Just $ envAdd x t γ

-------------------------------------------------------------------------------
tcExpr :: F.SEnv Type -> Expression SourcePos -> TCM Type
-------------------------------------------------------------------------------

tcExpr _ (IntLit _ _)               
  = return tInt 

tcExpr _ (BoolLit _ _)
  = return tBool

tcExpr γ (VarRef l x)               
  = case envFind x γ of 
      Just t  -> return t
      Nothing -> logError tErr l $ errorUnboundId x

tcExpr γ (PrefixExpr l o e)
  = tcCall γ l o (prefixOpTy o) [e]

tcExpr γ (InfixExpr l o e1 e2)        
  = tcCall γ l o (infixOpTy o) [e1, e2] 

tcExpr γ (CallExpr l (VarRef _ f@(Id _ _)) es)
  = case envFind f γ of 
      Just t  -> tcCall γ l f t es
      Nothing -> logError tErr l $ errorUnboundId f

tcExpr γ e 
  = convertError "tcExpr" e

----------------------------------------------------------------------------------
-- tcCall :: F.SEnv Type -> SourcePos -> Type -> [Expression SourcePos] -> TCM Type
----------------------------------------------------------------------------------

tcCall γ l z ft es 
  = case bkFun ft of
     Nothing           -> logError tErr l $ errorNonFunction z 
     Just (αs, ts, t') -> maybe tErr (\_ -> t') <$> unifyArgs γ l () es ts

unifyArgs γ l          = go 
  where 
    go θ [] []         = return $ Just θ
    go θ _  []         = logError Nothing l errorArgMismatch 
    go θ [] _          = logError Nothing l errorArgMismatch 
    go θ (e:es) (t:ts) = do te <- tcExpr γ e
                            if t == te 
                              then go θ es ts 
                              else logError Nothing l $ errorWrongType e t te

----------------------------------------------------------------------------------
envJoin :: SourcePos -> TCEnv -> TCEnv -> TCM TCEnv 
----------------------------------------------------------------------------------

envJoin _ Nothing x           = return x
envJoin _ x Nothing           = return x
envJoin l (Just γ1) (Just γ2) = envJoin' l γ1 γ2 

envJoin' l γ1 γ2  = forM_ ytts err >> return (Just (F.fromListSEnv zts))  
  where 
    zts          = [(x,t)    | (x,t,t') <- xtts, t == t']
    ytts         = [(y,t,t') | (y,t,t') <- xtts, t /= t']
    xtts         = [(x,t,t') | (x,t)    <- F.toListSEnv γ1, t' <- maybeToList (F.lookupSEnv x γ2)]
    err (y,t,t') = logError () l $ errorJoin y t t'

---------------------------------------------------------------------------------------
-- | Error Messages -------------------------------------------------------------------
---------------------------------------------------------------------------------------

assertTy l e t t'     = when (t /= t') $ logError () l $ errorWrongType e t t'

errorArgMismatch      = printf "Mismatch in Number of Args in Call" 
errorNonFunction f    = printf "Non-function type for %s" (ppshow f)  
errorUnboundId x      = printf "Identifier %s unbound" (ppshow x) 
errorWrongType e t t' = printf "Unexpected type for %s :: %s expected %s" (ppshow e) (ppshow t) (ppshow t')
errorJoin x t t'      = printf "Cannot join %s :: %s expected %s" (ppshow x) (ppshow t) (ppshow t') 

ppshow = render . pp


envFind = F.lookupSEnv . F.symbol
envAdd  = F.insertSEnv . F.symbol
