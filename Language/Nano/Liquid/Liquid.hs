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
      Just (_,xts,t) -> do let Γ' = envAdds Γ xts
                           _     <- tcSetReturn t
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
  = return $ Just Γ

-- x = e
tcStmt Γ (ExprStmt _ (AssignExpr l OpAssign x e))   
  = tcAsgn Γ l x e

-- s1;s2;...;sn
tcStmt Γ (BlockStmt _ stmts) 
  = tcStmts Γ stmts 

-- if b { s1 }
tcStmt Γ (IfSingleStmt l b s)
  = tcStmt Γ (IfStmt l b s (EmptyStmt l))

-- if b { s1 } else { s2 }
tcStmt Γ (IfStmt l e s1 s2)
  = do t     <- tcExpr Γ e
       assertTy l e t tBool
       Γ1    <- tcStmt Γ s1
       Γ2    <- tcStmt Γ s2
       return $ envJoin l Γ1 Γ2

-- var x1 [ = e1 ]; ... ; var xn [= en];
tcStmt Γ (VarDeclStmt _ ds)
  = tcSeq tcVarDecl Γ ds

-- return e 
tcStmt Γ (ReturnStmt l (Just e)) 
  = do t  <- tcExpr Γ e 
       t' <- tcGetReturn 
       assertTy l e t t' 
       return Nothing

-- OTHER (Not handled)
tcStmt Γ s 
  = convertError "tcStmt" s

-------------------------------------------------------------------------------
tcVarDecl :: Env Type -> VarDecl SourcePos -> TCM TCEnv  
-------------------------------------------------------------------------------

tcVarDecl Γ (VarDecl l x (Just e)) 
  = tcAsgn Γ l x e  
tcVarDecl Γ (VarDecl l x Nothing)  
  = return $ Just Γ

------------------------------------------------------------------------------------
tcAsgn :: Env Type -> SourcePos -> Id SourcePos -> Expression SourcePos -> TCM TCEnv
------------------------------------------------------------------------------------

tcAsgn Γ l x e 
  = do t <- tcExpr Γ e
       return $ Just $ envAdd Γ (x, t) 

-------------------------------------------------------------------------------
tcExpr :: Env Type -> Expression SourcePos -> TCM Type
-------------------------------------------------------------------------------

tcExpr _ (IntLit _ _)               
  = return tInt 

tcExpr _ (BoolLit _ _)
  = return tBool

tcExpr Γ (VarRef l x)               
  = case envFind x Γ of 
      Just t  -> return t
      Nothing -> tcError $ errorUnboundId x l

tcExpr Γ (PrefixExpr l o e)
  = tcCall Γ l (prefixOpTy o) [e]

tcExpr Γ (InfixExpr l o e1 e2)        
  = tcCall Γ l (infixOpTy o) [e1, e2] 

tcExpr Γ (CallExpr l (VarRef _ (Id _ f)) es)
  = case envFind f Γ of 
      Just t -> ... 
      Nothing -> tcError $ errorUnboundId f l  
      tcCall Γ l (

tcExpr Γ e 
  = convertError "tcExpr" e

----------------------------------------------------------------------------------
tcCall :: Env Type -> SourcePos -> Type -> [Expression SourcePos] -> TCM Type
----------------------------------------------------------------------------------

tcCall Γ l ft args 
  = error "TBD: tcCall"

----------------------------------------------------------------------------------
-- envJoin :: (Eq a) => Env a -> Env a -> TCM (Env a) 
----------------------------------------------------------------------------------

envJoin l Γ1 Γ2  = forM_ ytts err >> return (envFromList zts)  
  where 
    zts          = [(x,t)    | (x,t,t') <- xtts, t == t']
    ytts         = [(y,t,t') | (y,t,t') <- xtts, t /= t']
    xtts         = [(x,t,t') | (x,t)    <- envToList Γ1, t' <- maybeToList (envFind x Γ2)]
    err (y,t,t') = tcError $ errorJoin l y t t'

----------------------------------------------------------------------------------
-- | Base Types ------------------------------------------------------------------
----------------------------------------------------------------------------------

tInt  = TApp TInt  [] ()
tBool = TApp TBool [] ()
tVoid = TApp TVoid [] ()

infixOpTy              :: InfixOp -> Type
infixOpTy OpLT         = TFun [tInt, tInt]   tBool  
infixOpTy OpLEq        = TFun [tInt, tInt]   tBool
infixOpTy OpGT         = TFun [tInt, tInt]   tBool
infixOpTy OpGEq        = TFun [tInt, tInt]   tBool
infixOpTy OpEq         = TFun [tInt, tInt]   tBool
infixOpTy OpNEq        = TFun [tInt, tInt]   tBool
infixOpTy OpLAnd       = TFun [tBool, tBool] tBool 
infixOpTy OpLOr        = TFun [tBool, tBool] tBool
infixOpTy OpSub        = TFun [tInt, tInt]   tInt 
infixOpTy OpAdd        = TFun [tInt, tInt]   tInt 
infixOpTy OpMul        = TFun [tInt, tInt]   tInt 
infixOpTy OpDiv        = TFun [tInt, tInt]   tInt 
infixOpTy OpMod        = TFun [tInt, tInt]   tInt  
infixOpTy o            = convertError "infixOpTy" o

prefixOpTy             :: PrefixOp -> Type
prefixOpTy PrefixMinus = TFun [tInt] tInt
prefixOpTy PrefixLNot  = TFun [tBool] tBool
prefixOpTy o           = convertError "prefixOpTy" o

-------------------------------------------------------------------------------
-- | Typechecking monad -------------------------------------------------------
-------------------------------------------------------------------------------

data TCM a

tcSetReturn :: Type -> TCM ()
tcSetReturn = error "TODO: tcSetReturn"

tcGetReturn :: TCM Type 
tcSetReturn = error "TODO: tcGetReturn"

tcError :: Doc -> TCM ()
tcError = error "TODO: tcError" 

tcExecute :: TCM a -> Either [(SourcePos, Doc)] a
tcExecute = error "TODO: tcExecute" 

---------------------------------------------------------------------------------------
-- | Error Messages -------------------------------------------------------------------
---------------------------------------------------------------------------------------

assertTy l e t t'       = when (t /= t') $ tcError $ errorWrongType l e t t'

errorNonFunction f l    = text $ printf "Bad function type for %s defined at %s" 
                                   (showpp f) (showpp l)  
errorNoReturn f l       = text $ printf "Function %s defined at %s does not return" 
                                   (showpp f) (showpp l) 
errorUnboundId x l      = text $ printf "Identifier %s unbound at %s" 
                                   (showpp x) (showpp l)
errorWrongType l e t t' = text $ printf "Unexpected type for %s :: %s expected %s at %s" 
                                   (showpp e) (showpp t) (showpp t') (showpp l)
errorJoin l x t t'      = text $ printf "Cannot join %s :: %s expected %s at %s" 
                                   (showpp x) (showpp t) (showpp t') (showpp l)




