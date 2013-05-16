module Language.Nano.Liquid.Typecheck (typeCheck) where 

import           Control.Applicative                ((<$>), (<*>))
import           Control.Monad                
import qualified Data.HashSet as S 
import qualified Data.List as L
import           Data.Monoid
import           Data.Maybe                         (isJust, fromMaybe, maybeToList)
import           Language.Nano.Files
import           Language.Nano.Errors
import           Language.Nano.Types
import           Language.Nano.Liquid.Types
import           Language.Nano.Liquid.Parse 
import           Language.Nano.Liquid.TCMonad
import           Language.Nano.Liquid.Substitution

import           Language.ECMAScript3.Syntax
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Interface        (resultExit)
import           Language.Fixpoint.Misc             
import           Text.PrettyPrint.HughesPJ          (Doc, text, render, ($+$), (<+>))
import           Text.Printf                        (printf)
import           Language.ECMAScript3.PrettyPrint
import           System.Exit                        (exitWith)

-------------------------------------------------------------------------------
typeCheck   :: NanoBare -> IO (F.FixResult SourcePos)
-------------------------------------------------------------------------------

typeCheck   = either unsafe safe . execute . tcNano 

unsafe errs = do putStrLn "\n\n\nErrors Found!\n\n" 
                 forM_ errs $ \(l,e) -> putStrLn $ printf "Error at %s\n  %s\n" (ppshow l) e
                 return $ F.Unsafe (fst <$> errs)
    
safe _      = return F.Safe 

-------------------------------------------------------------------------------
-- | Type Check Environment ---------------------------------------------------
-------------------------------------------------------------------------------

--   We define this alias as the "output" type for typechecking any entity
--   that can create or affect binders (e.g. @VarDecl@ or @Statement@)
--   @Nothing@ means if we definitely hits a "return" 
--   @Just γ'@ means environment extended with statement binders

type TCEnv = Maybe (Env Type)

-------------------------------------------------------------------------------
-- | TypeCheck Nano Program ---------------------------------------------------
-------------------------------------------------------------------------------

tcNano     :: NanoBare -> TCM () 
tcNano pgm = forM_ fs $ tcFun γ0
  where
    γ0     = env pgm
    Src fs = fmap ann $ code pgm -- error "FIX THIS"

-------------------------------------------------------------------------------
-- | TypeCheck Function -------------------------------------------------------
-------------------------------------------------------------------------------

tcFun    :: Env Type -> FunctionStatement SourcePos -> TCM ()
tcFun γ (FunctionStmt l f xs body) 
  = do (αs, ts, t)    <- funTy l γ f xs
       let γ'          = envAddFun l f αs xs ts t γ 
       q              <- tcStmts γ' body
       when (isJust q) $ assertVoid l f t

funTy l γ f xs 
  = case bkFun =<< envFindTy f γ of
      Nothing        -> tcError l $ errorNonFunction f
      Just (αs,ts,t) -> do when (length xs /= length ts) $ tcError l $ errorArgMismatch
                           return (αs,ts,t)

envAddFun l f αs xs ts t = envAdds tyBinds . envAdds varBinds . envAddReturn f t 
  where  
    tyBinds              = [(Loc l α, tVar α) | α <- αs]
    varBinds             = zip xs ts

--------------------------------------------------------------------------------
tcSeq :: (Env Type -> a -> TCM TCEnv) -> Env Type -> [a] -> TCM TCEnv
--------------------------------------------------------------------------------

tcSeq f             = foldM step . Just 
  where 
    step Nothing _  = return Nothing
    step (Just γ) x = f γ x

--------------------------------------------------------------------------------
tcStmts :: Env Type -> [Statement SourcePos]  -> TCM TCEnv
--------------------------------------------------------------------------------

tcStmts = tcSeq tcStmt

-------------------------------------------------------------------------------
tcStmt :: Env Type -> Statement SourcePos  -> TCM TCEnv  
-------------------------------------------------------------------------------

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
       assertTy l "If condition" e t tBool
       γ1    <- tcStmt γ s1
       γ2    <- tcStmt γ s2
       envJoin l γ1 γ2

-- var x1 [ = e1 ]; ... ; var xn [= en];
tcStmt γ (VarDeclStmt _ ds)
  = tcSeq tcVarDecl γ ds

-- return e 
tcStmt γ (ReturnStmt l eo) 
  = do t  <- maybe (return tVoid) (tcExpr γ) eo 
       assertTy l "Return" eo t (envFindReturn γ) 
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
       θ          <- unifyArgs l γ es its 
       return      $ apply θ ot

instantiate l ft 
  = do let (αs, t) = bkAll ft 
       θ          <- fromList . zip αs . fmap tVar <$> fresh αs
       maybe err return $ bkFun $ apply θ t
    where
       err         = tcError l $ errorNonFunction ft

unifyArgs l γ es ts
  | length es /= length ts = tcError l errorArgMismatch 
  | otherwise              = do tes <- mapM (tcExpr γ) es
                                case unifys mempty ts tes of
                                  Left msg -> tcError l msg
                                  Right θ  -> validSubst l γ θ

validSubst       :: SourcePos -> Env Type -> Subst -> TCM Subst
validSubst l γ θ = mapM_ (validTyBind l γ) (toList θ) >> return θ

validTyBind l γ (α, t) 
  | bad1      = tcError l $ errorBoundTyVar α t 
  | bad2      = tcError l $ errorFreeTyVar t
  | otherwise = return True 
  where
    bad1      = envMem α γ                                  -- dom θ        \cap γ = empty 
    bad2      = not $ all (`envMem` γ) $ S.toList $ free t  -- free (rng θ) \subset γ

----------------------------------------------------------------------------------
envJoin :: SourcePos -> TCEnv -> TCEnv -> TCM TCEnv 
----------------------------------------------------------------------------------

envJoin _ Nothing x           = return x
envJoin _ x Nothing           = return x
envJoin l (Just γ1) (Just γ2) = envJoin' l γ1 γ2 

envJoin' l γ1 γ2 
  = do forM_ (envToList $ envLefts γall) err 
       return (Just $ envRights γall)
    where 
      γall = envIntersectWith meet γ1 γ2
      meet = \t1 t2 -> if t1 == t2 then Right t1 else Left (t1,t2)
      err  = \(y, (t, t')) -> tcError l $ errorJoin y t t'

-- envJoin' l γ1 γ2  = forM_ ytts err >> return (Just (envFromList zts))  
--   where 
--     zts          = [(x,t)    | (x,t,t') <- xtts, t == t']
--     ytts         = [(y,t,t') | (y,t,t') <- xtts, t /= t']
--     xtts         = [(x,t,t') | (x,t)    <- envToList γ1, t' <- maybeToList (F.lookupSEnv x γ2)]
--     err (y,t,t') = tcError l $ errorJoin y t t'

---------------------------------------------------------------------------------------
-- | Error Messages -------------------------------------------------------------------
---------------------------------------------------------------------------------------

assertTy l m e t t' = when (t /= t') $ tcError l $ errorWrongType m e t t'
assertVoid l f t    = assertTy l "Missing return" f tVoid t



