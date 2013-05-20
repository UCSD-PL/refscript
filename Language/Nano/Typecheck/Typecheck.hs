module Language.Nano.Typecheck.Typecheck (verifyFile, typeCheck) where 

import           Control.Applicative                ((<$>), (<*>))
import           Control.Monad                
import qualified Data.HashSet        as S 
import qualified Data.HashMap.Strict as M 
import qualified Data.List           as L
import qualified Data.Traversable    as T
import           Data.Monoid
import           Data.Maybe                         (isJust, fromMaybe, maybeToList)
import           Text.PrettyPrint.HughesPJ          (Doc, text, render, ($+$), (<+>), vcat)
import           Text.Printf                        (printf)
import           System.Exit                        (exitWith)

import           Language.Nano.Errors
import           Language.Nano.Types
import           Language.Nano.Env
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Parse 
import           Language.Nano.Typecheck.TCMonad
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Typecheck.SSA

import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Interface        (resultExit)
import           Language.Fixpoint.Misc             
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.PrettyPrint


-- main cfg 
--   = do rs   <- mapM verifyFile $ files cfg
--        let r = mconcat rs
--        donePhase (F.colorResult r) (render $ pp r) 
--        exitWith (resultExit r)

--------------------------------------------------------------------------------
-- | Top-level Verifier 
--------------------------------------------------------------------------------
verifyFile :: FilePath -> IO (F.FixResult SourcePos)
--------------------------------------------------------------------------------
verifyFile f = (either unsafe safe . execute . tcNano . ssaTransform) =<< parseNanoFromFile f

-------------------------------------------------------------------------------
typeCheck :: (F.Reftable r) => Nano AnnSSA (RType r) -> Nano AnnType (RType r) 
-------------------------------------------------------------------------------
typeCheck = either crash id . execute . tcNano  
  where 
    crash = errorstar . render . vcat . map (text . ppErr)
 

-- DEBUG MODE
-- verifyFile f 
--   = do nano <- parseNanoFromFile f 
--        donePhase Loud "Parse"
--        putStrLn . render . pp $ nano
--        let nanoSsa = ssaTransform nano
--        donePhase Loud "SSA Transform"
--        putStrLn . render . pp $ nanoSsa
--        r    <- either unsafe safe . execute . typeCheck $ nanoSsa
--        donePhase Loud "Typechecking"
--        return r

unsafe errs = do putStrLn "\n\n\nErrors Found!\n\n" 
                 forM_ errs (putStrLn . ppErr) 
                 return $ F.Unsafe (fst <$> errs)

ppErr (l, e) = printf "Error at %s\n  %s\n" (ppshow l) e

safe pgm@(Nano {code = Src fs})
  -- = return F.Safe
  = do forM fs $ T.mapM printAnn
       return F.Safe 

printAnn :: AnnBare -> IO () 
printAnn (Ann l fs) = when (not $ null fs) $ putStrLn $ printf "At %s: %s" (ppshow l) (ppshow fs)

-------------------------------------------------------------------------------
-- | TypeCheck Nano Program ---------------------------------------------------
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
tcNano :: (F.Reftable r) => Nano AnnSSA (RType r) -> TCM (Nano AnnType (RType r)) 
-------------------------------------------------------------------------------
tcNano p@(Nano {code = Src fs})
  = do m     <- tcNano' $ toType <$> p 
       return $ p {code = Src $ (patchAnn m <$>) <$> fs}

tcNano'     :: Nano AnnSSA Type -> TCM AnnInfo  
tcNano' pgm = M.unions <$> (forM fs $ tcFun γ0)
  where
    γ0     = env pgm
    Src fs = code pgm

logAnn :: AnnSSA -> TCM AnnSSA 
logAnn z@(Ann l fs) = forM_ fs (addAnn l) >> return z

patchAnn              :: AnnInfo -> AnnSSA -> AnnType
patchAnn m (Ann l fs) = Ann l $ sortNub $ (M.lookupDefault [] l m) ++ fs


-------------------------------------------------------------------------------
-- | Type Check Environment ---------------------------------------------------
-------------------------------------------------------------------------------

--   We define this alias as the "output" type for typechecking any entity
--   that can create or affect binders (e.g. @VarDecl@ or @Statement@)
--   @Nothing@ means if we definitely hits a "return" 
--   @Just γ'@ means environment extended with statement binders

type TCEnv = Maybe (Env Type)

-------------------------------------------------------------------------------
-- | TypeCheck Function -------------------------------------------------------
-------------------------------------------------------------------------------

tcFun    :: Env Type -> FunctionStatement AnnSSA -> TCM AnnInfo  
tcFun γ (FunctionStmt l f xs body) 
  = do (αs, ts, t)    <- funTy l γ f xs
       let γ'          = envAddFun l f αs xs ts t γ 
       q              <- tcStmts γ' body
       when (isJust q) $ unifyType l "Missing return" f tVoid t
       annm           <- getAnns
       mapM_ (validInst αs) (M.toList annm)
       return          $ annm
        
funTy l γ f xs 
  = case bkFun =<< envFindTy f γ of
      Nothing        -> tcError l $ errorNonFunction f
      Just (αs,ts,t) -> do when (length xs /= length ts) $ tcError l $ errorArgMismatch
                           return (αs,ts,t)

envAddFun l f αs xs ts t = envAdds tyBinds . envAdds (varBinds xs ts) . envAddReturn f t 
  where  
    tyBinds              = [(Loc (srcPos l) α, tVar α) | α <- αs]
    varBinds             = zip

validInst αs (l, ts)
  = case S.toList (βS `S.difference` αS) of 
      [] -> return ()
      βs -> tcError l $ errorFreeTyVar βs 
    where 
      βS = free ts
      αS = S.fromList αs


--------------------------------------------------------------------------------
tcSeq :: (Env Type -> a -> TCM TCEnv) -> Env Type -> [a] -> TCM TCEnv
--------------------------------------------------------------------------------

tcSeq f             = foldM step . Just 
  where 
    step Nothing _  = return Nothing
    step (Just γ) x = f γ x

--------------------------------------------------------------------------------
tcStmts :: Env Type -> [Statement AnnSSA]  -> TCM TCEnv
--------------------------------------------------------------------------------
tcStmts = tcSeq tcStmt

-------------------------------------------------------------------------------
tcStmt :: Env Type -> Statement AnnSSA -> TCM TCEnv  
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
       unifyType l "If condition" e t tBool
       γ1    <- tcStmt γ s1
       γ2    <- tcStmt γ s2
       envJoin l γ γ1 γ2

-- var x1 [ = e1 ]; ... ; var xn [= en];
tcStmt γ (VarDeclStmt _ ds)
  = tcSeq tcVarDecl γ ds

-- return e 
tcStmt γ (ReturnStmt l eo) 
  = do t <- maybe (return tVoid) (tcExpr γ) eo 
       unifyType l "Return" eo t $ envFindReturn γ 
       return Nothing

-- OTHER (Not handled)
tcStmt γ s 
  = convertError "tcStmt" s

-------------------------------------------------------------------------------
tcVarDecl :: Env Type -> VarDecl AnnSSA -> TCM TCEnv  
-------------------------------------------------------------------------------

tcVarDecl γ (VarDecl l x (Just e)) 
  = tcAsgn γ l x e  
tcVarDecl γ (VarDecl l x Nothing)  
  = return $ Just γ

------------------------------------------------------------------------------------
tcAsgn :: Env Type -> AnnSSA -> Id AnnSSA -> Expression AnnSSA -> TCM TCEnv
------------------------------------------------------------------------------------

tcAsgn γ l x e 
  = do t <- tcExpr γ e
       return $ Just $ envAdds [(x, t)] γ

-------------------------------------------------------------------------------
tcExpr :: Env Type -> Expression AnnSSA -> TCM Type
-------------------------------------------------------------------------------

tcExpr _ (IntLit _ _)               
  = return tInt 

tcExpr _ (BoolLit _ _)
  = return tBool

tcExpr γ (VarRef l x)
  = maybe (tcError l $ errorUnboundId x) return $ envFindTy x γ

tcExpr γ (PrefixExpr l o e)
  = tcCall γ l [e] (prefixOpTy o)

tcExpr γ (InfixExpr l o e1 e2)        
  = tcCall γ l [e1, e2] (infixOpTy o)

tcExpr γ (CallExpr l e es)
  = tcCall γ l es =<< tcExpr γ e 

tcExpr γ e 
  = convertError "tcExpr" e

----------------------------------------------------------------------------------
tcCall :: Env Type -> AnnSSA -> [Expression AnnSSA]-> Type -> TCM Type
----------------------------------------------------------------------------------
tcCall γ l es ft 
  = do (_,its,ot) <- instantiate l ft
       ets        <- mapM (tcExpr γ) es
       θ'         <- unifyTypes l "" its ets
       return      $ apply θ' ot

instantiate l ft 
  = do t' <- freshTyArgs (srcPos l) $ bkAll ft 
       maybe err return   $ bkFun t'
    where
       err = tcError l $ errorNonFunction ft


----------------------------------------------------------------------------------
envJoin :: AnnSSA -> Env Type -> TCEnv -> TCEnv -> TCM TCEnv 
----------------------------------------------------------------------------------

envJoin _ _ Nothing x           = return x
envJoin _ _ x Nothing           = return x
envJoin l γ (Just γ1) (Just γ2) = envJoin' l γ γ1 γ2 

-- OLD
-- envJoin' l _ γ1 γ2 
--   = do forM_ (envToList $ envLefts γall) err 
--        return (Just $ envRights γall)
--     where 
--       γall = envIntersectWith meet γ1 γ2
--       meet = \t1 t2 -> if t1 == t2 then Right t1 else Left (t1,t2)
--       err  = \(y, (t, t')) -> tcError l $ errorJoin y t t'

-- NEW update to use the SSA-Vars. Much simpler.

envJoin' l γ γ1 γ2
  = do let xs = [x | PhiVar x <- ann_fact l]
       ts    <- mapM (getPhiType l γ1 γ2) xs
       return $ Just $ envAdds (zip xs ts) γ 
  
getPhiType l γ1 γ2 x
  = case (envFindTy x γ1, envFindTy x γ2) of
      (Just t1, Just t2) -> if (t1 == t2) 
                              then return t1 
                              else tcError l $ errorJoin x t1 t2
      (_      , _      ) -> tcError l $ bugUnboundPhiVar x
      
