module Language.Nano.Typecheck.Typecheck (verifyFile, typeCheck) where 

import           Control.Applicative                ((<$>)) -- (<*>))
import           Control.Monad                
import           Control.Monad.State()
import qualified Data.HashSet        as S 
import qualified Data.HashMap.Strict as M 
import           Data.List           (nub)
import qualified Data.Traversable    as T
-- import           Data.Monoid
import           Data.Maybe                         (catMaybes, isJust, fromJust) -- fromMaybe, maybeToList)
import           Text.PrettyPrint.HughesPJ          (text, render, vcat)
import           Text.Printf                        (printf)

import           Language.Nano.Errors
import           Language.Nano.Types
import           Language.Nano.Env
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Parse 
import           Language.Nano.Typecheck.TCMonad
import           Language.Nano.Typecheck.Subst
import           Language.Nano.SSA.SSA
import qualified Language.Fixpoint.Types as F
-- import           Language.Fixpoint.Interface        (resultExit)
import           Language.Fixpoint.Misc             
import           Language.Fixpoint.PrettyPrint        (showpp)
-- import           System.Exit                        (exitWith)
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Parser        (SourceSpan (..))

--------------------------------------------------------------------------------
-- | Top-level Verifier 
--------------------------------------------------------------------------------
verifyFile :: FilePath -> IO (F.FixResult SourceSpan)
--------------------------------------------------------------------------------
--verifyFile f = tc =<< parseNanoFromFile f
--  where 
--   tc pgm    = either unsafe safe . execute pgm . tcNano . ssaTransform $ pgm 

-------------------------------------------------------------------------------
typeCheck     :: (F.Reftable r) => Nano AnnSSA (RType r) -> Nano AnnType (RType r) 
-------------------------------------------------------------------------------
typeCheck pgm = either crash id . execute pgm . tcNano $ pgm 
  where 
    crash     = errorstar . render . vcat . map (text . ppErr)

-- DEBUG MODE
verifyFile f 
   = do nano <- parseNanoFromFile f 
        {-donePhase Loud "Parse"-}
        {-putStrLn . render . pp $ nano-}
        let nanoSsa = ssaTransform nano
        donePhase Loud "SSA Transform"
        putStrLn . render . pp $ nanoSsa
        r    <- either unsafe safe $ execute nanoSsa $ tcNano nanoSsa
        donePhase Loud "Typechecking"
        return r

unsafe errs = do putStrLn "\n\n\nErrors Found!\n\n" 
                 forM_ errs (putStrLn . ppErr) 
                 return $ F.Unsafe (fst <$> errs)

ppErr (l, e) = printf "Error at %s\n  %s\n" (ppshow l) e

safe (Nano {code = Src fs})
  = do forM fs $ T.mapM printAnn
       return F.Safe 

printAnn :: AnnBare -> IO () 
printAnn (Ann l fs) = when (not $ null fs) $ putStrLn $ printf "At %s: %s" (ppshow l) (ppshow fs)

-------------------------------------------------------------------------------
-- | TypeCheck Nano Program ---------------------------------------------------
-------------------------------------------------------------------------------

patch p = tcNano p >>= patchPgm 

-------------------------------------------------------------------------------
tcNano :: (F.Reftable r) => Nano AnnSSA (RType r) -> TCM (Nano AnnType (RType r)) 
-------------------------------------------------------------------------------
tcNano p@(Nano {code = Src fs})
  = do m     <- tcNano' $ toType <$> p 
       return $ p {code = Src $ (patchAnn m <$>) <$> fs}


-------------------------------------------------------------------------------
tcNano' :: Nano AnnSSA Type -> TCM AnnInfo  
-------------------------------------------------------------------------------
tcNano' pgm@(Nano {code = Src fs}) 
  = do tcStmts (specs pgm) fs
       M.unions <$> getAllAnns

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

tcFun    :: Env Type -> FunctionStatement AnnSSA -> TCM TCEnv 
tcFun γ (FunctionStmt l f xs body) 
  = do (ft, (αs, ts, t)) <- funTy l f xs
       let γ'  = envAdds [(f, ft)] γ
       let γ'' = envAddFun l f αs xs ts t γ'
       accumAnn (\a -> catMaybes (map (validInst γ'') (M.toList {- $ tracePP "tcFun" -} a))) $  
         do q              <- tcStmts γ'' body
            when (isJust q) $ unifyType l "Missing return" f tVoid t
       return $ Just γ' 

tcFun _  _ = error "Calling tcFun not on FunctionStatement"

funTy l f xs 
  = do ft <- getDefType f 
       case bkFun ft of
         Nothing        -> logError (ann l) (errorUnboundId f) (tErr, tFunErr)
         Just (αs,ts,t) -> do when (length xs /= length ts) $ logError (ann l) errorArgMismatch ()
                              return (ft, (αs, b_type <$> ts, t))

envAddFun _ f αs xs ts t = envAdds tyBinds . envAdds (varBinds xs ts) . envAddReturn f t 
  where  
    tyBinds              = [(tVarId α, tVar α) | α <- αs]
    varBinds             = zip
    
    -- tyBinds              = [(Loc (srcPos l) α, tVar α) | α <- αs]

validInst γ (l, ts)
  = case [β | β <- S.toList $ free ts, not ((tVarId β) `envMem` γ)] of
      [] -> Nothing
      βs -> Just (l, errorFreeTyVar βs)
   
-- | Strings ahead: HACK Alert
tVarId (TV a l) = Id l $ "TVAR$$" ++ F.symbolString a   

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
  = do (γo, t) <- tcExpr γ e
       unifyType l "If condition" e t tBool
       let γ' = fromJust γo
       γ1      <- tcStmt γ' s1
       γ2      <- tcStmt γ' s2
       envJoin l γ' γ1 γ2

-- var x1 [ = e1 ]; ... ; var xn [= en];
tcStmt γ (VarDeclStmt _ ds)
  = tcSeq tcVarDecl γ ds

-- return e 
tcStmt γ (ReturnStmt l eo) 
  = do (γ', t) <- maybe (return (Just γ, tVoid)) (tcExpr γ) eo 
       let γ'' = fromJust γ'
       subType l γ'' eo t $ envFindReturn γ''
       return Nothing

tcStmt γ s@(FunctionStmt _ _ _ _)
  = tcFun γ s

-- OTHER (Not handled)
tcStmt _ s 
  = convertError "TC Cannot Handle: tcStmt" s

-------------------------------------------------------------------------------
tcVarDecl :: Env Type -> VarDecl AnnSSA -> TCM TCEnv  
-------------------------------------------------------------------------------

tcVarDecl γ (VarDecl l x (Just e)) 
  = tcAsgn γ l x e  
tcVarDecl γ (VarDecl _ _ Nothing)  
  = return $ Just γ

------------------------------------------------------------------------------------
tcAsgn :: Env Type -> AnnSSA -> Id AnnSSA -> Expression AnnSSA -> TCM TCEnv
------------------------------------------------------------------------------------

tcAsgn γ _ x e 
  = do (γ', t) <- tcExpr γ e
       return $ Just $ envAdds [(x, t)] (fromJust γ')

-------------------------------------------------------------------------------
tcExpr :: Env Type -> Expression AnnSSA -> TCM (TCEnv, Type)
-------------------------------------------------------------------------------

tcExpr γ (IntLit _ _)               
  = return (Just γ, tInt)

tcExpr γ (BoolLit _ _)
  = return (Just γ, tBool)

tcExpr γ (StringLit _ _)
  = return (Just γ, tString)

tcExpr γ (VarRef l x)
  = case envFindTy x γ of 
      Nothing -> logError (ann l) (errorUnboundIdEnv x γ) (Just γ, tErr)
      Just z  -> return (Just γ, z) 

tcExpr γ (PrefixExpr l o e)
  = tcCall γ l o [e] (prefixOpTy o γ)

tcExpr γ (InfixExpr l o e1 e2)        
  = tcCall γ l o [e1, e2] (infixOpTy o γ)

tcExpr γ (CallExpr l e es)
  = tcExpr γ e >>= \(γ',t) -> tcCall (fromJust γ') l e es t

tcExpr _ e 
  = convertError "tcExpr" e

----------------------------------------------------------------------------------
tcCall :: (PP fn) => Env Type -> AnnSSA -> fn -> [Expression AnnSSA]-> Type -> TCM (TCEnv, Type)
----------------------------------------------------------------------------------
tcCall γ0 l fn es ft 
  = do (_,its,ot) <- instantiate l fn ft
       (γ2, ets)  <- foldM (\(γ,ts) e -> 
                              tcExpr (fromJust γ) e >>= 
                              \(γ1,t) -> return (γ1, ts++[t])) (Just γ0,[]) es
       (γ3, θ')   <- subTypes l (fromJust γ2) (map Just es) ets (b_type <$> its)
       return      $ (Just γ3 , apply θ' ot)

instantiate l fn ft 
  = do t' <- freshTyArgs (srcPos l) $ bkAll ft 
       maybe err return   $ bkFun t'
    where
       err = logError (ann l) (errorNonFunction fn ft) tFunErr


----------------------------------------------------------------------------------
envJoin :: AnnSSA -> Env Type -> TCEnv -> TCEnv -> TCM TCEnv 
----------------------------------------------------------------------------------

envJoin _ _ Nothing x           = return x
envJoin _ _ x Nothing           = return x
envJoin l γ (Just γ1) (Just γ2) = envJoin' l γ γ1 γ2 

envJoin' l γ γ1 γ2
  = do let xs = [x | PhiVar x <- ann_fact l]
       ts    <- mapM (getPhiType l γ1 γ2) xs
       return $ Just $ envAdds (zip xs ts) γ 
  
getPhiType l γ1 γ2 x
  = case (envFindTy x γ1, envFindTy x γ2) of
      (Just t1, Just t2) -> if (t1 == t2) 
                              then return t1 
                              else mkUnion t1 t2 -- logError (ann l) (errorJoin x t1 t2) tErr
      (_      , _      ) -> if forceCheck x γ1 && forceCheck x γ2 
                              then logError (ann l) "Oh no, the HashMap GREMLIN is back...1" tErr
                              else logError (ann l) (bugUnboundPhiVar x) tErr
    where
      mkUnion t1 t2 = 
        case (prep t1, prep t2) of 
          (Just t1s, Just t2s) -> 
            case nub $ t1s ++ t2s of
              [ ] -> logError (ann l) (errorJoin x t1 t2) tErr
              [t] -> return $ t
              ts  -> return $ TApp TUn ts ()
          (_       , _       ) -> logError (ann l) (errorJoin x t1 t2) tErr
      prep (TApp TUn l _) = Just l
      prep t@(TApp _ _ _) = Just [t]
      prep t@(TVar _ _ )  = Just [t]
      prep _              = Nothing


forceCheck x γ 
  = elem x $ fst <$> envToList γ

