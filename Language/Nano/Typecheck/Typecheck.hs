{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TupleSections          #-}


module Language.Nano.Typecheck.Typecheck (verifyFile, typeCheck) where 

import           Control.Applicative                ((<$>))
import           Control.Monad                

import qualified Data.HashSet                       as HS 
import qualified Data.HashMap.Strict                as M 
import           Data.List                          (find)
import qualified Data.Traversable                   as T
import           Data.Monoid
import           Data.Maybe                         (catMaybes, isJust)
import           Data.Generics                   

import           Text.PrettyPrint.HughesPJ          (text, render, vcat, ($+$))
import           Text.Printf                        (printf)

import           Language.Nano.CmdLine              (getOpts)
import           Language.Nano.Errors
import           Language.Nano.Types
import           Language.Nano.Env
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Parse 
import           Language.Nano.Typecheck.TCMonad
import           Language.Nano.Typecheck.STMonad
import           Language.Nano.Typecheck.Subst
import           Language.Nano.SSA.SSA

import qualified Language.Fixpoint.Types            as F
import           Language.Fixpoint.Misc             as FM 
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Parser        (SourceSpan (..))
import           Debug.Trace                        hiding (traceShow)

import           System.Console.CmdArgs.Verbosity   as V


--------------------------------------------------------------------------------
-- | Top-level Verifier 
--------------------------------------------------------------------------------
verifyFile :: FilePath -> IO (F.FixResult (SourceSpan, String))
--------------------------------------------------------------------------------
--verifyFile f = tc =<< parseNanoFromFile f
--  where 
--   tc pgm    = either unsafe safe . execute pgm . tcNano . ssaTransform $ pgm 

-- | Debug mode
verifyFile f 
   = do nano <- parseNanoFromFile f
        whenLoud $ donePhase FM.Loud "Parse"
        {-putStrLn . render . pp $ nano-}
        let nanoSsa = ssaTransform nano
        whenLoud $ donePhase FM.Loud "SSA Transform"
        whenLoud $ putStrLn . render . pp $ nanoSsa
        let p =  execute nanoSsa $ tcAndPatch nanoSsa
        TC{ noFailCasts = nfc } <- getOpts
        r <- either unsafe (\q -> safe q >>= return . (`mappend` failCasts nfc q)) p
        whenLoud $ donePhase FM.Loud "Typechecking"
        return $ r


-------------------------------------------------------------------------------
typeCheck     :: (Data r, Typeable r, F.Reftable r) => Nano AnnSSA (RType r) -> (Nano AnnType (RType r))
-------------------------------------------------------------------------------
typeCheck pgm = either crash id (execute pgm (tcAndPatch pgm))
  where
    crash     = errorstar . render . vcat . map (text . ppErr)


unsafe errs = do putStrLn "\n\n\nErrors Found!\n\n" 
                 forM_ errs (putStrLn . ppErr) 
                 return $ F.Unsafe errs

ppErr (l, e) = printf "Error at %s\n  %s\n" (ppshow l) e

safe (Nano {code = Src fs})
  = do whenLoud $ forM_ fs $ T.mapM printAnn
       return F.Safe 

-------------------------------------------------------------------------------
failCasts :: (Data r, Typeable r) => 
  Bool -> Nano AnnSSA (RType r) -> F.FixResult (SourceSpan, String)
-------------------------------------------------------------------------------
failCasts False (Nano {code = Src fs}) | not $ null csts = F.Unsafe csts
                                       | otherwise       = F.Safe
  where csts = mapFst ann <$> allCasts fs
failCasts True   _                                       = F.Safe                                            
    

-------------------------------------------------------------------------------
allCasts :: [FunctionStatement AnnSSA] -> [(AnnSSA, [Char])]
-------------------------------------------------------------------------------
allCasts fs =  everything (++) ([] `mkQ` f) $ fs
  where f (DownCast l t)  = [(l, "Cast: " ++ ppshow t)]
        f (DeadCast l _)  = [(l, "DeadCode")]
        -- UpCasts are safe
        f _               = [ ]


-------------------------------------------------------------------------------
printAnn :: AnnBare -> IO () 
-------------------------------------------------------------------------------
printAnn (Ann l fs) = when (not $ null fs) $ putStrLn 
    $ printf "At %s: %s" (ppshow l) (ppshow fs)

-------------------------------------------------------------------------------
-- | TypeCheck Nano Program ---------------------------------------------------
-------------------------------------------------------------------------------
-- | The first argument true to tranform casted expressions e to Cast(e,T)
-------------------------------------------------------------------------------
tcAndPatch :: (Data r, Typeable r, F.Reftable r) => 
  Nano AnnSSA (RType r) -> TCM (Nano  AnnSSA (RType r))
-------------------------------------------------------------------------------
tcAndPatch p = 
  do  p1 <- tcNano p 
      p2 <- patchPgmM p1
      s  <- getSubst
      c  <- getCasts
      return $ trace (codePP p2 s c) p2
      -- return p2
  where 
    codePP (Nano {code = Src src}) sub cst = render $
          text "********************** CODE **********************"
      $+$ pp src
      $+$ text "***************** SUBSTITUTIONS ******************"
      $+$ pp sub
      $+$ text "******************** CASTS ***********************"
      $+$ pp cst
      $+$ text "**************************************************"


-------------------------------------------------------------------------------
tcNano :: (F.Reftable r) => Nano AnnSSA (RType r) -> TCM (Nano AnnType (RType r)) 
-------------------------------------------------------------------------------
tcNano p@(Nano {code = Src fs})
  = do m     <- tcNano' $ toType <$> p 
       return $ (trace "") $ p {code = Src $ (patchAnn m <$>) <$> fs}
    {-where-}
    {-  cachePP cache = render $-}
    {-        text "********************** CODE **********************"-}
    {-    $+$ pp cache-}
    {-    $+$ text "**************************************************"-}


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
--   @Nothing@ means if we definitely hit a "return" 
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
       accumAnn (\a -> catMaybes (map (validInst γ'') (M.toList a))) $  
         do q              <- tcStmts γ'' body
            when (isJust q) $ subTypeM_ l Nothing tVoid t
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
  = case [β | β <-  HS.toList $ free ts, not ((tVarId β) `envMem` γ)] of
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
tcStmt' γ (EmptyStmt _) 
  = return $ Just γ

-- x = e
tcStmt' γ (ExprStmt _ (AssignExpr l OpAssign (LVar lx x) e))   
  = tcAsgn γ l (Id lx x) e

-- e
tcStmt' γ (ExprStmt _ e)   
  = tcExpr γ e >> return (Just γ) 

-- s1;s2;...;sn
tcStmt' γ (BlockStmt _ stmts) 
  = tcStmts γ stmts 

-- if b { s1 }
tcStmt' γ (IfSingleStmt l b s)
  = tcStmt' γ (IfStmt l b s (EmptyStmt l))

-- if b { s1 } else { s2 }
tcStmt' γ (IfStmt l e s1 s2)
  = do  
    -- This check needs to be done even though 
    -- we're not gonna require to have a boolean 
    -- value here (see truthy and falsy)
        _ <- tcExpr γ e 
       -- subTypeM_ l (Just e) t tBool
        γ1      <- tcStmt' γ s1
        γ2      <- tcStmt' γ s2
        envJoin l γ γ1 γ2

-- var x1 [ = e1 ]; ... ; var xn [= en];
tcStmt' γ (VarDeclStmt _ ds)
  = tcSeq tcVarDecl γ ds

-- return e 
tcStmt' γ (ReturnStmt l eo) 
  = do t <- maybe (return tVoid) (tcExpr γ) eo
       subTypeM_ l eo t $ envFindReturn γ
       return Nothing

tcStmt' γ s@(FunctionStmt _ _ _ _)
  = tcFun γ s

-- OTHER (Not handled)
tcStmt' _ s 
  = convertError "TC Cannot Handle: tcStmt'" s

tcStmt γ s = tcStmt' γ s

-------------------------------------------------------------------------------
tcVarDecl :: Env Type -> VarDecl AnnSSA -> TCM TCEnv  
-------------------------------------------------------------------------------

tcVarDecl γ (VarDecl l x (Just e)) 
  = tcAsgn γ l x e  
tcVarDecl γ (VarDecl _ _ Nothing)  
-- TODO: add binding from the declared variable to undefined
  = return $ Just γ

------------------------------------------------------------------------------------
tcAsgn :: Env Type -> AnnSSA -> Id AnnSSA -> Expression AnnSSA -> TCM TCEnv
------------------------------------------------------------------------------------

tcAsgn γ _ x e 
  = do t <- tcExpr γ e
       return $ Just $ envAdds [(x, t)] γ



-------------------------------------------------------------------------------
tcExpr :: Env Type -> Expression AnnSSA -> TCM Type
-------------------------------------------------------------------------------
tcExpr γ e = setExpr (Just e) >> (tcExpr' γ e)


-------------------------------------------------------------------------------
tcExpr' :: Env Type -> Expression AnnSSA -> TCM Type
-------------------------------------------------------------------------------

tcExpr' _ (IntLit _ _)
  = return tInt

tcExpr' _ (BoolLit _ _)
  = return tBool

tcExpr' _ (StringLit _ _)
  = return tString

tcExpr' _ (NullLit _)
  = return tNull

tcExpr' γ (VarRef l x)
  = case envFindTy x γ of 
      Nothing -> logError (ann l) (errorUnboundIdEnv x γ) tErr
      Just z  -> return z

tcExpr' γ (PrefixExpr l o e)
  = tcCall γ l o [e] (prefixOpTy o γ)

tcExpr' γ (InfixExpr l o e1 e2)        
  = tcCall γ l o [e1, e2] (infixOpTy o γ)

tcExpr' γ (CallExpr l e es)
  = tcExpr γ e >>= tcCall γ l e es

tcExpr' γ (ObjectLit _ ps) 
  = tcObject γ ps

tcExpr' γ (DotRef l e i) 
  = tcAccess γ l e i

tcExpr' _ e 
  = convertError "tcExpr" e

----------------------------------------------------------------------------------
tcCall :: (PP fn) => Env Type -> AnnSSA -> fn -> [Expression AnnSSA]-> Type -> TCM Type
----------------------------------------------------------------------------------
tcCall γ l fn es ft 
  = do (_,its,ot) <- instantiate l fn ft
       ts         <- mapM (tcExpr γ) es
       θ'         <- subTypesM l (map Just es) ts (b_type <$> its)
       return      $ apply θ' ot

instantiate l fn ft 
  = do t' <-  {- tracePP "new Ty Args" <$> -} freshTyArgs (srcPos l) (bkAll ft)
       maybe err return   $ bkFun t'
    where
       err = logError (ann l) (errorNonFunction fn ft) tFunErr


----------------------------------------------------------------------------------
tcObject :: Env Type -> [(Prop AnnSSA, Expression AnnSSA)] -> TCM Type
----------------------------------------------------------------------------------
tcObject γ bs 
  = do 
      let (ps, es) = unzip bs
      bts <- zipWith B (map F.symbol ps) <$> mapM (tcExpr γ) es
      return $ TObj bts ()


----------------------------------------------------------------------------------
tcAccess :: Env Type -> AnnSSA -> Expression AnnSSA -> Id AnnSSA -> TCM Type
----------------------------------------------------------------------------------
tcAccess γ l e f = 
  tcExpr γ e >>= (unfoldTDefSafeTC >=> binders l e) >>= access f
  where
    access f               = return . maybe tUndef b_type . find (match $ F.symbol f)
    match s (B f _)        = s == f


----------------------------------------------------------------------------------
binders :: AnnSSA -> Expression AnnSSA -> Type -> TCM [Bind ()]
----------------------------------------------------------------------------------
binders _ _  (TObj b _ )       = return b
binders l e t@(TApp TUn ts _) = 
  case find isObj ts of
    Just _  -> error $ "UNIMPLEMENTED: Typecheck.hs, binders " -- addCast t' >> binders l e t'
    _       -> tcError l $ errorObjectAccess e t
binders l e t                 = tcError l $ errorObjectAccess e t
  

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
  

----------------------------------------------------------------------------------
getPhiType :: Annot b SourceSpan -> Env Type -> Env Type -> Id SourceSpan-> TCM Type
----------------------------------------------------------------------------------
getPhiType l γ1 γ2 x
  = do  td <- getTDefs
        case (envFindTy x γ1, envFindTy x γ2) of
          (Just t1, Just t2) -> return $ fst3 $ joinTypes (isSubType td) (t1, t2)
          (_      , _      ) -> if forceCheck x γ1 && forceCheck x γ2 
                                  then logError (ann l) "Oh no, the HashMap GREMLIN is back...1" tErr
                                  else logError (ann l) (bugUnboundPhiVar x) tErr


forceCheck x γ 
  = elem x $ fst <$> envToList γ

