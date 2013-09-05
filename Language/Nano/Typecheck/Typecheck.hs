{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE FlexibleContexts       #-}


module Language.Nano.Typecheck.Typecheck (verifyFile, typeCheck) where 

import           Control.Applicative                ((<$>))
import           Control.Monad                

import qualified Data.HashSet                       as HS 
import qualified Data.HashMap.Strict                as M 
import qualified Data.Traversable                   as T
import           Data.Monoid
import           Data.Maybe                         (catMaybes, isJust, fromJust)
import           Data.Generics                   

import           Text.PrettyPrint.HughesPJ          (text, render, vcat, ($+$), (<+>))
import           Text.Printf                        (printf)

import           Language.Nano.CmdLine              (getOpts)
import           Language.Nano.Errors
import           Language.Nano.Types
import           Language.Nano.Env
import           Language.Nano.Misc
import           Language.Nano.Typecheck.Compare
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Parse 
import           Language.Nano.Typecheck.TCMonad
import           Language.Nano.Typecheck.Subst
import           Language.Nano.SSA.SSA

import qualified Language.Fixpoint.Types            as F
import           Language.Fixpoint.Misc             as FM 
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Parser        (SourceSpan (..))
import           Debug.Trace                        hiding (traceShow)

import qualified System.Console.CmdArgs.Verbosity as V


--------------------------------------------------------------------------------
-- | Top-level Verifier 
--------------------------------------------------------------------------------
-- verifyFile :: FilePath -> IO (F.FixResult (SourceSpan, String))
--------------------------------------------------------------------------------
--verifyFile f = tc =<< parseNanoFromFile f
--  where 
--   tc pgm    = either unsafe safe . execute pgm . tcNano . ssaTransform $ pgm 

-- | Debug mode
verifyFile f 
   = do nano    <- parseNanoFromFile f
        V.whenLoud $ donePhase FM.Loud "Parse"
        putStrLn . render . pp $ nano
        let nanoSsa = ssaTransform nano
        V.whenLoud $ donePhase FM.Loud "SSA Transform"
        V.whenLoud $ putStrLn . render . pp $ nanoSsa
        verb    <- V.getVerbosity
        let p =  execute verb nanoSsa $ tcAndPatch nanoSsa
        TC{ noFailCasts = nfc } <- getOpts
        r <- either unsafe (\q -> safe q >>= return . (`mappend` failCasts nfc q)) p
        V.whenLoud $ donePhase FM.Loud "Typechecking"
        return $ r


-------------------------------------------------------------------------------
typeCheck ::
  (Data r, Ord r, PP r, F.Reftable r, Substitutable r (Fact_ r), Free (Fact_ r)) =>
  V.Verbosity -> Nano (AnnSSA_ r) (RType r) -> Nano (AnnType_ r) (RType r)
-------------------------------------------------------------------------------
typeCheck verb pgm = either crash id (execute verb pgm (tcAndPatch pgm))
  where
    crash          = errorstar . render . vcat . map (text . ppErr)


unsafe errs = do putStrLn "\n\n\nErrors Found!\n\n" 
                 forM_ errs (putStrLn . ppErr) 
                 return $ F.Unsafe errs

ppErr (l, e) = printf "Error at %s\n  %s\n" (ppshow l) e

safe (Nano {code = Src fs})
  = do V.whenLoud $ forM_ fs $ T.mapM printAnn
       return F.Safe 

-------------------------------------------------------------------------------
failCasts :: (Data r, Typeable r) => 
              Bool -> Nano (AnnSSA_ r) (RType r) -> F.FixResult (SourceSpan, String)
-------------------------------------------------------------------------------
failCasts False (Nano {code = Src fs}) | not $ null csts = F.Unsafe csts
                                       | otherwise       = F.Safe
  where csts = mapFst ann <$> allCasts fs
failCasts True   _                                       = F.Safe                                            
    

-------------------------------------------------------------------------------
allCasts :: (Data r, Typeable r) => [FunctionStatement (AnnSSA_ r)] -> [((AnnSSA_ r), [Char])]
-------------------------------------------------------------------------------
allCasts fs =  everything (++) ([] `mkQ` f) $ fs
  where f (DownCast l t)  = [(l, "DownCast: " ++ ppshow t)]
        f (DeadCast l _)  = [(l, "DeadCode")]
        -- UpCasts are safe
        f _               = [ ]


printAnn (Ann l fs) = when (not $ null fs) $ putStrLn 
    $ printf "At %s: %s" (ppshow l) (ppshow fs)

-------------------------------------------------------------------------------
-- | TypeCheck Nano Program ---------------------------------------------------
-------------------------------------------------------------------------------
-- | The first argument true to tranform casted expressions e to Cast(e,T)
-------------------------------------------------------------------------------
-- tcAndPatch :: (Data r, Typeable r, F.Reftable r, PP r, Ord r) => 
--     Nano (AnnSSA_ r) (RType r) -> TCM r (Nano (AnnSSA_ r) (RType r))
-------------------------------------------------------------------------------
tcAndPatch p = 
  do  checkTypeDefs p
      p1 <- tcNano p 
      p2 <- patchPgmM p1
      s  <- getSubst
      c  <- getCasts
      whenQuiet' (return p2) (return $ trace (codePP p2 s c) p2)
      -- return p1
  where 
    codePP (Nano {code = Src src}) sub cst = render $
          text "********************** CODE **********************"
      $+$ pp src
      $+$ text "***************** SUBSTITUTIONS ******************"
      $+$ pp sub
      $+$ text "******************** CASTS ***********************"
      $+$ vcat ((\(e,t) -> (pp $ ann $ getAnnotation e) <+> pp (e,t)) <$> cst)
      $+$ text "**************************************************"



-------------------------------------------------------------------------------
checkTypeDefs :: (Data r, Typeable r, F.Reftable r) => Nano (AnnSSA_ r) (RType r) -> TCM r ()
-------------------------------------------------------------------------------
checkTypeDefs pgm = reportAll $ grep
  where 
    ds        = defs pgm 
    ts        = tDefs pgm
    reportAll = mapM_ report
    report t  = tcError (srcPos t) $ errorUnboundType (ppshow t)

    -- There should be no undefined type constructors
    grep :: [Id SourceSpan] = everything (++) ([] `mkQ` g) ds
    g (TDef i) | not $ envMem i ts = [i]
    g _                            = [ ]
  
    -- TODO: Also add check for free top-level type variables, i.e. make sure 
    -- all type variables used are bound. Use something like:
    -- @everythingWithContext@


-------------------------------------------------------------------------------
tcNano :: (Ord r, PP r, F.Reftable r, Substitutable r (Fact_ r), Free (Fact_ r)) =>
  Nano (AnnSSA_ r) (RType r) -> TCM r (Nano (AnnType_ r) (RType r))
-------------------------------------------------------------------------------
tcNano p@(Nano {code = Src fs})
  = do m     <- tcNano' $ {- toType <$> -} p 
       return $ (trace "") $ p {code = Src $ (patchAnn m <$>) <$> fs}
    {-where-}
    {-  cachePP cache = render $-}
    {-        text "********************** CODE **********************"-}
    {-    $+$ pp cache-}
    {-    $+$ text "**************************************************"-}


-------------------------------------------------------------------------------
-- tcNano' :: Nano (AnnSSA_ r) (RType r) -> TCM r AnnInfo  
-------------------------------------------------------------------------------
tcNano' pgm@(Nano {code = Src fs}) 
  = do tcStmts (specs pgm) fs
       M.unions <$> getAllAnns

-- patchAnn              :: AnnInfo -> (AnnSSA_ r) -> (AnnType_ r)
patchAnn m (Ann l fs) = Ann l $ sortNub $ (M.lookupDefault [] l m) ++ fs

-------------------------------------------------------------------------------
-- | (RType r) Check Environment ---------------------------------------------------
-------------------------------------------------------------------------------

--   We define this alias as the "output" type for typechecking any entity
--   that can create or affect binders (e.g. @VarDecl@ or @Statement@)
--   @Nothing@ means if we definitely hit a "return" 
--   @Just γ'@ means environment extended with statement binders

type TCEnv r = Maybe (Env (RType r))

-------------------------------------------------------------------------------
-- | TypeCheck Function -------------------------------------------------------
-------------------------------------------------------------------------------

-- tcFun    :: (F.Reftable r) => Env (RType r) -> FunctionStatement (AnnSSA_ r) -> TCM r (TCEnv r)
tcFun γ (FunctionStmt l f xs body) 
  = do (ft, (αs, ts, t)) <- funTy l f xs
       let γ'  = envAdds [(f, ft)] γ
       let γ'' = envAddFun l f αs xs ts t γ'
       accumAnn (\a -> catMaybes (map (validInst γ'') (M.toList a))) $  
         do q              <- tcStmts γ'' body
            when (isJust q) $ void $ unifyTypeM l "Missing return" f tVoid t
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
tcSeq :: (Env (RType r) -> a -> TCM r (TCEnv r)) -> Env (RType r) -> [a] -> TCM r (TCEnv r)
--------------------------------------------------------------------------------

tcSeq f             = foldM step . Just 
  where 
    step Nothing _  = return Nothing
    step (Just γ) x = f γ x

--------------------------------------------------------------------------------
tcStmts :: (Ord r, PP r, F.Reftable r, Substitutable r (Fact_ r), Free (Fact_ r)) =>
            Env (RType r) -> [Statement (AnnSSA_ r)] -> TCM r (TCEnv r)
--------------------------------------------------------------------------------
tcStmts = tcSeq tcStmt

-------------------------------------------------------------------------------
tcStmt  :: (Ord r, PP r, F.Reftable r, Substitutable r (Fact_ r), Free (Fact_ r)) =>
            Env (RType r) -> Statement (AnnSSA_ r) -> TCM r (TCEnv r)
-------------------------------------------------------------------------------
-- skip
tcStmt' γ (EmptyStmt _) 
  = return $ Just γ

-- x = e
tcStmt' γ (ExprStmt _ (AssignExpr l OpAssign (LVar lx x) e))   
  = tcAsgn γ l (Id lx x) e

-- e1.x = e2
-- @e3.x@ should have the exact same type with @e2@
tcStmt' γ (ExprStmt _ (AssignExpr l2 OpAssign (LDot l3 e3 x) e2))
  = do  t2 <- tcExpr γ e2 
        t3 <- tcExpr γ e3
        tx <- safeDotAccess x t2
        unifyTypeM l2 "DotRef" e2 t2 tx
        return $ Just γ 
-- No strong updates allowed here - so return the same envirnment      

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
  = do  t <- tcExpr γ e 
    -- Doing check for boolean for the conditional for now
    -- TODO: Will have to suppert truthy/falsy later.
        unifyTypeM l "If condition" e t tBool
        γ1      <- tcStmt' γ s1
        γ2      <- tcStmt' γ s2
        envJoin l γ γ1 γ2

-- var x1 [ = e1 ]; ... ; var xn [= en];
tcStmt' γ (VarDeclStmt _ ds)
  = tcSeq tcVarDecl γ ds

-- return e 
tcStmt' γ (ReturnStmt l eo) 
  = do  t           <- maybe (return tVoid) (tcExpr γ) eo
        let rt       = envFindReturn γ 
        θ           <- unifyTypeM l "Return" eo t rt
        -- Apply the substitution
        let (rt',t') = mapPair (apply θ) (rt,t)
        -- Subtype the arguments against the formals and cast if 
        -- necessary based on the direction of the subtyping outcome
        maybeM_ (\e -> castM e t' rt') eo
        return Nothing

tcStmt' γ s@(FunctionStmt _ _ _ _)
  = tcFun γ s

-- OTHER (Not handled)
tcStmt' _ s 
  = convertError "TC Cannot Handle: tcStmt'" s

tcStmt γ s = tcStmt' γ s

-------------------------------------------------------------------------------
tcVarDecl :: (Ord r, PP r, F.Reftable r) => Env (RType r) -> VarDecl (AnnSSA_ r) -> TCM r (TCEnv r)
-------------------------------------------------------------------------------

tcVarDecl γ (VarDecl l x (Just e)) 
  = tcAsgn γ l x e  
tcVarDecl γ (VarDecl _ _ Nothing)  
-- TODO: add binding from the declared variable to undefined
  = return $ Just γ

------------------------------------------------------------------------------------
tcAsgn :: (PP r, Ord r, F.Reftable r) => 
  Env (RType r) -> (AnnSSA_ r) -> Id (AnnSSA_ r) -> Expression (AnnSSA_ r) -> TCM r (TCEnv r)
------------------------------------------------------------------------------------

tcAsgn γ _ x e 
  = do t <- tcExpr γ e
       return $ Just $ envAdds [(x, t)] γ



-------------------------------------------------------------------------------
tcExpr :: (Ord r, PP r, F.Reftable r) => Env (RType r) -> Expression (AnnSSA_ r) -> TCM r (RType r)
-------------------------------------------------------------------------------
tcExpr γ e = setExpr (Just e) >> (tcExpr' γ e)


-------------------------------------------------------------------------------
tcExpr' :: (Ord r, PP r, F.Reftable r) => Env (RType r) -> Expression (AnnSSA_ r) -> TCM r (RType r)
-------------------------------------------------------------------------------

tcExpr' _ (IntLit _ _)
  = return tInt

tcExpr' _ (BoolLit _ _)
  = return tBool

tcExpr' _ (StringLit _ _)
  = return tString

tcExpr' _ (NullLit _)
  = return tNull

tcExpr' γ (ArrayLit _ es)
  = tcArray γ es

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

tcExpr' γ (BracketRef l e (StringLit _ s))
  = tcAccess γ l e s

{--- General case of dynamic key dictionary access-}
{-tcExpr' γ (BracketRef l e1 e2)-}
{-  = do  t2 <- tcExpr γ e2-}
{-        unifyTypeM l "BracketRef" e2 t2 tString-}
{-        tcAccess γ l e1 s-}

tcExpr' _ e 
  = convertError "tcExpr" e

----------------------------------------------------------------------------------
tcCall :: (Ord r, F.Reftable r, PP r, PP fn) => 
  Env (RType r) -> (AnnSSA_ r) -> fn -> [Expression (AnnSSA_ r)]-> (RType r) -> TCM r (RType r)
----------------------------------------------------------------------------------
tcCall γ l fn es ft 
  = do  (_,ibs,ot)    <- instantiate l fn ft
        let its        = b_type <$> ibs
        -- Typecheck arguments
        ts            <- mapM (tcExpr γ) es
        -- Unify with formal parameter types
        θ             <- unifyTypesM l "tcCall" ts its
        -- Apply the substitution
        let (ts',its') = mapPair (apply θ) (ts,its)
        -- Subtype the arguments against the formals and cast if 
        -- necessary based on the direction of the subtyping outcome
        castsM es ts' its'
        return         $ apply θ ot

instantiate l fn ft 
  = do t' <-  {- tracePP "new Ty Args" <$> -} freshTyArgs (srcPos l) (bkAll ft)
       maybe err return   $ bkFun t'
    where
       err = logError (ann l) (errorNonFunction fn ft) tFunErr



----------------------------------------------------------------------------------
tcObject ::  (Ord r, F.Reftable r, PP r) => 
  Env (RType r) -> [(Prop (AnnSSA_ r), Expression (AnnSSA_ r))] -> TCM r (RType r)
----------------------------------------------------------------------------------
tcObject γ bs 
  = do 
      let (ps, es) = unzip bs
      bts <- zipWith B (map F.symbol ps) <$> mapM (tcExpr γ) es
      return $ TObj bts F.top


----------------------------------------------------------------------------------
tcAccess ::  (Ord r, F.Reftable r, PP r, F.Symbolic s, PP s) =>
  Env (RType r) -> (AnnSSA_ r) -> Expression (AnnSSA_ r) -> s -> TCM r (RType r)
----------------------------------------------------------------------------------
tcAccess γ _ e f = 
  -- TODO: handle case of Nothing being returned from dotAccess
  tcExpr γ e >>= safeDotAccess f


tcArray γ es = mapM (tcExpr γ) es >>= return . mkObj
  where 
    mkObj ts = tracePP (ppshow es) $ TObj (zipWith B (F.symbol . show <$> [0..]) ts) F.top


----------------------------------------------------------------------------------
envJoin :: (Ord r, F.Reftable r, PP r) =>
  (AnnSSA_ r) -> Env (RType r) -> TCEnv r -> TCEnv r -> TCM r (TCEnv r)
----------------------------------------------------------------------------------
envJoin _ _ Nothing x           = return x
envJoin _ _ x Nothing           = return x
envJoin l γ (Just γ1) (Just γ2) = envJoin' l γ γ1 γ2 

envJoin' l γ γ1 γ2
  = do let xs = [x | PhiVar x <- ann_fact l]
       ts    <- mapM (getPhiType l γ1 γ2) xs
       return $ Just $ envAdds (zip xs ts) γ 
  

----------------------------------------------------------------------------------
getPhiType ::  (Ord r, F.Reftable r, PP r) => 
  Annot b SourceSpan -> Env (RType r) -> Env (RType r) -> Id SourceSpan-> TCM r (RType r)
----------------------------------------------------------------------------------
getPhiType l γ1 γ2 x =
  case (envFindTy x γ1, envFindTy x γ2) of
    (Just t1, Just t2) -> do  env <- getTDefs
                              return $ fst4 $ compareTs env t1 t2
                          {-if t1 == t2-}
                          {-  then return t1 -}
                          {-  else tcError l $ errorJoin x t1 t2-}
    (_      , _      ) -> if forceCheck x γ1 && forceCheck x γ2 
                            then tcError (ann l) "Oh no, the HashMap GREMLIN is back...1"
                            else tcError (ann l) (bugUnboundPhiVar x)



forceCheck x γ 
  = elem x $ fst <$> envToList γ

