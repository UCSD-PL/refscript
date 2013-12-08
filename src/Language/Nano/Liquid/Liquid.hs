{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TupleSections        #-}

-- | Top Level for Refinement Type checker

module Language.Nano.Liquid.Liquid (verifyFile) where

import           Text.Printf                        (printf)
-- import           Text.PrettyPrint.HughesPJ          (Doc, text, render, ($+$), (<+>))
import           Control.Monad
import           Control.Applicative                ((<$>))
import           Control.Exception                  (throw)

import qualified Data.ByteString.Lazy               as B
import qualified Data.HashMap.Strict                as M
import           Data.Maybe                         (isJust, fromMaybe, listToMaybe)

import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Parser        (SourceSpan (..))

import qualified Language.Fixpoint.Types            as F
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Files
import           Language.Fixpoint.Interface        (solve)

import           Language.Nano.CmdLine              (getOpts)
import           Language.Nano.Errors
import           Language.Nano.Misc
import           Language.Nano.Types
import qualified Language.Nano.Annots               as A
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Parse
import           Language.Nano.Typecheck.Typecheck  (typeCheck) 
import           Language.Nano.Typecheck.Compare
import           Language.Nano.SSA.SSA
import           Language.Nano.Liquid.Types
import           Language.Nano.Liquid.CGMonad

import           System.Console.CmdArgs.Default

-- import           Debug.Trace                        (trace)

import qualified System.Console.CmdArgs.Verbosity as V

--------------------------------------------------------------------------------
verifyFile       :: FilePath -> IO (A.UAnnSol RefType, F.FixResult Error)
--------------------------------------------------------------------------------
verifyFile f =   
  do  p   <- parseNanoFromFile f
      cfg <- getOpts 
      verb    <- V.getVerbosity
      case typeCheck verb (ssaTransform p) of
        Left errs -> return $ (A.NoAnn, F.Crash errs "Type Errors")
        Right p'  -> reftypeCheck cfg f p'

-- DEBUG VERSION 
-- ssaTransform' x = tracePP "SSATX" $ ssaTransform x 

--------------------------------------------------------------------------------
reftypeCheck :: Config -> FilePath -> Nano AnnTypeR RefType -> IO (A.UAnnSol RefType, F.FixResult Error)
--------------------------------------------------------------------------------
reftypeCheck cfg f = solveConstraints f . generateConstraints cfg

--------------------------------------------------------------------------------
solveConstraints :: FilePath -> CGInfo -> IO (A.UAnnSol RefType, F.FixResult Error) 
--------------------------------------------------------------------------------
solveConstraints f cgi 
  = do (r, s)  <- solve def f [] $ cgi_finfo cgi
       let r'   = fmap (errorLiquid . srcPos . F.sinfo) r
       let ann  = cgi_annot cgi
       let sol  = applySolution s 
       return (A.SomeAnn ann sol, r') 



applySolution :: F.FixSolution -> A.UAnnInfo RefType -> A.UAnnInfo RefType 
applySolution = fmap . fmap . tx
  where
    tx s (F.Reft (x, zs))   = F.Reft (x, F.squishRefas (appSol s <$> zs))
    appSol _ ra@(F.RConc _) = ra 
    appSol s (F.RKvar k su) = F.RConc $ F.subst su $ M.lookupDefault F.PTop k s  

--------------------------------------------------------------------------------
generateConstraints     :: Config -> NanoRefType -> CGInfo 
--------------------------------------------------------------------------------
generateConstraints cfg pgm = getCGInfo cfg pgm $ consNano pgm

--------------------------------------------------------------------------------
consNano     :: NanoRefType -> CGM ()
--------------------------------------------------------------------------------
consNano pgm@(Nano {code = Src fs}) = consStmts (initCGEnv pgm) fs >> return ()

initCGEnv pgm = CGE (specs pgm) F.emptyIBindEnv [] emptyContext

--------------------------------------------------------------------------------
consFun :: CGEnv -> Statement (AnnType F.Reft) -> CGM CGEnv
--------------------------------------------------------------------------------
consFun g (FunctionStmt l f xs body) 
  = do ft             <- freshTyFun g l f =<< getDefType f
       g'             <- envAdds [(f, ft)] g
       forM (funTys l f xs ft) $ consFun1 l g' f xs body
       return g'

consFun _ s 
  = die $ bug (srcPos s) "consFun called not with FunctionStmt"

consFun1 l g' f xs body (i, ft) 
  = do g'' <- envAddFun l f i xs ft g'
       gm  <- consStmts g'' body
       maybe (return ()) (\g -> subType l g tVoid (envFindReturn g'')) gm


-- checkFormal x t 
--   | xsym == tsym = (x, t)
--   | otherwise    = errorstar $ errorArgName (srcPos x) xsym tsym
--   where 
--     xsym         = F.symbol x
--     tsym         = F.symbol t


-----------------------------------------------------------------------------------
-- envAddFun :: AnnTypeR -> CGEnv -> Id AnnTypeR -> [Id AnnTypeR] -> RefType -> CGM CGEnv
-----------------------------------------------------------------------------------
envAddFun l f i xs (αs, ts', t') g =   (return $ envPushContext i g) 
                                   >>= (return . envAddReturn f t' ) 
                                   >>= envAdds (varBinds xs ts')
                                   >>= envAdds tyBinds
  where
    tyBinds                        = [(Loc (srcPos l) α, tVar α) | α <- αs]
    varBinds                       = safeZip "envAddFun"
    -- (αs, ts', t')     = funTy l f xs ft

--------------------------------------------------------------------------------
consStmts :: CGEnv -> [Statement AnnTypeR]  -> CGM (Maybe CGEnv) 
--------------------------------------------------------------------------------
consStmts = consSeq consStmt

--------------------------------------------------------------------------------
consStmt :: CGEnv -> Statement AnnTypeR -> CGM (Maybe CGEnv) 
--------------------------------------------------------------------------------

-- | @consStmt g s@ returns the environment extended with binders that are
-- due to the execution of statement s. @Nothing@ is returned if the
-- statement has (definitely) hit a `return` along the way.

-- skip
consStmt g (EmptyStmt _) 
  = return $ Just g

-- x = e
consStmt g (ExprStmt _ (AssignExpr _ OpAssign (LVar lx x) e))   
  = consAsgn g (Id lx x) e

-- e3.x = e2
-- @e3.x@ should have the exact same type with @e2@
consStmt g (ExprStmt _ (AssignExpr l2 OpAssign (LDot _ e3 x) e2))
  = do  (x2,g2) <- consExpr g  e2
        (x3,g3) <- consExpr g2 e3
        let t2   = envFindTy x2 g2
            t3   = envFindTy x3 g3
        tx      <- safeGetProp x t3
        case tx of 
          -- NOTE: Atm assignment to non existing binding has no effect!
          TApp TUndef _ _ -> return $ Just g3
          _ -> do withAlignedM (subTypeContainers' "e.x = e" l2 g3) t2 tx
                  return   $ Just g3

-- e3[i] = e2
consStmt g (ExprStmt _ (AssignExpr l2 OpAssign (LBracket _ e3 (IntLit _ i)) e2))
  = do  (x2,g2) <- consExpr g  e2
        (x3,g3) <- consExpr g2 e3
        let t2   = tracePP (ppshow e2 ++ " ANF: " ++ ppshow x2) $ envFindTy x2 g2
            t3   = envFindTy x3 g3
        ti      <- safeGetIdx i t3
        withAlignedM (subTypeContainers' "e[i] = e" l2 g3) t2 ti
        return   $ Just g3


-- e
consStmt g (ExprStmt _ e)   
  = consExpr g e >> return (Just g) 

-- s1;s2;...;sn
consStmt g (BlockStmt _ stmts) 
  = consStmts g stmts 

-- if b { s1 }
consStmt g (IfSingleStmt l b s)
  = consStmt g (IfStmt l b s (EmptyStmt l))

-- HINT: 1. Use @envAddGuard True@ and @envAddGuard False@ to add the binder 
--          from the condition expression @e@ into @g@ to obtain the @CGEnv@ 
--          for the "then" and "else" statements @s1@ and @s2 respectively. 
--       2. Recursively constrain @s1@ and @s2@ under the respective environments.
--       3. Combine the resulting environments with @envJoin@ 

-- if e { s1 } else { s2 }
consStmt g (IfStmt l e s1 s2)
  = do (xe, ge) <- consExpr g e
       g1'      <- (`consStmt` s1) $ envAddGuard xe True  ge 
       g2'      <- (`consStmt` s2) $ envAddGuard xe False ge 
       envJoin l g g1' g2'




-- G   |- C :: xe, G1
-- Tinv = freshen(G1(x)) = {_|K}, ∀x∈Φ
-- G1(xe) = { v: boolean | ... }              //TODO
-- G1  |- G1(x) <: Tinv, ∀x∈Φ                 // Before the loop body: constraints on Φ Vars
-- G1, z:{xe}, ∀x∈Φ.x:Tinv |- B :: G2         // typecheck the loop where the condition holds and 
--                                            // the Φ vars have invariant types
-- G2  |- G2(x) <: Tinv, ∀x∈Φ                 // After the loop body: constraints on Φ vars
-- G3 = G2 + z:{¬xe} + { x: Tinv | x∈Φ }      // the environment after the loop should use the 
--                                            // invariant types for the Φ vars and also be updated
--                                            // with negation of the loop condition
-- ------------------------------------------
-- G |- while[Φ](C) { B } :: G3


-- var x0 = ...       // G0
-- while              // G1
--   ( ...x1... ) {
--   BODY
--                    // G2, x2
-- }
-- ... x1 ...         // G3,
  
consStmt g0 (WhileStmt l c b) = 
  do  
--  BEFORE LOOP
    (g1a , invs)   <- tInv g0 φ0
    zipWithM_ (sub "BeforeL-B" g0) ((`envFindTy` g0) <$> φ0) invs
--  CONDITION
    g1b            <- envAdds (zip φ1 invs) g1a
    (xe, g1c)      <- consExpr g1b c
    let g1d         = envAddGuard xe True g1c
--  BODY
    g2             <- fromJust' "Break loop" <$> consStmt g1d b
--  AFTER BODY
    zipWithM_ (sub "AfterLBD" g2) ((`envFindTy` g2) <$> φ2) invs
--  AFTER LOOP
    g3a            <- envAdds (zip φ1 invs) g0
    (xe', g3b)     <- consExpr g3a c
    -- Add the guard in the relevant environment
    return          $ Just $ envAddGuard xe' False g3b
  where 
    -- φ0: Φ vars before the loop
    -- φ1: Φ vars (invariant) at the beginning of the loop body
    -- φ2: Φ vars at the end of the loop body
    (φ0, φ1, φ2)    = unzip3 [φ | LoopPhiVar φs <- ann_fact l, φ <- φs]
    tInv g xs       = freshTyPhisWhile (ann l) g xs (toType <$> (`envFindTy` g) <$> xs)
    sub s g t t'    = subTypeContainers' ("While:" ++ s) l g t t'


-- var x1 [ = e1 ]; ... ; var xn [= en];
consStmt g (VarDeclStmt _ ds)
  = consSeq consVarDecl g ds

-- return e 
consStmt g (ReturnStmt l (Just e))
  = do  (xe, g') <- consExpr g e
        let te    = envFindTy xe g'
            rt    = envFindReturn g'
        if isTop rt
          then withAlignedM (subTypeContainers l g') te (setRTypeR te (rTypeR rt))
          else withAlignedM (subTypeContainers' "Return" l g') te rt
        return Nothing

-- return
consStmt _ (ReturnStmt _ Nothing)
  = return Nothing 

-- function f(x1...xn){ s }
consStmt g s@(FunctionStmt _ _ _ _)
  = Just <$> consFun g s

-- OTHER (Not handled)
consStmt _ s 
  = errorstar $ "consStmt: not handled " ++ ppshow s


------------------------------------------------------------------------------------
consVarDecl :: CGEnv -> VarDecl AnnTypeR -> CGM (Maybe CGEnv) 
------------------------------------------------------------------------------------
consVarDecl g v@(VarDecl _ x (Just e)) = do
    (x', g') <- consExprT g ct e
    Just <$> envAdds [(x, envFindTy x' g')] g'
  where
    ct = listToMaybe [ t | TAnnot t <- ann_fact $ getAnnotation v]

consVarDecl g (VarDecl _ _ Nothing)
  = return $ Just g

consExprT :: CGEnv -> Maybe RefType -> Expression AnnTypeR -> CGM (Id AnnTypeR, CGEnv) 
consExprT g ct (ArrayLit l es) = consArr l g ct es

consExprT g (Just ta) e = do
    (x, g') <- consExpr g e 
    checkElt g' $ envFindTy x g'
    return (x,g')
  where
    checkElt g t = withAlignedM (subTypeContainers l g) t ta
    l = getAnnotation e

consExprT g Nothing e = consExpr g e

------------------------------------------------------------------------------------
consAsgn :: CGEnv -> Id AnnTypeR -> Expression AnnTypeR -> CGM (Maybe CGEnv) 
------------------------------------------------------------------------------------
consAsgn g x e 
  = do (x', g') <- consExpr g e
       Just <$> envAdds [(x, envFindTy x' g')] g'


-- | @consExpr g e@ returns a pair (g', x') where x' is a fresh, 
--   temporary (A-Normalized) variable holding the value of `e`,
--   g' is g extended with a binding for x' (and other temps required for `e`)
------------------------------------------------------------------------------------
consExpr :: CGEnv -> Expression AnnTypeR -> CGM (Id AnnTypeR, CGEnv)
------------------------------------------------------------------------------------
consExpr g (Cast a e) 
  = consExpr g e >>= consCast a 

consExpr g (IntLit l i)               
  = envAddFresh "consExpr:IntLit" l (eSingleton tInt i) g

consExpr g (BoolLit l b)
  = envAddFresh "consExpr:BoolLit" l (pSingleton tBool b) g 

consExpr g (StringLit l s)
  = envAddFresh "consExpr:StringLit" l (eSingleton tString s) g

consExpr g (NullLit l)
  = envAddFresh "consExpr:NullLit" l tNull g

consExpr g (VarRef i x)
  = do addAnnot l x t
       return ({- trace ("consExpr:VarRef" ++ ppshow x ++ " : " ++ ppshow t)-} x, g) 
    where 
       t   = envFindTy x g
       l   = srcPos i

consExpr g (PrefixExpr l o e)
  = consCall g l o [e] (prefixOpTy o $ renv g)

consExpr g (InfixExpr l o e1 e2)        
  = consCall g l o [e1, e2] (infixOpTy o $ renv g)

consExpr g (CallExpr l e es)
  = do (x, g') <- consExpr g e 
       consCall g' l e es $ envFindTy x g'

consExpr g (DotRef l e s)
  = do  (x, g') <- consExpr g e
        t       <- safeGetProp (unId s) (envFindTy x g')
        envAddFresh "consExpr:DotRef" l t g'

-- e[i]
consExpr g (BracketRef l e i) = do
    (xe, g')  <- consExpr g e
    (xi, g'') <- consExpr g' i
    let ta = envFindTy xe g' 
        ti = envFindTy xi g''
    case (ta, ti) of
      (TArr _ _, TApp TInt _ _) -> do  
        t <- indexType ta
        withAlignedM (subTypeContainers' "Bounds" l g'') (eSingleton tInt xi) (bt xe)
        envAddFresh "consExpr:[IntLit]" l t g''
      _ -> errorstar $ "UNIMPLEMENTED[consExpr] " ++ 
                       "Can only use BracketRef to access array " ++
                       "type with an integer. Here used " ++ (ppshow ti)
  where
    -- bt x = { number | ((0 <= v) && (v < (len x)))}
    bt x = setRTypeR tInt (F.predReft $ F.PAnd [lo, hi x])
    lo   = F.PAtom F.Le (F.ECon $ F.I 0) v                             -- 0 <= v
    hi x = F.PAtom F.Lt v (F.EApp (F.stringSymbol "len") [F.eVar $ x]) -- v < len va
    v    = F.eVar $ F.vv Nothing


-- -- shadowed at the moment...
-- consExpr g (BracketRef l e (StringLit _ s))
--   = do  (x, g') <- consExpr g e
--         t       <- safeGetProp s (envFindTy x g') 
--         envAddFresh "consExpr[StringLit]" l t g'
        

consExpr g (ObjectLit l ps) 
  = consObj l g ps

consExpr _ e 
  = error $ (printf "consExpr: not handled %s" (ppshow e))


-------------------------------------------------------------------------------------------------
consCast :: AnnTypeR -> (Id AnnTypeR, CGEnv) -> CGM (Id AnnTypeR, CGEnv)
-------------------------------------------------------------------------------------------------
consCast a (x', g)
  = case envGetContextCast g a of
      Nothing        -> return (x, g)
      Just (UCST t)  -> consUpCast   g l x $ tracePP "consUpCast"   t
      Just (DCST t)  -> tracePP ("consDC2 @ " ++ ppshow x) <$> (consDownCast g l x $ tracePP ("consDC1" ++ ppshow x) t)
      Just (DC t)    -> consDeadCast g l   $ tracePP "consDeadCast" t
    where 
      l              = srcPos a
      x              = tracePP ("consCast a = " ++ ppshow a) x'

-- isCast l g a = isJust $ envGetContextCast l g a 

---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
consUpCast g l x t 
  = do γ      <- getTDefs
       let tx  = (`strengthen` (F.symbolReft x)) $ fst $ alignTs γ (envFindTy x g) t
       envAddFresh "consUpCast" l tx g

---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
consDownCast g l x t = 
  do γ   <- getTDefs
     tc  <- castTo l γ tx (toType t) 
     withAlignedM (subTypeContainers' "Downcast" l g) tx tc
     g'  <- envAdds [(x, tc)] g
     return (x, g')
     -- zog <- envAddFresh "consDownCast" l tc g'
     --return $ tracePP "consDOWNCAST" zog 
  where 
     tx   = envFindTy x g

castTo l γ t τ       = castStrengthen t . zipType2 γ botJoin t <$> bottify τ 
  where 
    bottify          = fmap (fmap F.bot) . true . rType 
    botJoin r1 r2 
      | F.isFalse r1 = r2
      | F.isFalse r2 = r1
      | otherwise    = die $ bug (srcPos l) msg
    msg              = printf "botJoin: t = %s τ = %s" (ppshow (t :: RefType)) (ppshow (τ :: Type))

castStrengthen t1 t2 
  | isUnion t1 && not (isUnion t2) = t2 `strengthen` (rTypeReft t1)
  | otherwise                      = t2

---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
consDeadCast g l t 
  = do subTypeContainers' "dead" l g tru fls
       envAddFresh "consDeadCast" l t g
    where
       tru = tTop
       fls = tTop `strengthen` F.predReft F.PFalse


---------------------------------------------------------------------------------------------
consCall :: (PP a) 
         => CGEnv -> AnnTypeR -> a -> [Expression AnnTypeR] -> RefType -> CGM (Id AnnTypeR, CGEnv)
---------------------------------------------------------------------------------------------

--   1. Fill in @instantiate@ to get a monomorphic instance of @ft@ 
--      i.e. the callee's RefType, at this call-site (You may want 
--      to use @freshTyInst@)
--   2. Use @consExpr@ to determine types for arguments @es@
--   3. Use @subTypes@ to add constraints between the types from (step 2) and (step 1)
--   4. Use the @F.subst@ returned in 3. to substitute formals with actuals in output type of callee.

consCall g l fn es ft0 
  = do (xes, g')    <- consScan consExpr g es
       let ts        = [envFindTy x g' | x <- xes]
       let ft        = calleeType l ts ft0
       (_,its,ot)   <- instantiate l g fn ft
       let (su, ts') = renameBinds its xes
       zipWithM_ (withAlignedM $ subTypeContainers' "call" l g') [envFindTy x g' | x <- xes] ts'
       envAddFresh "consCall" l (F.subst su ot) g'

-- instantiate :: AnnTypeR -> CGEnv -> RefType -> CGM RefType
instantiate l g fn ft 
  = do let (αs, t)      = bkAll ft
       let ts           = envGetContextTypArgs g l αs
       t'              <- freshTyInst l g αs ts t
       maybe err return $ bkFun t' 
    where 
       err = die $ errorNonFunction (srcPos l) fn ft  
    {-msg           = printf "instantiate [%s] %s %s" (ppshow $ ann l) (ppshow αs) (ppshow tbody)-}


---------------------------------------------------------------------------------
consScan :: (CGEnv -> a -> CGM (b, CGEnv)) -> CGEnv -> [a] -> CGM ([b], CGEnv)
---------------------------------------------------------------------------------
consScan step g xs  = go g [] xs 
  where 
    go g acc []     = return (reverse acc, g)
    go g acc (x:xs) = do (y, g') <- step g x
                         go g' (y:acc) xs

---------------------------------------------------------------------------------
consSeq  :: (CGEnv -> a -> CGM (Maybe CGEnv)) -> CGEnv -> [a] -> CGM (Maybe CGEnv) 
---------------------------------------------------------------------------------
consSeq f           = foldM step . Just 
  where 
    step Nothing _  = return Nothing
    step (Just g) x = f g x


---------------------------------------------------------------------------------
consObj :: AnnTypeR -> CGEnv -> [(Prop AnnTypeR, Expression AnnTypeR)] -> CGM (Id AnnTypeR, CGEnv)
---------------------------------------------------------------------------------
consObj l g pe = 
  do  let (ps, es) = unzip pe
      (xes, g')   <- consScan consExpr g es
      let pxs      = zipWith B (map F.symbol ps) $ (`envFindTy` g') <$> xes
      envAddFresh "consObj" l (TObj pxs F.top) g'
    

---------------------------------------------------------------------------------
consArr ::AnnTypeR -> CGEnv -> Maybe RefType -> [Expression AnnTypeR] -> CGM  (Id AnnTypeR, CGEnv)
---------------------------------------------------------------------------------
-- []
consArr l g (Just t@(TArr _ _ )) [] = 
  envAddFresh "consArr:empty" l t g

-- [e1, ... ]
-- XXX: The top-level refinement of the array is ignored at the moment.
-- Also adds a top-level refinement that captures the length of the array.
consArr l g (Just t@(TArr ta _)) es = do  
    (xes, g')   <- consScan consExpr g es
    let ts       = (`envFindTy` g') <$> xes
    checkElts g' ts
    let t'       = t `strengthen` lenReft
    envAddFresh "consArr" l t' g'
  where 
    checkElts    = mapM_ . checkElt
    checkElt g t = withAlignedM (subTypeContainers l g) t ta
    v            = rTypeValueVar t
    lenReft      = F.Reft (v, [F.RConc lenPred])
    lenPred      = F.PAtom F.Eq (F.expr $ length es) 
                    (F.EApp (F.stringSymbol "len") [F.eVar $ v])

consArr l _ Nothing _  = die $ errorMissingAnnot (srcPos l) "array literal" 
consArr l _ (Just _) _ = die $ errorBadAnnot     (srcPos l) "array literal" "array" 

