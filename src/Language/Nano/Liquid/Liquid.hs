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
import           Language.Nano.Typecheck.Subst      (getProp, getIdx)
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

initCGEnv pgm = CGE (specs pgm) F.emptyIBindEnv [] emptyContext (defs pgm)

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
       maybe (return ()) (\g -> subTypeContainers "return void" l g tVoid (envFindReturn g'')) gm


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

-- e1.fld = e2
consStmt g (ExprStmt l (AssignExpr l2 OpAssign (LDot l1 e1 fld) e2))
  = do (tfld, _) <- consPropRead getProp g l1 e1 fld
       (x2,  g') <- consExpr  g  e2
       let t2     = envFindTy x2 g'
       subTypeContainers "field-assignment" l2 g' t2 tfld
       return     $ Just g'


 -- do (e1', tfld) <- tcPropRead getProp γ l e1 fld
 --      (e2', t2)   <- tcExpr γ $ e2                    
 --      e2''        <- castM  ξ e2' t2 tfld
 --      return         (ExprStmt l (AssignExpr l2 OpAssign (LDot l1 e1' fld) e2''), Just γ)
 --   where
 --      ξ            = tce_ctx γ      


-- RJ: TODO FIELD -- @e3.x@ should have the exact same type with @e2@
-- RJ: TODO FIELD consStmt g (ExprStmt _ (AssignExpr l2 OpAssign (LDot _ e3 x) e2))
-- RJ: TODO FIELD   = do  (x2,g2) <- consExpr g  e2
-- RJ: TODO FIELD         (x3,g3) <- consExpr g2 e3
-- RJ: TODO FIELD         let t2   = envFindTy x2 g2
-- RJ: TODO FIELD             t3   = envFindTy x3 g3
-- RJ: TODO FIELD         tx      <- safeGetProp x t3
-- RJ: TODO FIELD         case tx of 
-- RJ: TODO FIELD           -- NOTE: Atm assignment to non existing binding has no effect!
-- RJ: TODO FIELD           TApp TUndef _ _ -> return $ Just g3
-- RJ: TODO FIELD           _ -> do {- ONLY AT CAST withAlignedM -} 
-- RJ: TODO FIELD                   (subTypeContainers "e.x = e" l2 g3) t2 tx
-- RJ: TODO FIELD                   return   $ Just g3

-- RJ: TODO JUNK -- e3[i] = e2
-- RJ: TODO JUNK consStmt g (ExprStmt _ (AssignExpr l2 OpAssign (LBracket _ e3 (IntLit _ i)) e2))
-- RJ: TODO JUNK   = do  (x2,g2) <- consExpr g  e2
-- RJ: TODO JUNK         (x3,g3) <- consExpr g2 e3
-- RJ: TODO JUNK         let t2   = tracePP (ppshow e2 ++ " ANF: " ++ ppshow x2) $ envFindTy x2 g2
-- RJ: TODO JUNK             t3   = envFindTy x3 g3
-- RJ: TODO JUNK         ti      <- safeGetIdx i t3
-- RJ: TODO JUNK         {- ONLY AT CAST withAlignedM -} 
-- RJ: TODO JUNK         (subTypeContainers "e[i] = e" l2 g3) t2 ti
-- RJ: TODO JUNK         return   $ Just g3

consStmt g (ExprStmt _ e) 
  = consExpr g e >> (return $ Just g)

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
    sub s g t t'    = subTypeContainers ("While:" ++ s) l g t t'


-- var x1 [ = e1 ]; ... ; var xn [= en];
consStmt g (VarDeclStmt _ ds)
  = consSeq consVarDecl g ds

-- return e 
consStmt g (ReturnStmt l (Just e))
  = do  (xe, g') <- consExpr g e
        let te    = envFindTy xe g'
            rt    = envFindReturn g'
        if isTop rt
          then {- ONLY AT CAST withAlignedM -} (subTypeContainers "ReturnTop" l g') te (setRTypeR te (rTypeR rt))
          else {- ONLY AT CAST withAlignedM -} (subTypeContainers "Return" l g') te rt
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
consVarDecl g v@(VarDecl _ x (Just e)) 
  = consAsgn g x e

--  do (x', g') <- consExprT g ct e
--     Just <$> envAdds [(x, envFindTy x' g')] g'
--  where
--    ct = listToMaybe [ t | TAnnot t <- ann_fact $ getAnnotation v]

consVarDecl g (VarDecl _ _ Nothing)
  = return $ Just g

------------------------------------------------------------------------------------
consExprT :: CGEnv -> Expression AnnTypeR -> Maybe RefType -> CGM (Id AnnTypeR, CGEnv) 
------------------------------------------------------------------------------------
consExprT g (ObjectLit l ps) to
  = consObjT l g ps to

consExprT g e to 
  = do (x, g') <- consExpr g e
       let te   = envFindTy x g'
       case to of
         Nothing -> return (x, g')
         Just t  -> do subTypeContainers "consExprT" l g' te t 
                       g'' <- envAdds [(x, t)] g'
                       return (x, g'')
    where
       l = getAnnotation e

------------------------------------------------------------------------------------
consAsgn :: CGEnv -> Id AnnTypeR -> Expression AnnTypeR -> CGM (Maybe CGEnv) 
------------------------------------------------------------------------------------
consAsgn g x e 
  = do (x', g') <- consExprT g e $ envFindSpec x g 
       Just <$> envAdds [(x, envFindTy x' g')] g'


-- | @consExpr g e@ returns a pair (g', x') where x' is a fresh, 
--   temporary (A-Normalized) variable holding the value of `e`,
--   g' is g extended with a binding for x' (and other temps required for `e`)
------------------------------------------------------------------------------------
consExpr :: CGEnv -> Expression AnnTypeR -> CGM (Id AnnTypeR, CGEnv)
------------------------------------------------------------------------------------
consExpr g (Cast a e)
  = consCast g a e

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
       return (x, g) 
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

-- e.f
consExpr g (DotRef l e (Id _ fld))
  = snd <$> consPropRead getProp g l e fld 

-- e["f"]
consExpr g (BracketRef l e (StringLit _ fld)) 
  = snd <$> consPropRead getProp g l e fld

-- e[i]
consExpr g (BracketRef l e (IntLit _ fld)) 
  = snd <$> consPropRead getIdx g l e fld 
  
-- e1[e2]
consExpr g (BracketRef l e1 e2) 
  = consCall g l BIBracketRef [e1, e2] $ builtinOpTy l BIBracketRef $ renv g 

-- e1[e2] = e3
consExpr g (AssignExpr l OpAssign (LBracket l1 e1 e2) e3)
  = consCall g l BIBracketAssign [e1,e2,e3] $ builtinOpTy l BIBracketAssign $ renv g

consExpr g e@(ArrayLit l es)
  = consCall g l BIArrayLit es $ arrayLitTy l (length es) $ renv g

consExpr g (ObjectLit l ps) 
  = consObjT l g ps Nothing

consExpr _ e 
  = error $ (printf "consExpr: not handled %s" (ppshow e))


-- consExpr g (BracketRef l e i) = do
--     (xe, g')  <- consExpr g e
--     (xi, g'') <- consExpr g' i
--     let ta = envFindTy xe g' 
--         ti = envFindTy xi g''
--     case (ta, ti) of
--       (TArr _ _, TApp TInt _ _) -> do  
--         t <- indexType ta
--         {- ONLY AT CAST withAlignedM -} 
--         (subTypeContainers "Bounds" l g'') (eSingleton tInt xi) (bt xe)
--         envAddFresh "consExpr:[IntLit]" l t g''
--       _ -> errorstar $ "UNIMPLEMENTED[consExpr] " ++ 
--                        "Can only use BracketRef to access array " ++
--                        "type with an integer. Here used " ++ (ppshow ti)
--   where
--     -- bt x = { number | ((0 <= v) && (v < (len x)))}
--     bt x = setRTypeR tInt (F.predReft $ F.PAnd [lo, hi x])
--     lo   = F.PAtom F.Le (F.ECon $ F.I 0) v                             -- 0 <= v
--     hi x = F.PAtom F.Lt v (F.EApp (rawStringSymbol "len") [F.eVar $ x]) -- v < len va
--     v    = F.eVar $ F.vv Nothing


-- -- shadowed at the moment...
-- consExpr g (BracketRef l e (StringLit _ s))
--   = do  (x, g') <- consExpr g e
--         t       <- safeGetProp s (envFindTy x g') 
--         envAddFresh "consExpr[StringLit]" l t g'
        



-------------------------------------------------------------------------------------------------
consCast :: CGEnv -> AnnTypeR -> Expression AnnTypeR -> CGM (Id AnnTypeR, CGEnv)
-------------------------------------------------------------------------------------------------
consCast g a e
  = case envGetContextCast g a of
      Just (DC t) -> consDeadCast g l t
      z           -> do (x, g') <- consExpr g e
                        case z of
                          Nothing        -> return (x, g')
                          Just (UCST t)  -> consUpCast   g' l x t
                          Just (DCST t)  -> consDownCast g' l x t
    where 
      l              = srcPos a

---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
consUpCast g l x t 
  = do γ      <- getTDefs
       let tx' = fst $ alignTs γ (envFindTy x g) t
       let tx  = (`strengthen` (F.symbolReft x)) $ tx' 
       envAddFresh "consUpCast" l tx g

---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
consDownCast g l x t = 
  do γ   <- getTDefs
     tc  <- castTo l γ tx (toType t) 
     withAlignedM (subTypeContainers "Downcast" l g) tx tc
     g'  <- envAdds [(x, tc)] g
     return (x, g')
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
  = do subTypeContainers "DeadCast" l g tru fls
       envAddFresh "consDeadCast" l t' g
    where
       tru = tTop
       fls = tTop `strengthen` F.predReft F.PFalse
       t'  = t    `strengthen` F.predReft F.PFalse


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
       let ft        = fromMaybe (err ts ft0) $ calleeType l ts ft0
       (_,its,ot)   <- instantiate l g fn ft
       let (su, ts') = renameBinds its xes
       zipWithM_ (subTypeContainers "Call" l g') [envFindTy x g' | x <- xes] ts'
       envAddFresh "consCall" l (F.subst su ot) g'
    where
       err ts ft0    = die $ errorNoMatchCallee (srcPos l) ts ft0 

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
-- consObjT :: AnnTypeR -> CGEnv -> [(Prop AnnTypeR, Expression AnnTypeR)] -> Maybe RefType -> CGM (Id AnnTypeR, CGEnv)
---------------------------------------------------------------------------------

-- Generate a fresh template for each literal to not over-specialize.
consObjT l g pe to 
  = do let (ps, es) = unzip pe
       (xes, g')   <- consScan consExpr g es
       let tLit     = (`TObj` F.top) $ zipWith B (F.symbol <$> ps) ((`envFindTy` g') <$> xes)
       t           <- maybe (freshTyObj l g tLit) return to
       subTypeContainers "object literal" l g' tLit t
       envAddFresh "consObj" l t g'
 
-- OLD consObj l g pe = 
-- OLD   do  let (ps, es) = unzip pe
-- OLD       (xes, g')   <- consScan consExpr g es
-- OLD       let pxs      = zipWith B (map F.symbol ps) $ (`envFindTy` g') <$> xes
-- OLD       envAddFresh "consObj" l (TObj pxs F.top) g'
    

-- ---------------------------------------------------------------------------------
-- consArrayLit ::AnnTypeR -> CGEnv -> Maybe RefType -> [Expression AnnTypeR] -> CGM  (Id AnnTypeR, CGEnv)
-- ---------------------------------------------------------------------------------
-- 
-- -- []
-- -- consArrayLit l g (Just t@(TArr _ _ )) [] = 
-- --   envAddFresh "consArrayLit:empty" l t g
-- 
-- -- [e1, ... ]
-- -- XXX: The top-level refinement of the array is ignored at the moment.
-- -- Also adds a top-level refinement that captures the length of the array.
-- consArrayLit l g (Just t@(TArr ta _)) es = do  
--     (xes, g')   <- consScan consExpr g es
--     let tes      = (`envFindTy` g') <$> xes
--     forM_ tes    $ \te -> subTypeContainers "ArrayConst" l g te ta
--     let t'       = t `strengthen` lenReft
--     envAddFresh "consArrayLit" l t' g'
--   where 
--     v            = rTypeValueVar t
--     lenReft      = F.Reft (v, [F.RConc lenPred])
--     lenPred      = F.PAtom F.Eq (F.expr $ length es) 
--                     (F.EApp (rawStringSymbol "len") [F.eVar $ v])
-- 
-- consArrayLit l _ Nothing _  = die $ errorMissingAnnot (srcPos l) "array literal" 
-- consArrayLit l _ (Just _) _ = die $ errorBadAnnot     (srcPos l) "array literal" "array" 


---------------------------------------------------------------------------------
consPropRead getter g l e fld
  = do (x, g')        <- consExpr g e
       tdefs          <- getTDefs 
       case getter l tdefs fld $ envFindTy x g' of
         Just (_, tf) -> (tf,) <$> envAddFresh "consPropRead" l tf g'
         Nothing      -> die $  errorPropRead (srcPos l) e fld




