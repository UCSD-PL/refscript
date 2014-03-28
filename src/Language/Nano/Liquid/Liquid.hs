{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TupleSections        #-}

-- | Top Level for Refinement Type checker
module Language.Nano.Liquid.Liquid (verifyFile) where

import           Text.Printf                        (printf)
-- import           Text.PrettyPrint.HughesPJ          (Doc, text, render, ($+$), (<+>))
import           Control.Monad
import           Control.Applicative                ((<$>))

import qualified Data.HashMap.Strict                as M
import           Data.Maybe                         (fromMaybe, listToMaybe)

import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations
import           Language.ECMAScript3.PrettyPrint

import qualified Language.Fixpoint.Types            as F
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Interface        (solve)

import           Language.Nano.CmdLine              (getOpts)
import           Language.Nano.Errors
import           Language.Nano.Env                  (envUnion)
import           Language.Nano.Types
import           Language.Nano.Typecheck.Subst
import qualified Language.Nano.Annots               as A
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Parse
import           Language.Nano.Typecheck.Typecheck  (typeCheck) 
import           Language.Nano.Typecheck.Lookup
import           Language.Nano.SSA.SSA
import           Language.Nano.Liquid.Types
import           Language.Nano.Liquid.Alias
import           Language.Nano.Liquid.CGMonad

import           System.Console.CmdArgs.Default

--import           Debug.Trace                        (trace)

type PPR r = (PP r, F.Reftable r)

--------------------------------------------------------------------------------
verifyFile    :: FilePath -> IO (A.UAnnSol RefType, F.FixResult Error)
--------------------------------------------------------------------------------
verifyFile f = parse f $ ssa $ tc $ refTc

parse f next = parseNanoFromFile f         >>= next
ssa   next p = ssaTransform p              >>= either (lerror . single) next
tc    next p = typeCheck (expandAliases p) >>= either lerror next
refTc      p = getOpts >>= solveConstraints (fp p) . (`generateConstraints` p) 

lerror       = return . (A.NoAnn,) . F.Unsafe
         
--------------------------------------------------------------------------------
solveConstraints :: FilePath -> CGInfo -> IO (A.UAnnSol RefType, F.FixResult Error) 
--------------------------------------------------------------------------------
solveConstraints f cgi 
  = do (r, s)  <- solve def f [] $ cgi_finfo cgi
       let r'   = fmap (errorLiquid . srcPos . F.sinfo) r
       let ann  = cgi_annot cgi
       let sol  = applySolution s 
       return (A.SomeAnn ann sol, r') 

--------------------------------------------------------------------------------
applySolution :: F.FixSolution -> A.UAnnInfo RefType -> A.UAnnInfo RefType 
--------------------------------------------------------------------------------
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

initCGEnv pgm = CGE (envUnion (specs pgm) (externs pgm)) 
                    F.emptyIBindEnv 
                    [] 
                    emptyContext 
                    (envUnion (specs pgm) (glVars pgm))

--------------------------------------------------------------------------------
consFun :: CGEnv -> Statement (AnnType F.Reft) -> CGM CGEnv
--------------------------------------------------------------------------------
consFun g (FunctionStmt l f xs body) 
  = cgFunTys l f xs (envFindTy f g) >>= mapM_ (consFun1 l g f xs body) >> return g
       
consFun _ s 
  = die $ bug (srcPos s) "consFun called not with FunctionStmt"

consFun1 l g' f xs body (i, ft) 
  = do g'' <- envAddFun l f i xs ft g'
       gm  <- consStmts g'' body
       maybe (return ()) (\g -> subTypeContainers "return void" l g tVoid (envFindReturn g'')) gm

envAddFun l f i xs (αs, ts', t') g =   (return $ envPushContext i g) 
                                   >>= (return . envAddReturn f t' ) 
                                   >>= envAdds (varBinds xs ts')
                                   >>= envAdds tyBinds
  where
    tyBinds                        = [(Loc (srcPos l) α, tVar α) | α <- αs]
    varBinds                       = safeZip "envAddFun"

--------------------------------------------------------------------------------
consStmts :: CGEnv -> [Statement AnnTypeR]  -> CGM (Maybe CGEnv) 
--------------------------------------------------------------------------------
consStmts g stmts 
  = do g' <- addStatementFunBinds g stmts 
       consSeq consStmt g' stmts

addStatementFunBinds g stmts = (mapM go $ concatMap getFunctionStatements stmts) >>= (`envAdds` g)
  where  go (FunctionStmt l f _ _) = (f,) <$> (freshTyFun g l f =<< getDefType f)
         go _                      = error "addStatementFunBinds: Only function statements should be here"

--------------------------------------------------------------------------------
consStmt :: CGEnv -> Statement AnnTypeR -> CGM (Maybe CGEnv) 
--------------------------------------------------------------------------------

-- | @consStmt g s@ returns the environment extended with binders that are
--   due to the execution of statement s. @Nothing@ is returned if the
--   statement has (definitely) hit a `return` along the way.

-- skip
consStmt g (EmptyStmt _) 
  = return $ Just g

-- x = e
consStmt g (ExprStmt l (AssignExpr _ OpAssign (LVar lx x) e))   
  = consAsgn g l (Id lx x) e

-- e1.fld = e2
consStmt g (ExprStmt _ (AssignExpr l2 OpAssign (LDot l1 e1 fld) e2))
  = do (tfld, _) <- consPropRead getProp g l1 e1 $ F.symbol fld
       (x2,  g') <- consExpr  g  e2
       let t2     = envFindTy x2 g'
       subTypeContainers "field-assignment" l2 g' t2 tfld
       return     $ Just g'

-- e
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

-- while e { s }  
consStmt g (WhileStmt l e s) 
  = Just <$> consWhile g l e s 

-- var x1 [ = e1 ]; ... ; var xn [= en];
consStmt g (VarDeclStmt _ ds)
  = consSeq consVarDecl g ds

-- return e 
consStmt g (ReturnStmt l (Just e))
  = do  (xe, g') <- consExpr g e
        let te    = envFindTy xe g'
            rt    = envFindReturn g'
        if isTop rt
          then (subTypeContainers "ReturnTop" l g') te (setRTypeR te (rTypeR rt))
          else (subTypeContainers "Return" l g') te rt
        return Nothing

-- return
consStmt _ (ReturnStmt _ Nothing)
  = return Nothing 

-- function f(x1...xn){ s }
consStmt g s@(FunctionStmt _ _ _ _)
  = Just <$> consFun g s

-- class A<S...> [extends B<T...>] [implements I,J,...] { ... }
consStmt g (ClassStmt l i _ _ ce) = do  
    -- * Compute / get the class type 
    (cid, TD _ αs _ _) <- findTySymWithIdOrDieM $ F.symbol i
    let tyBinds = [(Loc (srcPos l) α, tVar α) | α <- αs]
    -- * Add the type vars in the environment
    g' <- envAdds tyBinds g
    -- * Compute type for "this" and add that to the env as well
    --   - This type uses the classes type variables as type parameters.
    --   - For the moment this type does not have a refinement. Maybe use
    --     invariants to add some.
    let thisT = TApp (TRef cid) (tVar <$> αs) fTop  
    cgWithThis thisT $ mapM_ (consClassElt g') ce
    return $ Just g

-- OTHER (Not handled)
consStmt _ s 
  = errorstar $ "consStmt: not handled " ++ ppshow s


------------------------------------------------------------------------------------
consVarDecl :: CGEnv -> VarDecl AnnTypeR -> CGM (Maybe CGEnv) 
------------------------------------------------------------------------------------
consVarDecl g (VarDecl l x (Just e)) 
  = consAsgn g l x e

consVarDecl g (VarDecl l x Nothing)
  = case envFindAnnot l x g of
      Just  t -> Just <$> envAdds [(x, t)] g
      Nothing -> cgError l $ errorVarDeclAnnot (srcPos l) x


------------------------------------------------------------------------------------
consClassElt :: CGEnv -> ClassElt AnnTypeR -> CGM ()
------------------------------------------------------------------------------------
consClassElt g (Constructor l xs body) 
  = do  t <- cgFunTys l i xs $ head [ t | ConsAnn t <- ann_fact l ]
        -- Typecheck over all possible constructor signatures
        mapM_ (consFun1 l g i xs body) t
  where 
        i = Id l "constructor"

  -- Annotation will be included in `v`
consClassElt g (MemberVarDecl _ _ v) 
  = void $ consVarDecl g v
  
consClassElt g (MemberMethDecl l _ i xs body) 
  = do  ts <- cgFunTys l i xs $ pickElt l
        mapM_ (consFun1 l g i xs body) ts

pickElt :: AnnTypeR -> RefType
pickElt l = head [ t | VarAnn t <- ann_fact l ]


------------------------------------------------------------------------------------
consExprT :: CGEnv -> Expression AnnTypeR -> Maybe RefType -> CGM (Id AnnTypeR, CGEnv) 
------------------------------------------------------------------------------------
-- XXX: There are no annotations on boject or array literals directly - just on
-- variable declarations
-- 
-- consExprT g o@(ObjectLit _ _) to
--   = consObjT g o to
-- 
-- consExprT g e@(ArrayLit l _) (Just t)
--   = do (x, g')  <- consExpr g e
--        let te    = envFindTy x g'
--        subTypeContainers "consExprT" l g' te t
--        let t' = t `strengthen` F.substa (sf (rv te) (rv t)) (rTypeReft te)
--        g'' <- envAdds [(x, t')] g'
--        return (x, g'')
--     where
--        rv         = rTypeValueVar
--        sf s1 s2 s | s == s1   = s2
--                   | otherwise = s
 
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
 

--------------------------------------------------------------------------------
consAsgn :: CGEnv -> AnnTypeR -> Id AnnTypeR -> Expression AnnTypeR -> CGM (Maybe CGEnv) 
--------------------------------------------------------------------------------
consAsgn g l x e 
  = do  δ <- getDef
        t <- case envFindAnnot l x g of
        -- XXX: Flatten before applying freshTVar
               Just t  -> Just <$> freshTyVar g l (flattenType δ t)
               Nothing -> return $ Nothing
        (x', g') <- consExprT g e t
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

consExpr g (ThisRef l)
  = cgPeekThis >>= \t -> envAddFresh "consExpr:ThisRef" l t g

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
  = snd <$> consPropRead getProp g l e (F.symbol fld) 

-- e["f"]
consExpr g (BracketRef l e (StringLit _ fld)) 
  = snd <$> consPropRead getProp g l e (F.symbol fld)

-- e1[e2]
consExpr g (BracketRef l e1 e2) 
  = consCall g l BIBracketRef [e1, e2] $ builtinOpTy l BIBracketRef $ renv g 

-- e1[e2] = e3
consExpr g (AssignExpr l OpAssign (LBracket _ e1 e2) e3)
  = consCall g l BIBracketAssign [e1,e2,e3] $ builtinOpTy l BIBracketAssign $ renv g

-- [e1,...,en]
consExpr g (ArrayLit l es)
  = consCall g l BIArrayLit es $ arrayLitTy l (length es) $ renv g

-- {f1:e1,...,fn:en}
consExpr g (ObjectLit l bs) 
  = do  let (ps, es) = unzip bs
        (xes, g')   <- consScan consExpr g es
        let elts = zipWith mkElt (F.symbol <$> ps) (
          -- tracePP ("ObjLit types: " ++ ppshow xes) $ 
              (`envFindTy` g') <$> xes)
        let d = TD Nothing [] Nothing elts
        i <- addObjLitTyM d
        envAddFresh "consExpr:ObjectLit" l (TApp (TRef i) [] fTop) g'
    where
        mkElt s t = TE s True t 

-- new C(e, ...)
consExpr g (NewExpr l (VarRef _ i) es)
  = do  (tid, t@(TD _ vs _ _)) <- findTySymWithIdOrDieM $ F.symbol i
        tConst0 <- getPropTDefM l "constructor" t (tVar <$> vs)
        let tConstr = fix tid vs $ fromMaybe def tConst0
        consCall g l "constructor" es tConstr
    where
        fix tid vs (TFun ts _ r) = mkAll vs $ TFun ts (TApp (TRef tid) (tVar <$> vs) fTop) r
        fix _ _ t = error $ "BUG:consExpr NewExpr - not supported type for constructor: " ++ ppshow t
        def :: (PPR r) => RType r
        def = TFun [] tVoid fTop

-- super
consExpr g (SuperRef l) 
  = do  thisT <- cgPeekThis
        case thisT of
          TApp (TRef i) ts _ -> do
            TD _ vs pro _ <- findTyIdOrDieM i 
            case pro of 
              Just (p, ps) -> do
                let θ = fromList $ zip vs ts
                d <- fst <$> findTySymWithIdOrDieM p
                envAddFresh "consExpr:SuperRef" l (apply θ $ TApp (TRef d) ps fTop) g
              Nothing -> cgError l $ errorSuper (srcPos l) 
          _                  -> cgError l $ errorSuper (srcPos l) 


-- not handled
consExpr _ e 
  = error $ (printf "consExpr: not handled %s" (ppshow e))


--------------------------------------------------------------------------------
consCast :: CGEnv -> AnnTypeR -> Expression AnnTypeR -> CGM (Id AnnTypeR, CGEnv)
--------------------------------------------------------------------------------
consCast g a e
  = do  (x,g) <- consExpr g e 
        case envGetContextCast g a of
          CNo       -> return (x,g)
          CDead     -> consDeadCode g l x
          CUp t t'  -> consUpCast g l x t t'
          CDn t t'  -> consDownCast g l x t t'

          -- FIXME: TODO
          -- CFn t t'  -> return undefined
          -- CCs t t'  -> return undefined
    where  
      l = srcPos a

-- | Dead code 
consDeadCode g l x =
  do δ <- getDef
     let xBot = zipType δ (\_ -> F.bot) id xT xT  
     subTypeContainers "consDeadCode" l g xT xBot
     return (x, g)
  where 
     xT      = envFindTy x g

-- | Upcast
consUpCast g l x fromT toT = 
  do δ <- getDef
     let toT' = zipType δ (\p _ -> p) F.bot xT toT
     envAddFresh "consUpCast" l toT' g
  where 
     xT = envFindTy x g

-- | Downcast
consDownCast g l x fromT toT =
  do δ      <- getDef
     let (lhs, rhs) = (xT, zipType δ (\_ q -> q) F.bot toT xT)
     subTypeContainers "consDownCast" l g lhs rhs
     -- NOTE: The F.bot in the following should not really mattter
     let toT' = zipType δ (\p _ -> p) F.bot xT toT
     g'     <- envAdds [(x, toT')] g
     return  $ (x, g')
  where 
     xT      = envFindTy x g


-- -- Types carried over from Raw-Typechecking may lack some refinements introduced
-- -- at liquid. So we patch them here.
-- enhance δ l lq raw   = castStrengthen lq <$> (zipType2 botJoin lq =<< bottify raw)
--   where 
--   -- Bottify does not descend into TDefs - so we need to flatten 
--     bottify          = fmap (fmap F.bot) . true . flattenType δ . rType
-- 
-- castStrengthen t1 t2 
--   | isUnion t1 && not (isUnion t2) = t2 `strengthen` (rTypeReft t1)
--   | otherwise                      = t2
-- 
-- consDeadCast g l t 
--   = do subTypeContainers "DeadCast" l g tru fls
--        envAddFresh "consDeadCast" l t' g
--     where
--        tru = tTop
--        fls = tTop `strengthen` F.predReft F.PFalse
--        t'  = t    `strengthen` F.predReft F.PFalse


--------------------------------------------------------------------------------
consCall :: (PP a) => 
  CGEnv -> AnnTypeR -> a -> [Expression AnnTypeR] -> RefType -> CGM (Id AnnTypeR, CGEnv)
--------------------------------------------------------------------------------

--   1. Fill in @instantiate@ to get a monomorphic instance of @ft@ 
--      i.e. the callee's RefType, at this call-site (You may want 
--      to use @freshTyInst@)
--   2. Use @consExpr@ to determine types for arguments @es@
--   3. Use @subTypes@ to add constraints between the types from (step 2) and (step 1)
--   4. Use the @F.subst@ returned in 3. to substitute formals with actuals in output type of callee.

consCall g l fn es ft0 
  = do (xes, g')    <- consScan consExpr g es
       let ts        = [envFindTy x g' | x <- xes]
       let ft        = fromMaybe (fromMaybe (err ts ft0) (overload l)) (calleeType l ts ft0)
       (_,its,ot)   <- instantiate l g fn ft
       let (su, ts') = renameBinds its xes
       zipWithM_ (subTypeContainers "Call" l g') [envFindTy x g' | x <- xes] ts'
       envAddFresh "consCall" l (F.subst su ot) g'
    where
       overload l    = listToMaybe [ t | Overload (Just t) <- ann_fact l ]
       err ts ft0    = die $ errorNoMatchCallee (srcPos l) ts ft0 

-- instantiate :: AnnTypeR -> CGEnv -> RefType -> CGM RefType
instantiate l g fn ft 
  = do let (αs, t)      = bkAll ft
       --TODO: There is a TypInst missing here!!!
       let ts           = envGetContextTypArgs g l αs
       -- NOTE: Do we need to flatten here?
       t'              <- freshTyInst l g αs ts t
       maybe err return $ bkFun t' 
    where 
       err = cgError l $ errorNonFunction (srcPos l) fn ft  
       {-msg = printf "instantiate [%s] %s %s" (ppshow $ ann l) (ppshow αs) (ppshow tbody)-}


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


consPropRead getter g l e fld
  = do 
      (x, g')        <- consExpr g e
      let tx          = envFindTy x g'
      (this, g'')    <- envAddFresh "consPropRead:this" l tx g'
      δ              <- getDef
      case getter l (renv g'') δ fld tx of
        Just (_, tf) -> 
          do
            let tf'   = F.substa (sf (F.symbol "this") (F.symbol this)) tf
            g'''     <- envAddFresh "consPropRead:field" l tf' g''
            return    $ (tf', g''')
        Nothing         -> die $  errorPropRead (srcPos l) e fld
    where  
       sf s1 s2 = \s -> if s == s1 then s2
                                   else s

---------------------------------------------------------------------------------------------
consWhile :: CGEnv -> AnnTypeR -> Expression AnnTypeR -> Statement AnnTypeR -> CGM CGEnv
---------------------------------------------------------------------------------------------

{- Typing Rule for `while (cond) {body}`
   
      (a) xtIs         <- fresh G [ G(x) | x <- Φ]
      (b) GI            = G, xtIs
      (c) G            |- G(x)  <: GI(x)  , ∀x∈Φ      [base]
      (d) GI           |- cond : (xc, GI')
      (e) GI', xc:true |- body : GI''
      (f) GI''         |- GI''(x') <: GI(x)[Φ'/Φ]     [step]
      ---------------------------------------------------------
          G            |- while[Φ] (cond) body :: GI', xc:false

   The above rule assumes that phi-assignments have already been inserted. That is, 
    
      i = 0;
      while (i < n){
        i = i + 1;
      }

   Has been SSA-transformed to 

      i_0 = 0;
      i_2 = i_0;
      while [i_2] (i_2 < n) {
        i_1  = i_2 + 1;
        i_2' = i_1;
      }

 -}
consWhile g l cond body 
  = do  (gI,tIs)            <- freshTyPhis (srcPos l) g xs $ toType <$> ts  -- (a) (b) 
        _                   <- consWhileBase l xs tIs g                     -- (c)
        (xc, gI')           <- consExpr gI cond                             -- (d)
        z                   <- consStmt (envAddGuard xc True gI') body      -- (e)
        whenJustM z          $ consWhileStep l xs tIs                       -- (f) 
        return               $ envAddGuard xc False gI'
    where
        xs                   = concat [xs | PhiVar xs <- ann_fact l]
        ts                   = (`envFindTy` g) <$> xs 

consWhileBase l xs tIs g    = zipWithM_ (subTypeContainers "WhileBase" l g) xts_base tIs      -- (c)
  where 
   xts_base                 = (`envFindTy` g) <$> xs
 
consWhileStep l xs tIs gI'' = zipWithM_ (subTypeContainers "WhileStep" l gI'') xts_step tIs'  -- (f)
  where 
    xts_step                = (`envFindTy` gI'') <$> xs'
    tIs'                    = F.subst su <$> tIs
    xs'                     = mkNextId   <$> xs
    su                      = F.mkSubst   $  safeZip "consWhileStep" (F.symbol <$> xs) (F.eVar <$> xs')

whenJustM Nothing  _ = return ()
whenJustM (Just x) f = f x

----------------------------------------------------------------------------------
envJoin :: AnnTypeR -> CGEnv -> Maybe CGEnv -> Maybe CGEnv -> CGM (Maybe CGEnv)
----------------------------------------------------------------------------------
envJoin _ _ Nothing x           = return x
envJoin _ _ x Nothing           = return x
envJoin l g (Just g1) (Just g2) = Just <$> envJoin' l g g1 g2 

----------------------------------------------------------------------------------
envJoin' :: AnnTypeR -> CGEnv -> CGEnv -> CGEnv -> CGM CGEnv
----------------------------------------------------------------------------------

-- 1. use @envFindTy@ to get types for the phi-var x in environments g1 AND g2
-- 2. use @freshTyPhis@ to generate fresh types (and an extended environment with 
--    the fresh-type bindings) for all the phi-vars using the unrefined types 
--    from step 1.
-- 3. generate subtyping constraints between the types from step 1 and the fresh types
-- 4. return the extended environment.

envJoin' l g g1 g2
  = do  g1' <- envAdds (zip xs t1s) g1 
        g2' <- envAdds (zip xs t2s) g2
        -- t1s and t2s should have the same raw type, otherwise they wouldn't
        -- pass TC (we don't need to pad / fix them before joining).
        -- So we can use the raw type from one of the two branches and freshen
        -- up that one.
        -- TODO: Add a raw type check on t1 and t2
        (g',ts) <- freshTyPhis (srcPos l) g xs $ toType <$> t1s
        zipWithM_ (subTypeContainers "envJoin 1" l g1') [envFindTy x g1' | x <- xs] ts
        zipWithM_ (subTypeContainers "envJoin 2" l g2') [envFindTy x g2' | x <- xs] ts
        return g'
    where
        xs   = concat [xs | PhiVar xs <- ann_fact l] 
        t1s  = (`envFindTy` g1) <$> xs 
        t2s  = (`envFindTy` g2) <$> xs

