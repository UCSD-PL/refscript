{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TupleSections        #-}

-- | Top Level for Refinement Type checker
module Language.Nano.Liquid.Liquid (verifyFile) where

import           Text.Printf                        (printf)
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
-- import           Debug.Trace                        (trace)

type PPR r = (PP r, F.Reftable r)
type PPRS r = (PPR r, Substitutable r (Fact r)) 

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
       maybe (return ()) (\g -> subType l g tVoid (envFindReturn g'')) gm

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
  = do (xf, g' ) <- consPropRead getProp g l1 e1 $ F.symbol fld
       (x2, g'') <- consExpr g'  e2
       let t2     = envFindTy x2 g'
       subType l2 g'' (envFindTy x2 g'') (envFindTy xf g')
       return     $ Just g''

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
  = do (xe, ge) <- consCall g l "truthy" [e] (builtinOpTy l BITruthy $ renv g)
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
        -- subType does not need to return the unfolded types
        if isTop rt
          then (subType l g') te (setRTypeR te (rTypeR rt))
          else (subType l g') te rt
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
    TD _ αs _ _ <- findSymOrDieM i
    let tyBinds = [(Loc (srcPos l) α, tVar α) | α <- αs]
    -- * Add the type vars in the environment
    g' <- envAdds tyBinds g
    -- * Compute type for "this" and add that to the env as well
    --   - This type uses the classes type variables as type parameters.
    --   - For the moment this type does not have a refinement. Maybe use
    --     invariants to add some.
    let thisT = TApp (TRef $ F.symbol i) (tVar <$> αs) fTop  
    cgWithThis thisT $ mapM_ (consClassElt g') ce
    return $ Just g

-- OTHER (Not handled)
consStmt _ s 
  = errorstar $ "consStmt: not handled " ++ ppshow s


------------------------------------------------------------------------------------
consVarDecl :: CGEnv -> VarDecl AnnTypeR -> CGM (Maybe CGEnv) 
------------------------------------------------------------------------------------
consVarDecl g (VarDecl l x (Just e)) 
  = do t <- case envFindAnnot l x g of
              Just t  -> Just <$> freshTyVar g l t
              Nothing -> return $ Nothing
       (x', g') <- consExprT g e t
       Just <$> envAdds [(x, envFindTy x' g')] g'


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

consClassElt g (MemberVarDecl _ _ v) 
  = void $ consVarDecl g v
  
consClassElt g (MemberMethDecl l _ i xs body) 
  = do  ts <- cgFunTys l i xs $ safeHead "consClassElt" [ t | MethAnn t <- ann_fact l]
        mapM_ (consFun1 l g i xs body) ts

------------------------------------------------------------------------------------
consExprT :: CGEnv -> Expression AnnTypeR -> Maybe RefType -> CGM (Id AnnTypeR, CGEnv) 
------------------------------------------------------------------------------------
consExprT g e to 
  = do (x, g') <- consExpr g e
       let te   = envFindTy x g'
       case to of
         Nothing -> return (x, g')
         Just t  -> subType l g' te t >> (x,) <$> envAdds [(x, t)] g'
    where
       l = getAnnotation e


--------------------------------------------------------------------------------
consAsgn :: CGEnv -> AnnTypeR -> Id AnnTypeR -> Expression AnnTypeR -> CGM (Maybe CGEnv) 
--------------------------------------------------------------------------------
consAsgn g l x e 
  = do t <- case envFindAnnot l x g of
              Just t  -> Just <$> freshTyVar g l t
              Nothing -> return $ Nothing
       (x', g') <- consExprT g e t
       Just <$> envAdds [(x, envFindTy x' g')] g'


-- | @consExpr g e@ returns a pair (g', x') where x' is a fresh, 
-- temporary (A-Normalized) variable holding the value of `e`,
-- g' is g extended with a binding for x' (and other temps required for `e`)
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

-- super(e1,..,en)
consExpr g (CallExpr l e@(SuperRef _) es) 
  = do t <- f_type . getCons . t_elts <$> (getSuperDefM l =<< cgPeekThis)
       consCall g l e es t

consExpr g (CallExpr l e es)
  = do (x, g') <- consExpr g e 
       consCall g' l e es $ envFindTy x g'

-- e.f
consExpr g (DotRef l e (Id _ fld))
  = do (x,g') <- consPropRead getProp g l e (F.symbol fld)
       addAnnot (srcPos l) x (envFindTy x g')
       return  $ (x,g')

-- e["f"]
consExpr g (BracketRef l e (StringLit _ fld)) 
  = consPropRead getProp g l e (F.symbol fld)

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
        let tCons    = TCons (zipWith mkElt (F.symbol <$> ps) $ (`envFindTy` g') <$> xes) fTop
        envAddFresh "consExpr:ObjectLit" l tCons g'
    where
        mkElt s t = TE s True t 

-- new C(e, ...)
consExpr g (NewExpr l (VarRef _ i) es)
  = do  t@(TD _ vs _ _) <- findSymOrDieM i
        tConst0 <- getPropTDefM l "constructor" t (tVar <$> vs)
        let tConstr = fix (F.symbol i) vs $ fromMaybe def tConst0
        consCall g l "constructor" es tConstr
    where
        fix nm vs (TFun ts _ r) = mkAll vs $ TFun ts (TApp (TRef nm) (tVar <$> vs) fTop) r
        fix _ _ t = error $ "BUG:consExpr NewExpr - not supported type for constructor: " ++ ppshow t
        def :: (PPR r) => RType r
        def = TFun [] tVoid fTop

-- super
consExpr g (SuperRef l) 
  = do  thisT <- cgPeekThis
        case thisT of
          TApp (TRef i) ts _ -> do
            TD _ vs pro _ <- findSymOrDieM i 
            case pro of 
              Just (p, ps) -> do
                let θ = fromList $ zip vs ts
                envAddFresh "consExpr:SuperRef" l (apply θ $ TApp (TRef $ F.symbol p) ps fTop) g
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
          CDead t   -> consDeadCode g l x t
          CUp t t'  -> consUpCast g l x t t'
          CDn t t'  -> consDownCast g l x t t'
    where  
      l = srcPos a

-- | Dead code 
consDeadCode g l x t =
  do δ <- getDef
     let xBot = zipType δ (\_ -> F.bot) id xT xT  
     let tBot = zipType δ (\_ -> F.bot) id t  t
     subType l g xT xBot
     -- NOTE: return the target type (falsified)
     envAddFresh "consUpCast" l tBot g
  where 
     xT      = envFindTy x g

-- | Upcast
consUpCast g l x _ toT = 
  do δ <- getDef
     let toT' = zipType δ (\p _ -> p) F.bot xT toT
     envAddFresh "consUpCast" l toT' g
  where 
     xT = envFindTy x g

-- | Downcast
consDownCast g l x _ toT =
  do δ          <- getDef
     let (lh,rh) = (xT, zipType δ (\_ q -> q) F.bot toT xT)
     subType l g lh rh
     -- NOTE: The F.bot in the following should not really mattter
     let toT'    = zipType δ (\p _ -> p) F.bot xT toT
     g'         <- envAdds [(x, toT')] g
     return      $ (x, g')
  where 
     xT          = envFindTy x g


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
       zipWithM_ (subType l g') [envFindTy x g' | x <- xes] ts'
       envAddFresh "consCall" l (F.subst su ot) g'
    where
       overload l    = listToMaybe [ t | Overload (Just t) <- ann_fact l ]
       err ts ft0    = die $ errorNoMatchCallee (srcPos l) ts ft0 

---------------------------------------------------------------------------------
instantiate :: (PP a, PPRS F.Reft) => 
  AnnTypeR -> CGEnv -> a -> RefType -> CGM  ([TVar], [Bind F.Reft], RefType)
---------------------------------------------------------------------------------
instantiate l g fn ft 
  = do let (αs, t)      = bkAll ft
       let ts           = envGetContextTypArgs g l αs
       t'              <- freshTyInst l g αs ts t
       maybe err return $ bkFun t' 
    where 
       err = cgError l $ errorNonFunction (srcPos l) fn ft  


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


-- | consPropRead reads field `fld` from objet `e`. This function does the late
-- binding of "this" to the value in the left hand side.
consPropRead getter g l e fld
  = do (this, g')     <- consExpr g e
       let tx          = envFindTy this g'
       δ              <- getDef
       case getter l (renv g') δ fld tx of
         Just (_, tf) -> 
           do let tf'   = F.substa (sf (F.symbol "this") (F.symbol this)) tf
              envAddFresh "consPropRead:field" l tf' g'
         Nothing         -> die $  errorPropRead (srcPos l) e fld
    where  
       sf s1 s2 = \s -> if s == s1 then s2
                                   else s

---------------------------------------------------------------------------------
consWhile :: CGEnv -> AnnTypeR -> Expression AnnTypeR -> Statement AnnTypeR -> CGM CGEnv
---------------------------------------------------------------------------------

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

consWhileBase l xs tIs g    = zipWithM_ (subType l g) xts_base tIs      -- (c)
  where 
   xts_base                 = (`envFindTy` g) <$> xs
 
consWhileStep l xs tIs gI'' = zipWithM_ (subType l gI'') xts_step tIs'  -- (f)
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
        zipWithM_ (subType l g1') [envFindTy x g1' | x <- xs] ts
        zipWithM_ (subType l g2') [envFindTy x g2' | x <- xs] ts
        return g'
    where
        xs   = concat [xs | PhiVar xs <- ann_fact l] 
        t1s  = (`envFindTy` g1) <$> xs 
        t2s  = (`envFindTy` g2) <$> xs

