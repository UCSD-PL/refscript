{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TupleSections        #-}

-- | Top Level for Refinement Type checker
module Language.Nano.Liquid.Liquid (verifyFile) where

import           Control.Monad
import           Control.Applicative                ((<$>))

import qualified Data.HashMap.Strict                as M
import           Data.Maybe                         (listToMaybe, catMaybes, maybeToList, isJust)

import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations
import           Language.ECMAScript3.PrettyPrint

import qualified Language.Fixpoint.Config           as C
import qualified Language.Fixpoint.Types            as F
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Interface        (solve)

import           Language.Nano.Annots
import           Language.Nano.CmdLine              (Config,getOpts)
import           Language.Nano.Errors
import qualified Language.Nano.Env                  as E
import           Language.Nano.Locations
import           Language.Nano.Names
import           Language.Nano.Program
import           Language.Nano.Typecheck.Resolve
import           Language.Nano.Types
import           Language.Nano.Typecheck.Subst
import qualified Language.Nano.SystemUtils          as A
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Parse
import           Language.Nano.Typecheck.Typecheck  (typeCheck) 
import           Language.Nano.Typecheck.Lookup
import           Language.Nano.SSA.SSA
import           Language.Nano.Liquid.Types
import           Language.Nano.Liquid.Alias
import           Language.Nano.Liquid.CGMonad

import qualified Data.Text                          as T 
import           Data.Data
import           Data.Generics.Aliases                   ( mkQ)
import           Data.Generics.Schemes

import           System.Console.CmdArgs.Default
-- import           Debug.Trace                        (trace)

type PPR r = (PP r, F.Reftable r)
type PPRS r = (PPR r, Substitutable r (Fact r)) 

--------------------------------------------------------------------------------
verifyFile    :: FilePath -> [FilePath] -> IO (A.UAnnSol RefType, F.FixResult Error)
--------------------------------------------------------------------------------
verifyFile f fs = parse fs $ ssa $ tc $ refTc f

parse f next    = parseNanoFromFiles f         >>= next
ssa   next p    = ssaTransform p              >>= either (lerror . single) next
tc    next p    = typeCheck (expandAliases p) >>= either lerror next
refTc f    p    = getOpts >>= solveConstraints f . (`generateConstraints` p)

lerror          = return . (A.NoAnn,) . F.Unsafe
         
-- | solveConstraints
--   Call solve with `ueqAllSorts` enabled.
--------------------------------------------------------------------------------
solveConstraints :: FilePath -> CGInfo -> IO (A.UAnnSol RefType, F.FixResult Error) 
--------------------------------------------------------------------------------
solveConstraints f cgi 
  = do (r, s)  <- solve (C.withUEqAllSorts def True) f [] $ cgi_finfo cgi
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
consNano :: NanoRefType -> CGM ()
--------------------------------------------------------------------------------
consNano p@(Nano {code = Src fs}) 
  = do  g   <- freshenCGEnvM $ initGlobalEnv p
        consStmts g fs 
        return ()


-------------------------------------------------------------------------------
-- | Initialize environment
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
initGlobalEnv  :: NanoRefType -> CGEnv
-------------------------------------------------------------------------------
initGlobalEnv (Nano { code = Src ss }) = CGE nms bds grd ctx mod pth glb Nothing
  where
    nms       = E.envFromList $ visibleNames ss
    bds       = F.emptyIBindEnv
    grd       = []
    mod       = scrapeModules ss 
    ctx       = emptyContext
    glb       = M.fromList [ (F.symbol x, []) | x <- hoistGlobals ss ]
    pth       = AP $ QPath (srcPos dummySpan) []


-------------------------------------------------------------------------------
-- | Environment wrappers
-------------------------------------------------------------------------------

consEnvFindTypeDefM l γ x
  = case resolveRelNameInEnv γ x of 
      Just t  -> return t
      Nothing -> cgError l $ bugClassDefNotFound (srcPos l) x



--------------------------------------------------------------------------------
consFun :: CGEnv -> Statement (AnnType F.Reft) -> CGM CGEnv
--------------------------------------------------------------------------------
consFun g (FunctionStmt l f xs body) 
  = case envFindTy f g of
      Just spec -> do ft        <- cgFunTys l f xs spec
                      forM_ ft   $ consFun1 l g f xs body
                      return     $ g
      Nothing   -> cgError l $ errorMissingSpec (srcPos l) f
       
consFun _ s 
  = die $ bug (srcPos s) "consFun called not with FunctionStmt"

consFun1 l g f xs body (i, ft) 
  = envAddFun l f i xs ft g >>= (`consStmts` body)

-- consFun1 l g' f xs body (i, ft) 
--   = do g'' <- envAddFun l f i xs ft g'
--        gm  <- consStmts g'' body
--        maybe (return ()) (\g -> subType l g tVoid (envFindReturn g'')) gm


consMeth1 l g f xs body (i, _, ft) = consFun1 l g f xs body (i,ft)


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
  = do -- g' <- addFunAndGlobs g stmts     -- K-Var functions and globals 
       consSeq consStmt g stmts


-- FIXME: this will happen at entering functions, classes

-- addFunAndGlobs g stmts 
--   = do   g1 <- (\xts -> envAdds False xts g ) =<< mapM ff funs 
--          g2 <- (\xts -> envAdds True  xts g1) =<< mapM vv globs 
--          return g2
--   where  ff (l,f)      = (f,) <$> (freshTyFun g l =<< getDefType f)
--          vv (l,x,t)    = (x,) <$> freshTyVar g l t
--          funs          = definedFuns stmts 
--          globs         = globsInScope stmts 
--          -- globs         = definedGlobs stmts 
-- 
-- definedFuns       :: (Data a, Typeable a) => [Statement a] -> [(a,Id a)]
-- definedFuns stmts = everything (++) ([] `mkQ` fromFunction) stmts
--   where 
--     fromFunction (FunctionStmt l f _ _) = [(l,f)]
--     fromFunction _                      = []
-- 
-- globsInScope :: [Statement AnnTypeR] -> [(AnnTypeR, Id AnnTypeR, RefType)]
-- globsInScope stmts = [ (l,x,t) | VarDeclStmt _ vd <- stmts
--                                , VarDecl l x _    <- vd
--                                , VarAnn t         <- ann_fact l ] 
 

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

-- e1.f = e2
consStmt g (ExprStmt l (AssignExpr _ OpAssign (LDot _ e1 f) e2))
  = do (_,g') <- consCall g l BISetProp [e1,e2] $ setPropTy (F.symbol f) l $ cge_names g
       return  $ Just g'
   
-- e
consStmt g (ExprStmt _ e) 
  = consExpr g e >> (return $ Just g)

-- s1;s2;...;sn
consStmt g (BlockStmt _ stmts) 
  = consStmts g stmts 

-- if b { s1 }
consStmt g (IfSingleStmt l b s)
  = consStmt g (IfStmt l b s (EmptyStmt l))

--  1. Use @envAddGuard True@ and @envAddGuard False@ to add the binder 
--     from the condition expression @e@ into @g@ to obtain the @CGEnv@ 
--     for the "then" and "else" statements @s1@ and @s2 respectively. 
--  2. Recursively constrain @s1@ and @s2@ under the respective environments.
--  3. Combine the resulting environments with @envJoin@ 

-- if e { s1 } else { s2 }
consStmt g (IfStmt l e s1 s2)
  = do (xe, ge) <- consCall g l "truthy" [e] (builtinOpTy l BITruthy $ cge_names g)
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
consStmt g (ReturnStmt l eo)
  = do  _ <- consCall g l "return" (maybeToList eo) $ returnTy (envFindReturn g) (isJust eo)
        return Nothing

-- throw e 
consStmt g (ThrowStmt _ e)
  = consExpr g e >> return Nothing


-- function f(x1...xn){ s }
consStmt g s@(FunctionStmt _ _ _ _)
  = Just <$> consFun g s

--
-- class A<S...> [extends B<T...>] [implements I,J,...] { ... }
--
-- 1. Compute / get the class type 
-- 2. Add the type vars in the environment
-- 3. Compute type for "this" and add that to the env as well. This type uses the classes type 
--    variables as type parameters. 
--
consStmt g (ClassStmt l x _ _ ce) 
  = do  dfn      <- consEnvFindTypeDefM l g rn
        -- FIXME: Should this check be done at TC too?
        g'       <- envAdds [(Loc (ann l) α, tVar α) | α <- t_args dfn] g
        g''      <- envAdds [(Loc (ann l) "this", mkThis $ t_args dfn)] g'
        consClassElts (srcPos l) g'' x dfn ce
        return    $ Just g
  where
    tVars   αs    = [ tVar   α | α <- αs ] 
    rn            = RN $ QName (srcPos l) [] (F.symbol x)
    mkThis αs     = TApp (TRef rn) (tVars αs) fTop


-- OTHER (Not handled)
consStmt _ s 
  = errorstar $ "consStmt: not handled " ++ ppshow s


------------------------------------------------------------------------------------
consVarDecl :: CGEnv -> VarDecl AnnTypeR -> CGM (Maybe CGEnv) 
------------------------------------------------------------------------------------
consVarDecl g (VarDecl l x (Just e)) 
  -- Global variable (Non-SSA)
  | isGlobalVar x g = case envFindTy x g of
                        Just ta -> do (y, gy) <- consExpr g e
                                      let gy'  = envRemSpec x gy
                                      t       <- safeEnvFindTy y gy
                                      _       <- subType l gy' t ta
                                      return   $ Just gy
                        _        -> cgError l $ bugNoAnnotForGlob (ann l) x 
  | otherwise       = consAsgn g l x e
 
consVarDecl g (VarDecl l x Nothing)
  | isGlobalVar x g = case envFindTy x g of
                        Just ta -> Just <$> envAdds [(x, ta)] g
                        _       -> cgError l $ errorVarDeclAnnot (srcPos l) x
  | otherwise       = cgError l $ errorVarDeclAnnot (srcPos l) x


-- FIXME: Do the safeExtends check here. Also add casts in the TC phase wherever
-- needed
------------------------------------------------------------------------------------
consClassElts :: PP a => SourceSpan -> CGEnv -> a -> IfaceDef F.Reft -> [ClassElt AnnTypeR] -> CGM ()
------------------------------------------------------------------------------------
consClassElts l g x d ce 
   = mapM_ (consClassElt g x) ce


------------------------------------------------------------------------------------
consClassElt :: PP a => CGEnv -> a -> ClassElt AnnTypeR -> CGM ()
------------------------------------------------------------------------------------
consClassElt g _ (Constructor l xs body) 
  = case [ c | ConsAnn c  <- ann_fact l ] of
      [ConsSig ft]        -> do t <-    cgFunTys l i xs ft
                                mapM_ (consFun1 l g i xs body) t
      _                   -> cgError l $ unsupportedNonSingleConsTy $ srcPos l
  where 
    i = Id l "constructor"

consClassElt g cid (MemberVarDecl _ static (VarDecl l1 x eo))
  = case anns of 
      []  ->  cgError l1    $ errorClassEltAnnot (srcPos l1) cid x
      fs  ->  case eo of
                Just e     -> void <$> consCall g l1 "field init" [e] $ ft fs
                Nothing    -> return ()
  where
    anns | static    = [ s | StatAnn  s <- ann_fact l1 ]
         | otherwise = [ f | FieldAnn f <- ann_fact l1 ]
    ft flds = mkAnd $ catMaybes $ mkInitFldTy <$> flds
  
consClassElt g cid (MemberMethDecl l static i xs body) 
  = case anns of
      [mt]  -> do mts   <- cgMethTys l i mt
                  mapM_    (consMeth1 l g i xs body) mts
      _    -> cgError l  $ errorClassEltAnnot (srcPos l) cid i
  where
    anns | static    = [ (m, t) | StatAnn (StatSig _ m t)  <- ann_fact l ]
         | otherwise = [ (m, t) | MethAnn (MethSig _ m t)  <- ann_fact l ]


------------------------------------------------------------------------------------
consExprT :: CGEnv -> Expression AnnTypeR -> Maybe RefType -> CGM (Id AnnTypeR, CGEnv) 
------------------------------------------------------------------------------------
consExprT g e to 
  = do (x, g') <- consExpr g e
       te      <- safeEnvFindTy x g'
       case to of
         Nothing -> return (x, g')
         Just t  -> do subType l g' te t
                       return (x,g')
                       -- (x,) <$> envAdds False [(x, t)] g'
    where
       l = getAnnotation e


--------------------------------------------------------------------------------
consAsgn :: CGEnv -> AnnTypeR -> Id AnnTypeR -> Expression AnnTypeR -> CGM (Maybe CGEnv)
--------------------------------------------------------------------------------
consAsgn g l x e 
  = do (x', g') <- consExprT g e $ envFindTy x g
       if isGlobalVar x g 
         then return $ Just g'
         else do  t <- safeEnvFindTy x' g'
                  Just <$> envAdds [(x, t)] g'


-- | @consExpr g e@ returns a pair (g', x') where x' is a fresh, 
-- temporary (A-Normalized) variable holding the value of `e`,
-- g' is g extended with a binding for x' (and other temps required for `e`)
------------------------------------------------------------------------------------
consExpr :: CGEnv -> Expression AnnTypeR -> CGM (Id AnnTypeR, CGEnv)
------------------------------------------------------------------------------------
consExpr g (Cast a e)
  = consCast g a e

consExpr g (IntLit l i)               
  = envAddFresh l (eSingleton tInt i) g

consExpr g (BoolLit l b)
  = envAddFresh l (pSingleton tBool b) g 

consExpr g (StringLit l s)
  = envAddFresh l (eSingleton tString (T.pack s)) g

consExpr g (NullLit l)
  = envAddFresh l tNull g

consExpr g (ThisRef l)
  = case envFindTy (Id (ann l) "this") g of
      Just t  -> envAddFresh l t g
      Nothing -> cgError l $ errorUnboundId (ann l) "this" 

consExpr g (VarRef l x)
  = case envFindTy x g of
      Just t  -> addAnnot (srcPos l) x t >> return (x, g) 
      Nothing -> cgError l $ errorUnboundId (ann l) x

consExpr g (PrefixExpr l o e)
  = consCall g l o [e] (prefixOpTy o $ cge_names g)

consExpr g (InfixExpr l o@OpInstanceof e1 e2)
  = do (x,g')       <- consExpr g e2
       t            <- safeEnvFindTy x g'
       case t of
         TClass x   -> consCall g l o [e1, StringLit l2 (cc x)] $ infixOpTy o $ cge_names g'
         _          -> cgError l $ unimplemented (srcPos l) "tcCall-instanceof" $ ppshow e2
  where
    l2 = getAnnotation e2
    cc (RN (QName _ _ s)) = F.symbolString s 

consExpr g (InfixExpr l o e1 e2)        
  = consCall g l o [e1, e2]  $ infixOpTy o $ cge_names g

-- | e ? e1 : e2
consExpr g (CondExpr l e e1 e2)
  = consCall g l BICondExpr [e,e1,e2] $ builtinOpTy l BICondExpr $ cge_names g

-- | super(e1,..,en)
consExpr g (CallExpr l e@(SuperRef _) es) 
  = case envFindTy (Id (ann l) "this") g of
      Just t  ->  do  elts <- t_elts <$> getSuperDefM l t
                      case [ t | ConsSig t <- elts ] of
                        [ct] -> consCall g l e es ct
                        _    -> cgError l $ unsupportedNonSingleConsTy $ srcPos l
      Nothing ->  cgError l $ errorUnboundId (srcPos l) "this"

-- | e.m(es)
consExpr g (CallExpr l em@(DotRef _ e f) es)
  = do  (x,g') <- consExpr g e
        t      <- safeEnvFindTy x g'
        consCallDotRef g' l em (vr x) (getElt g f t) es
  where
    -- Add a VarRef so that e is not typechecked again
    vr          = VarRef $ getAnnotation e

-- | e(es)
consExpr g (CallExpr l e es)
  = do (x, g') <- consExpr g e 
       t       <- safeEnvFindTy x g'
       consCall g' l e es t

-- | e.f
consExpr g ef@(DotRef l e f)
  = do  (x,g') <- consExpr g e
        t      <- safeEnvFindTy x g'
        case getElt g' f t of 
          [FieldSig _ _ ft] -> consCall g' l ef [vr x] $ mkTy ft
          _                 -> cgError l $ errorExtractNonFld (srcPos l) f e t 
  where
    -- Add a VarRef so that e is not typechecked again
    vr       = VarRef $ getAnnotation e
    mkTy t   = mkFun ([α], [B (F.symbol "this") tα], t) 
    α        = TV (F.symbol "α" ) (srcPos l)
    tα       = TVar α fTop

-- FIXME: e["f"]

-- | e1[e2]
consExpr g (BracketRef l e1 e2) 
  = consCall g l BIBracketRef [e1, e2] $ builtinOpTy l BIBracketRef $ cge_names g 

-- | e1[e2] = e3
consExpr g (AssignExpr l OpAssign (LBracket _ e1 e2) e3)
  = consCall g l BIBracketAssign [e1,e2,e3] $ builtinOpTy l BIBracketAssign $ cge_names g

-- | [e1,...,en]
consExpr g (ArrayLit l es)
  = do  t <- scrapeQualifiers $ arrayLitTy l (length es) $ cge_names g
        consCall g l BIArrayLit es t

-- | {f1:e1,...,fn:en}
consExpr g (ObjectLit l bs) 
  = consCall g l "ObjectLit" es $ objLitTy l ps
  where
    (ps,es) = unzip bs
    

-- | new C(e, ...)
consExpr g (NewExpr l e es)
  = do  (x, g') <- consExpr g e
        t       <- safeEnvFindTy x g'
        case extractCtor g t of
          Just ct -> consCall g l "constructor" es ct
          Nothing -> cgError l $ errorConstrMissing (srcPos l) t 

-- | super
consExpr g (SuperRef l) 
  = case envFindTy (Id (ann l) "this") g of 
      Just (TApp (TRef x) ts _) -> 
          do  ID _ _ vs h _ <- consEnvFindTypeDefM l g x
              case h of 
                Just (p, ps) -> let θ = fromList $ zip vs ts in
                                let t = apply θ $ TApp (TRef p) ps fTop in
                                envAddFresh l t g
                Nothing -> cgError l $ errorSuper (srcPos l) 
      Nothing -> cgError l $ errorSuper (srcPos l) 

-- | function(xs) { }
consExpr g (FuncExpr l fo xs body) 
  = case anns of 
      [ft]  -> do kft       <- freshTyFun g l ft
                  fts       <- cgFunTys l f xs kft
                  forM_ fts  $ consFun1 l g f xs body
                  envAddFresh  l kft g 
      _    -> cgError l      $ errorNonSingleFuncAnn $ srcPos l
  where
    anns                     = [ t | FuncAnn t <- ann_fact l ]
    f                        = maybe (F.symbol "<anonymous>") F.symbol fo


-- not handled
consExpr _ e = cgError l $ unimplemented l "consExpr" e where l = srcPos  e


-- -- | `getConstr l g s` first checks whether input @s@ is a class, in which 
-- --   case it tries to retrieve a constructor binding, using a default one if 
-- --   that fails. Otherwise, it tries to retrieve an object with the same name 
-- --   from the environment that has a constructor property.
-- --
-- -- FIXME: Do not lookup the constructor by string. We have a special struct for
-- -- that.
-- ----------------------------------------------------------------------------------
-- getConstr :: IsLocated a => SourceSpan -> CGEnv -> Id a -> CGM RefType
-- ----------------------------------------------------------------------------------
-- getConstr l g s = 
--     case findSym s (cge_defs g) of
--       Just t | t_class t ->       -- This needs to be a class
--         do  z <- getPropTDefM  l "__constructor__" t $ tVar <$> t_args t
--             case z of 
--               Just (TFun bs _ r) -> return $ abs (t_args t) $ TFun bs (retT t) r
--               Just t             -> cgError l $ unsupportedConsTy l t
--               Nothing            -> return $ abs (t_args t) $ TFun [] (retT t) fTop
--       _ -> 
--         do  z <- getPropM l "__constructor__" $ envFindTy s g
--             case z of
--               Just t  -> return t
--               Nothing -> cgError l $ unsupportedNonSingleConsTy $ srcPos l
--   where
--     -- Constructor's return type is void - instead return the class type
--     -- FIXME: type parameters in returned type: inferred ... or provided !!! 
--     retT t   = TApp (TRef $ F.symbol s) (tVar <$> t_args t) fTop
--     abs [] t = t
--     abs vs t = foldr TAll t vs


--------------------------------------------------------------------------------
consCast :: CGEnv -> AnnTypeR -> Expression AnnTypeR -> CGM (Id AnnTypeR, CGEnv)
--------------------------------------------------------------------------------
consCast g a e
  = do  (x,g) <- consExpr g e 
        t     <- safeEnvFindTy x g
        case envGetContextCast g a of
          CNo       -> return (x,g)
          CDead t   -> consDeadCode g l x t
          CUp t t'  -> consUpCast g l x t t'
          CDn t t'  -> consDownCast g l x t t'
    where  
      l = srcPos a

-- | Dead code 
consDeadCode g l x t
  = do  tx   <- safeEnvFindTy x g
        xBot <- zipTypeM l g (fmap F.bot tx) tx
        tBot <- zipTypeM l g (fmap F.bot t) t
        subType l g tx xBot
        -- NOTE: return the target type (falsified)
        envAddFresh l tBot g

-- | UpCast(x, t1 => t2)
consUpCast g l x _ t2
  = do  tx   <- safeEnvFindTy x g
        ztx  <- zipTypeM l g tx t2
        envAddFresh l (ztx `eSingleton` x) g

-- | DownCast(x, t1 => t2)
consDownCast g l x _ t2
  = do  tx   <- safeEnvFindTy x g
        txx  <- zipTypeM l g tx tx 
        tx2  <- zipTypeM l g t2 tx
        ztx  <- zipTypeM l g tx t2
        subType l g txx tx2
        envAddFresh l ztx g


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
       -- Attempt to gather qualifiers here -- needed for object literal quals
       -- REMOVING qualifier scraping from here - expect tests to break.
       ts           <- mapM (\x -> safeEnvFindTy x g') xes
       case overload l of
         Just ft    -> do  (_,its,ot)   <- instantiate l g fn ft
                           let (su, ts') = renameBinds its xes
                           zipWithM_ (subType l g') ts ts'
                           envAddFresh l (F.subst su ot) g'
         Nothing    -> cgError l $ errorNoMatchCallee (srcPos l) fn (toType <$> ts) (toType <$> callSigs)
    where
       overload l    = listToMaybe [ lt | Overload cx t <-  ann_fact l 
                                        , cge_ctx g     == cx
                                        , lt            <- callSigs
                                        , toType t      == toType lt ]
       callSigs      = extractCall g ft0


consCallDotRef g l fn rcvr elts es 
    -- Static call
    | all isStaticSig elts
    = consCall g l fn es $ ft isStaticSig

    -- Virtual method call
    | all isMethodSig elts 
    = consCall g l fn (rcvr:es) $ ft isMethodSig

    -- Normal function call
    | all isFieldSig elts 
    = consCall g l fn es $ ft isFieldSig

    | otherwise
    = cgError l $ unsupportedDotRef (srcPos l) fn

  where
    ft f = mkAnd $ catMaybes $ mkEltFunTy <$> filter f elts


---------------------------------------------------------------------------------
instantiate :: (PP a, PPRS F.Reft) => 
  AnnTypeR -> CGEnv -> a -> RefType -> CGM  ([TVar], [Bind F.Reft], RefType)
---------------------------------------------------------------------------------
instantiate l g fn ft 
  = do  t'   <- freshTyInst l g αs ts t
        maybe err return $ bkFun t' 
    where 
      (αs, t) = bkAll ft
      ts      = envGetContextTypArgs g l αs
      err     = cgError l $ errorNonFunction (srcPos l) fn ft  


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
---------------------------------------------------------------------------------
consWhile :: CGEnv -> AnnTypeR -> Expression AnnTypeR -> Statement AnnTypeR -> CGM CGEnv
---------------------------------------------------------------------------------
consWhile g l cond body 
  = do  ts                  <- mapM (`safeEnvFindTy` g) xs 
        (gI,tIs)            <- freshTyPhis (srcPos l) g xs $ toType <$> ts  -- (a) (b) 
        _                   <- consWhileBase l xs tIs g                     -- (c)
        (xc, gI')           <- consExpr gI cond                             -- (d)
        z                   <- consStmt (envAddGuard xc True gI') body      -- (e)
        whenJustM z          $ consWhileStep l xs tIs                       -- (f) 
        return               $ envAddGuard xc False gI'
    where
        xs                   = concat [xs | PhiVar xs <- ann_fact l]

consWhileBase l xs tIs g    
  = do  xts_base             <- mapM (`safeEnvFindTy` g) xs
        zipWithM_ (subType l g) xts_base tIs                                -- (c)
 
consWhileStep l xs tIs gI''
  = do  xts_step              <- mapM (`safeEnvFindTy` gI'') xs'
        zipWithM_ (subType l gI'') xts_step tIs'  -- (f)
  where 
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
  = do  t1s     <- mapM (`safeEnvFindTy` g1) xs 
        t2s     <- mapM (`safeEnvFindTy` g2) xs
        g1'     <- envAdds (zip xs t1s) g1 
        g2'     <- envAdds (zip xs t2s) g2
        -- t1s and t2s should have the same raw type, otherwise they wouldn't
        -- pass TC (we don't need to pad / fix them before joining).
        -- So we can use the raw type from one of the two branches and freshen
        -- up that one.
        -- FIXME: Add a raw type check on t1 and t2
        (g',ts) <- freshTyPhis (srcPos l) g xs $ toType <$> t1s
        t1s'    <- mapM (`safeEnvFindTy` g1') xs
        t2s'    <- mapM (`safeEnvFindTy` g2') xs
        _       <- zipWithM_ (subType l g1') t1s' ts 
        _       <- zipWithM_ (subType l g2') t2s' ts      
        return   $ g'
    where
        xs   = concat [xs | PhiVar xs <- ann_fact l] 

