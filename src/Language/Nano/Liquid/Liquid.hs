{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TupleSections        #-}

-- | Top Level for Refinement Type checker
module Language.Nano.Liquid.Liquid (verifyFile) where

import           Control.Monad
import           Control.Applicative                ((<$>), (<*>))

import qualified Data.Traversable                   as T

import qualified Data.HashMap.Strict                as M
import           Data.Maybe                         (catMaybes, maybeToList, fromMaybe)

import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations
import           Language.ECMAScript3.PrettyPrint

import qualified Language.Fixpoint.Config           as C
import qualified Language.Fixpoint.Types            as F
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Interface        (solve)

import           Language.Nano.Misc                 (mseq, withSingleton, withSingleton')
import           Language.Nano.Annots
import           Language.Nano.CmdLine              (Config)
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
import           Language.Nano.Liquid.CGMonad
import qualified Data.Text                          as T 
import           System.Console.CmdArgs.Default

-- import           Debug.Trace                        (trace)
-- import qualified Data.Foldable                      as FO
-- import           Text.PrettyPrint.HughesPJ 

type PPR r = (PP r, F.Reftable r)
type PPRS r = (PPR r, Substitutable r (Fact r)) 

--------------------------------------------------------------------------------
verifyFile    :: Config -> FilePath -> [FilePath] -> IO (A.UAnnSol RefType, F.FixResult Error)
--------------------------------------------------------------------------------
verifyFile cfg f fs = parse     fs 
                    $ ssa 
                    $ tc    cfg 
                    $ refTc cfg f

parse fs next 
  = do  r <- parseNanoFromFiles fs
        donePhase Loud "Parse Files"
        nextPhase r next

ssa next p
  = do  r <- ssaTransform p              
        donePhase Loud "SSA Transform"
        nextPhase r next

tc cfg next p    
  = do  r <- typeCheck cfg p 
        donePhase Loud "Typecheck"
        nextPhase r next

refTc cfg f p
  = do donePhase Loud "Generate Constraints"
       solveConstraints f cgi
  where
    cgi = generateConstraints cfg p

nextPhase (Left l)  _    = return (A.NoAnn, l)
nextPhase (Right x) next = next x 
  


-- ppCasts (Nano { code = Src fs }) = 
--   fcat $ pp <$> [ (srcPos a, c) | a <- concatMap FO.toList fs
--                                 , TCast _ c <- ann_fact a ] 
         
-- | solveConstraints
--   Call solve with `ueqAllSorts` enabled.
--------------------------------------------------------------------------------
solveConstraints :: FilePath -> CGInfo -> IO (A.UAnnSol RefType, F.FixResult Error) 
--------------------------------------------------------------------------------
solveConstraints f cgi 
  = do (r, s)  <- solve (C.withUEqAllSorts def True) f [] $ cgi_finfo cgi
       let r'   = fmap (ci_info . F.sinfo) r
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
consNano p@(Nano {code = Src fs}) 
  = do  g   <- initGlobalEnv p
        consStmts g fs 
        return ()


-------------------------------------------------------------------------------
-- | Initialize environment
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
initGlobalEnv  :: NanoRefType -> CGM CGEnv
-------------------------------------------------------------------------------
initGlobalEnv (Nano { code = Src s }) = 
    do g <- freshenCGEnvM $ CGE nms bds grd ctx mod pth Nothing
       -- special casing undefined ... 
       envAdds "initGlobalEnv" [(Id (srcPos dummySpan) "undefined", (TApp TUndef [] fTop, ReadOnly))] g
  where
    nms       = E.envAdds nms_ids E.envEmpty
    nms_ids   = extras ++ visibleNames s
    extras    = [(Id (srcPos dummySpan) "undefined", (TApp TUndef [] fTop, ReadOnly))]
    bds       = F.emptyIBindEnv
    grd       = []
    mod       = scrapeModules s 
    ctx       = emptyContext
    pth       = AP $ QPath (srcPos dummySpan) []


-------------------------------------------------------------------------------
initModuleEnv :: (F.Symbolic n, PP n) => CGEnv -> n -> [Statement AnnTypeR] -> CGM CGEnv
-------------------------------------------------------------------------------
initModuleEnv g n s = freshenCGEnvM $ CGE nms bds grd ctx mod pth (Just g)
  where

    nms       = E.envAdds (visibleNames s) E.envEmpty
    bds       = cge_fenv g
    grd       = []
    mod       = cge_mod g
    ctx       = emptyContext
    pth       = extendAbsPath (cge_path g) n


-- | `initFuncEnv l f i xs (αs, ts, t) g` 
--
--    * Pushes a new context @i@
--    * Adds return type @t@
--    * Adds binders for the type variables @αs@
--    * Adds binders for the arguments @ts@
--    * Adds binder for the 'arguments' variable
--
initFuncEnv l f i xs (αs,thisTO,ts,t) g s =
    --  Compute base environment @g'@, then add extra bindings
        envAdds "initFunc-0" varBinds g'
    >>= envAdds "initFunc-1" tyBinds 
    >>= envAdds "initFunc-2" (visibleNames s)
    >>= envAdds "initFunc-3" argBind
    >>= envAdds "initFunc-4" thisBind
    >>= envAddReturn f t
    >>= freshenCGEnvM
  where
    g'        = CGE nms fenv grds ctx mod pth parent
    nms       = E.envEmpty
    fenv      = cge_fenv g
    grds      = []
    mod       = cge_mod g
    ctx       = pushContext i (cge_ctx g)
    pth       = cge_path g
    parent    = Just g
    tyBinds   = [(Loc (srcPos l) α, (tVar α, ReadOnly)) | α <- αs]
    varBinds  = zip (fmap ann <$> xs) (zip ts (repeat WriteLocal))
    argBind   = [(argId l, (argTy l ts (cge_names g), ReadOnly))]
    thisBind  = (\t -> (Id (srcPos dummySpan) "this", (t, WriteGlobal))) <$> maybeToList thisTO
    




-------------------------------------------------------------------------------
-- | Environment wrappers
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
consEnvFindTypeDefM :: IsLocated a => a -> CGEnv -> RelName -> CGM (IfaceDef F.Reft)
-------------------------------------------------------------------------------
consEnvFindTypeDefM l γ x
  = case resolveRelNameInEnv γ x of 
      Just t  -> return t
      Nothing -> cgError $ bugClassDefNotFound (srcPos l) x


-------------------------------------------------------------------------------
-- | Constraint generation
-------------------------------------------------------------------------------

--------------------------------------------------------------------------------
consFun :: CGEnv -> Statement (AnnType F.Reft) -> CGM CGEnv
--------------------------------------------------------------------------------
consFun g (FunctionStmt l f xs body) 
  = case envFindTy f g of
      Just spec -> do ft        <- cgFunTys l f xs spec
                      forM_ ft   $ consFun1 l g f xs body
                      return     $ g
      Nothing   -> cgError $ errorMissingSpec (srcPos l) f
       
consFun _ s 
  = die $ bug (srcPos s) "consFun called not with FunctionStmt"

-- | @consFun1@ checks a function body against a *one* of multiple
--   conjuncts of an overloaded (intersection) type signature.
--   Assume: len ts' = len xs

consFun1 l g f xs body (i, ft) 
  = initFuncEnv l f i xs ft g body >>= (`consStmts` body)

                                  
--------------------------------------------------------------------------------
consStmts :: CGEnv -> [Statement AnnTypeR]  -> CGM (Maybe CGEnv) 
--------------------------------------------------------------------------------
consStmts g stmts = consFold consStmt g stmts


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
  = consAsgn l g (Id lx x) e

-- e1.f = e2
consStmt g (ExprStmt l (AssignExpr _ OpAssign (LDot _ e1 f) e2))
  = mseq (consExpr g e1 Nothing) $ \(x1,g') ->
      do t <- safeEnvFindTy x1 g' 
         consSetProp g' x1 (fmap snd $ getProp g' False f t)
  where
    consSetProp g x rhsCtx = 
      do opTy      <- setPropTy l (F.symbol f) <$> safeEnvFindTy (builtinOpId BISetProp) g
         fmap snd <$> consCall g l BISetProp (FI Nothing [(vr x, Nothing),(e2, rhsCtx)]) opTy
    vr = VarRef $ getAnnotation e1
   
-- e
consStmt g (ExprStmt _ e) 
  = fmap snd <$> consExpr g e Nothing

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
consStmt g (IfStmt l e s1 s2) =
  mseq (safeEnvFindTy (builtinOpId BITruthy) g 
            >>= consCall g l "truthy" (FI Nothing [(e, Nothing)])) $ \(xe,ge) -> do
    g1'      <- (`consStmt` s1) $ envAddGuard xe True  ge 
    g2'      <- (`consStmt` s2) $ envAddGuard xe False ge 
    envJoin l g g1' g2'

-- while e { s }  
consStmt g (WhileStmt l e s) 
  = Just <$> consWhile g l e s 

-- var x1 [ = e1 ]; ... ; var xn [= en];
consStmt g (VarDeclStmt _ ds)
  = consFold consVarDecl g ds
    
-- return 
consStmt g (ReturnStmt l Nothing)
  = do _ <- subType l (errorLiquid' l) g tVoid (envFindReturn g) 
       return Nothing
       
-- return e 
consStmt g (ReturnStmt l (Just e))
  = do  _ <- consCall g l "return" (FI Nothing [(e, Just retTy)]) $ returnTy retTy True 
        return Nothing
  where
    retTy = envFindReturn g 

-- throw e 
consStmt g (ThrowStmt _ e)
  = consExpr g e Nothing >> return Nothing

-- function f(x1...xn);
consStmt g (FunctionDecl l n _ )
  = case envFindTy n g of
      Just _  -> return $ Just g
      Nothing -> cgError $ bugEnvFindTy (srcPos l) n

-- function f(x1...xn){ s }
consStmt g s@(FunctionStmt _ _ _ _)
  = Just <$> consFun g s

--
-- class A<V> [extends B<T>] {..}
--
--  * Add the type vars V in the environment
--  
--  * Compute type for "this" and add that to the env as well. 
--
consStmt g (ClassStmt l x _ _ ce) 
  = do  dfn      <- consEnvFindTypeDefM l g rn
        -- FIXME: Should this check be done at TC too?
        g'       <- envAdds "consStmt-class-0" [(Loc (ann l) α, (tVar α, WriteGlobal)) | α <- t_args dfn] g
        consClassElts g' dfn ce
        return    $ Just g
  where
    rn            = RN $ QName (srcPos l) [] (F.symbol x)

consStmt g (IfaceStmt _)
  = return $ Just g

consStmt g (ModuleStmt _ n body)
  = initModuleEnv g n body >>= (`consStmts` body) >> return (Just g)

-- OTHER (Not handled)
consStmt _ s 
  = errorstar $ "consStmt: not handled " ++ ppshow s


------------------------------------------------------------------------------------
consVarDecl :: CGEnv -> VarDecl AnnTypeR -> CGM (Maybe CGEnv) 
------------------------------------------------------------------------------------
consVarDecl g v@(VarDecl l x (Just e)) =
    withSingleton'
      (mseq (consExpr g e Nothing) $ \(y,gy) -> do
                t       <- safeEnvFindTy y gy
                Just   <$> envAdds "consVarDecl" [(x, (t, WriteLocal))] gy)

      (\ta -> mseq (consExpr g e $ Just ta) $ \(y, gy) -> do
                t       <- safeEnvFindTy y gy
                fta     <- freshTyVar gy l ta
                _       <- subType l (errorLiquid' l) gy t fta
                _       <- subType l (errorLiquid' l) gy fta ta
                Just   <$> envAdds "consVarDecl" [(x, (fta, WriteGlobal))] g)

      (cgError $ errorVarDeclAnnot (srcPos l) x)
      (scrapeVarDecl v)
 
consVarDecl g v@(VarDecl l x Nothing) =
  withSingleton' 
    (consVarDecl g $ VarDecl l x $ Just $ VarRef l $ Id l "undefined")
    ((Just <$>) . (`add` g) . (x,) . (,WriteGlobal))
    (cgError $ errorVarDeclAnnot (srcPos l) x)
    (scrapeVarDecl v)
  where 
    add xt = envAdds "consVarDecl" [xt] 


------------------------------------------------------------------------------------
consExprT :: AnnTypeR -> CGEnv -> Expression AnnTypeR -> Maybe RefType 
          -> CGM (Maybe (Id AnnTypeR, CGEnv)) 
------------------------------------------------------------------------------------
consExprT _ g e Nothing  = consExpr g e Nothing
consExprT l g e (Just t) = consCall  g l "consExprT" (FI Nothing [(e, Nothing)])
                         $ TFun Nothing [B (F.symbol "x") t] tVoid fTop

-- FIXME: Do safeExtends check here. Also add casts in the TC phase where needed
------------------------------------------------------------------------------------
consClassElts :: CGEnv -> IfaceDef F.Reft -> [ClassElt AnnTypeR] -> CGM ()
------------------------------------------------------------------------------------
consClassElts g dfn ce
   = mapM_ (consClassElt g dfn) ce


------------------------------------------------------------------------------------
consClassElt :: CGEnv -> IfaceDef F.Reft -> ClassElt AnnTypeR -> CGM ()
------------------------------------------------------------------------------------
consClassElt g dfn (Constructor l xs body) 
  = case findAnnot of
      Just ft -> do its <- cgCtorTys l i ft
                    g' <- envAdds "consClassElt-1" [(Loc (ann l) "this", (mkThis $ t_args dfn, WriteGlobal))] g
                    mapM_ (consFun1 l g' i xs body) its
      _       -> cgError $ unsupportedNonSingleConsTy $ srcPos l
  where 
    i             = Id l "constructor"
    findAnnot     = case [ t | ConsAnn (ConsSig t)  <- ann_fact l ] of
                      [t] -> Just t
                      [ ] -> Just $ TFun Nothing [] tVoid fTop
                      _   -> Nothing
    mkThis (_:αs) = TApp (TRef rn) (t_mutable : map tVar αs) fTop
    rn            = RN $ QName (srcPos l) [] (F.symbol $ t_name dfn)

consClassElt g dfn (MemberVarDecl _ static (VarDecl l1 x eo))
  = case anns of 
      []  ->  cgError       $ errorClassEltAnnot (srcPos l1) (t_name dfn) x
      fs  ->  case eo of
                Just e     -> void <$> consCall g l1 "field init" (FI Nothing [(e,Nothing)]) $ ft fs
                Nothing    -> return ()
  where
    anns | static    = [ s | StatAnn  s <- ann_fact l1 ]
         | otherwise = [ f | FieldAnn f <- ann_fact l1 ]
    ft flds = mkAnd $ catMaybes $ mkInitFldTy <$> flds
  
-- | Static method
consClassElt g dfn (MemberMethDecl l True i xs body) 
  = case anns of
      [t]   -> do its   <- cgFunTys l i xs t
                  mapM_    (consFun1 l g i xs body) its
      _     -> cgError  $ errorClassEltAnnot (srcPos l) (t_name dfn) i
  where
    anns     = [ t | StatAnn (StatSig _ _ t)  <- ann_fact l ]
 
-- | Instance method
consClassElt g dfn (MemberMethDecl l False i xs body) 
  = case anns of
      [(m,t)] -> do its   <- cgFunTys l i xs t --   cgMethTys l i (m,t)
                    g'    <- envAdds "consClassElt-1" [(Loc (ann l) "this", (mkThis m $ t_args dfn, WriteGlobal))] g
                    mapM_    (consFun1 l g' i xs body) its

      _       -> cgError  $ errorClassEltAnnot (srcPos l) (t_name dfn) i
  where
    anns                  = [ (m, t) | MethAnn (MethSig _ m t)  <- ann_fact l ]

    mkThis m (_:αs)       = TApp (TRef rn) (ofType m : map tVar αs) fTop
    rn                    = RN $ QName (srcPos l) [] (F.symbol $ t_name dfn)



--------------------------------------------------------------------------------
consAsgn :: AnnTypeR -> CGEnv -> Id AnnTypeR -> Expression AnnTypeR -> CGM (Maybe CGEnv)
--------------------------------------------------------------------------------
consAsgn l g x e =
  case envFindTyWithAsgn x g of 
    Just (t,WriteGlobal) -> mseq (consExprT l g e $ Just t) $ \(_, g') -> 
                              return    $ Just g'
    Just (t,a)           -> mseq (consExprT l g e $ Just t) $ \(x', g') -> do
                              t        <- safeEnvFindTy x' g'
                              Just    <$> envAdds "consAsgn-1" [(x,(t,a))] g'
    Nothing              -> mseq (consExprT l g e Nothing) $ \(x', g') -> do
                              t        <- safeEnvFindTy x' g'
                              Just    <$> envAdds "consAsgn-1" [(x,(t,WriteLocal))] g'


-- | @consExpr g e@ returns a pair (g', x') where x' is a fresh, 
-- temporary (A-Normalized) variable holding the value of `e`,
-- g' is g extended with a binding for x' (and other temps required for `e`)
------------------------------------------------------------------------------------
consExpr :: CGEnv -> Expression AnnTypeR -> Maybe RefType -> CGM (Maybe (Id AnnTypeR, CGEnv))
------------------------------------------------------------------------------------
consExpr g (Cast_ l e) _ =
  case envGetContextCast g l of
    CDead e' t' -> consDeadCode g l e' t'
    CNo         -> mseq (consExpr g e Nothing) $ return . Just
    CUp t t'    -> mseq (consExpr g e Nothing) $ \(x,g) -> Just <$> consUpCast   g l x t t'
    CDn t t'    -> mseq (consExpr g e Nothing) $ \(x,g) -> Just <$> consDownCast g l x t t'

-- | < t > e
consExpr g ex@(Cast l e) _ =
  withSingleton (consCast g l e) 
                (cgError $  bugNoCasts (srcPos l) ex) 
                [ ct | UserCast ct <- ann_fact l ]

consExpr g (IntLit l i) _
  = Just <$> envAddFresh l (eSingleton tInt i, WriteLocal) g

consExpr g (BoolLit l b) _
  = Just <$> envAddFresh l (pSingleton tBool b, WriteLocal) g 

consExpr g (StringLit l s) _
  = Just <$> envAddFresh l (eSingleton tString (T.pack s), WriteLocal) g

consExpr g (NullLit l) _
  = Just <$> envAddFresh l (tNull, WriteLocal) g

consExpr g (ThisRef l) _
  = case envFindTy (Id (ann l) "this") g of
      Just t  -> Just <$> envAddFresh l (t, WriteGlobal) g
      Nothing -> cgError $ errorUnboundId (ann l) "this" 

consExpr g (VarRef l x) _
  = case envFindTy x g of
      Just t  -> addAnnot (srcPos l) x t >> return (Just (x, g))
      Nothing -> cgError $ errorUnboundId (ann l) x

consExpr g (PrefixExpr l o e) _
  = do opTy         <- safeEnvFindTy (prefixOpId o) g
       consCall g l o (FI Nothing [(e,Nothing)]) opTy

consExpr g (InfixExpr l o@OpInstanceof e1 e2) _
  = mseq (consExpr g e2 Nothing) $ \(x, g') -> do
       t            <- safeEnvFindTy x g'
       case t of
         TClass x   -> do opTy <- safeEnvFindTy (infixOpId o) g
                          consCall g l o (FI Nothing ((,Nothing) <$> [e1, StringLit l2 (cc x)])) opTy
         _          -> cgError $ unimplemented (srcPos l) "tcCall-instanceof" $ ppshow e2
  where
    l2 = getAnnotation e2
    cc (RN (QName _ _ s)) = F.symbolString s 

consExpr g (InfixExpr l o e1 e2) _
  = do opTy <- safeEnvFindTy (infixOpId o) g
       consCall g l o (FI Nothing ((,Nothing) <$> [e1, e2])) opTy

-- | e ? e1 : e2
consExpr g (CondExpr l e e1 e2) to
  = do  opTy                      <- mkTy to <$> safeEnvFindTy (builtinOpId BICondExpr) g
        tt'                       <- freshTyFun g l $ rType tt
        (v,g')                    <- mapFst (VarRef l) <$> envAddFresh l (tt', WriteLocal) g
        consCallCondExpr g' l BICondExpr (FI Nothing ((,Nothing) <$> [e,v,e1,e2])) opTy
  where
    tt       = fromMaybe tTop to
    mkTy Nothing (TAll cv (TAll tv (TFun Nothing [B c_ tc, B t_ _, B x_ xt, B y_ yt] o r))) = 
      TAll cv (TAll tv (TFun Nothing [B c_ tc, B t_ tTop, B x_ xt, B y_ yt] o r))
    mkTy _ t = t

-- | super(e1,..,en)
consExpr g (CallExpr l (SuperRef _) es) _
  = case envFindTy (F.symbol "this") g of
      Just t -> case extractParent g t of 
                  Just (TApp (TRef x) _ _) -> case extractCtor g (TClass x) of
                                                Just ct -> consCall g l "super" (FI Nothing ((,Nothing) <$> es)) ct
                                                _       -> cgError $ errorUnboundId (ann l) "super"
                  Just _  -> cgError $ errorUnboundId (ann l) "super"
                  Nothing -> cgError $ errorUnboundId (ann l) "super"
      Nothing -> cgError $ errorUnboundId (ann l) "this"

-- | e.m(es)
consExpr g (CallExpr l em@(DotRef _ e f) es) _
  = mseq (consExpr g e Nothing) $ \(x,g') -> do 
      t      <- safeEnvFindTy x g'
      case t of 
        t | isVariadicCall f  -> 
            case es of
              []   -> cgError $ errorVariadicNoArgs (srcPos l) em
              v:vs -> consCall g l em (FI (Just (v, Nothing)) (nth vs)) t

          | otherwise         -> case getProp g True f t of
                                   Just (_, tf) -> consCall g l em (FI (Just (e, Nothing)) ((,Nothing) <$> es)) tf
                                   Nothing      -> cgError $ errorCallNotFound (srcPos l) e f
  where
    isVariadicCall f = F.symbol f == F.symbol "call"
    nth              = ((,Nothing) <$>)

-- | e(es)
consExpr g (CallExpr l e es) _
  = mseq (consExpr g e Nothing) $ \(x, g') -> 
      safeEnvFindTy x g' >>= consCall g' l e (FI Nothing ((,Nothing) <$> es))

-- | e.f
consExpr g ef@(DotRef l e f) _
  = mseq (consExpr g e Nothing) $ \(x,g') -> do
      te            <- safeEnvFindTy x g'
      case getProp g' False f te of
        Just (_, t) -> consCall g' l ef (FI Nothing ((,Nothing) <$> [vr x])) $ mkTy te t
        Nothing     -> cgError $ errorMissingFld (srcPos l) f te
  where
    mkTy s t = mkFun ([],Nothing,[B (F.symbol "this") s],t) 
    vr       = VarRef $ getAnnotation e

-- FIXME: e["f"]

-- | e1[e2]
consExpr g (BracketRef l e1 e2) _
  = do  opTy <- safeEnvFindTy (builtinOpId BIBracketRef) g
        consCall g l BIBracketRef (FI Nothing ((,Nothing) <$> [e1, e2])) opTy

-- | e1[e2] = e3
consExpr g (AssignExpr l OpAssign (LBracket _ e1 e2) e3) _
  = do  opTy <- safeEnvFindTy (builtinOpId BIBracketAssign) g
        consCall g l BIBracketAssign (FI Nothing ((,Nothing) <$> [e1,e2,e3])) opTy

-- | [e1,...,en]
consExpr g (ArrayLit l es) _
  = do  opTy <- arrayLitTy l (length es) <$> safeEnvFindTy (builtinOpId BIArrayLit) g
        consCall g l BIArrayLit (FI Nothing ((,Nothing) <$> es)) opTy

-- | {f1:e1,...,fn:en}
consExpr g (ObjectLit l bs) _
  = consCall g l "ObjectLit" (FI Nothing ((,Nothing) <$> es)) $ objLitTy l ps
  where
    (ps, es) = unzip bs

-- | new C(e, ...)
consExpr g (NewExpr l e es) _
  = mseq (consExpr g e Nothing) $ \(x,g') -> do
      t       <- safeEnvFindTy x g'
      case extractCtor g t of
        Just ct -> consCall g l "constructor" (FI Nothing ((,Nothing) <$> es)) ct
        Nothing -> cgError $ errorConstrMissing (srcPos l) t 

-- | super
consExpr g (SuperRef l) _
  = case envFindTy (Id (ann l) "this") g of 
      Just t   -> case extractParent g t of 
                    Just tp -> Just <$> envAddFresh l (tp, WriteGlobal) g
                    Nothing -> cgError $ errorSuper (ann l)
      Nothing  -> cgError $ errorSuper (ann l)

-- | function(xs) { }
consExpr g (FuncExpr l fo xs body) tCxtO
  = case anns of
      [ft] -> consFuncExpr ft
      _    -> case tCxtO of
                Just tCtx -> consFuncExpr tCtx
                Nothing   -> cgError $ errorNoFuncAnn $ srcPos l
  where
    consFuncExpr ft = do kft       <-  freshTyFun g l ft
                         fts       <-  cgFunTys l f xs kft
                         forM_ fts  $  consFun1 l g f xs body
                         Just      <$> envAddFresh l (kft, WriteGlobal) g 
  
    anns            = [ t | FuncAnn t <- ann_fact l ]
    f               = maybe (F.symbol "<anonymous>") F.symbol fo

-- not handled
consExpr _ e _ = cgError $ unimplemented l "consExpr" e where l = srcPos  e


--------------------------------------------------------------------------------
consCast :: CGEnv -> AnnTypeR -> Expression AnnTypeR -> RefType -> CGM (Maybe (Id AnnTypeR, CGEnv))
--------------------------------------------------------------------------------
-- | Only freshen if TFun, otherwise the K-var will be instantiated with false
consCast g l e tc
  = do  opTy    <- safeEnvFindTy (builtinOpId BICastExpr) g
        tc'     <- freshTyFun g l $ rType tc
        (v,g')  <- mapFst (VarRef l) <$> envAddFresh l (tc', WriteLocal) g
        consCall g' l "user-cast" (FI Nothing [(v, Nothing),(e,Just tc')]) opTy  
                      
-- | Dead code 
consDeadCode g l e t
  = do subType l e g t tBot
       return Nothing
    where
       tBot = t `strengthen` F.bot (rTypeR t)

-- | UpCast(x, t1 => t2)
consUpCast g l x _ t2
  = do  (tx,a)  <- safeEnvFindTyWithAsgn x g
        ztx     <- zipTypeM l g tx t2
        envAddFresh l (ztx `eSingleton` x,a) g

-- | DownCast(x, t1 => t2)
consDownCast g l x _ t2
  = do  (tx,a)  <- safeEnvFindTyWithAsgn x g
        txx     <- zipTypeM l g tx tx 
        tx2     <- zipTypeM l g t2 tx
        ztx     <- zipTypeM l g tx t2
        subType l (errorDownCast (srcPos l) txx t2) g txx tx2
        envAddFresh l (ztx,a) g


-- | `consCall g l fn ets ft0`:
--   
--   * @ets@ is the list of arguments, as pairs of expressions and optionally 
--     contextual types on them.
--   * @ft0@ is the function's signature -- the 'this' argument should have been
--     made explicit by this point.
--
--------------------------------------------------------------------------------
consCall :: PP a => CGEnv -> AnnTypeR -> a -> FuncInputs (Expression AnnTypeR, Maybe RefType)
                 -> RefType -> CGM (Maybe (Id AnnTypeR, CGEnv))
--------------------------------------------------------------------------------

--   1. Fill in @instantiateFTy@ to get a monomorphic instance of @ft@ 
--      i.e. the callee's RefType, at this call-site (You may want 
--      to use @freshTyInst@)
--   2. Use @consExpr@ to determine types for arguments @es@
--   3. Use @subType@ to add constraints between the types from (step 2) and (step 1)
--   4. Use the @F.subst@ returned in 3. to substitute formals with actuals in output type of callee.

consCall g l fn ets ft0 
  = mseq (consScan consExpr g ets) $ \(xes, g') -> do
      ts <- T.mapM (`safeEnvFindTy` g') xes
      withSingleton
        (\ft -> consInstantiate l g' fn ft ts xes)
        (cgError $ errorNoMatchCallee (srcPos l) fn (toType <$> ts) (toType <$> callSigs))
        [ lt | Overload cx t <- ann_fact l
             , cge_ctx g == cx
             , lt <- callSigs
             , toType t == toType lt ]
  where
    callSigs = extractCall g ft0
    
balance (FI (Just to) ts) (FI Nothing fs)  = (FI (Just to) ts, FI (Just $ B (F.symbol "this") to) fs)
balance (FI Nothing ts)   (FI (Just _) fs) = (FI Nothing ts, FI Nothing fs)
balance ts                fs               = (ts, fs) 

--------------------------------------------------------------------------------
consInstantiate :: (F.Symbolic b, PP a) 
                => AnnTypeR -> CGEnv -> a -> RefType -> FuncInputs RefType 
                -> FuncInputs b -> CGM (Maybe (Id AnnTypeR, CGEnv))
--------------------------------------------------------------------------------
consInstantiate l g fn ft ts xes 
  = do  (_,its1,ot)     <- instantiateFTy l g fn ft
        ts1             <- idxMapFI (instantiateTy l g) 1 ts
        let (ts2, its2)  = balance ts1 its1
        let (su, ts3)    = renameBinds (toList its2) (toList xes)
        _               <- zipWithM_ (subType l err g) (toList ts2) ts3
        Just           <$> envAddFresh l (F.subst su ot, WriteLocal) g
  where
    toList (FI x xs)     = maybeToList x ++ xs
    err                  = errorLiquid' l
    idxMapFI f i (FI Nothing ts)  = FI     Nothing        <$> mapM (uncurry f) (zip [i..] ts)
    idxMapFI f i (FI (Just t) ts) = FI <$> Just <$> f i t <*> mapM (uncurry f) (zip [(i+1)..] ts)



-- Special casing conditional expression call here because we'd like the
-- arguments to be typechecked under a different guard each.
consCallCondExpr g l fn ets ft0 
  = mseq (consCondExprArgs (srcPos l) consExpr g ets) $ \(xes, g') -> do
      ts <- T.mapM (`safeEnvFindTy` g') xes
      withSingleton
        (\ft -> consInstantiate l g' fn ft ts xes)
        (cgError $ errorNoMatchCallee (srcPos l) fn (toType <$> ts) (toType <$> callSigs))
        [ lt | Overload cx t <- ann_fact l, cge_ctx g == cx, lt <- callSigs, toType t == toType lt ]
  where
    callSigs      = extractCall g ft0

----------------------------------------------------------------------------------
instantiateTy :: AnnTypeR -> CGEnv -> Int -> RefType -> CGM RefType
----------------------------------------------------------------------------------
instantiateTy l g i t = freshTyInst l g αs ts t'
    where 
      (αs, t')        = bkAll t
      ts              = envGetContextTypArgs i g l αs

---------------------------------------------------------------------------------
instantiateFTy :: (PP a, PPRS F.Reft) => 
  AnnTypeR -> CGEnv -> a -> RefType -> CGM  ([TVar], FuncInputs (Bind F.Reft), RefType)
---------------------------------------------------------------------------------
instantiateFTy l g fn ft 
  = do  t'   <- freshTyInst l g αs ts t
        maybe err return $ bkFunBinds t' 
    where 
      (αs, t) = bkAll ft
      ts      = envGetContextTypArgs 0 g l αs
      err     = cgError $ errorNonFunction (srcPos l) fn ft  

-----------------------------------------------------------------------------------
consScan :: (CGEnv -> a -> b -> CGM (Maybe (c, CGEnv))) -> CGEnv -> FuncInputs (a,b) 
            -> CGM (Maybe (FuncInputs c, CGEnv))
-----------------------------------------------------------------------------------
consScan f g (FI (Just x) xs)
  = do  z  <- (uncurry $ f g) x 
        case z of
          Just (x', g') -> do zs  <- fmap (mapFst reverse) <$> consFold step ([], g') xs
                              case zs of
                                Just (xs', g'') -> return $ Just (FI (Just x') xs', g'')
                                _               -> return $ Nothing
          _             -> return Nothing
  where
    step (ys, g) (x,y) = fmap (mapFst (:ys))   <$> f g x y

consScan f g (FI Nothing xs) = 
    do  z <- fmap (mapFst reverse) <$> consFold step ([], g) xs
        case z of 
          Just (xs', g') -> return $ Just (FI Nothing xs', g')
          _              -> return $ Nothing 
  where
    step (ys, g) (x,y) = fmap (mapFst (:ys))   <$> f g x y

    
---------------------------------------------------------------------------------
consCondExprArgs :: (F.Symbolic c, IsLocated c, PP b, PP c) 
                 => SourceSpan 
                 -> (CGEnv -> Expression AnnTypeR -> b -> CGM (Maybe (c, CGEnv))) 
                 -> CGEnv 
                 -> FuncInputs (Expression AnnTypeR,b) 
                 -> CGM (Maybe (FuncInputs c, CGEnv))
---------------------------------------------------------------------------------
consCondExprArgs _ f g (FI Nothing [(c,tc),(t,tt),(x,tx),(y,ty)]) =
  do  z <- fmap (mapFst reverse)
            <$> do zc <- f g c tc
                   case zc of
                     Just (b, g') -> fmap (mapFst (++ [b]))
                                      <$> consFold (step b) ([],g') 
                                            [(t,tt,True),(x,tx,True),(y,ty,False)]
                                      -- The thrid argument for the first
                                      -- element is phony - it doesn't even
                                      -- matter
                     _            -> return Nothing 
      case z of 
        Just (xs', g'') -> return $ Just (FI Nothing xs', g'')
        _              -> return $ Nothing 
  where
    step c (ys, g) (x,y,b) = fmap (mapFst (:ys) . mapSnd envPopGuard) <$> f (envAddGuard c b g) x y

consCondExprArgs l _ _ _ = cgError $ impossible l "consCondExprArgs"
    

---------------------------------------------------------------------------------
consFold :: (t -> b -> CGM (Maybe t)) -> t -> [b] -> CGM (Maybe t)
---------------------------------------------------------------------------------
consFold f          = foldM step . Just 
  where 
    step Nothing _  = return Nothing
    step (Just g) x = f g x


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
  = do  -- ts                  <- mapM (`safeEnvFindTy` g) xs 
        -- (gI,tIs)            <- freshTyPhis (srcPos l) g xs $ toType <$> ts  -- (a) (b) 
        (gI,tIs)            <- freshTyPhis l g xs ts                        -- (a) (b) 
        _                   <- consWhileBase l xs tIs g                     -- (c)
        Just (xc, gI')      <- consExpr gI cond Nothing                     -- (d)
        z                   <- consStmt (envAddGuard xc True gI') body      -- (e)
        whenJustM z          $ consWhileStep l xs tIs                       -- (f) 
        return               $ envAddGuard xc False gI'
    where
        (xs,ts)              = unzip $ concat [xts | PhiVarTy xts <- ann_fact l]

consWhileBase l xs tIs g    
  = do  xts_base           <- mapM (`safeEnvFindTy` g) xs 
        xts_base'          <- zipWithM (zipTypeM (srcPos l) g) xts_base tIs -- (c)
        zipWithM_ (subType l err g) xts_base' tIs         
  where 
    err                      = errorLiquid' l

consWhileStep l xs tIs gI'' 
  = do  xts_step           <- mapM (`safeEnvFindTy` gI'') xs' 
        xts_step'          <- zipWithM (zipTypeM (srcPos l) gI'') xts_step tIs'
        zipWithM_ (subType l err gI'') xts_step' tIs'                       -- (f)
  where 
    tIs'                    = F.subst su <$> tIs
    xs'                     = mkNextId   <$> xs
    su                      = F.mkSubst   $  safeZip "consWhileStep" (F.symbol <$> xs) (F.eVar <$> xs')
    err                     = errorLiquid' l

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
  = do  t1s     <- mapM (`safeEnvFindTyWithAsgn` g1) xs 
        t2s     <- mapM (`safeEnvFindTyWithAsgn` g2) xs
        g1'     <- envAdds "envJoin-0" (zip xs t1s) g1 
        g2'     <- envAdds "envJoin-1" (zip xs t2s) g2
        -- t1s and t2s should have the same raw type, otherwise they wouldn't
        -- pass TC (we don't need to pad / fix them before joining).
        -- So we can use the raw type from one of the two branches and freshen
        -- up that one.
        -- FIXME: Add a raw type check on t1 and t2
        (g',ts) <- freshTyPhis l g xs $ toType . fst <$> t1s
        t1s'    <- mapM (`safeEnvFindTy` g1') xs
        t2s'    <- mapM (`safeEnvFindTy` g2') xs
        _       <- zipWithM_ (subType l err g1') t1s' ts 
        _       <- zipWithM_ (subType l err g2') t2s' ts      
        return   $ g'
    where
        xs   = concat [xs | PhiVar xs <- ann_fact l] 
        err  = errorLiquid' l 


errorLiquid' = errorLiquid . srcPos

-- Local Variables:
-- flycheck-disabled-checkers: (haskell-liquid)
-- End:
