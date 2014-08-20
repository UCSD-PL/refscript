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

import           Data.List                          (findIndex) 
import qualified Data.HashMap.Strict                as M
import           Data.Maybe                         (fromMaybe, listToMaybe, catMaybes, maybeToList, isJust)

import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations
import           Language.ECMAScript3.PrettyPrint

import qualified Language.Fixpoint.Config           as C
import qualified Language.Fixpoint.Types            as F
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Interface        (solve)

import           Language.Nano.Misc                 (mseq)
import           Language.Nano.CmdLine              (getOpts)
import           Language.Nano.Errors
import qualified Language.Nano.Env                  as Env --  (envUnion, envToList)
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
consNano     :: NanoRefType -> CGM ()
--------------------------------------------------------------------------------
consNano pgm@(Nano {code = Src fs}) 
  = do  g   <- initCGEnv pgm
        g'  <- envAdds True (Env.envToList $ externs pgm) g
        checkInterfaces pgm g'
        consStmts g' fs 
        return ()

checkInterfaces p g = 
     mapM_ (safeExtends l g δ) is
   where 
     δ           = defs p
     l           = srcPos dummySpan   -- FIXME  
     is          = [ d |d@(TD False _ _ _ _) <- tDefToList $ defs p ]


--------------------------------------------------------------------------------
initCGEnv :: NanoRefType -> CGM CGEnv
--------------------------------------------------------------------------------
initCGEnv pgm  
  = do  cd'   <- mapTDefEnvM fx cd
        setDef $ cd
        return $ CGE r f g cc cs cd'
  where 
      fx t    | isTFun t  = return t -- freshTyFun g0 l t -- Do just the fields for the moment
      fx t    | not (isTVar t) = freshTyVar g0 l t
      fx t    | otherwise = return t

      l       = srcPos dummySpan -- FIXME
      g0      = CGE r f g cc cs cd 
      r       = Env.envUnion (specs pgm) (externs pgm)
      f       = F.emptyIBindEnv 
      g       = [] 
      cc      = emptyContext 
      cs      = specs pgm
      cd      = defs pgm

--------------------------------------------------------------------------------
consFun :: CGEnv -> Statement (AnnType F.Reft) -> CGM CGEnv
--------------------------------------------------------------------------------
consFun g (FunctionStmt l f xs body) 
  = do  ft        <- cgFunTys l f xs (envFindTy f g) 
        forM_ ft   $ consFun1 l g f xs body
        return     $ g
       
consFun _ s 
  = die $ bug (srcPos s) "consFun called not with FunctionStmt"

consMeth1 l g f xs body (i, _, ft) = consFun1 l g f xs body (i,ft)

-- | @consFun1@ checks a function body against a *one* of multiple
--   conjuncts of an overloaded (intersection) type signature.
--   Assume: len ts' = len xs

consFun1 l g f xs body (i, ft) 
  = envAddFun l f i xs ft g >>= (`consStmts` body)

-- consFun1 l g' f xs body (i, ft) 
--   = do g'' <- envAddFun l f i xs ft g'
--        gm  <- consStmts g'' body
--        maybe (return ()) (\g -> subType l g tVoid (envFindReturn g'')) gm


envAddFun l f i xs (αs, ts', t') g =   (return $ envPushContext i g) 
                                   >>= (return . envAddReturn f t' ) 
                                   >>= envAdds False (varBinds xs ts')
                                   >>= envAdds False [argBind l ts' (renv g)]
                                   >>= envAdds False tyBinds
  where
    tyBinds                        = [(Loc (srcPos l) α, tVar α) | α <- αs]
    varBinds                       = safeZip "envAddFun"


-- | @argBind@ returns a dummy type binding `arguments :: T `
--   where T is an object literal containing the non-undefined `ts`.
    
argBind l ts g = (argId, immObjectLitTy l g ps' ts') 
  where
    argId      = Id l "arguments" 
    -- xs'           = take k xs
    ts'        = take k ts
    ps'        = PropNum l . toInteger <$> [0 .. k-1]
    k          = fromMaybe 0 $ findIndex isUndef ts

--     dArg        = ObjectLit l $ safeZip "argDecl" ps es 
--     es          = VarRef  l             <$> take k xs
--     l           = getAnnotation $ head body 



                                     
--------------------------------------------------------------------------------
consStmts :: CGEnv -> [Statement AnnTypeR]  -> CGM (Maybe CGEnv) 
--------------------------------------------------------------------------------
consStmts g stmts 
  = do g' <- addFunAndGlobs g stmts     -- K-Var functions and globals 
       consFold consStmt g' stmts

addFunAndGlobs g stmts 
  = do   g1 <- (\xts -> envAdds False xts g ) =<< mapM ff funs 
         g2 <- (\xts -> envAdds True  xts g1) =<< mapM vv globs 
         return g2
    where
         ff (l,f)    = (f,) <$> (freshTyFun g l =<< getDefType f)
         vv (l,x,t)  = (x,) <$> freshTyVar g l t
         funs        = definedFuns stmts 
         globs       = globsInScope stmts 
         -- globs    = definedGlobs stmts 

definedFuns       :: (Data a, Typeable a) => [Statement a] -> [(a,Id a)]
definedFuns stmts = everything (++) ([] `mkQ` fromFunction) stmts
  where 
    fromFunction (FunctionStmt l f _ _) = [(l,f)]
    fromFunction _                      = []

globsInScope :: [Statement AnnTypeR] -> [(AnnTypeR, Id AnnTypeR, RefType)]
globsInScope stmts = [ (l,x,t) | VarDeclStmt _ vd <- stmts
                               , VarDecl l x _    <- vd
                               , VarAnn t         <- ann_fact l ] 
 

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
   = fmap snd <$> (consCall g l BISetProp [e1,e2] $ setPropTy (F.symbol f) l $ renv g)
     
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
  = mseq (consCall g l "truthy" [e] bTru) $ \(xe, ge) -> do
      g1'      <- (`consStmt` s1) $ envAddGuard xe True  ge 
      g2'      <- (`consStmt` s2) $ envAddGuard xe False ge 
      envJoin l g g1' g2'
    where
      bTru = builtinOpTy l BITruthy $ renv g

-- while e { s }  
consStmt g (WhileStmt l e s) 
  = Just <$> consWhile g l e s 

-- var x1 [ = e1 ]; ... ; var xn [= en];
consStmt g (VarDeclStmt _ ds)
  = consFold consVarDecl g ds

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
consStmt g (ClassStmt l i _ _ ce) 
  = do  d@(TD _ _ αs _ _) <- findSymOrDieM i
        g'                <- envAdds False [(Loc (srcPos l) α, tVar α) | α <- αs] g
        cgWithThis           (TApp (TRef $ F.symbol i) (tVar <$> αs) fTop) $ consClassElts (srcPos l) g' i d ce
        g'                <- envAdds True [(i, TApp (TTyOf $ F.symbol i) [] fTop)] g
        return             $ Just g'

-- OTHER (Not handled)
consStmt _ s 
  = errorstar $ "consStmt: not handled " ++ ppshow s


------------------------------------------------------------------------------------
consVarDecl :: CGEnv -> VarDecl AnnTypeR -> CGM (Maybe CGEnv) 
------------------------------------------------------------------------------------
consVarDecl g (VarDecl l x (Just e)) 
  = case envGlobAnnot l x g of
      -- Global variable (Non-SSA)
      Just ta -> mseq (consExpr g e) $ \(y, gy) -> do
                   gy'     <- envRemSpec x gy
                   subType l gy' (envFindTy y gy) ta
                   return (Just gy)
      Nothing -> consAsgn g l x e
 
consVarDecl g (VarDecl l x Nothing)
  = case envGlobAnnot l x g of
      Just  t -> Just <$> envAdds False [(x, t)] g
      Nothing -> cgError l $ errorVarDeclAnnot (srcPos l) x


------------------------------------------------------------------------------------
consClassElts :: PP a => SourceSpan -> CGEnv -> a -> TDef F.Reft -> [ClassElt AnnTypeR] -> CGM ()
------------------------------------------------------------------------------------
consClassElts l g i d ce 
   = do  δ <- getDef
         -- FIXME: 
         -- There are no casts here, so we need to align the 
         -- types before doing subtyping on them.
         safeExtends l g δ d 
         mapM_ (consClassElt g i) ce


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
consExprT :: CGEnv -> Expression AnnTypeR -> Maybe RefType -> CGM (Maybe (Id AnnTypeR, CGEnv)) 
------------------------------------------------------------------------------------
consExprT g e to 
  = mseq (consExpr g e) $ \(x, g') -> do
      let te   = envFindTy x g'
      case to of
        Nothing -> return $ Just (x, g')
        Just t  -> do subType l g' te t
                      return $ Just (x, g')
                      -- (x,) <$> envAdds False [(x, t)] g'
    where
       l = getAnnotation e


--------------------------------------------------------------------------------
consAsgn :: CGEnv -> AnnTypeR -> Id AnnTypeR -> Expression AnnTypeR -> CGM (Maybe CGEnv)
--------------------------------------------------------------------------------
consAsgn g l x e 
  = mseq (consExprT g e $ envGlobAnnot l x g) $ \(x', g') -> do 
       if isGlobalVar x g 
         then return $ Just g'
         else Just <$> envAdds False [(x, envFindTy x' g')] g'


-- | @consExpr g e@ returns a pair (g', x') where x' is a fresh, 
-- temporary (A-Normalized) variable holding the value of `e`,
-- g' is g extended with a binding for x' (and other temps required for `e`)
------------------------------------------------------------------------------------
consExpr :: CGEnv -> Expression AnnTypeR -> CGM (Maybe (Id AnnTypeR, CGEnv))
------------------------------------------------------------------------------------
consExpr g (Cast a e)
  = consCast g a e

consExpr g (IntLit l i)               
  = Just <$> envAddFresh l (eSingleton tInt i) g

consExpr g (BoolLit l b)
  = Just <$> envAddFresh l (pSingleton tBool b) g 

consExpr g (StringLit l s)
  = Just <$> envAddFresh l (eSingleton tString (T.pack s)) g

consExpr g (NullLit l)
  = Just <$> envAddFresh l tNull g

consExpr g (ThisRef l)
  = do t   <- cgPeekThis 
       Just <$> envAddFresh l t g

consExpr g (VarRef i x)
  = do addAnnot (srcPos i) x $ envFindTy x g
       return $ Just (x, g) 

consExpr g (PrefixExpr l o e)
  = consCall g l o [e] (prefixOpTy o $ renv g)

consExpr g (InfixExpr l o@OpInstanceof e1 e2)
  = mseq (consExpr g e2) $ \(x, g') -> do
      case envFindTy x g' of
        TApp (TTyOf x) _ _ -> consCall g l o [e1, StringLit l2 (F.symbolString x)] (infixOpTy o $ renv g')
        _                  -> cgError l $ unimplemented (srcPos l) "tcCall-instanceof" $ ppshow e2
    where
       l2 = getAnnotation e2


consExpr g (InfixExpr l o e1 e2)        
  = consCall g l o [e1, e2]  $ infixOpTy o $ renv g

-- | e ? e1 : e2
consExpr g (CondExpr l e e1 e2)
  = consCall g l BICondExpr [e,e1,e2] $ builtinOpTy l BICondExpr $ renv g

-- | super(e1,..,en)
consExpr g (CallExpr l e@(SuperRef _) es) 
  = do elts <- t_elts <$> (getSuperDefM l =<< cgPeekThis)
       go [ t | ConsSig t <- elts ]
    where
       go [t] = consCall g l e es t
       go _   = cgError l $ unsupportedNonSingleConsTy $ srcPos l

-- | e.m(es)
consExpr g (CallExpr l em@(DotRef _ e f) es)
  = mseq (consExpr g e) $ \(x,g') -> do 
      δ      <- getDef
      consCallDotRef g' l em (vr x) (elt δ x g') es
    where
      -- Add a VarRef so that e is not typechecked again
      vr          = VarRef $ getAnnotation e
      elt δ x g   = getElt δ f $ envFindTy x g

-- | e(es)
consExpr g (CallExpr l e es)
  = mseq (consExpr g e) $ \(x, g') -> 
      consCall g' l e es $ envFindTy x g'

-- | e.f
consExpr g ef@(DotRef l e f)
  = mseq (consExpr g e) $ \(x,g') -> do
      δ      <- getDef        
      case getElt δ f $ envFindTy  x g' of 
        [FieldSig _ _ ft] -> consCall g' l ef [vr x] $ mkTy ft
        _                 -> cgError l $ errorExtractNonFld (srcPos l) f e 
    where
      -- Add a VarRef so that e is not typechecked again
      vr       = VarRef $ getAnnotation e
      mkTy t   = mkFun ([α], [B (F.symbol "this") tα], t) 
      α        = TV (F.symbol "α" ) (srcPos l)
      tα       = TVar α fTop

-- FIXME: e["f"]

-- | e1[e2]
consExpr g (BracketRef l e1 e2) 
  = consCall g l BIBracketRef [e1, e2] $ builtinOpTy l BIBracketRef $ renv g 

-- | e1[e2] = e3
consExpr g (AssignExpr l OpAssign (LBracket _ e1 e2) e3)
  = consCall g l BIBracketAssign [e1,e2,e3] $ builtinOpTy l BIBracketAssign $ renv g

-- TODO: Yuck. Please don't mix scrapeQualifiers and consGen
-- | [e1,...,en]
consExpr g (ArrayLit l es)
  = do  t <- scrapeQualifiers $ arrayLitTy l (length es) $ renv g
        consCall g l BIArrayLit es t

-- | {f1:e1,...,fn:en}
consExpr g (ObjectLit l bs) 
  = consCall g l "ObjectLit" es $ tracePP "objectLitTY" $ objLitTy l ps (renv g)
  where
    (ps, es) = unzip bs

-- | new C(e, ...)
consExpr g (NewExpr l (VarRef _ i) es)
  = getConstr (srcPos l) g i >>= consCall g l "constructor" es

-- | super
consExpr g (SuperRef l) 
  = do  z <- cgPeekThis 
        case z of 
          TApp (TRef i) ts _ -> 
              do  w <- findSymOrDieM i
                  case w of 
                    TD _ _ vs (Just (p, ps)) _ -> 
                        let θ = fromList $ zip vs ts in
                        let t = apply θ $ TApp (TRef $ F.symbol p) ps fTop in                        
                        Just <$> envAddFresh l t g
                    _                          -> cgError l $ errorSuper (srcPos l) 
          _ -> cgError l $ errorSuper (srcPos l) 

-- | function(xs) { }
consExpr g (FuncExpr l fo xs body) 
  = case anns of 
      [ft]  -> do kft       <- freshTyFun g l ft
                  fts       <- cgFunTys l f xs kft
                  forM_ fts  $ consFun1 l g f xs body
                  Just      <$> envAddFresh  l kft g 
      _    -> cgError l      $ errorNonSingleFuncAnn $ srcPos l
  where
    anns                     = [ t | FuncAnn t <- ann_fact l ]
    f                        = maybe (F.symbol "<anonymous>") F.symbol fo


-- not handled
consExpr _ e = cgError l $ unimplemented l "consExpr" e where l = srcPos  e

-- | `getConstr` first checks whether input @s@ is a class, in which case it
-- tries to retrieve a constructor binding, using a default one if that fails.
-- Otherwise, it tries to retrieve an object with the same name from the
-- environment that has a constructor property.
--
-- FIXME: Do not lookup the constructor by string. We have a special struct for
-- that.
----------------------------------------------------------------------------------
getConstr :: IsLocated a => SourceSpan -> CGEnv -> Id a -> CGM RefType
----------------------------------------------------------------------------------
getConstr l g s = 
    case findSym s (cge_defs g) of
      Just t | t_class t ->       -- This needs to be a class
        do  z <- getPropTDefM  l "__constructor__" t $ tVar <$> t_args t
            case z of 
              Just (TFun bs _ r) -> return $ abs (t_args t) $ TFun bs (retT t) r
              Just t             -> cgError l $ unsupportedConsTy l t
              Nothing            -> return $ abs (t_args t) $ TFun [] (retT t) fTop
      _ -> 
        do  z <- getPropM l "__constructor__" $ envFindTy s g
            case z of
              Just t  -> return t
              Nothing -> cgError l $ unsupportedNonSingleConsTy $ srcPos l
  where
    -- Constructor's return type is void - instead return the class type
    -- FIXME: type parameters in returned type: inferred ... or provided !!! 
    retT t   = TApp (TRef $ F.symbol s) (tVar <$> t_args t) fTop
    abs [] t = t
    abs vs t = foldr TAll t vs


--------------------------------------------------------------------------------
consCast :: CGEnv -> AnnTypeR -> Expression AnnTypeR -> CGM (Maybe (Id AnnTypeR, CGEnv))
--------------------------------------------------------------------------------
consCast g a e  
  | CDead _ t <- eCast = consDeadCode g l t 
  | otherwise          = mseq (consExpr g e) $ \(x,g) -> do 
                           δ     <- getDef
                           case eCast of
                             CNo       -> return $ Just (x,g)
                             CUp t t'  -> Just <$> consUpCast δ g l x t t'
                             CDn t t'  -> Just <$> consDownCast δ g l x t t'
                             _         -> error "impossible"
  where  
    l                  = srcPos a
    eCast              = envGetContextCast g a 
                       
-- | Dead code 
consDeadCode g l t
  = do subType l g t tBot
       return Nothing
    where
       tBot = strengthen t $ F.bot $ rTypeR t

-- | UpCast(x, t1 => t2)
consUpCast δ g l x _ t2 = envAddFresh l stx g
    where
        tx   = envFindTy x g
        ztx  = zipType δ tx t2
        stx  = ztx `eSingleton` x
    
-- | DownCast(x, t1 => t2)
consDownCast δ g l x _ t2
  = do  subType l g txx tx2
        envAddFresh l ztx g
    where 
        tx   = envFindTy x g
        -- This will drop all top-level refinements from union-level to its parts
        txx  = zipType δ tx tx 
        tx2  = zipType δ t2 tx
        ztx  = zipType δ tx t2



--------------------------------------------------------------------------------
consCall :: (PP a) => 
  CGEnv -> AnnTypeR -> a -> [Expression AnnTypeR] -> RefType -> CGM (Maybe (Id AnnTypeR, CGEnv))
--------------------------------------------------------------------------------

--   1. Fill in @instantiate@ to get a monomorphic instance of @ft@ 
--      i.e. the callee's RefType, at this call-site (You may want 
--      to use @freshTyInst@)
--   2. Use @consExpr@ to determine types for arguments @es@
--   3. Use @subTypes@ to add constraints between the types from (step 2) and (step 1)
--   4. Use the @F.subst@ returned in 3. to substitute formals with actuals in output type of callee.

consCall g l fn es ft0 
  = mseq (consScan consExpr g es) $ \(xes, g') -> do 
       -- Attempt to gather qualifiers here -- needed for object literal quals
       ts           <- mapM scrapeQualifiers [envFindTy x g' | x <- xes]
       δ            <- getDef
       case overload δ l of
         Just ft    -> do  (_,its,ot)   <- instantiate l g fn ft
                           let (su, ts') = renameBinds its xes
                           zipWithM_ (subType l g') ts ts'
                           Just <$> envAddFresh l (F.subst su ot) g'
         Nothing    -> cgError l $ errorNoMatchCallee (srcPos l) fn (toType <$> ts) (toType <$> getCallable δ ft0) 
    where
       overload δ l  = listToMaybe [ lt | Overload cx t <-  ann_fact l 
                                        , cge_ctx g     == cx
                                        , lt            <- getCallable δ ft0
                                        , toType t      == toType lt ]


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
-- consScan :: (CGEnv -> a -> CGM (b, CGEnv)) -> CGEnv -> [a] -> CGM ([b], CGEnv)
-- ---------------------------------------------------------------------------------
-- consScan step g xs  = go g [] xs 
--   where 
--     go g acc []     = return (reverse acc, g)
--     go g acc (x:xs) = do (y, g') <- step g x
--                          go g' (y:acc) xs

-----------------------------------------------------------------------------------
consScan :: (CGEnv -> a -> CGM (Maybe (b, CGEnv))) -> CGEnv -> [a] -> CGM (Maybe ([b], CGEnv))
-- ---------------------------------------------------------------------------------
consScan f g xs    = fmap (mapFst reverse) <$> consFold step ([], g) xs
  where
    step (ys, g) x = fmap (mapFst (:ys))   <$> f g x
    

---------------------------------------------------------------------------------
-- consFold  :: (CGEnv -> b -> CGM (Maybe CGEnv)) -> CGEnv -> [b] -> CGM (Maybe CGEnv) 
consFold :: (a -> b -> CGM (Maybe a)) -> a -> [b] -> CGM (Maybe a) 
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
  = do  (gI,tIs)            <- freshTyPhis (srcPos l) g xs $ toType <$> ts  -- (a) (b) 
        _                   <- consWhileBase l xs tIs g                     -- (c)
        Just (xc, gI')      <- consExpr gI cond                             -- (d)
        z                   <- consStmt (envAddGuard xc True gI') body      -- (e)
        whenJustM z          $ consWhileStep l xs tIs                       -- (f) 
        return               $ envAddGuard xc False gI'
    where
        xs                   = concat [xs | PhiVar xs <- ann_fact l]
        ts                   = (\t -> envFindTy t g) <$> xs 

consWhileBase l xs tIs g    = zipWithM_ (subType l g) xts_base tIs      -- (c)
  where 
   xts_base                 = (\t -> envFindTy t g) <$> xs
 
consWhileStep l xs tIs gI'' = zipWithM_ (subType l gI'') xts_step tIs'  -- (f)
  where 
    xts_step                = (\t -> envFindTy t gI'') <$> xs'
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
  = do  g1' <- envAdds False (zip xs t1s) g1 
        g2' <- envAdds False (zip xs t2s) g2
        -- t1s and t2s should have the same raw type, otherwise they wouldn't
        -- pass TC (we don't need to pad / fix them before joining).
        -- So we can use the raw type from one of the two branches and freshen
        -- up that one.
        -- FIXME: Add a raw type check on t1 and t2
        (g',ts) <- freshTyPhis (srcPos l) g xs $ toType <$> t1s
        zipWithM_ (subType l g1') [envFindTy x g1' | x <- xs] ts
        zipWithM_ (subType l g2') [envFindTy x g2' | x <- xs] ts
        return g'
    where
        xs   = concat [xs | PhiVar xs <- ann_fact l] 
        t1s  = (\t -> envFindTy t g1) <$> xs 
        t2s  = (\t -> envFindTy t g2) <$> xs



-- Local Variables:
-- flycheck-disabled-checkers: (haskell-liquid)
-- End:
