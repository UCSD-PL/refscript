{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}

module Language.Nano.SSA.SSA (ssaTransform) where

import           Control.Arrow                           ((***))
import           Control.Applicative                     ((<$>), (<*>))
import           Control.Monad
import           Data.Data
import qualified Data.List                               as L
import qualified Data.Foldable                           as FO
import qualified Data.HashMap.Strict                     as M
import qualified Data.HashSet as S
import           Data.Typeable                           ()
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations
import qualified Language.Fixpoint.Errors                as E
import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Types                 as F
import           Language.Nano.Env
import           Language.Nano.Errors
-- import           Language.Nano.Misc
import           Language.Nano.SSA.Types
import           Language.Nano.SSA.SSAMonad
import           Language.Nano.Typecheck.Types
import           Language.Nano.Types


----------------------------------------------------------------------------------
ssaTransform :: (PP r, F.Reftable r, Data r, Typeable r) => 
  NanoBareR r -> IO (Either E.Error (NanoSSAR r))
----------------------------------------------------------------------------------
ssaTransform = return . execute . ssaNano

-- | `ssaNano` Perfroms SSA transformation of the input program. The output
-- program is patched (annotated per AST) with information about:
-- * SSA-phi nodes
-- * Spec annotations (functions, global variable declarations)
-- * Type annotations (variable declarations (?), class elements)
----------------------------------------------------------------------------------
ssaNano :: Data r => NanoBareR r -> SSAM r (NanoSSAR r)
----------------------------------------------------------------------------------
ssaNano p@(Nano { code = Src fs })
  = do -- θ      <- getSsaEnv
       -- setGlobs $ M.fromList $ (\(l,i,_) -> (srcPos l,fmap srcPos i)) <$> definedGlobs fs
       -- setSsaEnv $ extSsaEnv classes θ   -- Taken care of in initGlobalEnv

       
       setGlobs $ allGlobs
       setMeas  $ S.fromList $ F.symbol <$> envIds (consts p)


       withAssignability ReadOnly ros $ 
         withAssignability WriteLocal wls $ 
           withAssignability WriteGlobal wgs $ do 
              (_,fs')  <- ssaStmts $ msrc fs
              ssaAnns  <- getAnns
              return    $ p {code = Src $ map (fmap (patch [ssaAnns, typeAnns])) fs' }
    where
    -- Initializers
      allGlobs        = S.fromList  $ getAnnotation  <$> fmap ann <$> writeGlobalVars fs
      (ros, wgs, wls) = variablesInScope allGlobs fs
      msrc            = (fmap srcPos <$>)

      typeAnns        = M.fromList $ concatMap
                        (FO.concatMap (\(Ann l an) -> (l,) <$> single <$> an)) fs

      patch ms l      = Ann l $ concatMap (M.lookupDefault [] l) ms


---------------------------------------------------------------------------------------
variablesInScope :: (Data a, IsLocated a) => S.HashSet SourceSpan -> [Statement a] 
                 -> ([Id SourceSpan], [Id SourceSpan], [Id SourceSpan])
---------------------------------------------------------------------------------------
variablesInScope gs fs = (ros, wgs, wls)
  where
    vs            = hoistVarDecls fs
    (wgs, wls)    = mapPair msrc $ L.partition (\s -> srcPos s `S.member` gs) vs 
    ros           = msrc $ hoistReadOnly fs
    msrc          = (fmap srcPos <$>)


-- ---------------------------------------------------------------------------------------
-- initGlobalEnv :: Data r => NanoBareR r -> SSAEnv
-- ---------------------------------------------------------------------------------------
-- initGlobalEnv p@(Nano { code = Src fs }) = SE allGlobs meas env mod nspace parent
--   where
--     allGlobs    = S.fromList  $ getAnnotation  <$> fmap ann <$> writeGlobalVars fs
--     meas        = S.fromList  $ F.symbol       <$> envIds (consts p)
--     env         = envUnionList [ros, wls, wgs]
--     mod         = envEmpty 
--     nspace      = []
--     parent      = Nothing
-- 
--     vs          = mapFst ann <$> hoistVarDecls fs
--     (_wgs,_wls) = L.partition (\(s,_) -> s `S.member` allGlobs) vs 
--     ros         = envFromList $ (,ReadOnly)    . snd <$> hoistReadOnly fs
--     wls         = envFromList $ (,WriteLocal)  . snd <$> _wls
--     wgs         = envFromList $ (,WriteGlobal) . snd <$> _wgs


-------------------------------------------------------------------------------------
ssaFun :: SourceSpan -> t -> [Var] -> [Statement SourceSpan] -> SSAM r [Statement SourceSpan]
-------------------------------------------------------------------------------------
ssaFun l _ xs body
  = do  θ <- getSsaEnv
        (ros, wgs, wls) <- (`variablesInScope` body) <$> getGlobs
        withAssignability ReadOnly (envIds θ) $                   -- Variables from OUTER scope are UNASSIGNABLE
          withAssignability ReadOnly ros $ 
            withAssignability WriteGlobal wgs $ 
              withAssignability WriteLocal wls $ 

            do  setSsaEnv    $ extSsaEnv ((returnId l) : xs) θ    -- Extend SsaEnv with formal binders
                (_, body')   <- ssaStmts body                     -- Transform function
                setSsaEnv θ                                       -- Restore Outer SsaEnv
                return        $ body'


-------------------------------------------------------------------------------------
ssaSeq :: (a -> SSAM r (Bool, a)) -> [a] -> SSAM r (Bool, [a])
-------------------------------------------------------------------------------------
ssaSeq f            = go True
  where
    go False zs     = return (False, zs)
    go b     []     = return (b    , [])
    go True  (x:xs) = do (b , y)  <- f x
                         (b', ys) <- go b xs
                         return      (b', y:ys)


-------------------------------------------------------------------------------------
ssaStmts :: [Statement SourceSpan] -> SSAM r (Bool, [Statement SourceSpan])
-------------------------------------------------------------------------------------------
ssaStmts ss = mapSnd flattenBlock <$> ssaSeq ssaStmt ss


-----------------------------------------------------------------------------------
ssaStmt ::  Statement SourceSpan -> SSAM r (Bool, Statement SourceSpan)
-------------------------------------------------------------------------------------
-- skip
ssaStmt s@(EmptyStmt _) 
  = return (True, s)

-- function foo(...): T;
ssaStmt s@(FunctionDecl _ _ _) 
  = return (True, s)

-- interface IA<V> extends IB<T> { ... }
ssaStmt s@(IfaceStmt _) 
  = return (True, s)

-- x = e
ssaStmt (ExprStmt l1 (AssignExpr l2 OpAssign (LVar l3 v) e)) = do
    let x        = Id l3 v
    (s, x', e') <- ssaAsgn l2 x e
    return         (True, prefixStmt l1 s $ ssaAsgnStmt l1 l2 x x' e')

-- e
ssaStmt (ExprStmt l e) = do
    (s, e') <- ssaExpr e
    return (True, prefixStmt l s $ ExprStmt l e')

-- s1;s2;...;sn
ssaStmt (BlockStmt l stmts) = do
    (b, stmts') <- ssaStmts stmts
    return (b,  BlockStmt l $ flattenBlock stmts')

-- if b { s1 }
ssaStmt (IfSingleStmt l b s)
  = ssaStmt (IfStmt l b s (EmptyStmt l))

-- if b { s1 } else { s2 }
ssaStmt (IfStmt l e s1 s2) = do
    (se, e')     <- ssaExpr e
    θ            <- getSsaEnv
    (θ1, s1')    <- ssaWith θ ssaStmt s1
    (θ2, s2')    <- ssaWith θ ssaStmt s2
    (θ', φ1, φ2) <- envJoin l θ1 θ2
    let stmt'     = prefixStmt l se $ IfStmt l e' (splice s1' φ1) (splice s2' φ2)
    case θ' of
      Just θ''   -> setSsaEnv θ'' >> return (True,  stmt')
      Nothing    ->                  return (False, stmt')
  {- where
    dbg (Just θ) = trace ("SSA ENV: " ++ ppshow θ) (Just θ) -}

-- while c { b }
ssaStmt (WhileStmt l cnd body)
  = do (xs, x0s)  <- unzip . map (\(x, (SI xo,_)) -> (x, xo)) <$> getLoopPhis body
       x1s        <- mapM (updSsaEnv l) xs
       θ1         <- getSsaEnv
       (sc, cnd') <- ssaExpr cnd
       when (not $ null sc) (ssaError $ errorUpdateInExpr l cnd)
       (t, body') <- ssaStmt body
       θ2         <- getSsaEnv
       let x2s     = [x2 | Just (SI x2) <- (`envFindTy` θ2) <$> xs]
       addAnn l    $ PhiVar x1s
       setSsaEnv θ1
       return      $ (t, (asgn x1s x0s) `presplice` (WhileStmt l cnd' (body' `splice` (asgn (mkNextId <$> x1s) x2s))))
    where
       asgn [] _   = Nothing
       asgn ls rs  = Just $ BlockStmt l $ zipWith (mkPhiAsgn l) ls rs

ssaStmt (ForStmt _  NoInit _ _ _ )     =
    errorstar "unimplemented: ssaStmt-for-01"

ssaStmt (ForStmt l v cOpt (Just (UnaryAssignExpr l1 o lv)) b) =
    ssaStmt (ForStmt l v cOpt (Just $ AssignExpr l1 (op o) lv (IntLit l1 1)) b)
  where
    op PrefixInc   = OpAssignAdd
    op PrefixDec   = OpAssignSub
    -- TODO: Should we worry for prefix or postfix?
    op PostfixInc  = OpAssignAdd
    op PostfixDec  = OpAssignSub

ssaStmt (ForStmt l (VarInit vds) cOpt (Just e@(AssignExpr l1 _ _ _)) b) =
    ssaForLoop l vds cOpt (ExprStmt l1 (expand e)) b
  where
    expand (AssignExpr l1 o lv e) = AssignExpr l1 OpAssign lv (infOp o l1 lv e)
    expand _ = errorstar "unimplemented: expand assignExpr"

ssaStmt (ForStmt l (VarInit vds) cOpt (Just i) b) =
    ssaForLoop l vds cOpt (ExprStmt (getAnnotation i) i) b


-- var x1 [ = e1 ]; ... ; var xn [= en];
ssaStmt (VarDeclStmt l ds) = do
    stvds' <- mapM ssaVarDecl ds
    return    (True, mkStmt $ foldr crunch ([], []) stvds') 
  where
    crunch ([], d) (ds, ss') = (d:ds, ss')
    crunch (ss, d) (ds, ss') = ([]  , mkStmts l ss (d:ds) ss')
    mkStmts l ss ds ss'      = ss ++ VarDeclStmt l ds : ss'
    mkStmt (ds', [] )        = VarDeclStmt l ds'
    mkStmt (ds', ss')        = BlockStmt l $ mkStmts l [] ds' ss'

-- return e
ssaStmt s@(ReturnStmt _ Nothing)
  = return (False, s)

-- return e
ssaStmt (ReturnStmt l (Just e)) = do
    (s, e') <- ssaExpr e
    return (False, prefixStmt l s $ ReturnStmt l (Just e'))

-- throw e
ssaStmt (ThrowStmt l e) = do
    (s, e') <- ssaExpr e
    return (False, prefixStmt l s $ ThrowStmt l e')


-- function f(...){ s }
ssaStmt (FunctionStmt l f xs bd)
  = (True,) <$> FunctionStmt l f xs <$> (ssaFun l (Just f) xs bd)

-- switch (e) { ... }
ssaStmt (SwitchStmt l e xs)
  = do
      id <- updSsaEnv (an e) (Id (an e) "__switchVar")
      let go (l, e, s) i = IfStmt (an s) (InfixExpr l OpStrictEq (VarRef l id) e) s i
      mapSnd (BlockStmt l) <$> ssaStmts
        [ VarDeclStmt (an e) [VarDecl (an e) id (Just e)], foldr go z sss ]
  where
      an                   = getAnnotation
      sss                  = [ (l, e, BlockStmt l $ remBr ss) | CaseClause l e ss <- xs ]
      z                    = headWithDefault (EmptyStmt l) [BlockStmt l $ remBr ss | CaseDefault l ss <- xs]

      remBr                = filter (not . isBr) . flattenBlock
      isBr (BreakStmt _ _) = True
      isBr _               = False
      headWithDefault a [] = a
      headWithDefault _ xs = head xs

-- class A extends B implements I,J,... { ... }
ssaStmt (ClassStmt l n e is bd) = do
  bd' <- mapM ssaClassElt bd
  return (True, ClassStmt l n e is bd')

-- OTHER (Not handled)
ssaStmt s
  = convertError "ssaStmt" s

ssaAsgnStmt l1 l2 x@(Id l3 v) x' e' 
  | x == x'   = ExprStmt l1    (AssignExpr l2 OpAssign (LVar l3 v) e')
  | otherwise = VarDeclStmt l1 [VarDecl l2 x' (Just e')]

ssaClassElt (Constructor l xs body)
  = do θ <- getSsaEnv
       withAssignability ReadOnly (envIds θ) $               -- Variables from OUTER scope are NON-ASSIGNABLE
         do setSsaEnv     $ extSsaEnv xs θ                   -- Extend SsaEnv with formal binders
            (_, body')   <- ssaStmts body                    -- Transform function
            setSsaEnv θ                                      -- Restore Outer SsaEnv
            return        $ Constructor l xs body'

-- Class fields are considered non-assignable
ssaClassElt v@(MemberVarDecl _ _ _ )    = return v

ssaClassElt (MemberMethDecl l s e xs body)
  = do θ <- getSsaEnv
       withAssignability ReadOnly (envIds θ) $               -- Variables from OUTER scope are NON-ASSIGNABLE
         do setSsaEnv     $ extSsaEnv ((returnId l) : xs) θ  -- Extend SsaEnv with formal binders
            (_, body')   <- ssaStmts body                    -- Transform function
            setSsaEnv θ                                      -- Restore Outer SsaEnv
            return        $ MemberMethDecl l s e xs body'

infOp OpAssign         _ _  = id
infOp OpAssignAdd      l lv = InfixExpr l OpAdd      (lvalExp lv)
infOp OpAssignSub      l lv = InfixExpr l OpSub      (lvalExp lv)
infOp OpAssignMul      l lv = InfixExpr l OpMul      (lvalExp lv)
infOp OpAssignDiv      l lv = InfixExpr l OpDiv      (lvalExp lv)
infOp OpAssignMod      l lv = InfixExpr l OpMod      (lvalExp lv)
infOp OpAssignLShift   l lv = InfixExpr l OpLShift   (lvalExp lv)
infOp OpAssignSpRShift l lv = InfixExpr l OpSpRShift (lvalExp lv)
infOp OpAssignZfRShift l lv = InfixExpr l OpZfRShift (lvalExp lv)
infOp OpAssignBAnd     l lv = InfixExpr l OpBAnd     (lvalExp lv)
infOp OpAssignBXor     l lv = InfixExpr l OpBXor     (lvalExp lv)
infOp OpAssignBOr      l lv = InfixExpr l OpBOr      (lvalExp lv)

lvalExp (LVar l s)          = VarRef l (Id l s)
lvalExp (LDot l e s)        = DotRef l e (Id l s)
lvalExp (LBracket l e1 e2)  = BracketRef l e1 e2

-------------------------------------------------------------------------------------
presplice :: Maybe (Statement SourceSpan) -> Statement SourceSpan -> Statement SourceSpan
-------------------------------------------------------------------------------------
presplice z s' = splice_ (getAnnotation s') z (Just s')

-------------------------------------------------------------------------------------
splice :: Statement a -> Maybe (Statement a) -> Statement a 
-------------------------------------------------------------------------------------
splice s z = splice_ (getAnnotation s) (Just s) z

splice_ l Nothing Nothing    = EmptyStmt l
splice_ _ (Just s) Nothing   = s
splice_ _ Nothing (Just s)   = s
splice_ _ (Just s) (Just s') = seqStmt (getAnnotation s) s s'


seqStmt _ (BlockStmt l s) (BlockStmt _ s') = BlockStmt l (s ++ s')
seqStmt l s s'                             = BlockStmt l [s, s']

-------------------------------------------------------------------------------------
prefixStmt :: a -> [Statement a] -> Statement a -> Statement a 
-------------------------------------------------------------------------------------
prefixStmt _ [] s = s 
prefixStmt l ss s = BlockStmt l $ flattenBlock $ ss ++ [s]

-------------------------------------------------------------------------------------
flattenBlock :: [Statement t] -> [Statement t]
-------------------------------------------------------------------------------------
flattenBlock = concatMap f
  where
    f (BlockStmt _ ss) = ss
    f s                = [s ]

-------------------------------------------------------------------------------------
ssaWith :: SsaEnv -> (a -> SSAM r (Bool, b)) -> a -> SSAM r (Maybe SsaEnv, b)
-------------------------------------------------------------------------------------
ssaWith θ f x = do
  setSsaEnv θ
  (b, x') <- f x
  (, x')  <$> (if b then Just <$> getSsaEnv else return Nothing)


-------------------------------------------------------------------------------------
ssaExpr    ::  Expression SourceSpan
           -> SSAM r ([Statement SourceSpan], Expression SourceSpan)
-------------------------------------------------------------------------------------

ssaExpr e@(IntLit _ _)
  = return ([], e)

ssaExpr e@(BoolLit _ _)
  = return ([], e)

ssaExpr e@(StringLit _ _)
  = return ([], e)

ssaExpr e@(NullLit _)
  = return ([], e)

ssaExpr e@(ThisRef _)
  = return ([], e)

ssaExpr e@(SuperRef _)
  = return ([], e)

ssaExpr   (ArrayLit l es)
  = ssaExprs (ArrayLit l) es

ssaExpr (VarRef l x)
  = ([],) <$> ssaVarRef l x

ssaExpr (CondExpr l c e1 e2)
  = do (sc, c') <- ssaExpr c
       θ        <- getSsaEnv
       e1'      <- ssaPureExprWith θ e1
       e2'      <- ssaPureExprWith θ e2
       return (sc, CondExpr l c' e1' e2')

        
ssaExpr (PrefixExpr l o e)
  = ssaExpr1 (PrefixExpr l o) e

ssaExpr (InfixExpr l o e1 e2)
  = ssaExpr2 (InfixExpr l o) e1 e2

ssaExpr (CallExpr l e es)
  = ssaExprs (\case es' -> CallExpr l (head es') (tail es')) (e : es)

ssaExpr (ObjectLit l ps)
  = ssaExprs (ObjectLit l . zip fs) es
  where
    (fs, es) = unzip ps

ssaExpr (DotRef l e i)
  = ssaExpr1 (\e' -> DotRef l e' i) e 

ssaExpr (BracketRef l e1 e2)
  = ssaExpr2 (BracketRef l) e1 e2

ssaExpr (NewExpr l e es)
  -- `e` is the class name - no need to SSA it.
  = ssaExprs (NewExpr l e) es

ssaExpr (Cast l e)
  = ssaExpr1 (Cast l) e

ssaExpr (FuncExpr l fo xs bd)
  = ([],) . FuncExpr l fo xs <$> ssaFun l fo xs bd

-- x = e
ssaExpr (AssignExpr l OpAssign (LVar lx v) e)
  = ssaAsgnExpr l lx (Id lx v) e
       
-- e1.f = e2
ssaExpr (AssignExpr l OpAssign (LDot ll e1 f) e2)
  = ssaExpr2 (\e1' e2' -> AssignExpr l OpAssign (LDot ll e1' f) e2') e1 e2

-- e1[e2] = e3
ssaExpr (AssignExpr l OpAssign (LBracket ll e1 e2) e3)
  = ssaExpr3 (\e1' e2' e3' -> AssignExpr l OpAssign (LBracket ll e1' e2') e3') e1 e2 e3

-- lv += e
ssaExpr (AssignExpr l op lv e)
  = ssaExpr (AssignExpr l OpAssign lv rhs)
  where
    rhs = InfixExpr l (assignInfix op) (lvalExp lv) e
        
-- x++
ssaExpr (UnaryAssignExpr l uop (LVar lv v))
  = do let x           = Id lv v
       xOld           <- ssaVarRef l x
       xNew           <- updSsaEnv l x
       let (eIn, eOut) = unaryExprs l uop xOld (VarRef l xNew)
       return  ([ssaAsgnStmt l lv x xNew eIn], eOut)

-- lv++
ssaExpr (UnaryAssignExpr l uop lv)
  = do lv'   <- ssaLval lv
       let e' = unaryExpr l uop (lvalExp lv')
       return ([], AssignExpr l OpAssign lv' e')
       
ssaExpr e
  = convertError "ssaExpr" e

ssaAsgnExpr l lx x e
  = do (s, x', e') <- ssaAsgn l x e
       return         (s ++ [ssaAsgnStmt l lx x x' e'], e')

-----------------------------------------------------------------------------
ssaLval    ::  LValue SourceSpan -> SSAM r (LValue SourceSpan)
-----------------------------------------------------------------------------
ssaLval (LVar lv v)
  = do VarRef _ (Id _ v') <- ssaVarRef lv (Id lv v)
       return (LVar lv v')

ssaLval (LDot ll e f)
  = (\e' -> LDot ll e' f) <$> ssaPureExpr e

ssaLval (LBracket ll e1 e2)
  = LBracket ll <$> ssaPureExpr e1 <*> ssaPureExpr e2
  

--------------------------------------------------------------------------
-- | Helpers for gathering assignments for purifying effectful-expressions
--------------------------------------------------------------------------
 
ssaExprs f es       = (concat *** f) . unzip <$> mapM ssaExpr es

ssaExpr1 = case1 ssaExprs -- (\case [e'] -> f e') [e]
ssaExpr2 = case2 ssaExprs -- (\case [e1', e2'] -> f e1' e2') [e1, e2]
ssaExpr3 = case3 ssaExprs -- (\case [e1', e2', e3'] -> f e1' e2' e3') [e1, e2, e3]
ssaPureExprWith θ e = snd <$> ssaWith θ (fmap (True,) . ssaPureExpr) e

ssaPureExpr e = do
  (s, e') <- ssaExpr e
  case s of
    []     -> return e'
    _      -> ssaError $ errorUpdateInExpr (getAnnotation e) e 

--------------------------------------------------------------------------
-- | Dealing with Generic Assignment Expressions
--------------------------------------------------------------------------
assignInfix :: AssignOp -> InfixOp
assignInfix OpAssignAdd      = OpAdd     
assignInfix OpAssignSub      = OpSub
assignInfix OpAssignMul      = OpMul   
assignInfix OpAssignDiv      = OpDiv   
assignInfix OpAssignMod      = OpMod   
assignInfix OpAssignLShift   = OpLShift
assignInfix OpAssignSpRShift = OpSpRShift
assignInfix OpAssignZfRShift = OpZfRShift
assignInfix OpAssignBAnd     = OpBAnd
assignInfix OpAssignBXor     = OpBXor
assignInfix OpAssignBOr      = OpBOr
assignInfix o                = error $ "assignInfix called with " ++ ppshow o


--------------------------------------------------------------------------
-- | Dealing with Unary Expressions
--------------------------------------------------------------------------
       
unaryExprs l u xOld xNew = (unaryExpr l u xOld, unaryId xOld xNew u)
    
unaryExpr l u e1         = InfixExpr l bop e1 (IntLit l 1) 
  where
    bop                  = unaryBinOp u

unaryBinOp PrefixInc     = OpAdd
unaryBinOp PostfixInc    = OpAdd
unaryBinOp _             = OpSub
unaryId _ x PrefixInc    = x
unaryId _ x PrefixDec    = x
unaryId x _ _            = x


-------------------------------------------------------------------------------------
ssaVarDecl :: VarDecl SourceSpan -> SSAM r ([Statement SourceSpan], VarDecl SourceSpan)
-------------------------------------------------------------------------------------
ssaVarDecl (VarDecl l x (Just e)) = do
    (s, x', e') <- ssaAsgn l x e
    return    (s, VarDecl l x' (Just e'))

ssaVarDecl (VarDecl l x Nothing) = do
    x' <- updSsaEnv l x
    return    ([], VarDecl l x' Nothing)

------------------------------------------------------------------------------------------
ssaVarRef ::  SourceSpan -> Id SourceSpan -> SSAM r (Expression SourceSpan)
------------------------------------------------------------------------------------------
ssaVarRef l x
  = do getAssignability x >>= \case
         WriteGlobal -> return e
         ReadOnly    -> maybe e  (VarRef l) <$> findSsaEnv x
         WriteLocal  -> findSsaEnv x >>= \case
             Just t  -> return   $ VarRef l t
             Nothing -> ssaError $ errorSSAUnboundId (srcPos x) x
    where
       e = VarRef l x
 

------------------------------------------------------------------------------------
ssaAsgn :: SourceSpan -> Id SourceSpan -> Expression SourceSpan ->
            SSAM r ([Statement SourceSpan], Id SourceSpan, Expression SourceSpan)
------------------------------------------------------------------------------------
ssaAsgn l x e  = do
    (s, e') <- ssaExpr e
    x'      <- updSsaEnv l x
    return (s, x', e')


-------------------------------------------------------------------------------------
envJoin :: SourceSpan -> Maybe SsaEnv -> Maybe SsaEnv
        -> SSAM r ( Maybe SsaEnv,
                    Maybe (Statement SourceSpan),
                    Maybe (Statement SourceSpan))
-------------------------------------------------------------------------------------
envJoin _ Nothing Nothing     = return (Nothing, Nothing, Nothing)
envJoin _ Nothing (Just θ)    = return (Just θ , Nothing, Nothing)
envJoin _ (Just θ) Nothing    = return (Just θ , Nothing, Nothing)
envJoin l (Just θ1) (Just θ2) = envJoin' l θ1 θ2

envJoin' l θ1 θ2 = do
    setSsaEnv θ'                          -- Keep Common binders
    stmts      <- forM phis $ phiAsgn l   -- Adds Phi-Binders, Phi Annots, Return Stmts
    θ''        <- getSsaEnv
    let (s1,s2) = unzip stmts
    return (Just θ'', Just $ BlockStmt l s1, Just $ BlockStmt l s2)
  where
    θ           = envIntersectWith meet θ1 θ2
    θ'          = envRights θ
    phis        = envToList $ envLefts θ
    meet        = \x1 x2 -> if x1 == x2 then Right x1 else Left (x1, x2)

phiAsgn l (x, (SI x1, SI x2)) = do
    x' <- updSsaEnv l x          -- Generate FRESH phi name
    addAnn l (PhiVar [x'])       -- RECORD x' as PHI-Var at l
    let s1 = mkPhiAsgn l x' x1   -- Create Phi-Assignments
    let s2 = mkPhiAsgn l x' x2   -- for both branches
    return $ (s1, s2)

mkPhiAsgn l x y = VarDeclStmt l [VarDecl l x (Just $ VarRef l y)]


-- Get the phi vars starting from an SSAEnv @θ@ and going through the statement @b@.
getLoopPhis b = do
    θ     <- getSsaEnv
    θ'    <- names <$> snd <$> tryAction (ssaStmt b)
    return $ envToList (envLefts $ envIntersectWith meet θ θ')
  where
    meet x x' = if x == x' then Right x else Left (x, x')


ssaForLoop l vds cOpt incExp b =
  do
    (b, sts') <- ssaStmts sts
    return     $ (b, BlockStmt l sts')
  where
    sts        = [VarDeclStmt l vds, WhileStmt l c bd]
    bd         = BlockStmt bl [b, incExp]
    bl         = getAnnotation b
    c          = maybe (BoolLit l True) id cOpt

-- Local Variables:
-- flycheck-disabled-checkers: (haskell-liquid)
-- End:
