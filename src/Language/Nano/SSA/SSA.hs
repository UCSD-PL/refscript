{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable   #-}

module Language.Nano.SSA.SSA (ssaTransform, ssaTransform') where 

import           Control.Applicative                ((<$>), (<*>))
import           Control.Monad                
import qualified Data.Foldable                    as     FO
import qualified Data.HashMap.Strict as M 
import           Data.Typeable
import           Data.Data
import           Data.Maybe                         (isJust)
import           Language.Nano.Types
import           Language.Nano.Errors
import           Language.Nano.Env
import           Language.Nano.Misc
import           Language.Nano.Typecheck.Types
import           Language.Nano.SSA.SSAMonad
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Syntax.Annotations
import           Language.Fixpoint.Misc             
import qualified Language.Fixpoint.Errors           as E
import qualified Language.Fixpoint.Types            as F
-- import           Debug.Trace                        hiding (traceShow)

ssaTransform' = execute . ssaNano

----------------------------------------------------------------------------------
ssaTransform :: (PP r, F.Reftable r, Data r, Typeable r) => 
  NanoBareR r -> IO (Either E.Error (NanoSSAR r))
----------------------------------------------------------------------------------
ssaTransform = return . ssaTransform' 


-- | `ssaNano` Perfroms SSA transformation of the input program. The output
--   program is patched (annotated per AST) with information about:
--   * SSA-phi nodes
--   * Spec annotations (functions, global variable declarations)
--   * Type annotations (variable declarations (?), class elements)
----------------------------------------------------------------------------------
ssaNano :: (PP r, F.Reftable r, Data r, Typeable r) => NanoBareR r -> SSAM r (NanoSSAR r)
----------------------------------------------------------------------------------
ssaNano p@(Nano { code = Src fs }) 
  = do θ         <- getSsaEnv 
       -- All program globals (Variables with annotations)
       setGlobs   $ tracePP "definedGlobs" $ fmap srcPos <$> definedGlobs fs

       -- FIXME: Classes? Need to isolate the ones in scope
       setSsaEnv  $ extSsaEnv classes θ

       withAssignability ReadOnly ros
                  $ do  (_,fs') <- ssaStmts (map (ann <$>) fs)
                        ssaAnns <- getAnns
                        return   $ p {code = Src $ map (fmap (patch [ssaAnns, typeAnns])) fs' }
    where
      typeAnns    = M.fromList $ concatMap 
                        (FO.concatMap (\(Ann l an) -> (l,) <$> single <$> an)) fs

      ros         = readOnlyVars p

      patch ms l  = Ann l $ concatMap (M.lookupDefault [] l) ms
      classes     = [ fmap ann i | ClassStmt _ i _ _ _ <- fs]

-------------------------------------------------------------------------------------
ssaFun :: F.Reftable r => SourceSpan -> t -> [Var] 
                       -> [Statement SourceSpan] -> SSAM r [Statement SourceSpan]
-------------------------------------------------------------------------------------
ssaFun l fo xs body 
  = do θ <- getSsaEnv  
       withAssignability ReadOnly (envIds θ) $               -- Variables from OUTER scope are UNASSIGNABLE
         do setSsaEnv     $ extSsaEnv ((returnId l) : xs) θ  -- Extend SsaEnv with formal binders
            (_, body')   <- ssaStmts body                    -- Transform function
            setSsaEnv θ                                      -- Restore Outer SsaEnv
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

-- | ssaStmts: Global variables are added in scope on a block basis
-------------------------------------------------------------------------------------
ssaStmts :: F.Reftable r => [Statement SourceSpan] -> SSAM r (Bool, [Statement SourceSpan])
-------------------------------------------------------------------------------------
ssaStmts ss
  = do  αs <- getGlobs
        withAssignability WriteGlobal αs $ mapSnd flattenBlock <$> ssaSeq ssaStmt ss

-------------------------------------------------------------------------------------
ssaStmt :: F.Reftable r => Statement SourceSpan -> SSAM r (Bool, Statement SourceSpan)
-------------------------------------------------------------------------------------
-- skip
ssaStmt s@(EmptyStmt _) 
  = return (True, s)

-- 
ssaStmt s@(FunctionDecl _ _ _) 
  = return (True, s)

-- 
ssaStmt s@(IfaceStmt _) 
  = return (True, s)

-- x = e
ssaStmt (ExprStmt l1 (AssignExpr l2 OpAssign (LVar l3 v) e)) = do 
    let x     = Id l3 v 
    (x', e') <- ssaAsgn l2 x e
    return    $ if x == x' then (True, ExprStmt l1 (AssignExpr l2 OpAssign (LVar l3 v) e'))
                           else (True, VarDeclStmt l1 [VarDecl l2 x' (Just e')])

-- e1.x = e2
ssaStmt (ExprStmt l1 (AssignExpr l2 OpAssign (LDot l3 e3 x) e2)) = do 
    e2' <- ssaExpr e2
    e3' <- ssaExpr e3
    return (True, ExprStmt l1 (AssignExpr l2 OpAssign (LDot l3 e3' x) e2'))

-- e1[e2] = e3
ssaStmt (ExprStmt z1 (AssignExpr l OpAssign (LBracket z2 e1 e2) e3))
  = do [e1', e2', e3'] <- mapM ssaExpr [e1, e2, e3]
       return (True, ExprStmt z1 (AssignExpr l OpAssign (LBracket z2 e1' e2') e3'))

-- e
ssaStmt (ExprStmt l e) = do 
    e' <- ssaExpr e
    return (True, ExprStmt l e')

-- s1;s2;...;sn
ssaStmt (BlockStmt l stmts) = do 
    (b, stmts') <- ssaStmts stmts
    return (b,  BlockStmt l $ flattenBlock stmts')

-- if b { s1 }
ssaStmt (IfSingleStmt l b s)
  = ssaStmt (IfStmt l b s (EmptyStmt l))

-- if b { s1 } else { s2 }
ssaStmt (IfStmt l e s1 s2) = do 
    e'           <- ssaExpr e
    θ            <- getSsaEnv
    (θ1, s1')    <- ssaWith θ ssaStmt s1
    (θ2, s2')    <- ssaWith θ ssaStmt s2
    (θ', φ1, φ2) <- envJoin l θ1 θ2
    let stmt'     = IfStmt l e' (splice s1' φ1) (splice s2' φ2)
    case θ' of
      Just θ''   -> setSsaEnv θ'' >> return (True,  stmt')
      Nothing    ->                  return (False, stmt')
  {- where 
    dbg (Just θ) = trace ("SSA ENV: " ++ ppshow θ) (Just θ) -}

-- while c { b }
ssaStmt (WhileStmt l cond body) 
  = do (xs, x0s)  <- unzip . map (\(x, (SI xo,_)) -> (x, xo)) <$> getLoopPhis body
       x1s        <- mapM (updSsaEnv l) xs
       θ1         <- getSsaEnv
       cond'      <- ssaExpr cond 
       (t, body') <- ssaStmt body
       θ2         <- getSsaEnv
       let x2s     = [x2 | Just (SI x2) <- (`envFindTy` θ2) <$> xs]
       addAnn l    $ PhiVar x1s
       setSsaEnv θ1
       return      $ (t, (asgn x1s x0s) `presplice` (WhileStmt l cond' (body' `splice` (asgn (mkNextId <$> x1s) x2s))))
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
    (_, ds') <- ssaSeq ssaVarDecl ds
    return (True, VarDeclStmt l ds')

-- return e 
ssaStmt s@(ReturnStmt _ Nothing) 
  = return (False, s)

-- return e 
ssaStmt (ReturnStmt l (Just e)) = do 
    e' <- ssaExpr e
    return (False, ReturnStmt l (Just e'))

-- throw e 
ssaStmt (ThrowStmt l e) = do 
    e' <- ssaExpr e
    return (False, ThrowStmt l e')


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
splice :: Statement SourceSpan -> Maybe (Statement SourceSpan) -> Statement SourceSpan
-------------------------------------------------------------------------------------
splice s z = splice_ (getAnnotation s) (Just s) z 

splice_ l Nothing Nothing    = EmptyStmt l
splice_ _ (Just s) Nothing   = s
splice_ _ Nothing (Just s)   = s
splice_ _ (Just s) (Just s') = seqStmt (getAnnotation s) s s'


seqStmt _ (BlockStmt l s) (BlockStmt _ s') = BlockStmt l (s ++ s')
seqStmt l s s'                             = BlockStmt l [s, s']

-------------------------------------------------------------------------------------
flattenBlock :: [Statement t] -> [Statement t]
-------------------------------------------------------------------------------------
flattenBlock = concatMap f
  where
    f (BlockStmt _ ss) = ss
    f s                = [s ]

-------------------------------------------------------------------------------------
ssaWith :: SsaEnv -> (t -> SSAM r (Bool, t)) -> t -> SSAM r (Maybe SsaEnv, t)
-------------------------------------------------------------------------------------
ssaWith θ f x = do 
  setSsaEnv θ
  (b, x') <- f x
  (, x')  <$> (if b then Just <$> getSsaEnv else return Nothing)

-------------------------------------------------------------------------------------
ssaExpr    :: F.Reftable r => Expression SourceSpan -> SSAM r (Expression SourceSpan) 
-------------------------------------------------------------------------------------

ssaExpr e@(IntLit _ _)               
  = return e 

ssaExpr e@(BoolLit _ _)
  = return e 

ssaExpr e@(StringLit _ _)
  = return e 

ssaExpr e@(NullLit _)               
  = return e 

ssaExpr e@(ThisRef _)
  = return e 

ssaExpr e@(SuperRef _)
  = return e 

ssaExpr   (ArrayLit l es)
  = ArrayLit l <$> (mapM ssaExpr es)

ssaExpr e@(VarRef l x) 
  = do getAssignability x >>= \case
         WriteGlobal -> return e
         ReadOnly    -> maybe e   (VarRef l) <$> findSsaEnv x
         WriteLocal  -> findSsaEnv x >>= \case
             Just t  -> return   $ VarRef l t
             Nothing -> ssaError $ errorSSAUnboundId (srcPos x) x

ssaExpr (CondExpr l c e1 e2) 
  = CondExpr l <$> ssaExpr c <*> ssaExpr e1 <*> ssaExpr e2 

ssaExpr (PrefixExpr l o e)
  = PrefixExpr l o <$> ssaExpr e

ssaExpr (InfixExpr l o e1 e2)        
  = InfixExpr l o <$> ssaExpr e1 <*> ssaExpr e2

ssaExpr (CallExpr l e es)
  = CallExpr l <$> ssaExpr e <*> mapM ssaExpr es

ssaExpr (ObjectLit l ps) 
  = ObjectLit l <$> mapM (mapSndM ssaExpr) ps

ssaExpr (DotRef l e i) 
  = DotRef l <$> ssaExpr e <*> return i

ssaExpr (BracketRef l e1 e2) 
  = BracketRef l <$> ssaExpr e1 <*> ssaExpr e2

ssaExpr (NewExpr l e es)
  -- `e` is the class name - no need to SSA it.
  = NewExpr l e <$> mapM ssaExpr es

ssaExpr (FuncExpr l fo xs bd)
  = FuncExpr l fo xs <$> ssaFun l fo xs bd 

ssaExpr (Cast l e)
  = Cast l <$> ssaExpr e

ssaExpr e 
  = convertError "ssaExpr" e

-------------------------------------------------------------------------------------
ssaVarDecl :: F.Reftable r => VarDecl SourceSpan -> SSAM r (Bool, VarDecl SourceSpan)
-------------------------------------------------------------------------------------
ssaVarDecl (VarDecl l x (Just e)) = do 
    (x', e') <- ssaAsgn l x e
    return    (True, VarDecl l x' (Just e'))

ssaVarDecl (VarDecl l x Nothing) = do
    x' <- updSsaEnv l x
    return    (True, VarDecl l x' Nothing)
--  = errorstar $ printf "Variable definition of %s at %s with no initialization is not supported." (ppshow x) (ppshow l)

------------------------------------------------------------------------------------
ssaAsgn :: F.Reftable r => SourceSpan -> Id SourceSpan -> Expression SourceSpan -> 
           SSAM r (Id SourceSpan, Expression SourceSpan)
------------------------------------------------------------------------------------
ssaAsgn l x e  = do 
    e' <- ssaExpr e 
    x' <- updSsaEnv l x
    return (x', e')


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

