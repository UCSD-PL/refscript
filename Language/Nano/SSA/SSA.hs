
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.Nano.SSA.SSA (ssaTransform) where 

import           Control.Applicative                ((<$>), (<*>))
import           Control.Monad                
import qualified Data.HashMap.Strict as M 
import           Data.Maybe                         (fromJust)
import           Language.Nano.Types
import           Language.Nano.Errors
import           Language.Nano.Env
import           Language.Nano.Misc
-- import           Language.Nano.Typecheck.Subst
import           Language.Nano.Typecheck.Types
import           Language.Nano.SSA.SSAMonad
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations
-- import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Parser        (SourceSpan (..))
import           Language.Fixpoint.Misc             
import qualified Language.Fixpoint.Types            as F
import           Text.Printf                        (printf)
-- import           Debug.Trace                        hiding (traceShow)

----------------------------------------------------------------------------------
ssaTransform :: (F.Reftable r) => Nano SourceSpan (RType r) -> Nano (Annot (Fact_ r) SourceSpan) (RType r)
----------------------------------------------------------------------------------
ssaTransform = either (errorstar . snd) id . execute . ssaNano 


----------------------------------------------------------------------------------
ssaNano :: F.Reftable r => Nano SourceSpan t -> SSAM r (Nano (Annot (Fact_ r) SourceSpan) t)
----------------------------------------------------------------------------------
ssaNano p@(Nano {code = Src fs}) = do 
    addImmutables $ envMap (\_ -> F.top) (specs p) 
    addImmutables $ envMap (\_ -> F.top) (defs  p) 
    addImmutables $ envMap (\_ -> F.top) (consts p) 
    (_,fs') <- ssaStmts fs -- mapM ssaFun fs
    anns    <- getAnns
    return   $ p {code = Src $ (patchAnn anns <$>) <$> fs'}

-- stripAnn :: AnnBare -> SSAM SourceSpan
-- stripAnn (Ann l fs) = forM_ fs (addAnn l) >> return l   

-- patchAnn     :: AnnInfo -> SourceSpan -> (AnnSSA_ r)
patchAnn m l = Ann l $ M.lookupDefault [] l m

-------------------------------------------------------------------------------------
ssaFun :: F.Reftable r => FunctionStatement SourceSpan -> SSAM r (FunctionStatement SourceSpan)
-------------------------------------------------------------------------------------
ssaFun (FunctionStmt l f xs body) = do 
    θ            <- getSsaEnv  
    imms         <- getImmutables

    addImmutables $ envMap (\_ -> F.top) θ              -- Variables from OUTER scope are IMMUTABLE
    setSsaEnv     $ extSsaEnv ((returnId l) : xs) θ  -- Extend SsaEnv with formal binders
    (_, body')   <- ssaStmts body                    -- Transform function

    setSsaEnv θ                                      -- Restore Outer SsaEnv
    setImmutables imms                               -- Restore Outer Immutables

    return        $ FunctionStmt l f xs body'

ssaFun _ = error "Calling ssaFun not with FunctionStmt"

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
ssaStmts :: F.Reftable r => [Statement SourceSpan] -> SSAM r (Bool, [Statement SourceSpan])
-------------------------------------------------------------------------------------
ssaStmts ss = mapSnd flattenBlock <$> ssaSeq ssaStmt ss

-------------------------------------------------------------------------------------
ssaStmt :: F.Reftable r => Statement SourceSpan -> SSAM r (Bool, Statement SourceSpan)
-------------------------------------------------------------------------------------
-- skip
ssaStmt s@(EmptyStmt _) 
  = return (True, s)

-- x = e
ssaStmt (ExprStmt l1 (AssignExpr l2 OpAssign (LVar l3 x) e)) = do 
    (x', e') <- ssaAsgn l2 (Id l3 x) e
    return (True, VarDeclStmt l1 [VarDecl l2 x' (Just e')])

-- e1.x = e2
ssaStmt (ExprStmt l1 (AssignExpr l2 OpAssign (LDot l3 e3 x) e2)) = do 
    e2' <- ssaExpr e2
    e3' <- ssaExpr e3
    return (True, ExprStmt l1 (AssignExpr l2 OpAssign (LDot l3 e3' x) e2'))
     
-- e1[i] = e2
ssaStmt (ExprStmt l1 (AssignExpr l2 OpAssign 
          (LBracket l3 e4@(VarRef _ _) (IntLit l5 i5)) e2)) = do  
    e2'   <- ssaExpr e2
    e4'   <- ssaExpr e4
    return $ (True, ExprStmt l1 (AssignExpr l2 OpAssign 
              (LBracket l3 e4' (IntLit l5 i5)) e2'))

-- x[i] = e ==> var x_SSA_1    = x;
--                  x_SSA_1[i] = e;
--              var x_SSA_3    = x_SSA_1;
--
--ssaStmt (ExprStmt l1 (AssignExpr l2 OpAssign (LBracket l3 e4@(VarRef l4 x4) (IntLit l5 i5)) e2))
--  = do e2'     <- ssaExpr e2
--       (_, vd@(VarDecl _ x4' _)) <- ssaVarDecl (VarDecl l4 x4 (Just e4))
--       let s1   = VarDeclStmt l1 [vd]
--       let s2   = ExprStmt l1 (AssignExpr l2 OpAssign 
--                      (LBracket l3 (VarRef l4 x4') (IntLit l5 i5)) e2')
--       return (True, BlockStmt l1 [s1, s2])


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
ssaStmt (WhileStmt l c b) = do  
    g0         <- getSsaEnv
    φ          <- getLoopPhis g0 b
    let φ0      = [x | (SI x, _) <- (snd <$> φ)]
    φ1         <- mapM (updSsaEnv l) (fst <$> φ)
    g1         <- getSsaEnv
    c'         <- ssaExpr c 
    (t, b')    <- ssaStmt b
    g2         <- getSsaEnv
    let φ2      = [x | Just (SI x) <- (`envFindTy` g2) <$> (fst <$> φ)]
    let f (x, (SI x0, SI x2)) x1 = (x0,x1,x2)
    addAnn l (LoopPhiVar $ zip3 φ0 φ1 φ2)
    setSsaEnv g1
    return (t, WhileStmt l c' b')
     

ssaStmt (ForStmt _  NoInit _ _ _ )     = 
    errorstar "unimplemented: ssaStmt-for-01"
ssaStmt (ForStmt _ NoInit _ Nothing _ ) =
    errorstar "unimplemented: ssaStmt-for-02"

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
    expand (AssignExpr l1 o lv e) = AssignExpr l1 OpAssign lv (inf o l1 lv e)
    expand _ = errorstar "unimplemented: expand assignExpr"

    inf OpAssign         _ _  = id
    inf OpAssignAdd      l lv = InfixExpr l OpAdd      (lvalToExp lv)
    inf OpAssignSub      l lv = InfixExpr l OpSub      (lvalToExp lv)
    inf OpAssignMul      l lv = InfixExpr l OpMul      (lvalToExp lv)
    inf OpAssignDiv      l lv = InfixExpr l OpDiv      (lvalToExp lv)
    inf OpAssignMod      l lv = InfixExpr l OpMod      (lvalToExp lv)
    inf OpAssignLShift   l lv = InfixExpr l OpLShift   (lvalToExp lv)
    inf OpAssignSpRShift l lv = InfixExpr l OpSpRShift (lvalToExp lv)
    inf OpAssignZfRShift l lv = InfixExpr l OpZfRShift (lvalToExp lv)
    inf OpAssignBAnd     l lv = InfixExpr l OpBAnd     (lvalToExp lv)
    inf OpAssignBXor     l lv = InfixExpr l OpBXor     (lvalToExp lv)
    inf OpAssignBOr      l lv = InfixExpr l OpBOr      (lvalToExp lv)

    lvalToExp (LVar l s)         = VarRef l (Id l s)
    lvalToExp (LDot l e s)       = DotRef l e (Id l s)
    lvalToExp (LBracket l e1 e2) = BracketRef l e1 e2

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

-- function f(...){ s }
ssaStmt s@(FunctionStmt _ _ _ _)
  = (True,) <$> ssaFun s

-- OTHER (Not handled)
ssaStmt s 
  = convertError "ssaStmt" s

-------------------------------------------------------------------------------------
splice :: Statement SourceSpan -> Maybe (Statement SourceSpan) -> Statement SourceSpan
-------------------------------------------------------------------------------------
splice s Nothing   = s
splice s (Just s') = seqStmt (getAnnotation s) s s' 

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

ssaExpr   (ArrayLit l es)
  = ArrayLit l <$> (mapM ssaExpr es)

ssaExpr e@(VarRef l x) = do 
    imm <- isImmutable x
    xo  <- findSsaEnv x
    case xo of
      Just z  -> return $ VarRef l z
      Nothing -> if imm 
                  then return e 
                  else ssaError (srcPos x) $ errorUnboundId x 

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

ssaExpr e 
  = convertError "ssaExpr" e

-------------------------------------------------------------------------------------
ssaVarDecl :: F.Reftable r => VarDecl SourceSpan -> SSAM r (Bool, VarDecl SourceSpan)
-------------------------------------------------------------------------------------
ssaVarDecl (VarDecl l x (Just e)) = do 
    (x', e') <- ssaAsgn l x e
    return    (True, VarDecl l x' (Just e'))

ssaVarDecl {-z@-}(VarDecl l x Nothing)  
  = errorstar $ printf "Cannot handle ssaVarDECL %s at %s" (ppshow x) (ppshow l)

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
  where 
    mkPhiAsgn l x y = VarDeclStmt l [VarDecl l x (Just $ VarRef l y)]


-- Get the phi vars starting from an SSAEnv @θ@ and going through the 
-- statement @b@.
getLoopPhis θ b = do
    θ'    <- names <$> snd <$> tryAction (ssaStmt b) 
    return $ envToList (envLefts $ envIntersectWith meet θ θ')
  where
    meet x1 x2 = if x1 == x2 then Right x1 else Left (x1, x2)


ssaForLoop l vds cOpt incExp b = 
  do
    (b, sts') <- ssaStmts sts
    return     $ (b, BlockStmt l sts')
  where
    sts        = [VarDeclStmt l vds, WhileStmt l c bd] 
    bd         = BlockStmt bl [b, incExp]
    bl         = getAnnotation b
    c          = maybe (BoolLit l True) id cOpt
    il         = getAnnotation

