module Language.Nano.Liquid.SSA (
  -- * SSA Transform
  ssaTransform
  ) where 

ssaTransform :: Nano -> NanoBare
ssaTransform pgm = execute $ ssaNano pgm

type Annm   = M.HashMap SourcePos Annot
type SsaEnv = Env (Id SourcePos) 

-------------------------------------------------------------------------------------
ssaNano :: NanoBare -> NanoBare
-------------------------------------------------------------------------------------
ssaNano p@(Nano {code = Src fs}) = p {code = Src $ ssaNano <$> fs}

-------------------------------------------------------------------------------------
ssaFun :: FunctionStmt SourcePos -> (FunctionStmt SourcePos)
-------------------------------------------------------------------------------------
ssaFun (FunctionStmt l f xs body) = FunctionStmt l f xs body'
  where 
    θ                             = initSsaEnv xs
    (_, body')                    = ssaStmts θ body

-------------------------------------------------------------------------------------
ssaSeq :: (SsaEnv -> a -> (Maybe SsaEnv, a)) -> SsaEnv -> [a] -> (Maybe SsaEnv, [a])  
-------------------------------------------------------------------------------------
ssaSeq f               = go True 
  where 
    go Nothing zs      = (Nothing, zs)
    go b    []         = (b,       [])
    go (Just θ) (x:xs) = let (b , y)  = f θ x
                             (b', ys) = go b xs
                         in  (b', y:ys)

-------------------------------------------------------------------------------------
ssaStmts   :: SsaEnv -> [Statement SourcePos] -> (Maybe SsaEnv, [Statement SourcePos])
-------------------------------------------------------------------------------------
ssaStmts = ssaSeq ssaStmt

-------------------------------------------------------------------------------------
ssaStmt    :: SsaEnv -> Statement SourcePos    -> (Maybe SsaEnv, Statement SourcePos)
-------------------------------------------------------------------------------------

-- skip
ssaStmt θ s@(EmptyStmt _) 
  = (Just θ, s)

-- x = e
ssaStmt θ (ExprStmt l1 (AssignExpr l2 OpAssign (LVar l3 x) e))   
  = let (θ', x', e') = ssaAsgn l2 θ x e
    in (Just θ', ExprStmt l1 (AssignExpr l2 OpAssign (LVar l3 x') e'))

-- e
ssaStmt θ (ExprStmt l e)   
  = let e' = ssaExpr θ e
    in  (Just θ, ExprStmt l e')

-- s1;s2;...;sn
ssaStmt (BlockStmt l stmts) 
  = let (b, stmts') <- ssaStmts θ stmts
    in  (b, BlockStmt l stmts')

-- if b { s1 }
ssaStmt θ (IfSingleStmt l b s)
  = ssaStmt θ (IfStmt l b s (EmptyStmt l))

-- if b { s1 } else { s2 }
ssaStmt θ (IfStmt l e s1 s2)
  = let e'        = ssaStmt θ e
        (θ1, s1') = ssaStmt θ s1
        (θ2, s2') = ssaStmt θ s2
        θ'        = joinSsaEnv l θ1 θ2 -- RECORD SIDE CONDITIONS!!!!
    in (θ', IfStmt l e' s1' s2')

-- var x1 [ = e1 ]; ... ; var xn [= en];
ssaStmt θ (VarDeclStmt l ds)
  = let (θ', ds') = tcSeq tcVarDecl θ ds
    in (θ', VarDeclStmt l ds')

-- return e 
ssaStmt θ (ReturnStmt l Nothing) 
  = (Nothing, ReturnStmt l Nothing)

-- return e 
ssaStmt γ (ReturnStmt l (Just e)) 
  = let e' = ssaExpr θ e
    in (Nothing, ReturnStmt l (Just e'))

-- OTHER (Not handled)
ssaStmt γ s 
  = convertError "ssaStmt" s



-------------------------------------------------------------------------------------
ssaExpr    :: SsaEnv -> Expression SourcePos -> Expression SourcePos 
-------------------------------------------------------------------------------------

ssaExpr _ e@(IntLit _ _)               
  = e 

ssaExpr _ e@(BoolLit _ _)
  = e 

ssaExpr θ (VarRef l x)
  = error "TBD:ssaExpr VarRef" 
        -- OLD: maybe (tcError l $ errorUnboundId x) return $ envFindTy x γ
        -- WARNING: make sure to NOT mess with global vars -- e.g. FUNCTION NAMES!
        -- MARK THOSE AS UNASSIGNABLE?

ssaExpr θ (PrefixExpr l o e)
  = PrefixExpr l o (ssaExpr θ e)

ssaExpr θ (InfixExpr l o e1 e2)        
  = InfixExpr l o (ssaExpr θ e1) (ssaExpr θ e2)

ssaExpr θ (CallExpr l e es)
  = CallExpr l (ssaExpr θ e) (ssaExpr θ <$> es)

ssaExpr γ e 
  = convertError "ssaExpr" e


-------------------------------------------------------------------------------------
ssaVarDecl :: SsaEnv -> VarDecl SourcePos -> (Maybe SsaEnv, VarDecl SourcePos)
-------------------------------------------------------------------------------------

ssaVarDecl θ (VarDecl l x (Just e)) 
  = let (θ', x', e') = ssaAsgn l θ x e
    in (Just θ, VarDecl l x' (Just e'))

ssaVarDecl θ z@(VarDecl l x Nothing)  
  = convertError "ssaVarDECL" z

------------------------------------------------------------------------------------
ssaAsgn :: SourcePos
        -> SsaEnv 
        -> Id SourcePos 
        -> Expression SourcePos 
        -> (SsaEnv, Id SourcePos, Expression SourcePos) 
------------------------------------------------------------------------------------
ssaAsgn θ x e = let e'       = ssaExpr θ e 
                    (θ', x') = updSsaEnv x θ
                 in (θ', x', e')

-------------------------------------------------------------------------------------
-- | SSA Monad ----------------------------------------------------------------------
-------------------------------------------------------------------------------------

joinSsaEnv = error "TBD: joinSsaEnv"
initSsaEnv = error "TBD"
joinSsaEnv = error "TBD"
updSsaEnv  = error "TBD"

-- instead of Maybe SsaEnv, 
-- 1. bake the "Nothing" INTO SsaEnv
-- 2. keep the JOIN info there as well. No need for MONAD. i.e. θ IS the monad.

