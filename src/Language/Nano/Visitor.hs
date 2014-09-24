{-# LANGUAGE ScopedTypeVariables #-}

module Language.Nano.Visitor (
    Transformable (..)
  , transFmap
    
  , Visitor (..)
  , defaultVisitor

  , visitNano
    
  ) where

import           Data.Monoid
import           Control.Applicative            ((<$>))
import           Control.Exception              (throw)
import           Language.Fixpoint.Misc         (mapSnd)
import           Language.Nano.Errors
import           Language.Nano.Typecheck.Types
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations
import           Language.Nano.Types
import           Language.Nano.Locations
import           Language.Nano.Annots
import           Language.Nano.Program -- Eeks, cycles!
import           Language.Nano.Typecheck.Types
import           Language.ECMAScript3.Syntax 
import           Language.ECMAScript3.PrettyPrint

--------------------------------------------------------------------------------
-- | Top-down visitors
--------------------------------------------------------------------------------

-- RJ: rewrite *ALL* queries and transforms with these.
-- 1. write transformer for cast/type annotations using tyvar binders from sigs.
-- 2. write IsNano using `Visitor` 
-- 3. write "everything" queries using `Visitor`

data Visitor acc ctx b = Visitor {
  -- | Context @ctx@ is built up in a "top-down" fashion but not across siblings
    ctxStmt :: ctx -> Statement b  -> ctx 
  , ctxExpr :: ctx -> Expression b -> ctx
  , ctxCElt :: ctx -> ClassElt b   -> ctx

  -- | Transforms are allowd to access current @ctx@
  , txStmt  :: ctx -> Statement b  -> Statement b
  , txExpr  :: ctx -> Expression b -> Expression b
  , txCElt  :: ctx -> ClassElt b   -> ClassElt b
  , txId    :: ctx -> Id b         -> Id b

  -- | Accumulations are allowed to access current @ctx@ but @acc@ value is monoidal
  , accStmt :: ctx -> Statement b  -> acc
  , accExpr :: ctx -> Expression b -> acc
  , accCElt :: ctx -> ClassElt b   -> acc
  }
                         
---------------------------------------------------------------------------------
defaultVisitor :: Monoid acc => Visitor acc ctx b
---------------------------------------------------------------------------------
defaultVisitor = Visitor {
    ctxStmt = \c _ -> c
  , ctxExpr = \c _ -> c
  , ctxCElt = \c _ -> c
  , txStmt  = \_ x -> x
  , txExpr  = \_ x -> x
  , txCElt  = \_ x -> x
  , txId    = \_ x -> x
  , accStmt = \_ _ -> mempty
  , accExpr = \_ _ -> mempty
  , accCElt = \_ _ -> mempty
  }           

--------------------------------------------------------------------------------
-- | Using Visitors
--------------------------------------------------------------------------------

visitNano :: (IsLocated b) => Visitor a ctx b -> ctx -> Nano b r -> Nano b r 
visitNano v c p = p { code = visitSource v c (code p) }

visitSource :: (IsLocated b) => Visitor a ctx b -> ctx -> Source b -> Source b 
visitSource v c (Src ss) = Src $ visitStmts v c ss 

visitStmts   :: (IsLocated b) => Visitor a ctx b -> ctx -> [Statement b] -> [Statement b] 
visitStmts v c ss = visitStmt v c <$> ss

visitStmt   :: (IsLocated b) => Visitor a ctx b -> ctx -> Statement b -> Statement b 
visitStmt v = vS
  where
    vE      = visitExpr v   
    vC      = visitCaseClause v   
    vI      = visitId   v   
    vS c s  = step c' s' where c'   = ctxStmt v c s
                               s'   = txStmt  v c' s
    step c (ExprStmt l e)           = ExprStmt     l (vE c e)
    step c (BlockStmt l ss)         = BlockStmt    l (vS c <$> ss)
    step c (IfSingleStmt l b s)     = IfSingleStmt l (vE c b) (vS c s)
    step c (IfStmt l b s1 s2)       = IfStmt       l (vE c b) (vS c s1) (vS c s2)
    step c (WhileStmt l b s)        = WhileStmt    l (vE c b) (vS c s) 
    step c (ForStmt l i t inc b)    = ForStmt      l (visitFInit v c i) (vE c <$> t) (vE c <$> inc) (vS c b)
    step c (ForInStmt l i e b)      = ForInStmt    l (visitFIInit v c i) (vE c e) (vS c b)
    step c (VarDeclStmt l ds)       = VarDeclStmt  l (visitVarDecl v c <$> ds)
    step c (ReturnStmt l e)         = ReturnStmt   l (vE c <$> e)
    step c (FunctionStmt l f xs b)  = FunctionStmt l (vI c f) (vI c <$> xs) (vS c <$> b)
    step c (SwitchStmt l e cs)      = SwitchStmt   l (vE c e) (vC c <$> cs)
    step c (ClassStmt l x xe xs es) = ClassStmt    l (vI c x) (vI c <$> xe) (vI c <$> xs) (visitClassElt v c <$> es) 
    step c (ThrowStmt l e)          = ThrowStmt    l (vE c e)
    step c (FunctionDecl l f xs)    = FunctionDecl l (vI c f) (vI c <$> xs)
    step c (ModuleStmt l m ss)      = ModuleStmt   l (vI c m) (vS c <$> ss) 
    step _ s@(IfaceStmt {})         = s 
    step _ s@(EmptyStmt {})         = s 
    step _ s                        = throw $ unimplemented l "visitStatement" s  where l = srcPos $ getAnnotation s


visitExpr   ::  (IsLocated b) => Visitor a ctx b -> ctx -> Expression b -> Expression b 
visitExpr v = vE
  where
    vS      = visitStmt       v   
    vC      = visitCaseClause v   
    vI      = visitId         v   
    vL      = visitLValue     v   
    vE c e  = step c' s' where c'   = ctxExpr v c  e
                               s'   = txExpr  v c' e
    step _ e@(BoolLit {})           = e 
    step _ e@(IntLit {})            = e 
    step _ e@(NullLit {})           = e 
    step _ e@(StringLit {})         = e 
    step _ e@(VarRef {})            = e 
    step _ e@(ThisRef {})           = e 
    step _ e@(SuperRef {})          = e 
    step c (ArrayLit l es)          = ArrayLit l (vE c <$> es)
    step c (CondExpr l e1 e2 e3)    = CondExpr l (vE c e1) (vE c e2) (vE c e3)
    step c (InfixExpr l o e1 e2)    = InfixExpr l o (vE c e1) (vE c e2)
    step c (PrefixExpr l o e)       = PrefixExpr l o (vE c e)
    step c (CallExpr l e es)        = CallExpr l (vE c e) (vE c  <$> es)
    step c (ObjectLit l bs)         = ObjectLit l (mapSnd (vE c) <$> bs)
    step c (DotRef l e f)           = DotRef l (vE c e) (vI c f)
    step c (BracketRef l e1 e2)     = BracketRef l (vE c e1) (vE c e2)
    step c (AssignExpr l o v e)     = AssignExpr l o (vL c v) (vE c e)
    step c (UnaryAssignExpr l o v)  = UnaryAssignExpr l o (vL c v) 
    step c (FuncExpr l f xs ss)     = FuncExpr l (vI c <$> f) (vI c <$> xs) (vS c <$> ss) 
    step c (NewExpr l e es)         = NewExpr  l (vE c e) (vE c <$> es)
    step c (Cast l e)               = Cast l (vE c e)
    step _ e                        = throw $ unimplemented l "visitExpr " e  where l = srcPos $ getAnnotation e 

visitClassElt :: (IsLocated b) => Visitor a ctx b -> ctx -> ClassElt b -> ClassElt b 
visitClassElt v = vCE 
  where
    vI       = visitId   v   
    vS       = visitStmt v   
    vD       = visitVarDecl v   
    vCE c ce = step c' ce' where c'     = ctxCElt v c  ce
                                 ce'    = txCElt  v c' ce
    step c (Constructor l xs ss)        = Constructor    l   (vI c <$> xs)          (vS c <$> ss)
    step c (MemberVarDecl l b d)        = MemberVarDecl  l b (vD c d) 
    step c (MemberMethDecl l b f xs ss) = MemberMethDecl l b (vI c f) (vI c <$> xs) (vS c <$> ss)

visitFInit ::  (IsLocated b) => Visitor a ctx b -> ctx -> ForInit b -> ForInit b
visitFInit v = step
  where
    step _ NoInit       = NoInit
    step c (VarInit ds) = VarInit  (visitVarDecl v c <$> ds)
    step c (ExprInit e) = ExprInit (visitExpr v c e)

visitFIInit :: (IsLocated b) => Visitor a ctx b -> ctx -> ForInInit b -> ForInInit b
visitFIInit v = step
  where
    step c (ForInVar x)  = ForInVar (visitId v c x)
    step c (ForInLVal l) = ForInLVal (visitLValue v c l)  

visitVarDecl :: (IsLocated b) =>  Visitor a ctx b -> ctx -> VarDecl b -> VarDecl b
visitVarDecl = error "TODO"

visitId :: (IsLocated b) => Visitor a ctx b -> ctx -> Id b -> Id b
visitId v c x = txId v c x 

visitLValue :: (IsLocated b) => Visitor a ctx b -> ctx -> LValue b -> LValue b
visitLValue  = error "TODO"


visitCaseClause :: (IsLocated b) => Visitor a ctx b -> ctx -> CaseClause b -> CaseClause b
visitCaseClause = error "TODO"


-- foldStmts :: (Monoid acc) => Visitor acc ctx b -> ctx -> [Statement b] -> acc 
-- foldStmts a = go
--   where
--     go = error "TODO" 
--  
-- 
-- foldStmt :: (Monoid acc) => Visitor acc ctx b -> ctx -> Statement b -> acc 
-- foldStmt a = go
--   where
--     go = error "TODO" 
--  
-- 
-- foldExpr :: (Monoid acc) => Visitor acc ctx b -> ctx -> Expression b -> acc 
-- foldExpr v = go
--   where
--     go = error "TODO" 

   
--------------------------------------------------------------------------------------------
-- | Visitors, deriving be damned. 
--------------------------------------------------------------------------------------------

class Transformable t where
  trans :: ([TVar] -> [Bind r] -> RType r -> RType r) -> [TVar] -> [Bind r] -> t r  -> t r

instance Transformable RType where
  trans = transRType

instance Transformable Bind where
  trans f as xs b = b { b_type = trans f as xs $ b_type b }
 
instance Transformable TypeMember where
  trans f as xs m = m { f_type = trans f as xs $ f_type m }

instance Transformable Fact where
  trans = error "TODO"

transRType :: ([TVar] -> [Bind r] -> RType r -> RType r) -> [TVar] -> [Bind r] -> RType r -> RType r

transRType f                  = go 
  where
    go as xs (TAnd ts)        = f as xs $ TAnd ts'            where ts' = go as xs <$> ts
    go as xs (TApp c ts r)    = f as xs $ TApp c ts' r        where ts' = go as xs <$> ts
    go as xs (TFun to bs t r) = f as xs $ TFun to' bs' t' r   where to' = go as xs'      <$> to
                                                                    bs' = trans f as xs' <$> bs
                                                                    t'  = go as xs'       $  t
                                                                    xs' = bs ++ xs
    go as xs (TCons ms m r)   = f as xs $ TCons ms' m r       where ms' = trans f as xs <$> ms
    go as xs (TAll a t)       = f as xs $ TAll a t'           where t'  = go (a:as) xs t 
    go as xs t                = f as xs t 

-- RJ: use newtype for AnnR and NanoBareR so we just make the below instances

transAnnR :: ([TVar] -> [Bind r] -> RType r -> RType r)
          -> [TVar] -> AnnR r  -> AnnR r
transAnnR f as ann = ann { ann_fact = trans f as [] <$> ann_fact ann}

transFmap :: (Functor thing) => ([TVar] -> [Bind r] -> RType r -> RType r)
          -> [TVar] -> thing (AnnR r)  -> thing (AnnR r)
transFmap f as = fmap (transAnnR f as) 

