{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Language.Rsc.Visitor
    ( Visitor, VisitorM (..)
    , defaultVisitor
    , scopeVisitor

    , visitRsc
    , visitStmts
    , visitStmtsT
    , foldRsc
    , foldStmts
    ) where

import           Control.Exception         (throw)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT, execState, modify, runState, runStateT)
import           Data.Functor.Identity     (Identity)
import qualified Data.Traversable          as T
import           Language.Rsc.AST
import           Language.Rsc.Errors
import           Language.Rsc.Locations
import           Language.Rsc.Misc         (mapSndM)
import           Language.Rsc.Names
import           Language.Rsc.Program


--------------------------------------------------------------------------------
-- | Top-down visitors
--------------------------------------------------------------------------------

-- RJ: rewrite *ALL* queries and transforms with these.
-- 1. write transformer for cast/type annotations using tyvar binders from sigs.
-- 2. write IsRsc using `Visitor`
-- 3. write "everything" queries using `Visitor`

data VisitorM m acc ctx b = Visitor {

    endStmt :: Statement b -> Bool
  , endExpr :: Expression b -> Bool

  -- | Context @ctx@ is built up in a "top-down" fashion but not across siblings
  , ctxStmt :: ctx -> Statement b  -> ctx
  , ctxExpr :: ctx -> Expression b -> ctx
  , ctxCElt :: ctx -> ClassElt b   -> ctx

  -- | Transforms are allowed to access current @ctx@
  , txStmt  :: ctx -> Statement b  -> Statement b
  , txExpr  :: ctx -> Expression b -> Expression b
  , txCElt  :: ctx -> ClassElt b   -> ClassElt b
  , txId    :: ctx -> Id b         -> Id b
  , txLVal  :: ctx -> LValue b     -> LValue b

  -- | Accumulations are allowed to access current @ctx@ but @acc@ value is monoidal
  --   Called after `ctxStmt`,etc.
  , accStmt :: ctx -> Statement b  -> acc
  , accExpr :: ctx -> Expression b -> acc
  , accCElt :: ctx -> ClassElt b   -> acc
  , accVDec :: ctx -> VarDecl  b   -> acc

  -- | Execute external monad - to be run after transformation has been applied
  , mStmt   :: Statement b   -> m (Statement b)
  , mExpr   :: Expression b  -> m (Expression b)

  }

type Visitor = VisitorM Identity

--------------------------------------------------------------------------------
defaultVisitor :: (Monad m, Functor m, Monoid acc) => VisitorM m acc ctx b
--------------------------------------------------------------------------------
defaultVisitor = Visitor {
    endStmt = \_   -> False
  , endExpr = \_   -> False
  , ctxStmt = \c _ -> c
  , ctxExpr = \c _ -> c
  , ctxCElt = \c _ -> c
  , txStmt  = \_ x -> x
  , txExpr  = \_ x -> x
  , txCElt  = \_ x -> x
  , txId    = \_ x -> x
  , txLVal  = \_ x -> x
  , accStmt = \_ _ -> mempty
  , accExpr = \_ _ -> mempty
  , accCElt = \_ _ -> mempty
  , accVDec = \_ _ -> mempty
  , mStmt   = return
  , mExpr   = return
  }

scopeVisitor :: (Monoid acc, Functor m, Monad m) => VisitorM m acc ctx b
scopeVisitor = defaultVisitor { endExpr = ee, endStmt = es }
  where
    es FunctionStmt {} = True
    es ClassStmt    {} = True
    es InterfaceStmt{} = True
    es ModuleStmt   {} = True
    es _               = False
    ee _               = True


--------------------------------------------------------------------------------
-- | Visitor API
--------------------------------------------------------------------------------
foldRsc :: (IsLocated b, Monoid a) => Visitor a ctx b -> ctx -> a -> Rsc b r -> a
foldRsc v c a p = snd $ execVisitM v c a p

foldStmts :: (IsLocated b, Monoid a) => Visitor a ctx b -> ctx -> a -> [Statement b] -> a
foldStmts v c a p = execState (visitStmtsM v c p) a

visitRsc :: (IsLocated b, Monoid a) =>   Visitor a ctx b -> ctx -> Rsc b r -> Rsc b r
visitRsc v c p = fst $ execVisitM v c mempty p

visitStmts :: (IsLocated b, Monoid s) => Visitor s ctx b -> ctx -> [Statement b] -> ([Statement b], s)
visitStmts v c p = runState (visitStmtsM v c p) mempty

visitStmtsT :: (IsLocated b, Monoid s, Functor m, Monad m)
            => VisitorM m s ctx b -> ctx -> [Statement b] -> m [Statement b]
visitStmtsT v c p = fst <$> runStateT (visitStmtsM v c p) mempty


--------------------------------------------------------------------------------
-- | Implementing Visitors
--------------------------------------------------------------------------------

execVisitM v c a p = runState (visitRscM v c p) a

type VisitT m acc = StateT acc m

accum :: (Monoid a, Monad m) => a -> VisitT m a ()
accum = modify . mappend

f <$$> x = T.traverse f x


visitRscM :: (Monad m, Functor m, Monoid a, IsLocated b)
           => VisitorM m a ctx b -> ctx -> Rsc b r -> VisitT m a (Rsc b r)
visitRscM v c p = do
  c'    <- visitSource v c (code p)
  return $ p { code = c' }

visitSource :: (Monad m, Functor m, Monoid a, IsLocated b)
            => VisitorM m a ctx b -> ctx -> Source b -> VisitT m a (Source b)
visitSource v c (Src ss) = Src <$> visitStmtsM v c ss

visitStmtsM   :: (Monad m, Functor m, Monoid a, IsLocated b)
              => VisitorM m a ctx b -> ctx -> [Statement b] -> VisitT m a [Statement b]
visitStmtsM v = mapM . visitStmtM v


visitStmtM   :: (Monad m, Functor m, Monoid a, IsLocated b)
             => VisitorM m a ctx b -> ctx -> Statement b -> VisitT m a (Statement b)
visitStmtM v = vS
  where
    vE      = visitExpr v
    vEE     = visitEnumElt v
    vC      = visitCaseClause v
    vI      = visitId v
    vS c s  | endStmt v s
            = accum acc >> return s
            | otherwise
            = accum acc >> lift (mStmt v s') >>= step c' where c'   = ctxStmt v c s
                                                               s'   = txStmt  v c' s
                                                               acc  = accStmt v c' s
    step c   (ExprStmt l e)          = ExprStmt     l <$> vE c e
    step c   (BlockStmt l ss)        = BlockStmt    l <$> (vS c <$$> ss)
    step c   (IfSingleStmt l b s)    = IfSingleStmt l <$> (vE c b) <*> (vS c s)
    step c   (IfStmt l b s1 s2)      = IfStmt       l <$> (vE c b)
                                                      <*> (vS c s1)
                                                      <*> (vS c s2)
    step c   (WhileStmt l b s)       = WhileStmt    l <$> (vE c b) <*> (vS c s)
    step c   (ForStmt l i t inc b)   = ForStmt      l <$> (visitFInit v c i)
                                                      <*> (vE c <$$> t)
                                                      <*> (vE c <$$> inc)
                                                      <*> (vS c b)
    step c   (ForInStmt l i e b)     = ForInStmt    l <$> (visitFIInit v c i)
                                                      <*> (vE c e)
                                                      <*> (vS c b)
    step c   (VarDeclStmt l ds)      = VarDeclStmt  l <$> (visitVarDecl v c <$$> ds)
    step c   (ReturnStmt l e)        = ReturnStmt   l <$> (vE c <$$> e)
    step c   (FunctionStmt l f xs b) = FunctionStmt l <$> (vI c f)
                                                      <*> (vI c <$$> xs)
                                                      <*> ((vS c <$$>) <$$> b)
    step c   (SwitchStmt l e cs)     = SwitchStmt   l <$> (vE c e) <*> (vC c <$$> cs)
    step c   (ClassStmt l x es)      = ClassStmt    l <$> (vI c x)
                                                      <*> (visitClassElt v c <$$> es)
    step c   (ThrowStmt l e)         = ThrowStmt    l <$> (vE c e)
    step c   (ModuleStmt l m ss)     = ModuleStmt   l <$> (vI c m) <*> (vS c <$$> ss)
    step _ s@(InterfaceStmt {})      = return s
    step _ s@(EmptyStmt {})          = return s
    step c   (EnumStmt l n es)       = EnumStmt     l <$> (vI c n) <*> (vEE c <$$> es)
    step _ s                         = throw $ unimplemented l "visitStatement" s  where l = srcPos s


visitEnumElt v c (EnumElt l i n)    = EnumElt l      <$> visitId v c i <*> return n


visitExpr :: (IsLocated b, Monoid a, Functor m, Monad m)
          => VisitorM m a ctx b -> ctx -> Expression b -> VisitT m a (Expression b)
visitExpr v = vE
   where
     vS      = visitStmtM       v
     vI      = visitId         v
     vL      = visitLValue     v
     vE c e  | endExpr v e
             = accum acc >> return e
             | otherwise
             = accum acc >> lift (mExpr v s') >>= step c' where c'  = ctxExpr v c  e
                                                                s'  = txExpr  v c' e
                                                                acc = accExpr v c' e
     step _ e@(BoolLit {})            = return e
     step _ e@(IntLit {})             = return e
     step _ e@(NumLit {})             = return e
     step _ e@(HexLit {})             = return e
     step _ e@(NullLit {})            = return e
     step _ e@(StringLit {})          = return e
     step _ e@(VarRef {})             = return e
     step _ e@(ThisRef {})            = return e
     step _ e@(SuperRef {})           = return e
     step c   (ArrayLit l es)         = ArrayLit l     <$> (vE c <$$> es)
     step c   (CondExpr l e1 e2 e3)   = CondExpr l     <$> (vE c e1) <*> (vE c e2) <*> (vE c e3)
     step c   (InfixExpr l o e1 e2)   = InfixExpr l o  <$> (vE c e1) <*> (vE c e2)
     step c   (PrefixExpr l o e)      = PrefixExpr l o <$> (vE c e)
     step c   (CallExpr l e es)       = CallExpr l     <$> (vE c e)  <*> (vE c <$$> es)
     step c   (ObjectLit l bs)        = ObjectLit l    <$> (mapSndM (vE c) <$$> bs)
     step c   (DotRef l e f)          = DotRef l       <$> (vE c e)  <*> (vI c f)
     step c   (BracketRef l e1 e2)    = BracketRef l   <$> (vE c e1) <*> (vE c e2)
     step c   (AssignExpr l o v e)    = AssignExpr l o <$> (vL c v)  <*> (vE c e)
     step c   (UnaryAssignExpr l o v) = UnaryAssignExpr l o <$> (vL c v)
     step c   (FuncExpr l f xs ss)    = FuncExpr l <$> (vI c <$$> f) <*> (vI c <$$> xs) <*> (vS c <$$> ss)
     step c   (NewExpr l e es)        = NewExpr  l <$> (vE c e) <*> (vE c <$$> es)
     step c   (Cast l e)              = Cast l     <$> (vE c e)
     step c   (Cast_ l e)             = Cast_ l    <$> (vE c e)
     step _ e                         = throw $ unimplemented l "visitExpr " e  where l = srcPos e

visitClassElt :: (Monad m, Functor m, Monoid a, IsLocated b)
              => VisitorM m a ctx b -> ctx -> ClassElt b -> VisitT m a (ClassElt b)
visitClassElt v = vCE
  where
    vI       = visitId   v
    vS       = visitStmtM v
    vCE c ce = accum acc >> step c' ce' where c'     = ctxCElt v c  ce
                                              ce'    = txCElt  v c' ce
                                              acc    = accCElt v c' ce
    step c (Constructor l xs ss)        = Constructor l <$> (vI c <$$> xs)
                                                        <*> (vS c <$$> ss)
    step c (MemberVarDecl l b i e)      = MemberVarDecl l b <$> (vI c i)
                                                            <*> (visitExpr v c <$$> e)
    step c (MemberMethDecl l b f xs ss) = MemberMethDecl l b <$> (vI c f)
                                                             <*> (vI c <$$> xs)
                                                             <*> (vS c <$$> ss)

visitFInit :: (Monad m, Functor m, Monoid a, IsLocated b)
           => VisitorM m a ctx b -> ctx -> ForInit b -> VisitT m a (ForInit b)
visitFInit v = step
  where
    step _ NoInit       = return NoInit
    step c (VarInit ds) = VarInit  <$> (visitVarDecl v c <$$> ds)
    step c (ExprInit e) = ExprInit <$> (visitExpr v c e)

visitFIInit :: (Monad m, Functor m, Monoid a, IsLocated b)
            => VisitorM m a ctx b -> ctx -> ForInInit b -> VisitT m a (ForInInit b)
visitFIInit v = step
  where
    step c (ForInVar x)  = ForInVar  <$> visitId v c x
    step c (ForInLVal l) = ForInLVal <$> visitLValue v c l

visitVarDecl :: (Monad m, Functor m, Monoid a, IsLocated b)
             =>  VisitorM m a ctx b -> ctx -> VarDecl b -> VisitT m a (VarDecl b)
visitVarDecl v c d@(VarDecl l x e)
  = accum (accVDec v c d) >> VarDecl l <$> (visitId v c x) <*> (visitExpr v c <$$> e)

visitId :: (Monad m, Functor m, Monoid a, IsLocated b)
        => VisitorM m a ctx b -> ctx -> Id b -> VisitT m a (Id b)
visitId v c x = return (txId v c x)

visitLValue :: (Monad m, Functor m, Monoid a, IsLocated b)
            => VisitorM m a ctx b -> ctx -> LValue b -> VisitT m a (LValue b)
visitLValue v c lv = step c (txLVal v c lv)
  where
    step c_ (LDot l e s)       = LDot     l <$> (visitExpr v c_ e) <*> return s
    step c_ (LBracket l e1 e2) = LBracket l <$> (visitExpr v c_ e1) <*> (visitExpr v c_ e2)
    step _ lv_@(LVar {})       = return lv_


visitCaseClause :: (Monad m, Functor m, Monoid a, IsLocated b)
                => VisitorM m a ctx b -> ctx -> CaseClause b -> VisitT m a (CaseClause b)
visitCaseClause v = step
  where
    step c (CaseClause l e ss) = CaseClause l  <$> (visitExpr v c e)  <*> (visitStmtM v c <$$> ss)
    step c (CaseDefault l ss)  = CaseDefault l <$> (visitStmtM v c <$$> ss)

