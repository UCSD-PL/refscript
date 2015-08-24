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

module Language.Nano.Visitor
    (
      Transformable (..)
    , transFmap

    , NameTransformable (..)
    , ntransFmap

    , Visitor, VisitorM (..)
    , defaultVisitor
    , scopeVisitor

    , visitNano
    , visitStmts
    , visitStmtsT
    , foldRsc
    , foldStmts
    ) where

import           Control.Applicative           ((<$>), (<*>))
import           Control.Exception             (throw)
import           Control.Monad.Trans.Class     (lift)
import           Control.Monad.Trans.State     (StateT, modify, runState, runStateT)
import           Data.Functor.Identity         (Identity)
import           Data.Monoid
import qualified Data.Traversable              as T
import qualified Language.Fixpoint.Types       as F
import           Language.Nano.Annots          hiding (Annot, err)
import           Language.Nano.AST
import           Language.Nano.Core.Env
import           Language.Nano.Errors
import           Language.Nano.Locations
import           Language.Nano.Misc            (mapSndM)
import           Language.Nano.Names
import           Language.Nano.Program
import           Language.Nano.Typecheck.Types
import           Language.Nano.Types


--------------------------------------------------------------------------------
-- | Top-down visitors
--------------------------------------------------------------------------------

-- RJ: rewrite *ALL* queries and transforms with these.
-- 1. write transformer for cast/type annotations using tyvar binders from sigs.
-- 2. write IsNano using `Visitor`
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
  , accStmt :: ctx -> Statement b  -> acc
  , accExpr :: ctx -> Expression b -> acc
  , accCElt :: ctx -> ClassElt b   -> acc
  , accVDec :: ctx -> VarDecl  b   -> acc

  -- | Execute external monad - to be run after transformation has been applied
  , mStmt   :: Statement b   -> m (Statement b)
  , mExpr   :: Expression b  -> m (Expression b)

  }

type Visitor = VisitorM Identity

---------------------------------------------------------------------------------
defaultVisitor :: (Monad m, Functor m, Monoid acc) => VisitorM m acc ctx b
---------------------------------------------------------------------------------
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
    es FunctionStmt{} = True
    es FuncAmbDecl {} = True
    es FuncOverload{} = True
    es ClassStmt   {} = True
    es IfaceStmt   {} = True
    es ModuleStmt  {} = True
    es _              = False
    ee _              = True


--------------------------------------------------------------------------------
-- | Visitor API
--------------------------------------------------------------------------------
foldRsc :: (IsLocated b, Monoid a) => Visitor a ctx b -> ctx -> a -> Rsc b r -> a
foldRsc v c a p = snd $ execVisitM v c a p

foldStmts :: (IsLocated b, Monoid a) => Visitor a ctx b -> ctx -> a -> [Statement b] -> a
foldStmts v c a p = snd $ runState (visitStmtsM v c p) a

visitNano :: (IsLocated b, Monoid a) =>   Visitor a ctx b -> ctx -> Rsc b r -> Rsc b r
visitNano v c p = fst $ execVisitM v c mempty p

visitStmts :: (IsLocated b, Monoid s) => Visitor s ctx b -> ctx -> [Statement b] -> ([Statement b], s)
visitStmts v c p = runState (visitStmtsM v c p) mempty

visitStmtsT :: (IsLocated b, Monoid s, Functor m, Monad m)
            => VisitorM m s ctx b -> ctx -> [Statement b] -> m [Statement b]
visitStmtsT v c p = fst <$> runStateT (visitStmtsM v c p) mempty


--------------------------------------------------------------------------------
-- | Implementing Visitors
--------------------------------------------------------------------------------

execVisitM v c a p = runState (visitNanoM v c p) a

type VisitT m acc = StateT acc m

accum :: (Monoid a, Monad m) => a -> VisitT m a ()
accum = modify . mappend

f <$$> x = T.traverse f x


visitNanoM :: (Monad m, Functor m, Monoid a, IsLocated b)
           => VisitorM m a ctx b -> ctx -> Rsc b r -> VisitT m a (Rsc b r)
visitNanoM v c p = do
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
    step c (ExprStmt l e)           = ExprStmt     l <$> vE c e
    step c (BlockStmt l ss)         = BlockStmt    l <$> (vS c <$$> ss)
    step c (IfSingleStmt l b s)     = IfSingleStmt l <$> (vE c b) <*> (vS c s)
    step c (IfStmt l b s1 s2)       = IfStmt       l <$> (vE c b) <*> (vS c s1) <*> (vS c s2)
    step c (WhileStmt l b s)        = WhileStmt    l <$> (vE c b) <*> (vS c s)
    step c (ForStmt l i t inc b)    = ForStmt      l <$> (visitFInit v c i) <*> (vE c <$$> t) <*> (vE c <$$> inc) <*> (vS c b)
    step c (ForInStmt l i e b)      = ForInStmt    l <$> (visitFIInit v c i) <*> (vE c e)     <*> (vS c b)
    step c (VarDeclStmt l ds)       = VarDeclStmt  l <$> (visitVarDecl v c <$$> ds)
    step c (ReturnStmt l e)         = ReturnStmt   l <$> (vE c <$$> e)
    step c (FunctionStmt l f xs b)  = FunctionStmt l <$> (vI c f) <*> (vI c <$$> xs) <*> (vS c <$$> b)
    step c (SwitchStmt l e cs)      = SwitchStmt   l <$> (vE c e) <*> (vC c <$$> cs)
    step c (ClassStmt l x xe xs es) = ClassStmt    l <$> (vI c x) <*> (vI c <$$> xe) <*> (vI c <$$> xs) <*> (visitClassElt v c <$$> es)
    step c (ThrowStmt l e)          = ThrowStmt    l <$> (vE c e)
    step c (FuncAmbDecl l f xs)     = FuncAmbDecl  l <$> (vI c f) <*> (vI c <$$> xs)
    step c (FuncOverload l f xs)    = FuncOverload l <$> (vI c f) <*> (vI c <$$> xs)
    step c (ModuleStmt l m ss)      = ModuleStmt   l <$> (vI c m) <*> (vS c <$$> ss)
    step _ s@(IfaceStmt {})         = return s
    step _ s@(EmptyStmt {})         = return s
    step c (EnumStmt l n es)        = EnumStmt     l <$> (vI c n) <*> (vEE c <$$> es)
    step _ s                        = throw $ unimplemented l "visitStatement" s  where l = srcPos s


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
     step _ e@(BoolLit {})           = return e
     step _ e@(IntLit {})            = return e
     step _ e@(HexLit {})            = return e
     step _ e@(NullLit {})           = return e
     step _ e@(StringLit {})         = return e
     step _ e@(VarRef {})            = return e
     step _ e@(ThisRef {})           = return e
     step _ e@(SuperRef {})          = return e
     step c (ArrayLit l es)          = ArrayLit l     <$> (vE c <$$> es)
     step c (CondExpr l e1 e2 e3)    = CondExpr l     <$> (vE c e1) <*> (vE c e2) <*> (vE c e3)
     step c (InfixExpr l o e1 e2)    = InfixExpr l o  <$> (vE c e1) <*> (vE c e2)
     step c (PrefixExpr l o e)       = PrefixExpr l o <$> (vE c e)
     step c (CallExpr l e es)        = CallExpr l     <$> (vE c e)  <*> (vE c <$$> es)
     step c (ObjectLit l bs)         = ObjectLit l    <$> (mapSndM (vE c) <$$> bs)
     step c (DotRef l e f)           = DotRef l       <$> (vE c e)  <*> (vI c f)
     step c (BracketRef l e1 e2)     = BracketRef l   <$> (vE c e1) <*> (vE c e2)
     step c (AssignExpr l o v e)     = AssignExpr l o <$> (vL c v)  <*> (vE c e)
     step c (UnaryAssignExpr l o v)  = UnaryAssignExpr l o <$> (vL c v)
     step c (FuncExpr l f xs ss)     = FuncExpr l <$> (vI c <$$> f) <*> (vI c <$$> xs) <*> (vS c <$$> ss)
     step c (NewExpr l e es)         = NewExpr  l <$> (vE c e) <*> (vE c <$$> es)
     step c (Cast l e)               = Cast l     <$> (vE c e)
     step _ e                        = throw $ unimplemented l "visitExpr " e  where l = srcPos e

visitClassElt :: (Monad m, Functor m, Monoid a, IsLocated b)
              => VisitorM m a ctx b -> ctx -> ClassElt b -> VisitT m a (ClassElt b)
visitClassElt v = vCE
  where
    vI       = visitId   v
    vS       = visitStmtM v
    vCE c ce = accum acc >> step c' ce' where c'     = ctxCElt v c  ce
                                              ce'    = txCElt  v c' ce
                                              acc    = accCElt v c' ce
    step c (Constructor l xs ss)       = Constructor    l   <$> (vI c <$$> xs) <*> (vS c <$$> ss)
    step c (MemberVarDecl l b i e)     = MemberVarDecl  l b <$> (vI c i)       <*> (visitExpr v c <$$> e)
    step c (MemberMethDecl l b f xs)   = MemberMethDecl l b <$> (vI c f)       <*> (vI c <$$> xs)
    step c (MemberMethDef l b f xs ss) = MemberMethDef  l b <$> (vI c f)       <*> (vI c <$$> xs) <*> (vS c <$$> ss)

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


--------------------------------------------------------------------------------------------
-- | Transform types
--------------------------------------------------------------------------------------------

class Transformable t where
  trans :: F.Reftable r => ([TVar] -> [BindQ q r] -> RTypeQ q r -> RTypeQ q r) -> [TVar] -> [BindQ q r] -> t q r  -> t q r

instance Transformable RTypeQ where
  trans = transRType

instance Transformable BindQ where
  trans f αs xs b = b { b_type = trans f αs xs $ b_type b }

instance Transformable FactQ where
  trans = transFact

instance Transformable CastQ where
  trans = transCast

instance Transformable TypeDeclQ where
  trans f αs xs (TD s@(TS _ b _) es) = TD (trans f αs xs s) (trans f αs' xs es)
    where
      αs' = map btvToTV (b_args b) ++ αs

instance Transformable TypeSigQ where
  trans f αs xs (TS k b h) = TS k (trans f αs xs b) (transIFDBase f αs' xs h)
      where
        αs' = map btvToTV (b_args b) ++ αs

instance Transformable TypeMembersQ where
  trans f αs xs (TM p m sp sm c k s n) = TM (fmap (trans f αs xs) p)
                                            (fmap (trans f αs xs) m)
                                            (fmap (trans f αs xs) sp)
                                            (fmap (trans f αs xs) sm)
                                            (fmap (trans f αs xs) c)
                                            (fmap (trans f αs xs) k)
                                            (fmap (trans f αs xs) s)
                                            (fmap (trans f αs xs) n)

instance Transformable BTGenQ where
  trans f αs xs (BGen n ts) = BGen n $ trans f αs xs <$> ts

instance Transformable TGenQ where
  trans f αs xs (Gen n ts) = Gen n $ trans f αs xs <$> ts

instance Transformable BTVarQ where
  trans f αs xs (BTV x l c) = BTV x l $ trans f αs xs <$> c

instance Transformable FieldInfoQ where
  trans f αs xs (FI o t t') = FI o (trans f αs xs t) (trans f αs xs t')

instance Transformable MethodInfoQ where
  trans f αs xs (MI o m t) = MI o m (trans f αs xs t)

instance Transformable ModuleDefQ where
  trans f αs xs (ModuleDef v t e p) = ModuleDef (envMap (trans f αs xs) v) (envMap (trans f αs xs) t) e p

instance Transformable VarInfoQ where
  trans f αs xs (VI a i t) = VI a i $ trans f αs xs t

instance Transformable FAnnQ where
  trans f αs xs (FA i s ys) = FA i s $ trans f αs xs <$> ys

transFmap ::  (F.Reftable r, Functor thing)
          => ([TVar] -> [BindQ q r] -> RTypeQ q r -> RTypeQ q r)
          -> [TVar] -> thing (FAnnQ q r) -> thing (FAnnQ q r)
transFmap f αs = fmap (trans f αs [])

transIFDBase f αs xs (es,is) = (trans f αs xs <$> es, trans f αs xs <$> is)

transFact :: F.Reftable r => ([TVar] -> [BindQ q r] -> RTypeQ q r -> RTypeQ q r)
                          -> [TVar] -> [BindQ q r] -> FactQ q r -> FactQ q r
transFact f = go
  where
    go αs xs (PhiVarTy (v,t))  = PhiVarTy      $ (v, trans f αs xs t)

    go αs xs (TypInst x y ts)  = TypInst x y   $ trans f αs xs <$> ts

    go αs xs (EltOverload x m) = EltOverload x $ trans f αs xs m
    go αs xs (Overload x t)    = Overload x    $ trans f αs xs t

    go αs xs (VarAnn a t)      = VarAnn a      $ trans f αs xs <$> t
    go αs xs (AmbVarAnn t)     = AmbVarAnn     $ trans f αs xs t

    go αs xs (FieldAnn m t)    = FieldAnn m    $ trans f αs xs t
    go αs xs (MethAnn m t)     = MethAnn m     $ trans f αs xs t
    go αs xs (ConsAnn t)       = ConsAnn       $ trans f αs xs t

    go αs xs (UserCast t)      = UserCast      $ trans f αs xs t
    go αs xs (FuncAnn t)       = FuncAnn       $ trans f αs xs t
    go αs xs (TCast x c)       = TCast x       $ trans f αs xs c

    go αs xs (ClassAnn ts)     = ClassAnn      $ trans f αs xs ts
    go αs xs (InterfaceAnn td) = InterfaceAnn  $ trans f αs xs td

    go _ _   t                 = t

transCast f = go
  where
    go _  _ CNo          = CNo
    go αs xs (CDead e t) = CDead e $ trans f αs xs t
    go αs xs (CUp t1 t2) = CUp (trans f αs xs t1) (trans f αs xs t2)
    go αs xs (CDn t1 t2) = CUp (trans f αs xs t1) (trans f αs xs t2)

transRType :: F.Reftable r
           => ([TVar] -> [BindQ q r] -> RTypeQ q r -> RTypeQ q r)
           ->  [TVar] -> [BindQ q r] -> RTypeQ q r -> RTypeQ q r
transRType f               = go
  where
    go αs xs (TPrim c r)   = f αs xs $ TPrim c r
    go αs xs (TVar v r)    = f αs xs $ TVar v r
    go αs xs (TOr ts)      = f αs xs $ TOr ts'       where ts' = go αs xs <$> ts
    go αs xs (TAnd ts)     = f αs xs $ TAnd ts'      where ts' = go αs xs <$> ts
    go αs xs (TRef n r)    = f αs xs $ TRef n' r     where n'  = trans f αs xs n
    go αs xs (TObj ms r)   = f αs xs $ TObj ms' r    where ms' = trans f αs xs ms
    go αs xs (TClass n)    = f αs xs $ TClass n'     where n'  = trans f αs xs n
    go αs xs (TMod m)      = f αs xs $ TMod m
    go αs xs (TAll a t)    = f αs xs $ TAll a t'     where t'  = go (btvToTV a:αs) xs t
    go αs xs (TFun bs t r) = f αs xs $ TFun bs' t' r where bs' = trans f αs xs' <$> bs
                                                           t'  = go αs xs' t
                                                           xs' = bs ++ xs
    go _  _  (TExp e)      = TExp e


--------------------------------------------------------------------------------------------
-- | Transform names
--------------------------------------------------------------------------------------------

class NameTransformable t where
  ntrans :: F.Reftable r => (QN p -> QN q) -> (QP p -> QP q) -> t p r  -> t q r

instance NameTransformable RTypeQ where
  ntrans = ntransRType

instance NameTransformable BindQ where
  ntrans f g b = b { b_type = ntrans f g $ b_type b }

instance NameTransformable FactQ where
  ntrans = ntransFact

instance NameTransformable CastQ where
  ntrans = ntransCast

instance NameTransformable TypeDeclQ where
  ntrans f g (TD s m) = TD (ntrans f g s) (ntrans f g m)

instance NameTransformable TypeSigQ where
  ntrans f g (TS k b (e,i)) = TS k (ntrans f g b) (ntrans f g <$> e, ntrans f g <$> i)

instance NameTransformable TypeMembersQ where
  ntrans f g (TM p m sp sm c k s n) = TM (fmap (ntrans f g) p)
                                         (fmap (ntrans f g) m)
                                         (fmap (ntrans f g) sp)
                                         (fmap (ntrans f g) sm)
                                         (fmap (ntrans f g) c)
                                         (fmap (ntrans f g) k)
                                         (fmap (ntrans f g) s)
                                         (fmap (ntrans f g) n)

instance NameTransformable BTGenQ where
  ntrans f g (BGen n ts) = BGen (f n) (ntrans f g <$> ts)

instance NameTransformable TGenQ where
  ntrans f g (Gen n ts) = Gen (f n) (ntrans f g <$> ts)

instance NameTransformable BTVarQ where
  ntrans f g (BTV x l c) = BTV x l $ ntrans f g <$> c

instance NameTransformable FieldInfoQ where
  ntrans f g (FI o t t') = FI o (ntrans f g t) (ntrans f g t')

instance NameTransformable MethodInfoQ where
  ntrans f g (MI o m t) = MI o m (ntrans f g t)

ntransFmap ::  (F.Reftable r, Functor t)
           => (QN p -> QN q) -> (QP p -> QP q) -> t (FAnnQ p r) -> t (FAnnQ q r)
ntransFmap f g = fmap (ntrans f g)

ntransFact f g = go
  where
    go (PhiVar v)        = PhiVar        $ v
    go (PhiVarTC v)      = PhiVarTC      $ v
    go (PhiVarTy (v,t))  = PhiVarTy      $ (v, ntrans f g t)
    go (PhiPost v)       = PhiPost       $ v
    go (TypInst x y ts)  = TypInst x y   $ ntrans f g <$> ts
    go (EltOverload x m) = EltOverload x $ ntrans f g m
    go (Overload x t)    = Overload x    $ ntrans f g t
    go (VarAnn a t)      = VarAnn a      $ ntrans f g <$> t
    go (AmbVarAnn t)     = AmbVarAnn     $ ntrans f g t
    go (FieldAnn m t)    = FieldAnn m    $ ntrans f g t
    go (MethAnn m t)     = MethAnn m     $ ntrans f g t
    go (ConsAnn t)       = ConsAnn       $ ntrans f g t
    go (UserCast t)      = UserCast      $ ntrans f g t
    go (FuncAnn  t)      = FuncAnn       $ ntrans f g t
    go (TCast x c)       = TCast x       $ ntrans f g c
    go (ClassAnn t)      = ClassAnn      $ ntrans f g t
    go (InterfaceAnn t)  = InterfaceAnn  $ ntrans f g t
    go (ExportedElt)     = ExportedElt
    go (ReadOnlyVar)     = ReadOnlyVar
    go (ModuleAnn m)     = ModuleAnn     $ m
    go (EnumAnn e)       = EnumAnn       $ e
    go (BypassUnique)    = BypassUnique

ntransCast :: F.Reftable r => (QN p -> QN q) -> (QP p -> QP q) -> CastQ p r -> CastQ q r
ntransCast f g = go
  where
    go CNo         = CNo
    go (CDead e t) = CDead e $ ntrans f g t
    go (CUp t1 t2) = CUp (ntrans f g t1) (ntrans f g t2)
    go (CDn t1 t2) = CUp (ntrans f g t1) (ntrans f g t2)

ntransRType :: F.Reftable r => (QN p -> QN q) -> (QP p -> QP q) -> RTypeQ p r -> RTypeQ q r
ntransRType f g         = go
  where
    go (TPrim p r)   = TPrim p r
    go (TVar v r)    = TVar v r
    go (TOr ts)      = TOr ts'        where ts' = go <$> ts
    go (TAnd ts)     = TAnd ts'       where ts' = go <$> ts
    go (TRef n r)    = TRef n' r      where n'  = ntrans f g n
    go (TObj ms r)   = TObj ms' r     where ms' = ntrans f g ms
    go (TClass n)    = TClass n'      where n'  = ntrans f g n
    go (TMod p)      = TMod p'        where p'  = g p
    go (TAll a t)    = TAll a' t'     where a'  = ntrans f g a
                                            t'  = go t
    go (TFun bs t r) = TFun bs' t' r  where bs' = ntrans f g <$> bs
                                            t'  = go t
    go (TExp e)      = TExp e

instance NameTransformable FAnnQ where
  ntrans f g (FA i s ys) = FA i s $ ntrans f g <$> ys

