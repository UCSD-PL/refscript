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

module Language.Nano.Visitor (
    Transformable (..)
  , transFmap

  , NameTransformable (..)
  , ntransFmap

  , Visitor, VisitorM (..)
  , defaultVisitor

  , visitNano
  , visitStmts
  , visitStmtsT
  , foldNano

  -- * Traversals / folds / maps
  , hoistTypes
  , hoistGlobals
  , hoistBindings
  , visibleVars
  , scrapeModules
  , writeGlobalVars
  , scrapeVarDecl
  , extractQualifiedNames
  , replaceAbsolute
  , replaceDotRef
  , fixEnums
  , fixFunBinders

  , mkTypeMembers
  , mkVarEnv

  ) where

import           Control.Applicative              ((<$>), (<*>))
import           Control.Exception                (throw)
import           Control.Monad
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State        (StateT, modify, runState, runStateT)
import           Data.Data
import           Data.Default
import           Data.Functor.Identity            (Identity)
import           Data.Generics
import qualified Data.HashSet                     as H
import qualified Data.IntMap                      as I
import           Data.List                        (partition)
import qualified Data.Map.Strict                  as M
import           Data.Maybe                       (fromMaybe, listToMaybe, maybeToList)
import           Data.Monoid
import           Data.Text                        (pack, splitOn)
import qualified Data.Traversable                 as T
import           Language.Fixpoint.Types.Errors
import           Language.Fixpoint.Misc           hiding ((<$$>))
import           Language.Fixpoint.Types.Names          (symbolString)
import qualified Language.Fixpoint.Types          as F
import qualified Language.Fixpoint.Types.Visitor        as V
import           Language.Nano.Annots             hiding (err)
import           Language.Nano.Env
import           Language.Nano.Errors
import           Language.Nano.Liquid.Types       ()
import           Language.Nano.Locations
import           Language.Nano.Misc               (concatMapM, mapPair, mapSndM, single, (<###>), (<##>))
import           Language.Nano.Names
import           Language.Nano.Program
import           Language.Nano.Syntax
import           Language.Nano.Syntax.Annotations
import           Language.Nano.Syntax.PrettyPrint
import           Language.Nano.Typecheck.Resolve
import           Language.Nano.Typecheck.Types
import           Language.Nano.Types

-- import           Debug.Trace                        (trace)

import           Text.PrettyPrint.HughesPJ
import           Text.Printf

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
    endStmt = const False
  , endExpr = const False
  , ctxStmt = const
  , ctxExpr = const
  , ctxCElt = const
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
foldNano :: (IsLocated b, Monoid a) => Visitor a ctx b -> ctx -> a -> Nano b r -> a
foldNano v c a p = snd $ execVisitM v c a p

visitNano :: (IsLocated b, Monoid a) =>   Visitor a ctx b -> ctx -> Nano b r -> Nano b r
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
           => VisitorM m a ctx b -> ctx -> Nano b r -> VisitT m a (Nano b r)
visitNanoM v c p = do
  c'    <- visitSource v c (code p)
  return $ p { code = c' }

visitSource :: (Monad m, Functor m, Monoid a, IsLocated b)
            => VisitorM m a ctx b -> ctx -> Source b -> VisitT m a (Source b)
visitSource v c (Src ss) = Src <$> visitStmtsM v c ss

visitStmtsM   :: (Monad m, Functor m, Monoid a, IsLocated b)
              => VisitorM m a ctx b -> ctx -> [Statement b] -> VisitT m a [Statement b]
visitStmtsM v c = mapM (visitStmtM v c)


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
    step c (IfSingleStmt l b s)     = IfSingleStmt l <$> vE c b <*> vS c s
    step c (IfStmt l b s1 s2)       = IfStmt       l <$> vE c b <*> vS c s1 <*> vS c s2
    step c (WhileStmt l b s)        = WhileStmt    l <$> vE c b <*> vS c s
    step c (ForStmt l i t inc b)    = ForStmt      l <$> visitFInit v c i  <*> (vE c <$$> t) <*> (vE c <$$> inc) <*> vS c b
    step c (ForInStmt l i e b)      = ForInStmt    l <$> visitFIInit v c i <*> vE c e     <*> vS c b
    step c (VarDeclStmt l ds)       = VarDeclStmt  l <$> (visitVarDecl v c <$$> ds)
    step c (ReturnStmt l e)         = ReturnStmt   l <$> (vE c <$$> e)
    step c (FunctionStmt l f xs b)  = FunctionStmt l <$> vI c f <*> (vI c <$$> xs) <*> (vS c <$$> b)
    step c (SwitchStmt l e cs)      = SwitchStmt   l <$> vE c e <*> (vC c <$$> cs)
    step c (ClassStmt l x xe xs es) = ClassStmt    l <$> vI c x <*> (vI c <$$> xe) <*> (vI c <$$> xs) <*> (visitClassElt v c <$$> es)
    step c (ThrowStmt l e)          = ThrowStmt    l <$> vE c e
    step c (FuncAmbDecl l f xs)     = FuncAmbDecl  l <$> vI c f <*> (vI c <$$> xs)
    step c (FuncOverload l f xs)    = FuncOverload l <$> vI c f <*> (vI c <$$> xs)
    step c (ModuleStmt l m ss)      = ModuleStmt   l <$> vI c m <*> (vS c <$$> ss)
    step _ s@(IfaceStmt {})         = return s
    step _ s@(EmptyStmt {})         = return s
    step c (EnumStmt l n es)        = EnumStmt     l <$> vI c n <*> (vEE c <$$> es)
    step _ s                        = throw $ unimplemented l "visitStatement" s  where l = srcPos $ getAnnotation s


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
     step _ e@(NumLit {})            = return e
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
     step _ e                        = throw $ unimplemented l "visitExpr " e  where l = srcPos $ getAnnotation e

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
    step c (LDot l e s)       = LDot     l <$> (visitExpr v c e) <*> return s
    step c (LBracket l e1 e2) = LBracket l <$> (visitExpr v c e1) <*> (visitExpr v c e2)
    step _ lv@(LVar {})       = return lv


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
  trans f as xs b = b { b_type = trans f as xs $ b_type b }

instance Transformable TypeMemberQ where
  trans f as xs = mapElt' (trans f as xs)

instance Transformable FactQ where
  trans = transFact

instance Transformable CastQ where
  trans = transCast

instance Transformable IfaceDefQ where
  trans = transIFD

transIFD f as xs idf = idf { t_base = transIFDBase f as' xs  $  t_base idf
                           , t_elts = trans        f as' xs <$> t_elts idf
                           }
    where
      as'            = (t_args idf) ++ as

transIFDBase f as xs (es,is) = (transClassAnn1 f as xs <$> es, transClassAnn1 f as xs <$> is)


transFact :: F.Reftable r
          => ([TVar] -> [BindQ q r] -> RTypeQ q r -> RTypeQ q r)
          -> [TVar] -> [BindQ q r] -> FactQ q r -> FactQ q r
transFact f = go
  where
    go as xs (VarAnn (a,t))    = VarAnn        $ (a,trans f as xs <$> t)
    go as xs (AmbVarAnn t)     = AmbVarAnn     $ trans f as xs t
    go as xs (FieldAnn m)      = FieldAnn      $ trans f as xs m
    go as xs (MethAnn  m)      = MethAnn       $ trans f as xs m
    go as xs (StatAnn  m)      = StatAnn       $ trans f as xs m
    go as xs (ConsAnn  m)      = ConsAnn       $ trans f as xs m
    go as xs (UserCast t)      = UserCast      $ trans f as xs t
    go as xs (FuncAnn  t)      = FuncAnn       $ trans f as xs t
    go as xs (IfaceAnn ifd)    = IfaceAnn      $ trans f as xs ifd
    go as xs (ClassAnn c)      = ClassAnn      $ transClassAnn f as xs c
    go as xs (TypInst x y ts)  = TypInst x y   $ trans f as xs <$> ts
    go as xs (EltOverload x m) = EltOverload x $ trans f as xs m
    go as xs (Overload x t)    = Overload x    $ trans f as xs t
    go as xs (TCast x c)       = TCast x       $ trans f as xs c
    go _ _   t                 = t

transCast f = go
  where
    go _  _ CNo          = CNo
    go as xs (CDead e t) = CDead e $ trans f as xs t
    go as xs (CUp t1 t2) = CUp (trans f as xs t1) (trans f as xs t2)
    go as xs (CDn t1 t2) = CUp (trans f as xs t1) (trans f as xs t2)

transClassAnn f as xs (as',es,is) = (as', transClassAnn1 f (as' ++ as) xs <$> es
                                        , transClassAnn1 f (as' ++ as) xs <$> is)

transClassAnn1 f as xs (n,ts) =  (n, trans f as xs <$> ts)


transRType :: F.Reftable r
           => ([TVar] -> [BindQ q r] -> RTypeQ q r -> RTypeQ q r)
           ->  [TVar] -> [BindQ q r] -> RTypeQ q r -> RTypeQ q r

transRType f                  = go
  where
    go as xs (TAnd ts)        = f as xs $ TAnd ts'            where ts' = go as xs <$> ts
    go as xs (TApp c ts r)    = f as xs $ TApp c ts' r        where ts' = go as xs <$> ts
    go as xs (TFun to bs t r) = f as xs $ TFun to' bs' t' r   where to' = go as xs'      <$> to
                                                                    bs' = trans f as xs' <$> bs
                                                                    t'  = go as xs'       $  t
                                                                    xs' = bs ++ xs
    go as xs (TCons m ms r)   = f as xs $ TCons m' ms' r      where ms' = trans f as xs <$> ms
                                                                    m'  = toType $ trans f as xs (ofType m)
    go as xs (TAll a t)       = f as xs $ TAll a t'           where t'  = go (a:as) xs t
    go as xs (TRef n ts r)    = f as xs $ TRef n ts' r        where ts' = go  as xs <$> ts
    go as xs (TSelf m)        = f as xs $ TSelf m'            where m'  = go  as xs m
    go _  _  t                = t

-- RJ: use newtype for AnnR and NanoBareR so we just make the below instances

transAnnR :: F.Reftable r => ([TVar] -> [BindQ q r] -> RTypeQ q r -> RTypeQ q r)
          -> [TVar] -> AnnQ q r  -> AnnQ q r
transAnnR f as ann = ann { ann_fact = trans f as [] <$> ann_fact ann}

transFmap ::  (F.Reftable r, Functor thing)
          => ([TVar] -> [BindQ q r] -> RTypeQ q r -> RTypeQ q r)
          -> [TVar]
          -> thing (AnnQ q r)
          -> thing (AnnQ q r)
transFmap f as = fmap (transAnnR f as)



--------------------------------------------------------------------------------------------
-- | Transform names
--------------------------------------------------------------------------------------------

class NameTransformable t where
  ntrans :: F.Reftable r => (QN p -> QN q) -> (QP p -> QP q) -> t p r  -> t q r

instance NameTransformable RTypeQ where
  ntrans = ntransRType

instance NameTransformable BindQ where
  ntrans f g b = b { b_type = ntrans f g $ b_type b }

instance NameTransformable TypeMemberQ where
  ntrans f g = mapElt' (ntrans f g)

instance NameTransformable FactQ where
  ntrans = ntransFact

instance NameTransformable CastQ where
  ntrans = ntransCast

instance NameTransformable IfaceDefQ where
  ntrans = ntransIFD

ntransFmap ::  (F.Reftable r, Functor t) => (QN p -> QN q) -> (QP p -> QP q) -> t (AnnQ p r) -> t (AnnQ q r)
ntransFmap f g = fmap (ntransAnnR f g)


ntransIFD :: F.Reftable r => (QN p -> QN q) -> (QP p -> QP q) -> IfaceDefQ p r -> IfaceDefQ q r
ntransIFD f g idf = idf { t_name =            f    $  t_name idf
                        , t_base = ntransBase f g  $  t_base idf
                        , t_elts = ntrans     f g <$> t_elts idf }

ntransBase f g (es,is) = (ntransBase1 f g <$> es, ntransBase1 f g <$> is)
ntransBase1 :: (F.Reftable r) => (QN p -> QN q) -> (QP p -> QP q) -> (QN p, [RTypeQ p r]) -> (QN q, [RTypeQ q r])
ntransBase1 f g (n,ts) = (f n, ntrans f g <$> ts)

ntransFact f g = go
  where
    go (PhiVar v)        = PhiVar        $ v
    go (PhiVarTC v)      = PhiVarTC      $ v
    go (PhiPost v)       = PhiPost       $ v
    go (PhiVarTy (v,t))  = PhiVarTy      $ (v, ntrans f g t)
    go (VarAnn (a, t))   = VarAnn        $ (a, ntrans f g <$> t)
    go (AmbVarAnn t)     = AmbVarAnn     $ ntrans f g t
    go (ExportedElt)     = ExportedElt
    go (ReadOnlyVar)     = ReadOnlyVar
    go (BypassUnique)    = BypassUnique
    go (FieldAnn m)      = FieldAnn      $ ntrans f g m
    go (MethAnn  m)      = MethAnn       $ ntrans f g m
    go (StatAnn  m)      = StatAnn       $ ntrans f g m
    go (ConsAnn  m)      = ConsAnn       $ ntrans f g m
    go (UserCast t)      = UserCast      $ ntrans f g t
    go (FuncAnn  t)      = FuncAnn       $ ntrans f g t
    go (IfaceAnn ifd)    = IfaceAnn      $ ntrans f g ifd
    go (ClassAnn c)      = ClassAnn      $ ntransClassAnn f g c
    go (EnumAnn c)       = EnumAnn       $ c
    go (ModuleAnn c)     = ModuleAnn     $ c
    go (TypInst x y ts)  = TypInst x y   $ ntrans f g <$> ts
    go (EltOverload x m) = EltOverload x $ ntrans f g m
    go (Overload x t)    = Overload x    $ ntrans f g t
    go (TCast x c)       = TCast x       $ ntrans f g c


ntransCast :: F.Reftable r => (QN p -> QN q) -> (QP p -> QP q) -> CastQ p r -> CastQ q r
ntransCast f g = go
  where
    go CNo         = CNo
    go (CDead e t) = CDead e $ ntrans f g t
    go (CUp t1 t2) = CUp (ntrans f g t1) (ntrans f g t2)
    go (CDn t1 t2) = CUp (ntrans f g t1) (ntrans f g t2)

ntransClassAnn f g (as,es,is) = (as, ntransClassAnn1 f g <$> es, ntransClassAnn1 f g <$> is)
ntransClassAnn1 f g (n,ts) =  (f n, ntrans f g <$> ts)

ntransRType :: F.Reftable r => (QN p -> QN q) -> (QP p -> QP q) -> RTypeQ p r -> RTypeQ q r
ntransRType f g         = go
  where
    go (TApp c ts r)    = TApp c ts' r        where ts' = go <$> ts
    go (TVar v r)       = TVar v r
    go (TFun to bs t r) = TFun to' bs' t' r   where to' = go <$> to
                                                    bs' = ntrans f g <$> bs
                                                    t'  = go t
    go (TCons m ms r)   = TCons m' ms' r      where ms' = ntrans f g <$> ms
                                                    m'  = ntrans f g m
    go (TAll a t)       = TAll a t'           where t'  = go t
    go (TAnd ts)        = TAnd ts'            where ts' = go <$> ts
    go (TRef n ts r)    = TRef n' ts' r       where n'  = f n
                                                    ts' = go <$> ts
    go (TSelf m)        = TSelf m'            where m'  = go m
    go (TClass n)       = TClass n'           where n'  = f n
    go (TModule p)      = TModule p'          where p'  = g p
    go (TEnum n)        = TEnum n'            where n'  = f n
    go (TExp e)         = TExp e
--
-- RJ: use newtype for AnnR and NanoBareR so we just make the below instances

ntransAnnR :: F.Reftable r => (QN p -> QN q) -> (QP p -> QP q) -> AnnQ p r -> AnnQ q r
ntransAnnR f g ann = ann { ann_fact = ntrans f g <$> ann_fact ann}

-- ntransFmap :: (Functor (t r)) => (QN p -> QN q) -> (QP p -> QP q) -> t p r -> t q r
-- ntransFmap f g = fmap (ntransAnnR f g)


---------------------------------------------------------------------------
-- | AST Traversals
---------------------------------------------------------------------------

type BindInfo r = (Id (AnnType r), AnnType r, SyntaxKind, Assignability, Initialization)

-- | Find all language level bindings in the scope of @s@.
--   This includes:
--
--    * function definitions/declarations,
--    * classes,
--    * modules,
--    * variables
--
--   E.g. declarations in the If-branch of a conditional expression. Note how
--   declarations do not escape module or function blocks.
--
-------------------------------------------------------------------------------
hoistBindings :: Data r => [Statement (AnnType r)] -> [BindInfo r]
-------------------------------------------------------------------------------
hoistBindings = snd . visitStmts vs ()
  where
    vs = scopeVisitor { accStmt = accStmt', accVDec = accVDec' }

    accStmt' _ (FunctionStmt l n _ _)  = [(n, l, fdk, ro, ii)]
    accStmt' _ (FuncAmbDecl l n _)     = [(n, l, fak, id, ii)]
    accStmt' _ (FuncOverload l n _  )  = [(n, l, fok, id, ii)]
    accStmt' _ (ClassStmt l n _ _ _ )  = [(n, l, cdk, ro, ii)]
    accStmt' _ (ModuleStmt l n _)      = [(n, l { ann_fact = modAnn  n l }, mdk, ro, ii)]
    accStmt' _ (EnumStmt l n _)        = [(n, l { ann_fact = enumAnn n l }, edk, ro, ii)]
    accStmt' _ _                       = []

    accVDec' _ (VarDecl l n init)      = [(n, l, vdk, varAsgn l, fromInit init)] ++
                                         [(n, l, vdk, wg, fromInit init) | AmbVarAnn _  <- ann_fact l]

    fromInit (Just _) = ii
    fromInit _        = ui
    varAsgn l         = fromMaybe WriteLocal
                      $ listToMaybe [ a | VarAnn (a,_) <- ann_fact l ]

    vdk  = VarDeclKind
    fdk  = FuncDefKind
    fak  = FuncAmbientKind
    fok  = FuncOverloadKind
    cdk  = ClassDefKind
    mdk  = ModuleDefKind
    edk  = EnumDefKind
    wg   = WriteGlobal
    ro   = ReadOnly
    id   = ImportDecl
    ui   = Uninitialized
    ii   = Initialized
    modAnn  n l = ModuleAnn (F.symbol n) : ann_fact l
    enumAnn n l = EnumAnn   (F.symbol n) : ann_fact l


-- | Find classes / interfaces in scope
-------------------------------------------------------------------------------
hoistTypes :: IsLocated a => [Statement a] -> [Statement a]
-------------------------------------------------------------------------------
hoistTypes  = snd . visitStmts (scopeVisitor { accStmt = accStmt' }) ()
  where
    accStmt' _ s@(ClassStmt {}) = [s]
    accStmt' _ s@(IfaceStmt {}) = [s]
    accStmt' _ _                = [ ]


-------------------------------------------------------------------------------
hoistGlobals :: Data r => [Statement (AnnType r)] -> [Id (AnnType r)]
-------------------------------------------------------------------------------
hoistGlobals = snd . visitStmts (scopeVisitor { accVDec = accVDec' }) ()
  where
    accVDec' _ (VarDecl l x _) = [ x | VarAnn (WriteGlobal,_) <- ann_fact l ]
                              ++ [ x | AmbVarAnn _            <- ann_fact l ]

-- | Summarise all nodes in top-down, left-to-right order, carrying some state
--   down the tree during the computation, but not left-to-right to siblings,
--   and also stop when a condition is true.
---------------------------------------------------------------------------
everythingButWithContext :: s -> (r -> r -> r) -> GenericQ (s -> (r, s, Bool)) -> GenericQ r
---------------------------------------------------------------------------
everythingButWithContext s0 f q x
  | stop      = r
  | otherwise = foldl f r (gmapQ (everythingButWithContext s' f q) x)
    where (r, s', stop) = q x s0


---------------------------------------------------------------------------
-- | AST Folds
---------------------------------------------------------------------------

-- Only descend down modules
-------------------------------------------------------------------------------
collectModules :: (IsLocated a, Data a) => [Statement a] -> [(AbsPath, [Statement a])]
-------------------------------------------------------------------------------
collectModules ss = topLevel : rest ss
  where
    rest                      = everythingButWithContext [] (++) $ ([],,False) `mkQ` f
    f e@(ModuleStmt _ x ms) s = let p = s ++ [F.symbol x] in
                                ([(QP AK_ (srcPos e) p, ms)], p, False)
    f _                    s  = ([], s, True)
    topLevel                  = (QP AK_ (srcPos dummySpan) [], ss)

-- Not including class, module, enum names
---------------------------------------------------------------------------------------
visibleVars :: Data r => [Statement (AnnSSA r)] -> [(Id SrcSpan, VarInfo r)]
---------------------------------------------------------------------------------------
visibleVars s = [ (ann <$> n, (k,v,a,t,i))  | (n,l,k,a,i) <- hoistBindings s
                                            , f           <- ann_fact l
                                            , t           <- annToType (ann l) n a f
                                            , let v        = visibility l ]
  where
    annToType _ _ ReadOnly   (VarAnn (_,t)) = maybeToList t -- Hoist ReadOnly vars (i.e. function defs)
    annToType _ _ ImportDecl (VarAnn (_,t)) = maybeToList t -- Hoist ImportDecl (i.e. function decls)
    annToType _ _ ReadOnly   (AmbVarAnn t)  = [t] -- Hoist ReadOnly vars (i.e. function defs)
    annToType _ _ ImportDecl (AmbVarAnn t)  = [t] -- Hoist ImportDecl (i.e. function decls)
    annToType _ _ _          _              = [ ]

---------------------------------------------------------------------------------------
extractQualifiedNames :: PPR r => [Statement (AnnRel r)]
                               -> (H.HashSet AbsName, H.HashSet AbsPath)
---------------------------------------------------------------------------------------
extractQualifiedNames stmts = (namesSet, modulesSet)
  where
    allModStmts             = collectModules stmts
    modulesSet              = H.fromList $ fst <$> allModStmts
    namesSet                = H.fromList [ nm | (ap,ss) <- allModStmts
                                              , nm <- typeNames ap ss ]

-- | Replace all relative qualified names and paths in a program with full ones.
---------------------------------------------------------------------------------------
replaceAbsolute :: PPR r => NanoBareRelR r -> NanoBareR r
---------------------------------------------------------------------------------------
replaceAbsolute pgm@(Nano { code      = Src ss
                          , fullNames = ns
                          , fullPaths = ps })
                    = pgm { code = Src $ (tr <$>) <$> ss }
  where
    tr l            = ntransAnnR (safeAbsName l) (safeAbsPath l) l
    safeAbsName l a = case absAct (absoluteName ns) l a of
                        Just a' -> a'
                        -- If it's a type alias, don't throw error
                        Nothing | isAlias a -> toAbsoluteName a
                                | otherwise -> throw $ errorUnboundName (srcPos l) a
    safeAbsPath l a = case absAct (absolutePath ps) l a of
                        Just a' -> a'
                        Nothing -> throw $ errorUnboundPath (srcPos l) a

    isAlias (QN RK_ _ [] s) = envMem s $ tAlias pgm
    isAlias (QN _   _ _  _) = False

    absAct f l a    = I.lookup (ann_id l) mm >>= (`f` a)
    mm              = snd $ visitStmts vs (QP AK_ def []) ss
    vs              = defaultVisitor { ctxStmt = cStmt }
                                     { accStmt = acc   }
                                     { accExpr = acc   }
                                     { accCElt = acc   }
                                     { accVDec = acc   }
    cStmt (QP AK_ l p) (ModuleStmt _ x _)
                    = QP AK_ l $ p ++ [F.symbol x]
    cStmt q _       = q
    acc c s         = I.singleton (ann_id a) c where a = getAnnotation s


-- | Replace `a.b.c...z` with `offset(offset(...(offset(a),"b"),"c"),...,"z")`
---------------------------------------------------------------------------------------
replaceDotRef :: NanoBareR F.Reft -> NanoBareR F.Reft
---------------------------------------------------------------------------------------
replaceDotRef p@(Nano{ code = Src fs, tAlias = ta, pAlias = pa, invts = is })
    = p { code         = Src $      tf       <##>  fs
        , tAlias       = transRType tt [] [] <###> ta
        , pAlias       =            tt [] [] <##>  pa
        , invts        = transRType tt [] [] <##>  is
        }
  where
    tf (Ann l a facts) = Ann l a $ transFact tt [] [] <$> facts
    tt _ _             = fmap $ V.trans vs () ()
    vs                 = V.defaultVisitor { V.txExpr = tx }
    tx _ (F.EVar s)    | (x:y:zs) <- pack "." `splitOn` pack (symbolString s)
                       = foldl offset (F.eVar x) (y:zs)
    tx _ e             = e
    offset k v         = F.mkEApp offsetLocSym [F.expr k, F.expr v]


-- | Replace `TRef x _ _` where `x` is a name for an enumeration with `number`
---------------------------------------------------------------------------------------
fixEnums :: PPR r => NanoBareR r -> NanoBareR r
---------------------------------------------------------------------------------------
fixEnums p@(Nano { code = Src ss, pModules = m })
               = p { code     = Src $ (tr <$>) <$> ss
                   , pModules = qenvMap (fixEnumsInModule p) m}
  where
    tr         = transAnnR f []
    f _ _      = fixEnumInType p

fixEnumInType p (TRef x [] r) | Just e <- resolveEnumInPgm p x
                              = if isBvEnum e then tBV32 `strengthen` r
                                              else tInt  `strengthen` r
fixEnumInType _ t             = t

fixEnumsInModule p m@(ModuleDef { m_variables = mv, m_types = mt })
               = m { m_variables = mv', m_types = mt' }
  where
   mv'         = envMap f mv
   f (v,a,t,i) = (v,a,fixEnumInType p t,i)
   mt'         = envMap (transIFD g [] []) mt
   g _ _       = fixEnumInType p



-- | Add a '#' at the end of every function binder (to avoid capture)
--
fixFunBinders p@(Nano { code = Src ss, pModules = m })
               = p { code     = Src $ (tr <$>) <$> ss
                   , pModules = qenvMap fixFunBindersInModule m }
  where
    tr         = transAnnR f []
    f _ _      = fixFunBindersInType

fixFunBindersInType t | Just is <- bkFuns t = mkAnd $ map (mkFun . f) is
                      | otherwise           = t
  where
    f (vs,s,yts,t)    = (vs,ssm s,ssb yts,ss t)
      where
        ks            = [ y | B y _ <- yts ]
        ks'           = (F.eVar . (`F.suffixSymbol` F.symbol "")) <$> ks
        su            = F.mkSubst $ zip ks ks'
        ss            = F.subst su
        ssb bs        = [ B (ss s) (ss t) | B s t <- bs ]
        ssm           = (ss <$>)


fixFunBindersInModule m@(ModuleDef { m_variables = mv, m_types = mt })
               = m { m_variables = mv', m_types = mt' }
  where
   mv'         = envMap f mv
   f (v,a,t,i) = (v,a,fixFunBindersInType t,i)
   mt'         = envMap (transIFD g [] []) mt
   g _ _       = fixFunBindersInType



-- | `scrapeModules ss` creates a module store from the statements in @ss@
--   For every module we populate:
--
--    * m_variables with: functions, variables, class constructors, modules
--
--    * m_types with: classes and interfaces
--
---------------------------------------------------------------------------------------
scrapeModules :: PPR r => NanoBareR r -> Either (F.FixResult Error) (NanoBareR r)
---------------------------------------------------------------------------------------
scrapeModules pgm@(Nano { code = Src stmts })
  = do  mods  <- return $ collectModules stmts
        mods' <- mapM mkMod mods

        return $ pgm { pModules = qenvFromList mods' }
  where

    mkMod :: PPR r => (AbsPath, [Statement (AnnR r)]) -> Either (F.FixResult Error) (AbsPath, ModuleDefQ AK r)
    mkMod (p,ss)      = do  ve <- return $ varEnv p ss
                            te <- typeEnv p ss
                            ee <- return $ enumEnv ss
                            return (p, ModuleDef ve te ee p)

    drop1 (_,b,c,d,e) = (b,c,d,e)

    varEnv p          = envMap drop1 . mkVarEnv . vStmts p

    typeEnv p ss      = tStmts p ss >>= return . envFromList

    enumEnv           = envFromList  . eStmts

    vStmts                         = concatMap . vStmt

    vStmt _ (VarDeclStmt _ vds)    = [(ss x,(vdk, vis l, a , t, ui)) | VarDecl l x _ <- vds
                                                                     , VarAnn (a, Just t) <- ann_fact l ]
                                  ++ [(ss x,(vdk, vis l, wg, t, ii)) | VarDecl l x _ <- vds
                                                                     , AmbVarAnn t <- ann_fact l ]
    -- The Assignabilities below are overwitten to default values
    vStmt _ (FunctionStmt l x _ _) = [(ss x,(fdk, vis l, ro, t, ii)) | VarAnn (_,Just t) <- ann_fact l ]
    vStmt _ (FuncAmbDecl l x _)    = [(ss x,(fak, vis l, id, t, ii)) | VarAnn (_,Just t) <- ann_fact l ]
    vStmt _ (FuncOverload l x _)   = [(ss x,(fok, vis l, id, t, ii)) | VarAnn (_,Just t) <- ann_fact l ]
    vStmt p (ClassStmt l x _ _ _)  = [(ss x,(cdk, vis l, ro, TClass  $ nameInPath l p x, ii)) ]
    vStmt p (ModuleStmt l x _)     = [(ss x,(mdk, vis l, ro, TModule $ pathInPath l p x, ii)) ]
    vStmt p (EnumStmt l x _)       = [(ss x,(edk, vis l, ro, TEnum   $ nameInPath l p x, ii)) ]
    vStmt _ _                      = []

    vdk  = VarDeclKind
    fdk  = FuncDefKind
    fak  = FuncAmbientKind
    fok  = FuncOverloadKind
    cdk  = ClassDefKind
    mdk  = ModuleDefKind
    edk  = EnumDefKind
    vis  = visibility
    wg   = WriteGlobal
    ro   = ReadOnly
    id   = ImportDecl
    ui   = Uninitialized
    ii   = Initialized

    tStmts                   = concatMapM . tStmt

    tStmt ap c@ClassStmt{}   = single <$> resolveType ap c
    tStmt ap c@IfaceStmt{}   = single <$> resolveType ap c
    tStmt _ _                = return []

    eStmts                   = concatMap eStmt

    eStmt (EnumStmt _ n es)  = [(fmap srcPos n, EnumDef (F.symbol n) (envFromList $ sEnumElt <$> es))]
    eStmt _                  = []
    sEnumElt (EnumElt _ s e) = (F.symbol s, fmap (const ()) e)
    ss                       = fmap ann


typeNames :: IsLocated a => AbsPath -> [ Statement a ] -> [ AbsName ]
typeNames (QP AK_ _ ss) = concatMap go
  where
    go (ClassStmt l x _ _ _) = [ QN AK_ (srcPos l) ss $ F.symbol x ]
    go (EnumStmt l x _ )     = [ QN AK_ (srcPos l) ss $ F.symbol x ]
    go (IfaceStmt l x )      = [ QN AK_ (srcPos l) ss $ F.symbol x ]
    go _                     = []


-- visibility :: Annot (Fact r) a -> Visibility
visibility l | ExportedElt `elem` ann_fact l = Exported
             | otherwise                     = Local


---------------------------------------------------------------------------------------
mkVarEnv :: PPR r => F.Symbolic s => [(s, VarInfo r)] -> Env (VarInfo r)
---------------------------------------------------------------------------------------
mkVarEnv                     = envFromListWithKey mergeVarInfo
                             . concatMap f . M.toList
                             . foldl merge M.empty
  where
    merge ms (x,(s,v,a,t,i)) = M.insertWith (++) (F.symbol x) [(s,v,a,t,i)] ms
    f (s, vs)   = [ (s,(k,v,w, g t [ t' | (FuncOverloadKind, _, _, t', _) <- vs ], i))
                                    | (k@FuncDefKind    , v, w, t, i) <- vs ] ++
              amb [ (s,(k,v,w,t,i)) | (k@FuncAmbientKind, v, w, t, i) <- vs ] ++
                  [ (s,(k,v,w,t,i)) | (k@VarDeclKind    , v, w, t, i) <- vs ] ++
                  [ (s,(k,v,w,t,i)) | (k@ClassDefKind   , v, w, t, i) <- vs ] ++
                  [ (s,(k,v,w,t,i)) | (k@ModuleDefKind  , v, w, t, i) <- vs ] ++
                  [ (s,(k,v,w,t,i)) | (k@EnumDefKind    , v, w, t, i) <- vs ]
    g t []                   = t
    g _ ts                   = mkAnd ts
    amb [ ]                  = [ ]
    amb [a]                  = [a]
    amb ((s,(k,v,w,t,i)):xs) = [(s,(k,v,w, mkAnd (t : map tyOf xs),i))]
    tyOf (_,(_,_,_,t,_))     = t

mergeVarInfo _ (ModuleDefKind, v1, a1, t1, i1) (ModuleDefKind, v2, a2, t2, i2)
  | (v1, a1, t1, i1) == (v2, a2, t2, i2) = (ModuleDefKind, v1, a1, t1, i1)
mergeVarInfo x _ _ = throw $ errorDuplicateKey (srcPos x) x

---------------------------------------------------------------------------------------
resolveType :: PPR r => AbsPath
                     -> Statement (AnnR r)
                     -> Either (F.FixResult Error) (Id SrcSpan, IfaceDef r)
---------------------------------------------------------------------------------------
resolveType (QP AK_ _ ss) (ClassStmt l c _ _ cs)
  | [(m:vs,e,i)]     <- classAnns
  = do  ts           <- typeMembers (TVar m fTop) cs
        return        $ (cc, ID (QN AK_ (srcPos l) ss (F.symbol c)) ClassKind (m:vs) (e,i) ts)
  | otherwise
  = Left              $ F.Unsafe [err (sourceSpanSrcSpan l) errMsg ]
  where
    classAnns         = [ t | ClassAnn t <- ann_fact l ]
    cc                = fmap ann c
    errMsg            = "Invalid class annotation: "
                     ++ show (intersperse comma (map pp classAnns))

resolveType _ (IfaceStmt l c)
  | [t] <- ifaceAnns  = Right (fmap ann c,t)
  | otherwise         = Left $ F.Unsafe [err (sourceSpanSrcSpan l) errMsg ]
  where
    ifaceAnns         = [ t | IfaceAnn t <- ann_fact l ]
    errMsg            = "Invalid interface annotation: "
                     ++ show (intersperse comma (map pp ifaceAnns))

resolveType _ s       = Left $ F.Unsafe $ single
                      $ err (sourceSpanSrcSpan $ getAnnotation s)
                      $ "Statement\n" ++ ppshow s ++ "\ncannot have a type annotation."

---------------------------------------------------------------------------------------
typeMembers :: PPR r => Mutability -> [ClassElt (AnnR r)] -> Either (F.FixResult Error) (TypeMembers r)
---------------------------------------------------------------------------------------
typeMembers dm                     = mkTypeMembers dm . concatMap go
  where
    go (MemberVarDecl l  s x _)    = [(l,ss x ,sk s,MemDefinition ,f) | FieldAnn f <- ann_fact l]
    go (MemberMethDef l  s x _ _ ) = [(l,ss x ,sk s,MemDefinition ,f) | MethAnn  f <- ann_fact l]
    go (MemberMethDecl l s x _ )   = [(l,ss x ,sk s,MemDeclaration,f) | MethAnn  f <- ann_fact l]
    go (Constructor l _ _)         = [(l,cs   ,im  ,MemDefinition ,a) | ConsAnn  a <- ann_fact l]

    sk True                        = StaticMember
    sk False                       = InstanceMember
    im                             = InstanceMember
    cs                             = ctorSymbol
    ss                             = F.symbol

---------------------------------------------------------------------------------------
mkTypeMembers :: (Eq q, IsLocated l, PPR r)
              => MutabilityQ q
              -> [(l,F.Symbol,StaticKind, MemberKind, TypeMemberQ q r)]
              -> Either (F.FixResult Error) (TypeMembersQ q r)
---------------------------------------------------------------------------------------
mkTypeMembers dm l0       = do m  <- foldM addTm M.empty l0
                               l  <- T.mapM (join . prtn) $ M.toList m
                               return $ fixMut $ M.fromList l
  where

    addTm ms (l,s,k,m,t)  | s == F.symbol t
                          = Right $ M.insertWith (++) (s,k) [Loc (srcPos l) (m,t)] ms
                          | otherwise
                          = Left  $ F.Unsafe $ single
                          $ err (sourceSpanSrcSpan l)
                          $ printf "Member '%s' does not match with annotation: %s"
                              (ppshow s) (ppshow t)

    prtn (k,v)            = (k,) . mapPair (map $ fmap snd)
                          $ partition ((== MemDefinition) . fst . val) v

    join (k,([t],[]))     = Right (k,val t)                   -- Single definition
    join (k,(ds ,ts))     | not (null ts)
                          = Right (k,foldl1 joinElts $ val <$> ts)
                          | otherwise
                          = Left  $ F.Unsafe
                          $ map (\(Loc l v) -> err (sourceSpanSrcSpan l) $ msg (fst k) v) $ ds ++ ts

    msg  k v              = printf "The following annotation for member '%s' is invalid:\n%s"
                              (ppshow k)  (ppshow v)

    fixMut                = M.map fixFldMut

    fixFldMut (FieldSig s o m t) | isInheritedMutability m = FieldSig s o dm t
    fixFldMut f                  = f

joinElts (CallSig t1)           (CallSig t2)         = CallSig           $ joinTys t1 t2
joinElts (ConsSig t1)           (ConsSig t2)         = ConsSig           $ joinTys t1 t2
joinElts (IndexSig x1 s1 t1)    (IndexSig _ _ t2)    = IndexSig x1 s1    $ joinTys t1 t2
joinElts (FieldSig x1 o1 m1 t1) (FieldSig _ _ m2 t2) | m1 == m2
                                                     = FieldSig x1 o1 m1 $ joinTys t1 t2
joinElts (MethSig x1 t1)        (MethSig _ t2)       = MethSig  x1       $ joinTys t1 t2
joinElts t                      _                    = t

joinTys t1 t2 = mkAnd $ bkAnd t1 ++ bkAnd t2


-- | `writeGlobalVars p` returns symbols that have `WriteMany` status, i.e. may be
--    re-assigned multiply in non-local scope, and hence
--    * cannot be SSA-ed
--    * cannot appear in refinements
--    * can only use a single monolithic type (declared or inferred)
-------------------------------------------------------------------------------
writeGlobalVars           :: Data r => [Statement (AnnType r)] -> [Id (AnnType r)]
-------------------------------------------------------------------------------
writeGlobalVars stmts      = everything (++) ([] `mkQ` fromVD) stmts
  where
    fromVD (VarDecl l x _) = [ x | VarAnn _ <- ann_fact l ] ++ [ x | AmbVarAnn _ <- ann_fact l ]


-- | scrapeVarDecl: Scrape a variable declaration for annotations
----------------------------------------------------------------------------------
scrapeVarDecl :: VarDecl (AnnSSA r) -> [(SyntaxKind, Assignability, Maybe (RType r))]
----------------------------------------------------------------------------------
scrapeVarDecl (VarDecl l _ _)
  = [ (VarDeclKind   , a       , t) | VarAnn (a, t) <- ann_fact l ]
 ++ [ (AmbVarDeclKind, ReadOnly, Just t) | AmbVarAnn t <- ann_fact l ]
 ++ [ (FieldDefKind  , ReadOnly, Just t) | FieldAnn (FieldSig _ _ _ t) <- ann_fact l ]
 -- The last Assignability value is dummy
