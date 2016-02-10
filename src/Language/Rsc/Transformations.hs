{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

module Language.Rsc.Transformations (
    Transformable (..), NameTransformable (..), AnnotTransformable (..)
  , transFmap, ntransFmap, ntransPure
  , emapReft, mapReftM, mapTypeMembersM
  , replaceDotRef, replaceAbsolute
  , fixFunBinders
  ) where

import           Control.Arrow                   ((***))
import           Control.Monad.State.Strict
import           Data.Default
import           Data.Functor.Identity
import           Data.Generics
import qualified Data.HashSet                    as HS
import qualified Data.IntMap.Strict              as I
import           Data.List                       (find)
import           Data.Text                       (pack, splitOn)
import qualified Data.Traversable                as T
import qualified Language.Fixpoint.Types         as F
import           Language.Fixpoint.Types.Errors
import           Language.Fixpoint.Types.Names   (suffixSymbol, symbolString)
import qualified Language.Fixpoint.Types.Visitor as FV
import           Language.Rsc.Annotations
import           Language.Rsc.AST
import           Language.Rsc.Core.Env
import           Language.Rsc.Errors
import           Language.Rsc.Locations
import           Language.Rsc.Misc
import           Language.Rsc.Module
import           Language.Rsc.Names
import           Language.Rsc.Pretty
import           Language.Rsc.Program
import           Language.Rsc.Symbols
import           Language.Rsc.Traversals
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
import           Language.Rsc.Visitor


--------------------------------------------------------------------------------
-- | Transformable
--------------------------------------------------------------------------------

class Transformable t where
  trans :: F.Reftable r => ([TVar] -> [BindQ q r] -> RTypeQ q r -> RTypeQ q r)
                        -> [TVar] -> [BindQ q r] -> t q r  -> t q r

instance Transformable RTypeQ where
  trans = transRType

instance Transformable BindQ where
  trans f αs xs b = b { b_type = trans f αs xs $ b_type b }

instance Transformable FactQ where
  trans = transFact

instance Transformable TypeDeclQ where
  trans f αs xs (TD s@(TS _ b _) es) = TD (trans f αs xs s) (trans f αs' xs es)
    where
      αs' = map btvToTV (b_args b) ++ αs

instance Transformable TypeSigQ where
  trans f αs xs (TS k b h) = TS k (trans f αs xs b) (transIFDBase f αs' xs h)
      where
        αs' = map btvToTV (b_args b) ++ αs

instance Transformable TypeMembersQ where
  trans f αs xs (TM m sm c k s n) =
    TM (fmap g m) (fmap g sm) (fmap g c) (fmap g k) (fmap (g *** g) s) (fmap (g *** g) n)
    where
      g = trans f αs xs

instance Transformable BTGenQ where
  trans f αs xs (BGen n ts) = BGen n $ trans f αs xs <$> ts

instance Transformable TGenQ where
  trans f αs xs (Gen n ts) = Gen n $ trans f αs xs <$> ts

instance Transformable BTVarQ where
  trans f αs xs (BTV x l c) = BTV x l $ trans f αs xs <$> c

instance Transformable TypeMemberQ where
  trans f αs xs (FI n o a t') = FI n o (trans f αs xs a) (trans f αs xs t')
  trans f αs xs (MI n o mts)  = MI n o (mapSnd (trans f αs xs) <$> mts)

instance Transformable ModuleDefQ where
  trans f αs xs (ModuleDef v t e p)
    = ModuleDef (envMap (trans f αs xs) v) (envMap (trans f αs xs) t) e p

instance Transformable SymInfoQ where
  trans f αs xs (SI x l a i t) = SI x l a i $ trans f αs xs t

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
    go αs xs (PhiVarTy (v,t))    = PhiVarTy      $ (v, trans f αs xs t)
    go αs xs (TypInst x y ts)    = TypInst x y   $ trans f αs xs <$> ts
    go αs xs (EltOverload x m t) = EltOverload x (trans f αs xs m) (trans f αs xs t)
    go αs xs (VarAnn l a t)      = VarAnn l a    $ trans f αs xs <$> t
    go αs xs (MemberAnn t)       = MemberAnn     $ trans f αs xs t
    go αs xs (CtorAnn t)         = CtorAnn       $ trans f αs xs t
    go αs xs (UserCast t)        = UserCast      $ trans f αs xs t
    go αs xs (SigAnn l t)        = SigAnn l      $ trans f αs xs t
    go αs xs (ClassAnn l ts)     = ClassAnn l    $ trans f αs xs ts
    go αs xs (InterfaceAnn td)   = InterfaceAnn  $ trans f αs xs td

    -- TODO: do we need to translate TypeCast?
    go _ _   t                   = t

-- | transRType :
--
--  Binds (αs and bs) accumulate on the left.
--
transRType :: F.Reftable r
           => ([TVar] -> [BindQ q r] -> RTypeQ q r -> RTypeQ q r)
           ->  [TVar] -> [BindQ q r] -> RTypeQ q r -> RTypeQ q r
transRType f               = go
  where
    go αs xs (TPrim c r)   = f αs xs $ TPrim c r
    go αs xs (TVar v r)    = f αs xs $ TVar v r
    go αs xs (TOr ts r)    = f αs xs $ TOr ts' r     where ts' = go αs xs <$> ts
    go αs xs (TAnd ts)     = f αs xs $ TAnd ts'      where ts' = mapSnd (go αs xs) <$> ts
    go αs xs (TRef n r)    = f αs xs $ TRef n' r     where n'  = trans f αs xs n
    go αs xs (TObj m ms r) = f αs xs $ TObj m' ms' r where m'  = trans f αs xs m
                                                           ms' = trans f αs xs ms
    go αs xs (TClass n)    = f αs xs $ TClass n'     where n'  = trans f αs xs n
    go αs xs (TMod m)      = f αs xs $ TMod m
    go αs xs (TAll a t)    = f αs xs $ TAll a t'     where t'  = go αs' xs t
                                                           αs' = αs ++ [btvToTV a]
    go αs xs (TFun bs t r) = f αs xs $ TFun bs' t' r where bs' = trans f αs xs' <$> bs
                                                           t'  = go αs xs' t
                                                           xs' = bs ++ xs
    go _  _  (TExp e)      = TExp e


--------------------------------------------------------------------------------
-- | Transform names
--------------------------------------------------------------------------------

ntransPure :: (NameTransformable t, F.Reftable r) => (QN p -> QN q) -> (QP p -> QP q) -> t p r -> t q r
ntransPure f g a = runIdentity (ntrans f' g' a)
  where g' = return . g
        f' = return . f

class NameTransformable t where
  ntrans :: (Monad m, Applicative m, F.Reftable r)
         => (QN p -> m (QN q)) -> (QP p -> m (QP q)) -> t p r  -> m (t q r)

instance NameTransformable RTypeQ where
  ntrans = ntransRType

instance NameTransformable BindQ where
  ntrans f g (B s o t) = B s o <$> ntrans f g t

instance NameTransformable FactQ where
  ntrans = ntransFact

instance NameTransformable TypeDeclQ where
  ntrans f g (TD s m) = TD <$> ntrans f g s <*> ntrans f g m

instance NameTransformable TypeSigQ where
  ntrans f g (TS k b (e,i))
    = TS k <$> ntrans f g b <*> liftM2 (,) (mapM (ntrans f g) e) (mapM (ntrans f g) i)

instance NameTransformable TypeMembersQ where
  ntrans f g (TM m sm c k s n) = TM <$> T.mapM h m
                                    <*> T.mapM h sm
                                    <*> T.mapM h c
                                    <*> T.mapM h k
                                    <*> T.mapM (\(m_, t_) -> (,) <$> h m_ <*> h t_) s
                                    <*> T.mapM (\(m_, t_) -> (,) <$> h m_ <*> h t_) n
    where
      h = ntrans f g

instance NameTransformable BTGenQ where
  ntrans f g (BGen n ts) = BGen <$> f n <*> mapM (ntrans f g) ts

instance NameTransformable TGenQ where
  ntrans f g (Gen n ts) = Gen <$> f n <*> mapM (ntrans f g) ts

instance NameTransformable BTVarQ where
  ntrans f g (BTV x l c) = BTV x l <$> T.mapM (ntrans f g) c

instance NameTransformable TypeMemberQ where
  ntrans f g (FI x o m t) = FI x o <$> ntrans f g m <*> ntrans f g t
  ntrans f g (MI x o mts) = MI x o <$> mapM (mapPairM (ntrans f g) (ntrans f g)) mts

ntransFmap ::  (F.Reftable r, Applicative m, Monad m, T.Traversable t)
           => (QN p -> m (QN q)) -> (QP p -> m (QP q)) -> t (FAnnQ p r) -> m (t (FAnnQ q r))
ntransFmap f g x = T.mapM (ntrans f g) x

ntransFact f g = go
  where
    go (PhiVar v)          = pure $ PhiVar v
    go (PhiVarTC v)        = pure $ PhiVarTC v
    go (PhiPost v)         = pure $ PhiPost v
    go (Overload x m i)    = pure $ Overload x m i
    go (EnumAnn e)         = pure $ EnumAnn e
    go (BypassUnique)      = pure $ BypassUnique
    go (DeadCast x es)     = pure $ DeadCast x es
    go (TypeCast x t)      = pure $ TypeCast x t -- TODO: transform this?
    go (ModuleAnn l m)     = ModuleAnn l   <$> g m
    go (PhiVarTy (v,t))    = PhiVarTy      <$> (v,) <$>  ntrans f g t
    go (TypInst x y ts)    = TypInst x y   <$> mapM (ntrans f g) ts
    go (EltOverload x m t) = EltOverload x <$> ntrans f g m <*> ntrans f g t
    go (VarAnn l a t)      = VarAnn l a    <$> T.mapM (ntrans f g) t
    go (MemberAnn t)       = MemberAnn     <$> ntrans f g t
    go (CtorAnn t)         = CtorAnn       <$> ntrans f g t
    go (UserCast t)        = UserCast      <$> ntrans f g t
    go (SigAnn l t)        = SigAnn l      <$> ntrans f g t
    go (ClassAnn l t)      = ClassAnn l    <$> ntrans f g t
    go (InterfaceAnn t)    = InterfaceAnn  <$> ntrans f g t


ntransRType :: (Monad m, Applicative m, F.Reftable r)
            => (QN p -> m (QN q)) -> (QP p -> m (QP q)) -> RTypeQ p r -> m (RTypeQ q r)
ntransRType f g t    = go t
  where
    go (TPrim p r)   = pure $ TPrim p r
    go (TVar v r)    = pure $ TVar v r
    go (TExp e)      = pure $ TExp e
    go (TOr ts r)    = TOr  <$> ts'
                            <*> pure r    where ts' = mapM go ts
    go (TAnd ts)     = TAnd <$> ts'       where ts' = mapM (mapSndM go) ts
    go (TRef n r)    = TRef <$> n'
                            <*> pure r    where n'  = ntrans f g n
    go (TObj m ms r) = TObj <$> m'
                            <*> ms'
                            <*> pure r    where m'  = ntrans f g m
                                                ms' = ntrans f g ms
    go (TClass n)    = TClass <$> n'      where n'  = ntrans f g n
    go (TMod p)      = TMod   <$> p'      where p'  = g p
    go (TAll a t)    = TAll   <$> a'
                              <*> t'      where a'  = ntrans f g a
                                                t'  = go t
    go (TFun bs t r) = TFun   <$> bs'
                              <*> t'
                              <*> pure r  where bs' = mapM (ntrans f g) bs
                                                t'  = go t

instance NameTransformable FAnnQ where
  ntrans f g (FA i s ys) = FA i s <$> mapM (ntrans f g) ys


--------------------------------------------------------------------------------
-- | Transformers over @RType@
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
emapReft  :: PPR r => ([F.Symbol] -> r -> r') -> [F.Symbol] -> RTypeQ q r -> RTypeQ q r'
--------------------------------------------------------------------------------
emapReft f γ (TVar α r)     = TVar α (f γ r)
emapReft f γ (TPrim c r)    = TPrim c (f γ r)
emapReft f γ (TRef n r)     = TRef (emapReftGen f γ n) (f γ r)
emapReft f γ (TAll α t)     = TAll (emapReftBTV f γ α) (emapReft f γ t)
emapReft f γ (TFun xts t r) = TFun (emapReftBind f γ' <$> xts)
                                   (emapReft f γ' t) (f γ r)
                              where γ' = (b_sym <$> xts) ++ γ
emapReft f γ (TObj m xts r) = TObj (emapReft f γ m) (emapReftTM f γ xts) (f γ r)
emapReft f γ (TClass n)     = TClass (emapReftBGen f γ n)
emapReft _ _ (TMod m)       = TMod m
emapReft f γ (TOr ts r)     = TOr (emapReft f γ <$> ts) (f γ r)
emapReft f γ (TAnd ts)      = TAnd (mapSnd (emapReft f γ) <$> ts)
emapReft _ _ _              = error "Not supported in emapReft"

emapReftBTV f γ (BTV s l c) = BTV s l $ emapReft f γ <$> c
emapReftGen f γ (Gen n ts)  = Gen n $ emapReft f γ <$> ts
emapReftBGen f γ (BGen n ts) = BGen n $ emapReftBTV f γ <$> ts
emapReftBind f γ (B x o t)  = B x o $ emapReft f γ t
emapReftTM f γ (TM m sm c k s n)
  = TM (fmap (emapReftElt f γ) m)
       (fmap (emapReftElt f γ) sm)
       (emapReft f γ <$> c)
       (emapReft f γ <$> k)
       ((emapReft f γ *** emapReft f γ) <$> s)
       ((emapReft f γ *** emapReft f γ) <$> n)

emapReftElt f γ (FI x m a t) = FI x m (emapReft f γ a) (emapReft f γ t)
emapReftElt f γ (MI x m mts) = MI x m (mapPair (emapReft f γ) <$> mts)

--------------------------------------------------------------------------------
mapReftM :: (F.Reftable r, PP r, Applicative m, Monad m)
         => (r -> m r') -> RTypeQ q r -> m (RTypeQ q r')
--------------------------------------------------------------------------------
mapReftM f (TVar α r)      = TVar α  <$> f r
mapReftM f (TPrim c r)     = TPrim c <$> f r
mapReftM f (TRef n r)      = TRef    <$> mapReftGenM f n <*> f r
mapReftM f (TFun xts t r)  = TFun    <$> mapM (mapReftBindM f) xts <*> mapReftM f t <*> f r
mapReftM f (TAll α t)      = TAll    <$> mapReftBTV f α <*> mapReftM f t
mapReftM f (TAnd ts)       = TAnd    <$> mapM (mapSndM (mapReftM f)) ts
mapReftM f (TOr ts r)      = TOr     <$> mapM (mapReftM f) ts <*> f r
mapReftM f (TObj m xts r)  = TObj    <$> mapReftM f m
                                     <*> mapTypeMembers f xts <*> f r
mapReftM f (TClass n)      = TClass  <$> mapReftBGenM f n
mapReftM _ (TMod a)        = TMod    <$> pure a
mapReftM _ t               = error $ "Not supported in mapReftM: " ++ ppshow t

mapReftBTV   f (BTV s l c) = BTV s l <$> T.mapM (mapReftM f)   c
mapReftGenM  f (Gen n ts)  = Gen n   <$>   mapM (mapReftM f)   ts
mapReftBGenM f (BGen n ts) = BGen n  <$>   mapM (mapReftBTV f) ts
mapReftBindM f (B x o t)   = B x o   <$>         mapReftM f    t

mapTypeMembers f (TM m sm c k s n)
  = TM <$> T.mapM (mapReftElt f) m
       <*> T.mapM (mapReftElt f) sm
       <*> T.mapM (mapReftM f) c
       <*> T.mapM (mapReftM f) k
       <*> T.mapM (\(m_,t_) -> (,) <$> mapReftM f m_ <*> mapReftM f t_) s
       <*> T.mapM (\(m_,t_) -> (,) <$> mapReftM f m_ <*> mapReftM f t_) n

mapReftElt f (FI x m a t) = FI x m <$> mapReftM f a <*> mapReftM f t
mapReftElt f (MI x m mts) = MI x m <$> mapM (mapPairM (mapReftM f) (mapReftM f)) mts

--------------------------------------------------------------------------------
mapTypeMembersM :: (Applicative m, Monad m)
                => (RType r -> m (RType r)) -> TypeMembers r -> m (TypeMembers r)
--------------------------------------------------------------------------------
mapTypeMembersM f (TM m sm c k s n)
  = TM <$> T.mapM memMapM m
       <*> T.mapM memMapM sm
       <*> T.mapM f c
       <*> T.mapM f k
       <*> T.mapM (\(m_, t_) -> (,) <$> f m_ <*> f t_) s
       <*> T.mapM (\(m_, t_) -> (,) <$> f m_ <*> f t_) n
  where
    memMapM (FI x o a t) = FI x o <$> f a <*> f t
    memMapM (MI x o mts) = MI x o <$> mapM (mapSndM f) mts


--------------------------------------------------------------------------------
-- | Replace all relatively qualified names/paths with absolute ones.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
replaceAbsolute :: (PPR r, Data r, Typeable r) => BareRelRsc r -> (BareRsc r, [Error])
--------------------------------------------------------------------------------
replaceAbsolute pgm@(Rsc { code = Src ss }) = (pgm { code = Src ss' }, sOut)
  where
    (ss', sOut)     = runState (mapM (T.mapM (\l -> ntrans (safeAbsName l) (safeAbsPath l) l)) ss) []
    (ns, ps)        = accumNamesAndPaths ss

    safeAbsName l a@(absAct (absoluteName ns) l -> n)
      | Just a' <- n = return a'
      | Nothing <- n
      , isAlias a    = return $ toAbsoluteName a
      | otherwise    = modify (errorUnboundName (srcPos l) a:) >> pure (mkAbsName [] a)

    safeAbsPath l a@(absAct (absolutePath ps) l -> n)
      | Just a' <- n = return a'
      | otherwise    = modify (errorUnboundPath (srcPos l) a:) >> pure (mkAbsPath [])

    isAlias (QN (QP RK_ _ []) s) = envMem s $ tAlias pgm
    isAlias (QN _ _) = False

    absAct f l a    = I.lookup (fId l) mm >>= (`f` a)

    mm              = snd $ visitStmts vs (QP AK_ def []) ss
    vs              = defaultVisitor { ctxStmt = cStmt }
                                     { accStmt = acc   }
                                     { accExpr = acc   }
                                     { accCElt = acc   }
                                     { accVDec = acc   }
    cStmt (QP AK_ l p) (ModuleStmt _ x _)
                    = QP AK_ l $ p ++ [F.symbol x]
    cStmt q _       = q
    acc c s         = I.singleton (fId a) c where a = getAnnotation s


--------------------------------------------------------------------------------
-- | Replace `a.b.c...z` with `offset(offset(...(offset(a),"b"),"c"),...,"z")`
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
replaceDotRef :: RefScript -> RefScript
--------------------------------------------------------------------------------
replaceDotRef p@(Rsc { code = Src fs, tAlias = ta, pAlias = pa, invts = is })
    = p { code         = Src $ tf       <##>  fs
        , tAlias       = trans tt [] [] <###> ta
        , pAlias       =       tt [] [] <##>  pa
        , invts        = trans tt [] [] <##>  is
        }
  where
    tf (FA l a facts) = FA l a $ trans tt [] [] <$> facts
    tt _ _            = fmap $ FV.trans vs () ()

    vs                = FV.defaultVisitor { FV.txExpr = tx }
    tx _ (F.EVar s)   | (x:y:zs) <- pack "." `splitOn` pack (symbolString s)
                      = foldl offset (F.eVar x) (y:zs)
    tx _ e            = e
    offset k v        = F.mkEApp offsetLocSym [F.expr k, F.expr v]


--
-- XXX: Treat this at lookup
--
-- --------------------------------------------------------------------------------
-- -- | Replace `TRef x _ _` where `x` is a name for an enumeration with `number`
-- --------------------------------------------------------------------------------
--
-- --------------------------------------------------------------------------------
-- fixEnums :: PPR r => QEnv (ModuleDef r) -> BareRsc r -> (QEnv (ModuleDef r), BareRsc r)
-- --------------------------------------------------------------------------------
-- fixEnums m p@(Rsc { code = Src ss }) = (m',p')
--   where
--     p'    = p { code = Src $ (trans f [] [] <$>) <$> ss }
--     m'    = fixEnumsInModule m `qenvMap` m
--     f _ _ = fixEnumInType m
--
-- fixEnumInType :: F.Reftable r => QEnv (ModuleDef r) -> RType r -> RType r
-- fixEnumInType ms (TRef (Gen (QN p x) []) r)
--   | Just m <- qenvFindTy p ms
--   , Just e <- envFindTy x $ m_enums m
--   = if isBvEnum e then tBV32 `strengthen` r
--                   else tNum  `strengthen` r
-- fixEnumInType _ t = t
--
-- fixEnumsInModule :: F.Reftable r => QEnv (ModuleDef r) -> ModuleDef r -> ModuleDef r
-- fixEnumsInModule m = trans (const $ const $ fixEnumInType m) [] []
--

--------------------------------------------------------------------------------
-- | Add a '#' at the end of every function binder (to avoid capture)
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
fixFunBinders :: RefScript -> RefScript
--------------------------------------------------------------------------------
fixFunBinders p@(Rsc { code = Src ss }) = p'
  where
    p' = p { code = Src $ (trans fixFunBindersInType [] [] <$>) <$> ss }

fixFunBindersInType _ bs = go
  where
    ks               = [ y | B y _ _ <- bs ]
    ss               = (`suffixSymbol` F.symbol "")
    ks'              = map (F.eVar . ss) ks
    sub             :: F.Subable a => a -> a
    sub              = F.subst (F.mkSubst (zip ks ks'))

    go (TFun bs t r) = TFun [B (ss s) o ts | B s o ts <- bs] t (sub r)
    go t             = toplevel sub t

-- When costructing the substitution haskmap, if the list contains duplicate
-- mappings, the later mappings take precedence.


--------------------------------------------------------------------------------
-- | Spec Transformer
--------------------------------------------------------------------------------

class AnnotTransformable t where
  strans :: (ctx -> a -> b) -> (ctx -> b -> ctx) -> ctx -> t a -> t b

instance AnnotTransformable Statement where
  strans = stransStatement

instance AnnotTransformable Expression where
  strans = stransExpression

instance AnnotTransformable Id where
  strans = stransId

instance AnnotTransformable ForInInit where
  strans = stransForInInit

instance AnnotTransformable ForInit where
  strans = stransForInit

instance AnnotTransformable CatchClause where
  strans = stransCatchClause

instance AnnotTransformable VarDecl where
  strans = stransVarDecl

instance AnnotTransformable ClassElt where
  strans = stransClassElt

instance AnnotTransformable EnumElt where
  strans = stransEnumElt

instance AnnotTransformable Prop where
  strans = stransProp

instance AnnotTransformable LValue where
  strans = stransLvalue

stransStatement f g ctx st = go st
  where
    a    = getAnnotation st
    b    = f ctx a
    ctx' = g ctx b
    ss   = strans f g ctx'
    go (BlockStmt _ sts)         = BlockStmt b (ss <$> sts)
    go (EmptyStmt _)             = EmptyStmt b
    go (ExprStmt _ e)            = ExprStmt b (ss e)
    go (IfStmt _ e s1 s2)        = IfStmt b (ss e) (ss s1) (ss s2)
    go (IfSingleStmt _ e s)      = IfSingleStmt b (ss e) (ss s)
    go (WhileStmt _ e s)         = WhileStmt b (ss e) (ss s)
    go (DoWhileStmt _ s e)       = DoWhileStmt b (ss s) (ss e)
    go (BreakStmt _ i)           = BreakStmt b (ss <$> i)
    go (ContinueStmt _ i)        = ContinueStmt b (ss <$> i)
    go (LabelledStmt _ i s)      = LabelledStmt b (ss i) (ss s)
    go (ForInStmt _ fi e s)      = ForInStmt b (ss fi) (ss e) (ss s)
    go (ForStmt _ fi me1 me2 s)  = ForStmt b (ss fi) (ss <$> me1) (ss <$> me2) (ss s)
    go (TryStmt _ s mcc ms)      = TryStmt b (ss s) (ss <$> mcc) (ss <$> ms)
    go (ThrowStmt _ e)           = ThrowStmt b (ss e)
    go (ReturnStmt _ me)         = ReturnStmt b (ss <$> me)
    go (WithStmt _ e s)          = WithStmt b (ss e) (ss s)
    go (VarDeclStmt _ vs)        = VarDeclStmt b (ss <$> vs)
    go (FunctionStmt _ i is mss) = FunctionStmt b (ss i) (ss <$> is) ((ss <$>) <$> mss)
    go (ClassStmt _ i cs)        = ClassStmt b (ss i) (ss <$> cs)
    go (ModuleStmt _ i sts)      = ModuleStmt b (ss i) (ss <$> sts)
    go (InterfaceStmt _ i)       = InterfaceStmt b (ss i)
    go (EnumStmt _ i es)         = EnumStmt b (ss i) (ss <$> es)
    go s                         = error $ "[unimplemented] stransStatement for " ++ ppshow s

stransExpression f g ctx exp = go exp
  where
    a    = getAnnotation exp
    b    = f ctx a
    ctx' = g ctx b
    ss   = strans f g ctx'
    go (StringLit _ s)          = StringLit b s
    go (RegexpLit _ s b1 b2)    = RegexpLit b s b1 b2
    go (NumLit _ d)             = NumLit b d
    go (IntLit _ i)             = IntLit b i
    go (BoolLit _ bl)           = BoolLit b bl
    go (NullLit _)              = NullLit b
    go (ArrayLit _ es)          = ArrayLit b (ss <$> es)
    go (ObjectLit _ pes)        = ObjectLit b ((\(p,e) -> (ss p, ss e)) <$> pes)
    go (HexLit _ s)             = HexLit b s
    go (ThisRef _)              = ThisRef b
    go (VarRef _ i)             = VarRef b (ss i)
    go (DotRef _ e i)           = DotRef b (ss e) (ss i)
    go (BracketRef _ e1 e2)     = BracketRef b (ss e1) (ss e2)
    go (NewExpr _ e es)         = NewExpr b (ss e) (ss <$> es)
    go (PrefixExpr _ op e)      = PrefixExpr b op (ss e)
    go (UnaryAssignExpr _ op l) = UnaryAssignExpr b op (ss l)
    go (InfixExpr _ op e1 e2)   = InfixExpr b op (ss e1) (ss e2)
    go (CondExpr _ e1 e2 e3)    = CondExpr b (ss e1) (ss e2) (ss e3)
    go (AssignExpr _ op l e)    = AssignExpr b op (ss l) (ss e)
    go (ListExpr _ es)          = ListExpr b (ss <$> es)
    go (CallExpr _ e es)        = CallExpr b (ss e) (ss <$> es)
    go (SuperRef _)             = SuperRef b
    go (FuncExpr _ mi is sts)   = FuncExpr b (ss <$> mi) (ss <$> is) (ss <$> sts)
    go (Cast _ e)               = Cast b (ss e)
    go (Cast_ _ e)              = Cast_ b (ss e)

stransId f _ ctx (Id a s) = Id (f ctx a) s

stransForInInit f g ctx (ForInVar i)  = ForInVar (strans f g ctx i)
stransForInInit f g ctx (ForInLVal i) = ForInLVal (strans f g ctx i)

stransForInit _ _ _   NoInit        = NoInit
stransForInit f g ctx (VarInit vs)  = VarInit (strans f g ctx <$> vs)
stransForInit f g ctx (ExprInit e)  = ExprInit (strans f g ctx e)

stransCatchClause f g ctx (CatchClause a i s) = CatchClause b (ss i) (ss s)
  where
    b    = f ctx a
    ctx' = g ctx b
    ss   = strans f g ctx'

stransVarDecl f g ctx (VarDecl a i me) = VarDecl b (ss i) (ss <$> me)
  where
    b    = f ctx a
    ctx' = g ctx b
    ss   = strans f g ctx'

stransClassElt f g ctx ce = go ce
  where
    a    = getAnnotation ce
    b    = f ctx a
    ctx' = g ctx b
    ss   = strans f g ctx'
    go (Constructor    _ is sts)      = Constructor b (ss <$> is) (ss <$> sts)
    go (MemberVarDecl  _ st i me)     = MemberVarDecl b st (ss i) (ss <$> me)
    go (MemberMethDecl _ st i is sts) = MemberMethDecl b st (ss i) (ss <$> is) (ss <$> sts)

stransEnumElt f g ctx (EnumElt a i e) = EnumElt b (ss i) (ss e)
  where
    b    = f ctx a
    ctx' = g ctx b
    ss   = strans f g ctx'

stransProp f g ctx p = go p
  where
    a    = getAnnotation p
    b    = f ctx a
    ctx' = g ctx b
    ss   = strans f g ctx'
    go (PropId _ i)     = PropId b (ss i)
    go (PropString _ s) = PropString b s
    go (PropNum _ i)    = PropNum b i

stransLvalue f g ctx lv = go lv
  where
    a    = getAnnotation lv
    b    = f ctx a
    ctx' = g ctx b
    ss   = strans f g ctx'
    go (LVar _ s)         = LVar b s
    go (LDot _ e s)       = LDot b (ss e) s
    go (LBracket _ e1 e2) = LBracket b (ss e1) (ss e2)


--------------------------------------------------------------------------
-- | Name transformation
--------------------------------------------------------------------------

-- | `absoluteName env p r` returns `Just a` where `a` is the absolute path of
--   the relative name `r` when referenced in the context of the absolute path
--   `p`; `Nothing` otherwise.
--
--   If p = A.B.C and r = C.D.E then the paths that will be checked in this
--   order are:
--
--    A.B.C.C.D.E
--    A.B.C.D.E
--    A.C.D.E
--    C.D.E
--
---------------------------------------------------------------------------------
absoluteName :: HS.HashSet AbsName -> AbsPath -> RelName -> Maybe AbsName
---------------------------------------------------------------------------------
absoluteName ns (QP AK_ _ p) (QN (QP RK_ _ ss) s) =
    find (`HS.member` ns) $ (`mkAbsName` s) . (++ ss) <$> prefixes p
  where
    prefixes        = map reverse . suffixes . reverse
    suffixes []     = [[]]
    suffixes (x:xs) = (x:xs) : suffixes xs

---------------------------------------------------------------------------------
absolutePath :: HS.HashSet AbsPath -> AbsPath -> RelPath -> Maybe AbsPath
---------------------------------------------------------------------------------
absolutePath ps (QP AK_ _ p) (QP RK_ _ ss) =
    find (`HS.member` ps) $ mkAbsPath . (++ ss) <$> prefixes p
  where
    prefixes        = map reverse . suffixes . reverse
    suffixes []     = [[]]
    suffixes (x:xs) = (x:xs) : suffixes xs

toAbsoluteName (QN (QP RK_ l ss) s) = QN (QP AK_ l ss) s
