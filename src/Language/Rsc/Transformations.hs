{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Language.Rsc.Transformations (

    Transformable (..), NameTransformable (..), AnnotTransformable (..)
  , transFmap, ntransFmap

  , emapReft, mapReftM, mapTypeMembersM

  -- , convertTVar, convertTVars
  , replaceDotRef, replaceAbsolute

  , fixFunBinders

  ) where

import           Control.Applicative          hiding (empty)
import           Control.Exception            (throw)
import           Data.Default
import           Data.Generics
import qualified Data.HashSet                 as HS
import qualified Data.IntMap.Strict           as I
import           Data.Maybe                   (fromMaybe)
import           Data.Maybe                   (listToMaybe)
import           Data.Monoid                  hiding ((<>))
import           Data.Text                    (pack, splitOn)
import qualified Data.Traversable             as T
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc       (mapSnd)
import           Language.Fixpoint.Names      (symSepName)
import qualified Language.Fixpoint.Types      as F
import qualified Language.Fixpoint.Visitor    as FV
import           Language.Rsc.Annotations
import           Language.Rsc.AST
import           Language.Rsc.Core.Env
import           Language.Rsc.Errors
import           Language.Rsc.Locations
import           Language.Rsc.Misc
import           Language.Rsc.Names
import           Language.Rsc.Pretty
import           Language.Rsc.Program
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
  trans f αs xs (MI o mts) = MI o (mapSnd (trans f αs xs) <$> mts)

instance Transformable ModuleDefQ where
  trans f αs xs (ModuleDef v t e p)
    = ModuleDef (envMap (trans f αs xs) v) (envMap (trans f αs xs) t) e p

instance Transformable VarInfoQ where
  trans f αs xs (VI l a i t) = VI l a i $ trans f αs xs t

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

    go αs xs (VarAnn l a t)    = VarAnn l a    $ trans f αs xs <$> t

    go αs xs (FieldAnn t)      = FieldAnn      $ trans f αs xs t
    go αs xs (MethAnn t)       = MethAnn       $ trans f αs xs t
    go αs xs (CtorAnn t)       = CtorAnn       $ trans f αs xs t

    go αs xs (UserCast t)      = UserCast      $ trans f αs xs t
    go αs xs (SigAnn l t)      = SigAnn l      $ trans f αs xs t
    go αs xs (TCast x c)       = TCast x       $ trans f αs xs c

    go αs xs (ClassAnn l ts)   = ClassAnn l    $ trans f αs xs ts
    go αs xs (InterfaceAnn td) = InterfaceAnn  $ trans f αs xs td

    go _ _   t                 = t

transCast f = go
  where
    go _  _ CNo          = CNo
    go αs xs (CDead e t) = CDead e $ trans f αs xs t
    go αs xs (CUp t1 t2) = CUp (trans f αs xs t1) (trans f αs xs t2)
    go αs xs (CDn t1 t2) = CUp (trans f αs xs t1) (trans f αs xs t2)

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
    go αs xs (TOr ts)      = f αs xs $ TOr ts'       where ts' = go αs xs <$> ts
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
  ntrans f g (MI o mts)  = MI o (mapSnd (ntrans f g) <$> mts)

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
    go (Overload x i)    = Overload x i
    go (VarAnn l a t)    = VarAnn l a    $ ntrans f g <$> t
    go (FieldAnn t)      = FieldAnn      $ ntrans f g t
    go (MethAnn t)       = MethAnn       $ ntrans f g t
    go (CtorAnn t)       = CtorAnn       $ ntrans f g t
    go (UserCast t)      = UserCast      $ ntrans f g t
    go (SigAnn l t)      = SigAnn l      $ ntrans f g t
    go (TCast x c)       = TCast x       $ ntrans f g c
    go (ClassAnn l t)    = ClassAnn l    $ ntrans f g t
    go (InterfaceAnn t)  = InterfaceAnn  $ ntrans f g t
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
    go (TAnd ts)     = TAnd ts'       where ts' = mapSnd go <$> ts
    go (TRef n r)    = TRef n' r      where n'  = ntrans f g n
    go (TObj m ms r) = TObj m' ms' r  where m'  = ntrans f g m
                                            ms' = ntrans f g ms
    go (TClass n)    = TClass n'      where n'  = ntrans f g n
    go (TMod p)      = TMod p'        where p'  = g p
    go (TAll a t)    = TAll a' t'     where a'  = ntrans f g a
                                            t'  = go t
    go (TFun bs t r) = TFun bs' t' r  where bs' = ntrans f g <$> bs
                                            t'  = go t
    go (TExp e)      = TExp e

instance NameTransformable FAnnQ where
  ntrans f g (FA i s ys) = FA i s $ ntrans f g <$> ys


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
emapReft f γ (TOr ts)       = TOr (emapReft f γ <$> ts)
emapReft f γ (TAnd ts)      = TAnd (mapSnd (emapReft f γ) <$> ts)
emapReft _ _ _              = error "Not supported in emapReft"

emapReftBTV f γ (BTV s l c) = BTV s l $ emapReft f γ <$> c
emapReftGen f γ (Gen n ts)  = Gen n $ emapReft f γ <$> ts
emapReftBGen f γ (BGen n ts) = BGen n $ emapReftBTV f γ <$> ts
emapReftBind f γ (B x t)    = B x $ emapReft f γ t
emapReftTM f γ (TM p m sp sm c k s n)
  = TM (fmap (emapReftFI f γ) p)
       (fmap (emapReftMI f γ) m)
       (fmap (emapReftFI f γ) sp)
       (fmap (emapReftMI f γ) sm)
       (emapReft f γ <$> c)
       (emapReft f γ <$> k)
       (emapReft f γ <$> s)
       (emapReft f γ <$> n)

emapReftFI f γ (FI m t1 t2) = FI m (emapReft f γ t1) (emapReft f γ t2)
emapReftMI f γ (MI m mts  ) = MI m (mapSnd (emapReft f γ) <$> mts)

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
mapReftM f (TOr ts)        = TOr     <$> mapM (mapReftM f) ts
mapReftM f (TObj m xts r)  = TObj    <$> mapReftM f m <*> mapTypeMembers f xts <*> f r
mapReftM f (TClass n)      = TClass  <$> mapReftBGenM f n
mapReftM _ (TMod a)        = TMod    <$> pure a
mapReftM _ t               = error $ "Not supported in mapReftM: " ++ ppshow t

mapReftBTV   f (BTV s l c) = BTV s l <$> T.mapM (mapReftM f)   c
mapReftGenM  f (Gen n ts)  = Gen n   <$>   mapM (mapReftM f)   ts
mapReftBGenM f (BGen n ts) = BGen n  <$>   mapM (mapReftBTV f) ts
mapReftBindM f (B x t)     = B x     <$>         mapReftM f    t

mapTypeMembers f (TM p m sp sm c k s n)
  = TM <$> T.mapM (mapReftFI f) p
       <*> T.mapM (mapReftMI f) m
       <*> T.mapM (mapReftFI f) sp
       <*> T.mapM (mapReftMI f) sm
       <*> T.mapM (mapReftM f) c
       <*> T.mapM (mapReftM f) k
       <*> T.mapM (mapReftM f) s
       <*> T.mapM (mapReftM f) n

mapReftFI f (FI m t1 t2) = FI m <$> mapReftM f t1 <*> mapReftM f t2
mapReftMI f (MI m mts  ) = MI m <$> mapM (mapSndM (mapReftM f)) mts

--------------------------------------------------------------------------------
mapTypeMembersM :: (Applicative m, Monad m)
                => (RType r -> m (RType r)) -> TypeMembers r -> m (TypeMembers r)
--------------------------------------------------------------------------------
mapTypeMembersM f (TM p m sp sm c k s n)
  = TM <$> T.mapM (mapFieldInfoM f) p
       <*> T.mapM (mapMethInfoM f) m
       <*> T.mapM (mapFieldInfoM f) sp
       <*> T.mapM (mapMethInfoM f) sm
       <*> T.mapM f c
       <*> T.mapM f k
       <*> T.mapM f s
       <*> T.mapM f n

mapFieldInfoM f (FI o m t) = FI o <$> f m <*> f t
mapMethInfoM  f (MI o mts) = MI o <$> mapM (mapSndM f) mts



-- --------------------------------------------------------------------------------
-- -- | Convert bound TRefs to TVars
-- --------------------------------------------------------------------------------
--
-- --------------------------------------------------------------------------------
-- convertTVars :: (PP r, F.Reftable r) => BareRelRsc r -> BareRelRsc r
-- --------------------------------------------------------------------------------
-- convertTVars = visitRsc convertTvarVisitor []
--
-- --------------------------------------------------------------------------------
-- convertTVar    :: (PP r, F.Reftable r, Transformable t) => [TVar] -> t q r -> t q r
-- --------------------------------------------------------------------------------
-- convertTVar as = trans tx as []
--   where
--     tx αs _ t@(TRef (Gen c []) r) | Just α <- mkTvar αs c = TVar α r
--     tx _  _ t = t
--
-- mkTvar :: (IsLocated a, F.Symbolic a) => [TVar] -> a -> Maybe TVar
-- mkTvar αs r = listToMaybe [ α { tv_loc = srcPos r }  | α <- αs, F.symbol α == F.symbol r]
--
-- --------------------------------------------------------------------------------
-- convertTvarVisitor :: (PP r, F.Reftable r) => Visitor () [TVar] (AnnRel r)
-- --------------------------------------------------------------------------------
-- convertTvarVisitor = defaultVisitor {
--     ctxStmt = ctxStmtTvar
--   , ctxCElt = ctxCEltTvar
--   , txStmt  = transFmap (const . convertTVar)
--   , txExpr  = transFmap (const . convertTVar)
--   , txCElt  = transFmap (const . convertTVar)
--   }
--
-- ctxStmtTvar as s = go s ++ as
--   where
--     go :: Statement (AnnRel r)  -> [TVar]
--     go s@(FunctionStmt {}) = grab s
--     go s@(InterfaceStmt {})    = grab s
--     go s@(ClassStmt {})    = grab s
--     go s@(ModuleStmt {})   = grab s
--     go _                   = []
--
--     grab :: Statement (FAnnQ q r) -> [TVar]
--     grab = concatMap factTVars . fFact . getAnnotation
--
-- ctxCEltTvar as s = go s ++ as
--   where
--     go :: ClassElt (AnnRel r)  -> [TVar]
--     go s@Constructor{}    = grab s
--     go s@MemberMethDecl{} = grab s
--     go _                  = []
--
--     grab :: ClassElt (FAnnQ q r) -> [TVar]
--     grab = concatMap factTVars . fFact . getAnnotation
--
-- --------------------------------------------------------------------------------
-- factTVars :: FactQ q r -> [TVar]
-- --------------------------------------------------------------------------------
-- factTVars = go
--   where
--     tvars t | Just ts <- bkFuns t
--             = HS.toList $ foldUnions
--             $ map HS.fromList [ btvToTV <$> t | (t, _, _) <- ts ]
--             | otherwise
--             = []
--
--     foldUnions (α:αs) = foldl HS.intersection α αs
--     foldUnions _      = HS.empty
--
--     go (VarAnn _ _ (Just t)) = tvars t
--     go (SigAnn _ t)          = tvars t
--     go (FieldAnn (FI _ _ t)) = tvars t
--     go (MethAnn (MI _ mts))  = concatMap (tvars . snd) mts
--     go (CtorAnn t)           = tvars t
--     go (ClassAnn _ sig)      = btvToTV <$> b_args (sigTRef sig)
--     go (InterfaceAnn d)      = btvToTV <$> b_args (sigTRef (typeSig d))
--     go _                     = []


--------------------------------------------------------------------------------
-- | Replace all relatively qualified names/paths with absolute ones.
--------------------------------------------------------------------------------

-- instance NameTransformable FRsc where
--   ntrans f g (FRsc (Rsc (Src ss ) cst ta pa pq inv max opt)) =
--               FRsc (Rsc (Src ss') cst ta pa pq inv max opt)
--     where
--       ss'   = (ntrans f g <$>) <$> ss
--

--------------------------------------------------------------------------------
replaceAbsolute :: (PPR r, Data r, Typeable r) => BareRelRsc r -> BareRsc r
--------------------------------------------------------------------------------
replaceAbsolute pgm@(Rsc { code = Src ss }) = pgm { code = Src $ (tr <$>) <$> ss }
  where
    (ns, ps)        = accumNamesAndPaths ss
    tr l            = ntrans (safeAbsName l) (safeAbsPath l) l
    safeAbsName l a = case absAct (absoluteName ns) l a of
                        Just a' -> a'
                        -- If it's a type alias, don't throw error
                        Nothing | isAlias a -> toAbsoluteName a
                                | otherwise -> diePP $ errorUnboundName (srcPos l) a
    safeAbsPath l a = fromMaybe (throw $ errorUnboundPath (srcPos l) a)
                                (absAct (absolutePath ps) l a)

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
    tx _ (F.EVar s)   | (x:y:zs) <- pack "." `splitOn` pack (F.symbolString s)
                      = foldl offset (F.eVar x) (y:zs)
    tx _ e            = e
    offset k v        = F.EApp offsetLocSym [F.expr k, F.expr v]


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
    ks               = [ y | B y _ <- bs ]
    ss               = (`mappend` F.symbol [symSepName])
    ks'              = map (F.eVar . ss) ks
    sub             :: F.Subable a => a -> a
    sub              = F.subst (F.mkSubst (zip ks ks'))

    go (TFun bs t r) = TFun [B (ss s) ts | B s ts <- bs] t (sub r)
    go t             = toplevel t sub

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
    go (BlockStmt a sts)         = BlockStmt b (ss <$> sts)
    go (EmptyStmt a)             = EmptyStmt b
    go (ExprStmt a e)            = ExprStmt b (ss e)
    go (IfStmt a e s1 s2)        = IfStmt b (ss e) (ss s1) (ss s2)
    go (IfSingleStmt a e s)      = IfSingleStmt b (ss e) (ss s)
    go (WhileStmt a e s)         = WhileStmt b (ss e) (ss s)
    go (DoWhileStmt a s e)       = DoWhileStmt b (ss s) (ss e)
    go (BreakStmt a i)           = BreakStmt b (ss <$> i)
    go (ContinueStmt a i)        = ContinueStmt b (ss <$> i)
    go (LabelledStmt a i s)      = LabelledStmt b (ss i) (ss s)
    go (ForInStmt a fi e s)      = ForInStmt b (ss fi) (ss e) (ss s)
    go (ForStmt a fi me1 me2 s)  = ForStmt b (ss fi) (ss <$> me1) (ss <$> me2) (ss s)
    go (TryStmt a s mcc ms)      = TryStmt b (ss s) (ss <$> mcc) (ss <$> ms)
    go (ThrowStmt a e)           = ThrowStmt b (ss e)
    go (ReturnStmt a me)         = ReturnStmt b (ss <$> me)
    go (WithStmt a e s)          = WithStmt b (ss e) (ss s)
    go (VarDeclStmt a vs)        = VarDeclStmt b (ss <$> vs)
    go (FunctionStmt a i is mss) = FunctionStmt b (ss i) (ss <$> is) ((ss <$>) <$> mss)
    go (ClassStmt a i cs)        = ClassStmt b (ss i) (ss <$> cs)
    go (ModuleStmt a i sts)      = ModuleStmt b (ss i) (ss <$> sts)
    go (InterfaceStmt a i)       = InterfaceStmt b (ss i)
    go (EnumStmt a i es)         = EnumStmt b (ss i) (ss <$> es)
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
    go (NullLit a)              = NullLit b
    go (ArrayLit _ es)          = ArrayLit b (ss <$> es)
    go (ObjectLit _ pes)        = ObjectLit b ((\(p,e) -> (ss p, ss e)) <$> pes)
    go (HexLit _ s)             = HexLit b s
    go (ThisRef a)              = ThisRef b
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
    go (SuperRef a)             = SuperRef b
    go (FuncExpr _ mi is sts)   = FuncExpr b (ss <$> mi) (ss <$> is) (ss <$> sts)
    go (Cast _ e)               = Cast b (ss e)
    go (Cast_ _ e)              = Cast_ b (ss e)

stransId f _ ctx (Id a s) = Id (f ctx a) s

stransForInInit f g ctx (ForInVar i) = ForInVar (strans f g ctx i)

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
    go (Constructor a is sts)         = Constructor b (ss <$> is) (ss <$> sts)
    go (MemberVarDecl a st i me)      = MemberVarDecl b st (ss i) (ss <$> me)
    go (MemberMethDecl a st i is sts) = MemberMethDecl b st (ss i) (ss <$> is) (ss <$> sts)

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
    go (PropId a i)     = PropId b (ss i)
    go (PropString a s) = PropString b s
    go (PropNum a i)    = PropNum b i

stransLvalue f g ctx lv = go lv
  where
    a    = getAnnotation lv
    b    = f ctx a
    ctx' = g ctx b
    ss   = strans f g ctx'
    go (LVar _ s)         = LVar b s
    go (LDot _ e s)       = LDot b (ss e) s
    go (LBracket _ e1 e2) = LBracket b (ss e1) (ss e2)

