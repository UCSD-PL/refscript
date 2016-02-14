{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Rsc.Traversals (

  -- * Type Traversals
    foldReft, efoldReft, efoldRType

  -- * Accumulators
  , accumNamesAndPaths
  , accumModuleStmts
  , hoistBindings

  -- * Check well-formedness
  --
  -- TODO
  --
  -- , checkTypeWF
  ) where

import           Data.Default
import           Data.Generics
import qualified Data.HashSet             as H
import qualified Data.List                as L
import           Data.Maybe               (catMaybes, fromMaybe, listToMaybe, maybeToList)
import qualified Language.Fixpoint.Types  as F
import           Language.Rsc.Annotations
import           Language.Rsc.AST
import           Language.Rsc.Locations
import           Language.Rsc.Names
import           Language.Rsc.Types
import           Language.Rsc.Visitor


--------------------------------------------------------------------------------
-- | fold over @RType@
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
foldReft   :: F.Reftable r => (r -> a -> a) -> a -> RTypeQ q r -> a
efoldReft  :: F.Reftable r => (RTypeQ q r -> b) -> (F.SEnv b -> r -> a -> a)          -> F.SEnv b -> a -> RTypeQ q r -> a
efoldRType :: F.Reftable r => (RTypeQ q r -> b) -> (F.SEnv b -> RTypeQ q r -> a -> a) -> F.SEnv b -> a -> RTypeQ q r -> a
--------------------------------------------------------------------------------
foldReft   f   = efold (\_ -> ()) (\_ _ -> id) (\_ -> f) F.emptySEnv
efoldReft  f h = efold f          (\_ _ -> id) h
efoldRType f g = efold f          g            (\_ _ -> id)

--------------------------------------------------------------------------------
efold :: F.Reftable r => (            RTypeQ q r -> b)         -- f
                      -> (F.SEnv b -> RTypeQ q r -> a -> a)    -- g
                      -> (F.SEnv b -> r          -> a -> a)    -- h
                      -> F.SEnv b -> a -> RTypeQ q r -> a
--------------------------------------------------------------------------------
efold f g h                  = go
  where
    go γ z t@(TPrim _ r)     = h γ r $ g γ t z
    go γ z t@(TVar _ r)      = h γ r $ g γ t z
    go γ z t@(TOr ts r)      = h γ r $ g γ t $ gos γ z ts
    go γ z t@(TAnd ts)       = g γ t $ gos γ z $ map snd ts
    go γ z t@(TRef n r  )    = h γ r $ g γ t $ gos γ z $ g_args n
    go γ z t@(TObj _ xts r)  = h γ r $ g γ t $ efoldTypeMembers' f g h xts γ z
    go γ z t@(TClass n)      = g γ t $ gos γ z $ catMaybes $ btv_constr <$> b_args n
    go γ z t@(TMod _)        = g γ t z
    go γ z t@(TAll bs t')    = g γ t $ go γ (gos γ z $ maybeToList $ btv_constr bs) t'
    go γ z t@(TFun xts t' r) = h γ r $ g γ t $ go γ' (gos γ' z $ map b_type xts) t'
                               where γ' = foldr ext γ xts
    go γ z t@(TExp _)        = g γ t z
    gos γ z ts               = L.foldl' (go γ) z ts
    ext (B x _ t)            = x `F.insertSEnv` f t

--------------------------------------------------------------------------------
efoldTypeMembers' :: F.Reftable r => (            RTypeQ q r -> b)         -- f
                                  -> (F.SEnv b -> RTypeQ q r -> a -> a)    -- g
                                  -> (F.SEnv b -> r          -> a -> a)    -- h
                                  -> TypeMembersQ q r -> F.SEnv b -> a -> a
--------------------------------------------------------------------------------
efoldTypeMembers' g f h (TM m sm c k s n) γ z =
    L.foldl' (efold g f h γ) z $ ml ++ sml ++ cl ++ kl ++ sl ++ nl
  where
    ml  = L.foldr step [] (F.toListSEnv m)
    sml = L.foldr step [] (F.toListSEnv sm)
    step (_, FI _ _ _ t') = ([t']        ++)
    step (_, MI _ _ mts ) = (map snd mts ++)
    cl  = maybeToList c
    kl  = maybeToList k
    sl  = map snd $ maybeToList s
    nl  = map snd $ maybeToList n


-- debugTyBinds p@(Rsc { code = Src SS }) = trace msg p
--   where
--     xts = [(x, t) | (x, (t, _)) <- visibleNames ss ]
--     msg = unlines $ "debugTyBinds:" : (ppshow <$> xts)

--------------------------------------------------------------------------------
-- | AST Folds
--------------------------------------------------------------------------------


-- class Foldable t where
--   fold :: (F.Reftable r, Monoid a)
--        => ([TVar] -> [BindQ q r] -> RTypeQ q r -> a)
--        ->  [TVar] -> [BindQ q r] -> t q r  -> a
--
-- instance Foldable RTypeQ where
--   fold = foldRType
--
-- instance Foldable BindQ where
--   fold f αs xs (B _ t) = fold f αs xs t
--
-- -- instance Foldable FactQ where
-- --   trans = transFact
-- --
-- -- instance Foldable CastQ where
-- --   trans = transCast
-- --
-- -- instance Foldable TypeDeclQ where
-- --   trans f αs xs (TD s@(TS _ b _) es) = TD (trans f αs xs s) (trans f αs' xs es)
-- --     where
-- --       αs' = map btvToTV (b_args b) ++ αs
-- --
-- -- instance Foldable TypeSigQ where
-- --   trans f αs xs (TS k b h) = TS k (trans f αs xs b) (transIFDBase f αs' xs h)
-- --       where
-- --         αs' = map btvToTV (b_args b) ++ αs
--
-- instance Foldable TypeMembersQ where
--   fold f αs xs (TM m sm c k s n) = mconcat [ Fld.foldMap (fold f αs xs) m
--                                            , Fld.foldMap (fold f αs xs) sm
--                                            , Fld.foldMap (fold f αs xs) c
--                                            , Fld.foldMap (fold f αs xs) k
--                                            , Fld.foldMap (fold f αs xs) s
--                                            , Fld.foldMap (fold f αs xs) n
--                                            ]
--
-- instance Foldable BTGenQ where
--   fold f αs xs (BGen n ts) = mconcat $ fold f αs xs <$> ts
--
-- instance Foldable TGenQ where
--   fold f αs xs (Gen n ts) = mconcat $ fold f αs xs <$> ts
--
-- instance Foldable BTVarQ where
--   fold f αs xs (BTV x l (Just t)) = fold f αs xs t
--   fold f αs xs (BTV x l _)        = mempty
--
-- instance Foldable TypeMemberQ where
--   fold f αs xs (FI _ _ t) = fold f αs xs t
--   fold f αs xs (MI o mts) = mconcat $ fold f αs xs . snd <$> mts
-- --
-- -- instance Foldable ModuleDefQ where
-- --   trans f αs xs (ModuleDef v t e p)
-- --     = ModuleDef (envMap (trans f αs xs) v) (envMap (trans f αs xs) t) e p
-- --
-- -- instance Foldable SymInfoQ where
-- --   trans f αs xs (VI l a i t) = VI l a i $ trans f αs xs t
-- --
-- -- instance Foldable FAnnQ where
-- --   trans f αs xs (FA i s ys) = FA i s $ trans f αs xs <$> ys
-- --
-- -- transFmap ::  (F.Reftable r, Functor thing)
-- --           => ([TVar] -> [BindQ q r] -> RTypeQ q r -> RTypeQ q r)
-- --           -> [TVar] -> thing (FAnnQ q r) -> thing (FAnnQ q r)
-- -- transFmap f αs = fmap (trans f αs [])
-- --
-- -- transIFDBase f αs xs (es,is) = (trans f αs xs <$> es, trans f αs xs <$> is)
-- --
-- -- transFact :: F.Reftable r => ([TVar] -> [BindQ q r] -> RTypeQ q r -> RTypeQ q r)
-- --                           -> [TVar] -> [BindQ q r] -> FactQ q r -> FactQ q r
-- -- transFact f = go
-- --   where
-- --     go αs xs (PhiVarTy (v,t))  = PhiVarTy      $ (v, trans f αs xs t)
-- --
-- --     go αs xs (TypInst x y ts)  = TypInst x y   $ trans f αs xs <$> ts
-- --
-- --     go αs xs (EltOverload x m) = EltOverload x $ trans f αs xs m
-- --     go αs xs (Overload x t)    = Overload x    $ trans f αs xs t
-- --
-- --     go αs xs (VarAnn l a t)    = VarAnn l a    $ trans f αs xs <$> t
-- --
-- --     go αs xs (FieldAnn t)      = FieldAnn      $ trans f αs xs t
-- --     go αs xs (MethAnn t)       = MethAnn       $ trans f αs xs t
-- --     go αs xs (CtorAnn t)       = CtorAnn       $ trans f αs xs t
-- --
-- --     go αs xs (UserCast t)      = UserCast      $ trans f αs xs t
-- --     go αs xs (SigAnn l t)      = SigAnn l      $ trans f αs xs t
-- --     go αs xs (TCast x c)       = TCast x       $ trans f αs xs c
-- --
-- --     go αs xs (ClassAnn l ts)   = ClassAnn l    $ trans f αs xs ts
-- --     go αs xs (InterfaceAnn td) = InterfaceAnn  $ trans f αs xs td
-- --
-- --     go _ _   t                 = t
-- --
-- -- transCast f = go
-- --   where
-- --     go _  _ CNo          = CNo
-- --     go αs xs (CDead e t) = CDead e $ trans f αs xs t
-- --     go αs xs (CUp t1 t2) = CUp (trans f αs xs t1) (trans f αs xs t2)
-- --     go αs xs (CDn t1 t2) = CUp (trans f αs xs t1) (trans f αs xs t2)
--
--
-- -- TODO instance Foldable FRsc where
-- -- TODO   fold f αs xs (FRsc (Rsc (Src ss ) cst ta pa pq inv max opt)) =
-- -- TODO     foldStmts visitor ([], []) ss
-- -- TODO       where
-- -- TODO         visitor = defaultVisitor
--
--
-- -- | foldRType
-- --
-- --  Binds (αs and bs) accumulate on the left.
-- --
-- foldRType :: (F.Reftable r, Monoid a)
--           => ([TVar] -> [BindQ q r] -> RTypeQ q r -> a)
--           ->  [TVar] -> [BindQ q r] -> RTypeQ q r -> a
-- foldRType f                   = go
--   where
--     go αs xs t@(TPrim _ _)    = f αs xs t
--     go αs xs t@(TVar _ _)     = f αs xs t
--     go αs xs t@(TOr ts)       = f αs xs t <> Fld.foldMap (go αs xs) ts
--     go αs xs t@(TAnd ts)      = f αs xs t <> Fld.foldMap (go αs xs) (map snd ts)
--     go αs xs t@(TRef n r)     = f αs xs t <> fold f αs xs n
--     go αs xs t@(TObj ms r)    = f αs xs t <> fold f αs xs ms
--     go αs xs t@(TClass n)     = f αs xs t <> fold f αs xs n
--     go αs xs t@(TMod _)       = f αs xs t
--     go αs xs t@(TAll a t')    = f αs xs t <> fold f αs' xs t'
--                                 where αs' = αs ++ [btvToTV a]
--     go αs xs t@(TFun bs t' r) = f αs xs t <> Fld.foldMap (fold f αs xs') bs <> fold f αs xs' t'
--                                 where xs' = bs ++ xs
--     go αs xs t@(TExp e)       = f αs xs t


type PPRD r = (F.Reftable r, Data r, Typeable r)

-- | Summarise all nodes in top-down, left-to-right order, carrying some state
--   down the tree during the computation, but not left-to-right to siblings,
--   and also stop when a condition is true.
--------------------------------------------------------------------------------
everythingButWithContext :: s -> (r -> r -> r) -> GenericQ (s -> (r, s, Bool)) -> GenericQ r
--------------------------------------------------------------------------------
everythingButWithContext s0 f q x
  | stop      = r
  | otherwise = foldl f r (gmapQ (everythingButWithContext s' f q) x)
    where (r, s', stop) = q x s0

-- | Accumulate moudules (only descend down modules)
--------------------------------------------------------------------------------
accumModuleStmts :: (IsLocated a, Data a, Typeable a) => [Statement a] -> [(AbsPath, [Statement a])]
--------------------------------------------------------------------------------
accumModuleStmts ss = topLevel : rest ss
  where
    topLevel = (QP AK_ def [], ss)
    rest     = everythingButWithContext [] (++) $ ([],,False) `mkQ` f
    f e@(ModuleStmt _ x ms) s = let p = s ++ [F.symbol x] in
                                ([(QP AK_ (srcPos e) p, ms)], p, False)
    f _ s  = ([], s, True)

--------------------------------------------------------------------------------
accumNamesAndPaths :: PPRD r => [Statement (AnnRel r)] -> (H.HashSet AbsName, H.HashSet AbsPath)
--------------------------------------------------------------------------------
accumNamesAndPaths stmts = (namesSet, modulesSet)
  where
    allModStmts = accumModuleStmts stmts
    modulesSet  = H.fromList [ ap | (ap, _) <- allModStmts ]
    namesSet    = H.fromList [ nm | (ap,ss) <- allModStmts
                                  , nm      <- accumAbsNames ap ss ]

--------------------------------------------------------------------------------
accumAbsNames :: IsLocated a => AbsPath -> [Statement a] -> [AbsName]
--------------------------------------------------------------------------------
accumAbsNames (QP AK_ _ ss)  = concatMap go
  where
    go (ClassStmt l x _ )   = [ QN (QP AK_ (srcPos l) ss) $ F.symbol x ]
    go (EnumStmt l x _ )    = [ QN (QP AK_ (srcPos l) ss) $ F.symbol x ]
    go (InterfaceStmt l x ) = [ QN (QP AK_ (srcPos l) ss) $ F.symbol x ]
    go _                    = []


type BindInfo a = (Id a, a, SyntaxKind, Assignability, Initialization)

-- | Returns all bindings in the scope of @s@ (functions, classes, modules, variables).
--------------------------------------------------------------------------------
hoistBindings :: [Statement (AnnR r)] -> [BindInfo (AnnR r)]
--------------------------------------------------------------------------------
hoistBindings = snd . visitStmts vs ()
  where
    vs = scopeVisitor { accStmt = acs, accVDec = acv }

    acs _ (FunctionStmt a x _ (Just _)) = [(x, a, FuncDeclKind  , RdOnly , Initialized)]
    acs _ (FunctionStmt a x _ Nothing ) = [(x, a, FuncDeclKind  , Ambient, Initialized)]
    acs _ (ClassStmt    a x _         ) = [(x, a, ClassDeclKind , RdOnly , Initialized)]
    acs _ (ModuleStmt   a x _         ) = [(x, a, ModuleDeclKind, RdOnly , Initialized)]
    acs _ (EnumStmt     a x _         ) = [(x, a, EnumDeclKind  , RdOnly , Initialized)]
    acs _ _                             = []

    acv _ _                             = []

    -- PV: these are block-scoped now - they don't get lifted.
    -- acv _ (VarDecl l n ii)              = [(n, l, VarDeclKind, varAsgn l, inited l ii)]

    inited l _        | any isAmbient (fFact l)
                      = Initialized
    inited _ (Just _) = Initialized
    inited _ _        = Uninitialized

    isAmbient (VarAnn _ Ambient _) = True
    isAmbient _                    = False

    varAsgn   l = fromMaybe WriteLocal $ listToMaybe [ a | VarAnn _ a _ <- fFact l ]


--------------------------------------------------------------------------------
-- | Well-formedness
--------------------------------------------------------------------------------
--
--
-- TODO: Move to TypeUtilities ?

-- checkTypeWF :: RefScript -> Either FError ()
-- checkTypeWF p = undefined
--   case foldRsc checkTypeWFVisitor () [] p of
--     []  -> Right ()
--     lts -> Left $ F.Unsafe [ errorIllFormedType l t | (l, t) <- lts ]


-- checkTypeWFVisitor :: (Monad m, Functor m, PP r) => VisitorM m () () (AnnR r)
-- checkTypeWFVisitor = defaultVisitor {
--   mExpr = \e -> do let a = getAnnotation e
--                    wellFormedAnnotation a
--                    return e
-- }

-- wellFormedAnnotation :: (Monad m, Functor m, PP r) => AnnR r -> m ()
-- wellFormedAnnotation (FA _ l fs) = forM_ fs (wellFormedFact l)
--
-- wellFormedFact :: (Monad m, Functor m, PP r) => SrcSpan -> Fact r -> m ()
-- wellFormedFact l (SigAnn _ t) = undefined
--
