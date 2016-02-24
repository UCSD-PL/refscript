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

import qualified Data.HashSet             as H
import qualified Data.List                as L
import           Data.Maybe               (catMaybes, maybeToList)
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
    go γ z t@(TObj _ xts r)  = h γ r $ g γ t $ efoldTypeMembers' f g h xts γ' z
                               where γ' = F.insertSEnv thisSym (f t) γ
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


-- | Accumulate moudules (only descend down modules)
--------------------------------------------------------------------------------
accumModuleStmts :: IsLocated a => [Statement a] -> [(AbsPath, [Statement a])]
--------------------------------------------------------------------------------
accumModuleStmts ss = foldStmts moduleVis emptyPath [acc0] ss
  where
    acc0      = (emptyPath, ss)
    moduleVis = defaultVisitor { endExpr = eE, endStmt = eS
                               , ctxStmt = cS, accStmt = aS }
    eS ModuleStmt{} = False
    eS _            = True
    eE _            = True
    cS ctx (ModuleStmt _ x _ ) = extendAbsPath ctx (F.symbol x)
    cS ctx _                   = ctx
    aS ctx (ModuleStmt _ _ ms) = [(ctx, ms)]
    aS _   _                   = []


--------------------------------------------------------------------------------
accumNamesAndPaths
  :: F.Reftable r
  => [Statement (AnnRel r)] -> (H.HashSet AbsName, H.HashSet AbsPath)
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
    --
    -- XXX: Perhaps allow ambient ones?
    --
    -- acv _ (VarDecl l n ii)              = [(n, l, VarDeclKind, varAsgn l, inited l ii)]

    -- inited l _        | any isAmbient (fFact l) = Initialized
    -- inited _ (Just _) = Initialized
    -- inited _ _        = Uninitialized

    -- isAmbient (VarAnn _ Ambient _) = True
    -- isAmbient _                    = False

    -- varAsgn   l = fromMaybe WriteLocal $ listToMaybe [ a | VarAnn _ a _ <- fFact l ]


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
