{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Rsc.Traversals (

  -- * Type Traversals
    foldReft, efoldReft, efoldRType

  -- * Accumulators
  , accumNamesAndPaths, accumModuleStmts, accumVars

  -- * Check well-formedness
  , checkTypeWF
  ) where

import           Control.Applicative      hiding (empty)
import           Data.Default
import           Data.Generics
import qualified Data.HashSet             as H
import qualified Data.List                as L
import           Data.Maybe               (catMaybes, fromMaybe, listToMaybe, maybeToList)
import           Language.Fixpoint.Errors
import qualified Language.Fixpoint.Types  as F
import           Language.Rsc.Annotations      hiding (err)
import           Language.Rsc.AST
import           Language.Rsc.Locations
import           Language.Rsc.Names
import           Language.Rsc.Pretty
import           Language.Rsc.Types
import           Language.Rsc.Visitor


-- type FError = F.FixResult Error



--------------------------------------------------------------------------------
-- | fold over @RType@
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
foldReft   :: PPR r => (r -> a -> a) -> a -> RTypeQ q r -> a
efoldReft  :: PPR r => (RTypeQ q r -> b) -> (F.SEnv b -> r -> a -> a)          -> F.SEnv b -> a -> RTypeQ q r -> a
efoldRType :: PPR r => (RTypeQ q r -> b) -> (F.SEnv b -> RTypeQ q r -> a -> a) -> F.SEnv b -> a -> RTypeQ q r -> a
--------------------------------------------------------------------------------
foldReft   f   = efold (\_ -> ()) (\_ _ -> id) (\_ -> f) F.emptySEnv
efoldReft  f h = efold f          (\_ _ -> id) h
efoldRType f g = efold f          g            (\_ _ -> id)

--------------------------------------------------------------------------------
efold :: PPR r => (            RTypeQ q r -> b)         -- f
               -> (F.SEnv b -> RTypeQ q r -> a -> a)    -- g
               -> (F.SEnv b -> r          -> a -> a)    -- h
               -> F.SEnv b -> a -> RTypeQ q r -> a
--------------------------------------------------------------------------------
efold f g h                  = go
  where
    go γ z t@(TPrim _ r)     = h γ r $ g γ t z
    go γ z t@(TVar _ r)      = h γ r $ g γ t z
    go γ z t@(TOr ts)        = g γ t $ gos γ z ts
    go γ z t@(TAnd ts)       = g γ t $ gos γ z ts
    go γ z t@(TRef n r)      = h γ r $ g γ t $ gos γ z $ g_args n
    go γ z t@(TObj xts r)    = h γ r $ g γ t $ efoldTypeMembers' f g h xts γ z
    go γ z t@(TClass n)      = g γ t $ gos γ z $ catMaybes $ btv_constr <$> b_args n
    go γ z t@(TMod _)        = g γ t z
    go γ z t@(TAll _ t')     = g γ t $ go γ z t'
    go γ z t@(TFun xts t' r) = h γ r $ g γ t $ go γ' (gos γ' z $ map b_type xts) t'
                               where γ' = foldr ext γ xts
    go γ z t@(TExp _)        = g γ t z
    gos γ z ts               = L.foldl' (go γ) z ts
    ext (B x t)              = x  `F.insertSEnv` f t

--------------------------------------------------------------------------------
efoldTypeMembers' :: PPR r => (            RTypeQ q r -> b)         -- f
                           -> (F.SEnv b -> RTypeQ q r -> a -> a)    -- g
                           -> (F.SEnv b -> r          -> a -> a)    -- h
                           -> TypeMembersQ q r -> F.SEnv b -> a -> a
--------------------------------------------------------------------------------
efoldTypeMembers' g f h (TM p m sp sm c k s n) γ z =
    L.foldl' (efold g f h γ) z $ pl ++ ml ++ spl ++ sml ++ cl ++ kl ++ sl ++ nl
  where
    pl  = L.foldr (\(_, FI _ t t') -> ([t,t'] ++) ) [] $ F.toListSEnv p
    ml  = L.foldr (\(_, MI _ _ t ) -> ([t]    ++) ) [] $ F.toListSEnv m
    spl = L.foldr (\(_, FI _ t t') -> ([t,t'] ++) ) [] $ F.toListSEnv sp
    sml = L.foldr (\(_, MI _ _ t ) -> ([t]    ++) ) [] $ F.toListSEnv sm
    cl  = maybeToList c
    kl  = maybeToList k
    sl  = maybeToList s
    nl  = maybeToList n


-- debugTyBinds p@(Rsc { code = Src SS }) = trace msg p
--   where
--     xts = [(x, t) | (x, (t, _)) <- visibleNames ss ]
--     msg = unlines $ "debugTyBinds:" : (ppshow <$> xts)

--------------------------------------------------------------------------------
-- | AST Folds
--------------------------------------------------------------------------------

type PPRD r = (PPR r, Data r, Typeable r)

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
    namesSet    = H.fromList [ nm | (ap,ss) <- allModStmts, nm <- accumAbsNames ap ss ]

--------------------------------------------------------------------------------
accumAbsNames :: IsLocated a => AbsPath -> [Statement a] -> [AbsName]
--------------------------------------------------------------------------------
accumAbsNames (QP AK_ _ ss)  = concatMap go
  where
    go (ClassStmt l x _ _ _) = [ QN (QP AK_ (srcPos l) ss) $ F.symbol x ]
    go (EnumStmt l x _ )     = [ QN (QP AK_ (srcPos l) ss) $ F.symbol x ]
    go (InterfaceStmt l x )      = [ QN (QP AK_ (srcPos l) ss) $ F.symbol x ]
    go _                     = []

-- TODO: Add modules as well?
--------------------------------------------------------------------------------
accumVars :: PPR r => [Statement (AnnR r)] -> [(Id SrcSpan, SyntaxKind, VarInfo r)]
--------------------------------------------------------------------------------
accumVars s = [ (fSrc <$> n, k, VI loc a i t) | (n,l,k,a,i) <- hoistBindings s
                                              , fact        <- fFact l
                                              , (loc, t)    <- annToType fact ]
  where
    annToType (ClassAnn l (TS _ b _)) = [(l, TClass b)]       -- Class
    annToType (SigAnn   l t)          = [(l, t)]              -- Function
    annToType (VarAnn   l _ (Just t)) = [(l, t)]              -- Variables
    annToType _                       = [ ]

type BindInfo a = (Id a, a, SyntaxKind, Assignability, Initialization)

-- | Returns all bindings in the scope of @s@ (functions, classes, modules, variables).
--------------------------------------------------------------------------------
hoistBindings :: [Statement (AnnR r)] -> [BindInfo (AnnR r)]
--------------------------------------------------------------------------------
hoistBindings = snd . visitStmts vs ()
  where
    vs = scopeVisitor { accStmt = acs, accVDec = acv }

    acs _ (FunctionStmt a x _ _) = [(x, a, FuncDeclKind, Ambient, Initialized)]
    acs _ (ClassStmt a x _ _ _ ) = [(x, a, ClassDeclKind, Ambient, Initialized)]
    acs _ (ModuleStmt a x _)     = [(x, a { fFact = modAnn x a }, ModuleDeclKind, Ambient, Initialized)]
    acs _ (EnumStmt a x _)       = [(x, a { fFact = enumAnn x a }, EnumDeclKind, Ambient, Initialized)]
    acs _ _                      = []

    acv _ (VarDecl l n ii)       = [(n, l, VarDeclKind, varAsgn l, tracePP (ppshow n) $ inited l ii)]

    inited l _        | any isAmbient (fFact l)
                      = Initialized
    inited _ (Just _) = Initialized
    inited _ _        = Uninitialized

    isAmbient (VarAnn _ Ambient _) = True
    isAmbient _                    = False

    varAsgn l = fromMaybe WriteLocal $ listToMaybe [ a | VarAnn _ a _ <- fFact l ]

    modAnn  n l = ModuleAnn (F.symbol n) : fFact l
    enumAnn n l = EnumAnn   (F.symbol n) : fFact l



--------------------------------------------------------------------------------
-- | Well-formedness
--------------------------------------------------------------------------------
--
--
-- TODO: Move to TypeUtilities ?

-- checkTypeWF :: RefScript -> Either FError ()
checkTypeWF p = undefined
--   case foldRsc checkTypeWFVisitor () [] p of
--     []  -> Right ()
--     lts -> Left $ F.Unsafe [ errorIllFormedType l t | (l, t) <- lts ]


-- checkTypeWFVisitor :: (Monad m, Functor m, PP r) => VisitorM m [(SrcSpan, RType r)] () b
checkTypeWFVisitor = undefined -- defaultVisitor

