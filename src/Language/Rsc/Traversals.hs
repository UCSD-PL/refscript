{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Rsc.Traversals (
    scrapeQuals
  , accumNamesAndPaths
  , accumModuleStmts

  , accumVars
  ) where

import           Control.Applicative            hiding (empty)
import           Data.Default
import           Data.Generics
import qualified Data.HashSet                   as H
import           Data.Maybe                     (fromMaybe, listToMaybe, maybeToList)
import           Language.Fixpoint.Errors
import qualified Language.Fixpoint.Types        as F
import           Language.Rsc.Annots            hiding (err)
import           Language.Rsc.AST
import           Language.Rsc.Liquid.Qualifiers
import           Language.Rsc.Locations
import           Language.Rsc.Names
import           Language.Rsc.Pretty
import           Language.Rsc.Types
import           Language.Rsc.Visitor


-- | Extracts all qualifiers from a RefScript program
---------------------------------------------------------------------------------
scrapeQuals :: [F.Qualifier] -> [Statement (AnnRel F.Reft)] -> [F.Qualifier]
---------------------------------------------------------------------------------
scrapeQuals qs ss = qs ++ qualifiers (mkUq $ foldStmts tbv [] ss)
  where
    tbv = defaultVisitor { accStmt = gos, accCElt = goe }

    gos _ (FunctionStmt l f _ _) = [(f, t) | SigAnn t <- fFact l]
    gos _ (VarDeclStmt _ vds)    = [(x, t) | VarDecl l x _ <- vds, VarAnn _ (Just t) <- fFact l]
    gos _ _                      = []

    goe _ (Constructor l _ _)        = [(x, t) | CtorAnn  t <- fFact l, let x = Id l "ctor" ]
    goe _ (MemberVarDecl l _ x _)    = [(x, t) | FieldAnn (FI _ _ t) <- fFact l ]
    goe _ (MemberMethDecl l _ x _ _) = [(x, t) | MethAnn  (MI _ _ t) <- fFact l ]

mkUq = zipWith tx ([0..] :: [Int])
  where
    tx i (Id l s, t) = (Id l $ s ++ "_" ++ show i, t)

-- debugTyBinds p@(Rsc { code = Src ss }) = trace msg p
--   where
--     xts = [(x, t) | (x, (t, _)) <- visibleNames ss ]
--     msg = unlines $ "debugTyBinds:" : (ppshow <$> xts)

---------------------------------------------------------------------------
-- | AST Folds
---------------------------------------------------------------------------

type PPRD r = (PPR r, Data r, Typeable r)

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

-- | Accumulate moudules (only descend down modules)
-------------------------------------------------------------------------------
accumModuleStmts :: (IsLocated a, Data a, Typeable a) => [Statement a] -> [(AbsPath, [Statement a])]
-------------------------------------------------------------------------------
accumModuleStmts ss = topLevel : rest ss
  where
    topLevel = (QP AK_ def [], ss)
    rest     = everythingButWithContext [] (++) $ ([],,False) `mkQ` f
    f e@(ModuleStmt _ x ms) s = let p = s ++ [F.symbol x] in
                                ([(QP AK_ (srcPos e) p, ms)], p, False)
    f _ s  = ([], s, True)

---------------------------------------------------------------------------------------
accumNamesAndPaths :: PPRD r => [Statement (AnnRel r)] -> (H.HashSet AbsName, H.HashSet AbsPath)
---------------------------------------------------------------------------------------
accumNamesAndPaths stmts = (namesSet, modulesSet)
  where
    allModStmts = accumModuleStmts stmts
    modulesSet  = H.fromList [ ap | (ap, _) <- allModStmts ]
    namesSet    = H.fromList [ nm | (ap,ss) <- allModStmts, nm <- accumAbsNames ap ss ]

---------------------------------------------------------------------------------------
accumAbsNames :: IsLocated a => AbsPath -> [Statement a] -> [AbsName]
---------------------------------------------------------------------------------------
accumAbsNames (QP AK_ _ ss)  = concatMap go
  where
    go (ClassStmt l x _ _ _) = [ QN (QP AK_ (srcPos l) ss) $ F.symbol x ]
    go (EnumStmt l x _ )     = [ QN (QP AK_ (srcPos l) ss) $ F.symbol x ]
    go (InterfaceStmt l x )      = [ QN (QP AK_ (srcPos l) ss) $ F.symbol x ]
    go _                     = []

-- TODO: Add modules as well?
---------------------------------------------------------------------------------------
accumVars :: [Statement (AnnR r)] -> [(Id SrcSpan, SyntaxKind, VarInfo r)]
---------------------------------------------------------------------------------------
accumVars s = [ (fSrc <$> n, k, VI a i t)  | (n,l,k,a,i) <- hoistBindings s
                                         , f <- fFact l, t <- annToType a f ]
  where
    annToType _ (ClassAnn (TS _ b _)) = [TClass b]    -- Class
    annToType _       (SigAnn t)      = [t]           -- Function
    annToType Ambient (VarAnn _ t)    = maybeToList t -- Variables
    annToType _       _               = [ ]

type BindInfo a = (Id a, a, SyntaxKind, Assignability, Initialization)

-- | Returns all bindings in the scope of @s@ (functions, classes, modules, variables).
---------------------------------------------------------------------------------------
hoistBindings :: [Statement (AnnR r)] -> [BindInfo (AnnR r)]
---------------------------------------------------------------------------------------
hoistBindings = snd . visitStmts vs ()
  where
    vs = scopeVisitor { accStmt = acs, accVDec = acv }

    acs _ (FunctionStmt a x _ _) = [(x, a, FuncDeclKind, Ambient, Initialized)]
    acs _ (ClassStmt a x _ _ _ ) = [(x, a, ClassDeclKind, Ambient, Initialized)]
    acs _ (ModuleStmt a x _)     = [(x, a { fFact = modAnn x a }, ModuleDeclKind, Ambient, Initialized)]
    acs _ (EnumStmt a x _)       = [(x, a { fFact = enumAnn x a }, EnumDeclKind, Ambient, Initialized)]
    acs _ _                      = []

    acv _ (VarDecl l n ii)       = [(n, l, VarDeclKind, varAsgn l, inited ii)]

    -- TODO: Add support for ambient variable declarations
    --  : [(n, l, VarDeclKind, WriteGlobal, inited ii) | AmbVarAnn _  <- fFact l]

    inited (Just _) = Initialized
    inited _        = Uninitialized
    varAsgn l       = fromMaybe WriteLocal $ listToMaybe [ a | VarAnn a _ <- fFact l ]

    modAnn  n l = ModuleAnn (F.symbol n) : fFact l
    enumAnn n l = EnumAnn   (F.symbol n) : fFact l

