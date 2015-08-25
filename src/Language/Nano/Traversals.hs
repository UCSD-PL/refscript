{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Nano.Traversals (
    scrapeQuals
  , accumNamesAndPaths
  , accumModuleStmts
  ) where

import           Control.Applicative           hiding (empty)
import           Data.Default
import           Data.Generics
import qualified Data.HashSet                  as H
import           Data.List                     (partition)
import qualified Data.Map.Strict               as M
import           Data.Maybe                    (fromMaybe, listToMaybe, maybeToList)
import qualified Data.Traversable              as T
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Names
import qualified Language.Fixpoint.Types       as F
import           Language.Nano.Annots          hiding (err)
import           Language.Nano.AST
import           Language.Nano.Core.Env
-- import           Language.Nano.Environment
-- import           Language.Nano.Liquid.Qualifiers
import           Language.Nano.Locations
import           Language.Nano.Names
import           Language.Nano.Pretty
import           Language.Nano.Program
import           Language.Nano.Typecheck.Types
import           Language.Nano.Types
import           Language.Nano.Visitor
import qualified Text.PrettyPrint.HughesPJ     as P


-- | Extracts all qualifiers from a RefScript program
---------------------------------------------------------------------------------
scrapeQuals :: [F.Qualifier] -> [Statement (AnnRel F.Reft)] -> [F.Qualifier]
---------------------------------------------------------------------------------
scrapeQuals qs ss = qs ++ qualifiers (mkUq $ foldStmts tbv [] ss)
  where
    qualifiers = undefined
    tbv = defaultVisitor { accStmt = stmtTypeBindings
                         , accCElt = celtTypeBindings }

mkUq = zipWith tx ([0..] :: [Int])
  where
    tx i (Id l s, t) = (Id l $ s ++ "_" ++ show i, t)

stmtTypeBindings _ = go
  where
    go (FunctionStmt l f _ _) = [(f, t) | FuncAnn t          <- fFact l ] ++
                                [(f, t) | VarAnn  _ (Just t) <- fFact l ]
    go (VarDeclStmt _ vds)    = [(x, t) | VarDecl l x _      <- vds
                                        , VarAnn  _ (Just t) <- fFact l ]
    go _                      = []

celtTypeBindings _               = go
  where
    go (Constructor l _ _)       = [(x, t) | ConsAnn  t <- fFact l, let x = Id l "ctor" ]
    go (MemberVarDecl l _ x _)   = [(x, t) | FieldAnn _ (FI _ _ t) <- fFact l ]
    go (MemberMethDef l _ x _ _) = [(x, t) | MethAnn  _ (MI _ _ t) <- fFact l ]
    go _                         = []

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
accumAbsNames :: IsLocated a => AbsPath -> [ Statement a ] -> [ AbsName ]
---------------------------------------------------------------------------------------
accumAbsNames (QP AK_ _ ss)  = concatMap go
  where
    go (ClassStmt l x _ _ _) = [ QN (QP AK_ (srcPos l) ss) $ F.symbol x ]
    go (EnumStmt l x _ )     = [ QN (QP AK_ (srcPos l) ss) $ F.symbol x ]
    go (IfaceStmt l x )      = [ QN (QP AK_ (srcPos l) ss) $ F.symbol x ]
    go _                     = []


-- Not including class, module, enum names
---------------------------------------------------------------------------------------
visibleVars :: Data r => [Statement (AnnSSA r)] -> [(Id SrcSpan, VarInfo r)]
---------------------------------------------------------------------------------------
visibleVars s = [ (fSrc <$> n, VI a i t)  | (n,l,k,a,i) <- hoistBindings s
                                          , f           <- fFact l
                                          , t           <- annToType a f ]
  where
    annToType Ambient (VarAnn _ t)  = maybeToList t -- Hoist ReadOnly & ImportDecls
    annToType Ambient (AmbVarAnn t) = [t] -- Hoist ReadOnly vars (i.e. function defs)
    annToType Ambient (AmbVarAnn t) = [t] -- Hoist ImportDecl (i.e. function decls)
    annToType _       _             = [ ]

type BindInfo a = (Id a, a, SyntaxKind, Assignability, Initialization)

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

-------------------------------------------------------------------------------
hoistBindings :: Data r => [Statement (AnnSSA r)] -> [BindInfo (AnnSSA r)]
-------------------------------------------------------------------------------
hoistBindings = snd . visitStmts vs ()
  where
    vs = scopeVisitor { accStmt = acs, accVDec = acv }

    acs _ (FunctionStmt a x _ _) = [(x, a, FuncDefKind, Ambient, Initialized)]
    acs _ (FuncAmbDecl a x _)    = [(x, a, FuncAmbientKind, Ambient, Initialized)]
    acs _ (FuncOverload a x _  ) = [(x, a, FuncOverloadKind, Ambient, Initialized)]
    acs _ (ClassStmt a x _ _ _ ) = [(x, a, ClassDefKind, Ambient, Initialized)]
    acs _ (ModuleStmt a x _)     = [(x, a { fFact = modAnn x a }, ModuleDefKind, Ambient, Initialized)]
    acs _ (EnumStmt a x _)       = [(x, a { fFact = enumAnn x a }, EnumDefKind, Ambient, Initialized)]
    acs _ _                      = []

    acv _ (VarDecl l n ii)       = [(n, l, VarDeclKind, varAsgn l, inited ii)] ++
                                   [(n, l, VarDeclKind, WriteGlobal, inited ii) | AmbVarAnn _  <- fFact l]

    inited (Just _) = Initialized
    inited _        = Uninitialized
    varAsgn l       = fromMaybe WriteLocal $ listToMaybe [ a | VarAnn a _ <- fFact l ]

    modAnn  n l = ModuleAnn (F.symbol n) : fFact l
    enumAnn n l = EnumAnn   (F.symbol n) : fFact l

