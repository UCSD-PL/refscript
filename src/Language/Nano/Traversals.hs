{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Nano.Traversals (
    scrapeQuals
  ) where

import           Control.Applicative              hiding (empty)
import           Control.Exception                (throw)
import           Data.Default
import           Data.Generics
import           Data.Graph.Inductive.Query.Monad ((><))
import qualified Data.HashSet                     as H
import           Data.List                        (partition)
import qualified Data.Map.Strict                  as M
import           Data.Maybe                       (fromMaybe, listToMaybe, maybeToList)
import qualified Data.Traversable                 as T
import           Debug.Trace
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Names
import           Language.Fixpoint.Parse
import qualified Language.Fixpoint.Types          as F
import           Language.Nano.Annots             hiding (err)
import           Language.Nano.AST
import           Language.Nano.Core.Env
import           Language.Nano.Environment
import           Language.Nano.Errors
import           Language.Nano.Liquid.Qualifiers
import           Language.Nano.Locations
import           Language.Nano.Names
import           Language.Nano.Pretty
import           Language.Nano.Program
import           Language.Nano.Typecheck.Types
import           Language.Nano.Types
import           Language.Nano.Visitor
import qualified Text.PrettyPrint.HughesPJ        as P
import           Text.Printf

type FError = F.FixResult Error


-- | Extracts all qualifiers from a RefScript program
---------------------------------------------------------------------------------
scrapeQuals :: RefScript -> [F.Qualifier]
---------------------------------------------------------------------------------
scrapeQuals p = qualifiers $ mkUq $ foldRsc tbv [] [] p
  where
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

--
-- TODO
--
debugTyBinds = undefined
-- debugTyBinds p@(Rsc { code = Src ss }) = trace msg p
--   where
--     xts = [(x, t) | (x, (t, _)) <- visibleNames ss ]
--     msg = unlines $ "debugTyBinds:" : (ppshow <$> xts)


-- | `accumModules ss` creates a module store from the statements in @ss@
--   For every module we populate:
--
--    * m_variables with: functions, variables, class constructors, modules
--
--    * m_types with: classes and interfaces
--
------------------------------------------------------------------------------------
accumModules :: (PPR r, Typeable r, Data r) => BareRsc r -> Either FError (QEnv (ModuleDef r))
------------------------------------------------------------------------------------
accumModules pgm@(Rsc { code = Src stmts }) =
    mapM mkMod (accumModuleStmts stmts) >>= return . qenvFromList . map toQEnvList
  where
    toQEnvList p = (m_path p, p)
    mkMod (p,ss) = ModuleDef <$> varEnv p ss <*> typeEnv p ss <*> enumEnv ss <*> return p

    -- | Variables
    varEnv p = return . fromListToEnv . vStmts p
    vStmts   = concatMap . vStmt

    vStmt _ (VarDeclStmt _ vds) =
         [(ss x, VarDeclKind, VI a Uninitialized t ) | VarDecl l x _ <- vds, VarAnn a (Just t) <- fFact l ]
      ++ [(ss x, VarDeclKind, VI WriteGlobal Initialized t) | VarDecl l x _ <- vds, AmbVarAnn t <- fFact l ]
    -- The Assignabilities below are overwitten to default values
    vStmt _ (FunctionStmt l x _ _) =
         [(ss x, FuncDefKind, VI Ambient Initialized t) | VarAnn _ (Just t) <- fFact l ]
    vStmt _ (FuncAmbDecl l x _) =
         [(ss x, FuncAmbientKind, VI Ambient Initialized t) | VarAnn _ (Just t) <- fFact l ]
    vStmt _ (FuncOverload l x _) =
         [(ss x, FuncOverloadKind, VI Ambient Initialized t) | VarAnn _ (Just t) <- fFact l ]
    vStmt p (ClassStmt l x _ _ _) =
         [(ss x, ClassDefKind, VI Ambient Initialized $ TType ClassK b) | ClassAnn (TS _ b _) <- fFact l ]
    vStmt p (ModuleStmt l x _) =
         [(ss x, ModuleDefKind, VI Ambient Initialized $ TMod $ pathInPath l p x)]
    vStmt p (EnumStmt l x _) =
         [(ss x, EnumDefKind, VI Ambient Initialized $ TType EnumK $ BGen (QN p $ symbol x) []) ]
    vStmt _ _ = []

    -- | Type Definitions
    typeEnv p ss      = tStmts p ss >>= return . envFromList
    tStmts p ss              = concatMapM (tStmt p) ss

    tStmt ap c@ClassStmt{}   = single <$> resolveType ap c
    tStmt ap c@IfaceStmt{}   = single <$> resolveType ap c
    tStmt _ _                = return $ [ ]

    -- | Enumerations
    enumEnv a                = return $ envFromList $ eStmts a
    eStmts a                 = concatMap eStmt a

    eStmt (EnumStmt _ n es)  = [(fmap srcPos n, EnumDef (symbol n) (envFromList $ sEnumElt <$> es))]
    eStmt _                  = []
    sEnumElt (EnumElt _ s e) = (symbol s, fmap (const ()) e)

    ss                       = fmap fSrc

---------------------------------------------------------------------------------------
resolveType :: PPR r => AbsPath -> Statement (AnnR r) -> Either FError (Id SrcSpan, TypeDecl r)
---------------------------------------------------------------------------------------
resolveType (QP AK_ _ ss) (ClassStmt l c _ _ cs)
  | [ts] <- cas
  = typeMembers cs >>= return . (cc,) . TD ts
  | otherwise
  = Left $ F.Unsafe [err (sourceSpanSrcSpan l) errMsg ]
  where
    cc     = fmap fSrc c
    cas    = [ ts | ClassAnn ts <- fFact l ]
    errMsg = "Invalid class annotation: " ++ show (intersperse P.comma (map pp cas))

resolveType _ (IfaceStmt l c)
  | [t] <- ifaceAnns  = Right (fmap fSrc c,t)
  | otherwise         = Left $ F.Unsafe [err (sourceSpanSrcSpan l) errMsg ]
  where
    ifaceAnns         = [ t | InterfaceAnn t <- fFact l ]
    errMsg            = "Invalid interface annotation: "
                     ++ show (intersperse P.comma (map pp ifaceAnns))

resolveType _ s       = Left $ F.Unsafe $ single
                      $ err (sourceSpanSrcSpan $ getAnnotation s)
                      $ "Statement\n" ++ ppshow s ++ "\ncannot have a type annotation."


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
    modulesSet  = H.fromList [ ap | (ap,ss) <- allModStmts ]
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


-- | Given a list of class elements, returns a @TypeMembers@ structure
---------------------------------------------------------------------------------------
typeMembers :: PPR r => [ClassElt (AnnR r)] -> Either (F.FixResult Error) (TypeMembers r)
---------------------------------------------------------------------------------------
typeMembers cs = TM <$> ps <*> ms <*> sps <*> sms <*> call <*> ctor <*> sidx <*> nidx
  where
    ps         = pure  $ F.fromListSEnv props
    ms         = meths $ methDefs ++ methDecls
    sps        = pure  $ F.fromListSEnv sprops
    sms        = meths $ smethDefs ++ smethDecls
    call       = pure  $ Nothing
    sidx       = pure  $ Nothing    -- XXX: This could be added
    nidx       = pure  $ Nothing

    props      = [ (F.symbol x, f) | MemberVarDecl l False x _ <- cs, FieldAnn _ f <- fFact l ]

    sprops     = [ (F.symbol x, f) | MemberVarDecl l True x _ <- cs, FieldAnn _ f <- fFact l ]

    meths m    = fmap (F.fromListSEnv . map (F.symbol >< val) . M.toList)
               $ T.sequence
               $ M.mapWithKey (\k v -> snd <$> join (prtn k v))
               $ M.fromListWith (++)
               $ m

    prtn k v   = (k,) . mapPair (map snd) $ partition ((== MemDef) . fst) v

    -- Allowed annotations include a single definition without any declarations,
    -- or two or more declarations (overloads)
    join (k,([t],[])) = Right $ (k, t)                   -- Single definition
    join (k,(ds ,ts)) | length ts > 1
                      = Right $ (k,foldl1 joinMI ts)
                      | otherwise
                      = Left  $ F.Unsafe
                      $ map (\(Loc l v) -> err (sourceSpanSrcSpan l) $ msg k v) $ ds ++ ts

    msg k v = printf "The following annotation for member '%s' is invalid:\n%s" (ppshow k) (ppshow v)

    joinMI (Loc l (MI o m t)) (Loc _ (MI _ _ t')) = Loc l $ MI o m $ mkAnd [t, t']

    methDefs   = [ (x, [(MemDef , Loc (fSrc l) m)]) | MemberMethDef  l False x _ _ <- cs, MethAnn _ m <- fFact l ]
    methDecls  = [ (x, [(MemDecl, Loc (fSrc l) m)]) | MemberMethDecl l False x _   <- cs, MethAnn _ m <- fFact l ]
    smethDefs  = [ (x, [(MemDef , Loc (fSrc l) m)]) | MemberMethDef  l True  x _ _ <- cs, MethAnn _ m <- fFact l ]
    smethDecls = [ (x, [(MemDecl, Loc (fSrc l) m)]) | MemberMethDecl l True  x _   <- cs, MethAnn _ m <- fFact l ]

    ctor       = pure $ listToMaybe [ t | Constructor l _ _ <- cs, ConsAnn t <- fFact l ]


-- Not including class, module, enum names
---------------------------------------------------------------------------------------
visibleVars :: Data r => [Statement (AnnSSA r)] -> [(Id SrcSpan, VarInfo r)]
---------------------------------------------------------------------------------------
visibleVars s = [ (fSrc <$> n, VI a i t)  | (n,l,k,a,i) <- hoistBindings s
                                          , f           <- fFact l
                                          , t           <- annToType a f ]
  where
    annToType Ambient (VarAnn _ t)  = maybeToList t -- Hoist ReadOnly vars (i.e. function defs)
    annToType Ambient (VarAnn _ t)  = maybeToList t -- Hoist ImportDecl (i.e. function decls)
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

