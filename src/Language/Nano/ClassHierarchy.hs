{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Language.Nano.ClassHierarchy
    ( ClassHierarchy (..)
    , CoercionKind (..)

    -- * Build CHA
    , mkCHA, mkVarEnv

    -- * Queries
    , isClassType, isEnumType, isAncestor, ancestors
    , nonStaticFields, inheritedNonStaticFields
    , classAncestors, interfaceAncestors
    , getImmediateSuperclass, getSuperType
    , boundKeys, immFields

    -- * Type Transformations
    , weaken, toTypeMembers, expandType

    -- * Resolve symbols
    , resolveType, resolveModule, resolveEnum
    ) where

import           Control.Applicative               hiding (empty)
import           Control.Exception                 (throw)
import           Control.Monad                     (liftM)
import           Control.Monad.Trans.Class         (lift)
import           Data.Foldable                     (foldlM)
import           Data.Generics
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree
import           Data.Graph.Inductive.Query.BFS
import           Data.Graph.Inductive.Query.DFS
import           Data.Graph.Inductive.Query.Monad  ((><))
import qualified Data.HashMap.Strict               as HM
import qualified Data.HashSet                      as HS
import           Data.List                         (find, partition)
import qualified Data.Map.Strict                   as M
import           Data.Maybe                        (fromMaybe, isJust, listToMaybe, maybeToList)
import           Data.Monoid                       hiding ((<>))
import qualified Data.Traversable                  as T
import           Data.Tuple                        (swap)
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc            (concatMapM, intersperse, mapPair, single)
import qualified Language.Fixpoint.Types           as F
import           Language.Nano.Annots              hiding (err)
import           Language.Nano.AST
import           Language.Nano.Core.Env
import           Language.Nano.Errors
import           Language.Nano.Locations
import           Language.Nano.Names
import           Language.Nano.Pretty
import           Language.Nano.Program
import           Language.Nano.Traversals
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Typecheck.Types
import           Language.Nano.Types
import qualified Text.PrettyPrint.HughesPJ         as P
import           Text.Printf

type FError = F.FixResult Error


data ClassHierarchy r = CHA {
  --
  -- | A graph whose nodes are labeled with the absolute path
  -- of the current type and edges are labeled with the generic
  -- type that corresponds to the partitcular ancestor relationship.
  -- For example,
  --
  --    class A extends B<T> { ... }
  --
  --
  -- implies an edge:
  --
  --           B<T>
  --      A ---------> B
  --
    cGraph       :: Gr (TypeSig r) (TGen r)

  --
  -- | Mapping from full name of type to Node representation in graph
  --
  , cNodesToKeys :: HM.HashMap AbsName Int
  --
  -- | Module definitions
  --
  , cModules     :: QEnv (ModuleDef r)
  }


instance Functor ClassHierarchy where
  fmap f (CHA g n m) = CHA (nmap (fmap f) $ emap (fmap f) g) n (fmap (fmap f) m)


------------------------------------------------------------------------------------
mkCHA :: (PPR r, Typeable r, Data r) => BareRsc r -> Either (F.FixResult Error) (ClassHierarchy r)
------------------------------------------------------------------------------------
mkCHA rsc = accumModules rsc >>= pure . fromModuleDef


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
accumModules (Rsc { code = Src stmts }) =
    mapM mkMod (accumModuleStmts stmts) >>= return . qenvFromList . map toQEnvList
  where
    toQEnvList p = (m_path p, p)
    mkMod (p,s) = ModuleDef <$> varEnv p s <*> typeEnv s <*> enumEnv s <*> return p

    -- | Variables
    varEnv p = return . mkVarEnv . vStmts p
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
    vStmt _ (ClassStmt l x _ _ _) =
         [(ss x, ClassDefKind, VI Ambient Initialized $ TClass b) | ClassAnn (TS _ b _) <- fFact l ]
    vStmt p (ModuleStmt l x _) =
         [(ss x, ModuleDefKind, VI Ambient Initialized $ TMod $ pathInPath l p x)]
    vStmt p (EnumStmt _ x _) =
         [(ss x, EnumDefKind, VI Ambient Initialized $ TRef (Gen (QN p $ F.symbol x) []) fTop) ]
    vStmt _ _ = []

    -- | Type Definitions
    typeEnv = liftM envFromList . tStmts
    tStmts  = concatMapM tStmt

    tStmt c@ClassStmt{} = single <$> declOfStmt c
    tStmt c@IfaceStmt{} = single <$> declOfStmt c
    tStmt _             = return $ [ ]

    -- | Enumerations
    enumEnv = return . envFromList . eStmts
    eStmts  = concatMap eStmt

    eStmt (EnumStmt _ n es)  = [(fmap srcPos n, EnumDef (F.symbol n) (envFromList $ sEnumElt <$> es))]
    eStmt _                  = []
    sEnumElt (EnumElt _ s e) = (F.symbol s, fmap (const ()) e)

    ss                       = fmap fSrc

---------------------------------------------------------------------------------------
mkVarEnv :: F.Symbolic s => [(s, SyntaxKind, VarInfo r)] -> Env (VarInfo r)
---------------------------------------------------------------------------------------
mkVarEnv = envMap snd
              . envFromListWithKey mergeVarInfo
              . concatMap f
              . M.toList
              . foldl merge M.empty
  where
    merge ms (x, k, v) = M.insertWith (++) (F.symbol x) [(k,v)] ms

    f (s, vs)   = [ (s, (k, g v [ v' | (FuncOverloadKind, v') <- vs ])) | (k@FuncDefKind, v) <- vs ] ++
                    ( (s,) . (FuncAmbientKind,) <$> amb [ v | (FuncAmbientKind, v) <- vs ] ) ++
                  [ (s, (k, v)) | (k@VarDeclKind, v) <- vs ] ++
                  [ (s, (k, v)) | (k@ClassDefKind, v) <- vs ] ++
                  [ (s, (k, v)) | (k@ModuleDefKind, v) <- vs ] ++
                  [ (s, (k, v)) | (k@EnumDefKind, v) <- vs ]

    g v []                = v
    g _ vs@(VI a i _ : _) = VI a i $ mkAnd $ v_type <$> vs

    amb [ ] = [ ]
    amb [v] = [v]
    amb vs@(VI a i _ : _) = [ VI a i $ mkAnd $ v_type <$> vs ]

    mergeVarInfo x _ _ = throw $ errorDuplicateKey (srcPos x) x


---------------------------------------------------------------------------------------
declOfStmt :: PPR r => Statement (AnnR r) -> Either FError (Id SrcSpan, TypeDecl r)
---------------------------------------------------------------------------------------
declOfStmt (ClassStmt l c _ _ cs)
  | [ts] <- cas
  = typeMembers cs >>= return . (cc,) . TD ts
  | otherwise
  = Left $ F.Unsafe [err (sourceSpanSrcSpan l) errMsg ]
  where
    cc     = fmap fSrc c
    cas    = [ ts | ClassAnn ts <- fFact l ]
    errMsg = "Invalid class annotation: " ++ show (intersperse P.comma (map pp cas))

declOfStmt (IfaceStmt l c)
  | [t] <- ifaceAnns  = Right (fmap fSrc c,t)
  | otherwise         = Left $ F.Unsafe [err (sourceSpanSrcSpan l) errMsg ]
  where
    ifaceAnns         = [ t | InterfaceAnn t <- fFact l ]
    errMsg            = "Invalid interface annotation: "
                     ++ show (intersperse P.comma (map pp ifaceAnns))

declOfStmt s         = Left $ F.Unsafe $ single
                      $ err (sourceSpanSrcSpan $ getAnnotation s)
                      $ "Statement\n" ++ ppshow s ++ "\ncannot have a type annotation."


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




---------------------------------------------------------------------------
fromModuleDef :: QEnv (ModuleDef r) -> ClassHierarchy r
---------------------------------------------------------------------------
fromModuleDef ms = CHA graph nk ms
  where
    graph      = mkGraph ns es
    ns         = zip ([0..] :: [Int]) $ fst <$> data_
    es         = (`concatMap` data_) $ \(TS _ (BGen s _) _, ts) ->
                   [ (σ, τ, g) | (t, g) <- ts
                               , σ <- maybeToList $ HM.lookup s nk
                               , τ <- maybeToList $ HM.lookup t nk ]
    nk         = HM.fromList $ (\(TS _ (BGen n _) _, b) -> (n, b)) <$> swap <$> ns

    data_      = concatMap foo $ qenvToList ms

    foo (_, m) = map bar $ envToList $ m_types m
    bar (_, d) | TD s _ <- d, TS _ _ h <- s
               = (s, [ (x, g) | g@(Gen x _) <- uncurry (++) h ])

--------------------------------------------------------------------------------
isClassType :: ClassHierarchy r -> RTypeQ AK t -> Bool
--------------------------------------------------------------------------------
isClassType cha n | TRef (Gen x _) _ <- n
                  , Just d           <- resolveType cha x
                  , TD (TS k _ _) _  <- d
                  = k == ClassTDK
                  | otherwise
                  = False

--------------------------------------------------------------------------------
isEnumType :: ClassHierarchy r -> RType t -> Bool
--------------------------------------------------------------------------------
isEnumType (cModules -> mod) (TRef (Gen (QN p x) []) _)
  | Just m <- qenvFindTy p mod = isJust $ envFindTy x $ m_enums m
isEnumType _ _ = False

numberInterface      = mkAbsName [] $ F.symbol "Number"
stringInterface      = mkAbsName [] $ F.symbol "String"
booleanInterface     = mkAbsName [] $ F.symbol "Boolean"
objectInterface      = mkAbsName [] $ F.symbol "Object"
functionInterface    = mkAbsName [] $ F.symbol "Function"
emptyObjectInterface = mkAbsName [] $ F.symbol "EmptyObject"

--------------------------------------------------------------------------------
resolveModule :: ClassHierarchy r -> AbsPath -> Maybe (ModuleDef r)
resolveType   :: ClassHierarchy r -> AbsName -> Maybe (TypeDeclQ AK r)
resolveEnum   :: ClassHierarchy r -> AbsName -> Maybe EnumDef
--------------------------------------------------------------------------------
resolveModule (cModules -> ms) = (`qenvFindTy` ms)
resolveType cha (QN p s) = resolveModule cha p >>= envFindTy s . m_types
resolveEnum cha (QN p s) = resolveModule cha p >>= envFindTy s . m_enums

-- | `toTypeMembers static cha d` converts TypeDecl @d@ to the corresponding
-- TypeMembers.
--  * If this is a static class reference (e.g. originating from a TClass)
--    then the result may include a constructor.
--  * An interface reference may include the constructor either way.
---------------------------------------------------------------------------
toTypeMembers :: F.Reftable r => ClassHierarchy r -> TypeDecl r -> TypeMembers r
---------------------------------------------------------------------------
toTypeMembers cha (TD (TS k _ (h,_)) es) =
    es `mappend` heritage h
  where
    expd = expandWithSubst cha
    res  = resolveType cha
    heritage (Gen p ys: _) = fromMaybe mempty $ (`expd` ys) <$> res p
    heritage _             = mempty

---------------------------------------------------------------------------
expandWithSubst :: (SubstitutableQ q r (TypeMembers r), F.Reftable r)
                => ClassHierarchy r -> TypeDeclQ AK r -> [RTypeQ q r] -> TypeMembers r
---------------------------------------------------------------------------
expandWithSubst cha t@(TD (TS _ (BGen _ bvs) _) _) ts = apply θ $ toTypeMembers cha t
  where
    θ = fromList $ zip (btvToTV <$> bvs) ts


data CoercionKind = Coercive | NonCoercive

-- | `expandType c γ t` expands type @t@ to an object type (TObj)
--  * It is not intended to be called with mutability types (returns `Nothing`)
--  * If @c@ is `Coercive`, then primitive types will be treated as their
--    object counterparts, i.e. String, Number, Boolean.
---------------------------------------------------------------------------
expandType :: (ExprReftable Int r, F.Reftable r)
           => CoercionKind -> ClassHierarchy r -> RType r -> Maybe (RType r)
---------------------------------------------------------------------------
expandType _ _ t@(TObj _ _) = Just t

-- | Enumeration
expandType _ cha (TRef (Gen n []) _)
  | Just e <- resolveEnum cha n
  = Just $ TObj (tmFromFieldList $ concatMap mkField $ envToList $ e_mapping e) fTop
  where
    -- TODO
    mkField (k, IntLit _ i) = [(k, FI Req tImm (tNum `strengthen` exprReft i))]
    mkField (k, HexLit _ s) | Just e <- bitVectorValue s
                            = [(k, FI Req tImm (tBV32 `strengthen` e))]
    mkField _               = []

expandType _ _ t@(TRef _ _) | mutRelated t = Nothing

-- | Type Reference
expandType _ cha t@(TRef (Gen n ts) r)
  | isClassType cha t = (`TObj` r) . fltInst <$> ms
  | otherwise         = (`TObj` r)           <$> ms
  where
    ms = expandWithSubst cha <$> resolveType cha n <*> return ts
    fltInst (TM p _ m _ _ _ s n) = TM p mempty m mempty Nothing Nothing s n

expandType _ cha (TClass (BGen n ts))
  = (`TObj` fTop) . fltStat <$> ms
  where
    ms  = expandWithSubst cha <$> resolveType cha n <*> return ts'
    ts' = [ tVar $ TV x s | BTV x s _ <- ts ] -- these shouldn't matter anyway
    fltStat (TM _ p _ m c k _ _) = TM mempty p mempty m c k Nothing Nothing

expandType _ cha (TMod n)
  = (`TObj` fTop) <$> tmFromFields
                   .  fmap toFieldInfo
                   .  m_variables
                  <$> resolveModule cha n
  where
    toFieldInfo (val -> VI _ _ t) = FI Req tImm t

-- Common cases end here. The rest are only valid if non-coercive
expandType NonCoercive _ _ = Nothing

expandType _ cha (TPrim TNumber _)
  = (`TObj` fTop) <$> (toTypeMembers cha <$> resolveType cha numberInterface)
expandType _ cha (TPrim TString _)
  = (`TObj` fTop) <$> (toTypeMembers cha <$> resolveType cha stringInterface)
expandType _ cha (TPrim TBoolean _)
  = (`TObj` fTop) <$> (toTypeMembers cha <$> resolveType cha booleanInterface)

expandType _ _ t  = Just t

-- | `weaken γ A B T..`: Given a relative type name @A@  distinguishes two
-- cases:
--
-- * If A<V..> extends B<S..> (i.e. type B is an ancestor of A), then returns
--   B applied to T.., substituted accordingly to match B's type arguments.
--
-- * If A </: B then return @Nothing@.
--
---------------------------------------------------------------------------
weaken :: PPR r => ClassHierarchy r -> TGen r -> AbsName -> Maybe (TGen r)
---------------------------------------------------------------------------
weaken (CHA g m _) tr@(Gen s _) t
  | s == t                    = Just tr
  | otherwise
  = do (n1, n2) <- (,) <$> HM.lookup s m <*> HM.lookup t m
       case unwrap $ lesp n1 n2 g of
         []   -> Nothing
         path -> foldlM onEdge tr $ toNodes <$> toEdges path
  where
    unwrap (LP lpath) = lpath
    toEdges xs = init xs `zip` tail xs
    toNodes ((n1,_),(n2,_)) = (n1,n2)
    onEdge (Gen _ t1s) (n1, n2)
      = do  TS _ (BGen _ b1s) (e1, i1) <- lab g n1
            TS _ (BGen x2 _) _ <- lab g n2
            let θ = fromList $ map btvToTV b1s `zip` t1s
            Gen _ t2 <- find ((x2 ==) . g_name) $ e1 ++ i1
            return $ Gen x2 $ apply θ t2

---------------------------------------------------------------------------
ancestors :: TypeDeclKind -> ClassHierarchy t -> AbsName -> [AbsName]
---------------------------------------------------------------------------
ancestors k (CHA g m _) s =
    [ n | cur <- maybeToList $ HM.lookup s m
        , anc <- reachable cur g
        , TS k' (BGen n _) _ <- maybeToList $ lab g anc
        , k' == k ]

---------------------------------------------------------------------------
nonStaticFields :: ClassHierarchy r -> AbsName -> [F.Symbol]
---------------------------------------------------------------------------
nonStaticFields (CHA g m modules) x
  = HS.toList . HS.unions $ HS.fromList . flds <$> ps
  where
    flds (QN p y) = [ s | ms      <- maybeToList $ qenvFindTy p modules
                        , TD _ es <- maybeToList $ envFindTy y $ m_types ms
                        , s       <- map fst $ F.toListSEnv $ tm_prop es ]

    ps            = [ n | cur <- maybeToList $ HM.lookup x m
                        , anc <- reachable cur g
                        , TS k (BGen n _) _ <- maybeToList $ lab g anc
                        , k == ClassTDK ]

---------------------------------------------------------------------------
inheritedNonStaticFields :: ClassHierarchy r -> AbsName -> [F.Symbol]
---------------------------------------------------------------------------
inheritedNonStaticFields (CHA g m mod) x
  = HS.toList . HS.unions $ HS.fromList . flds <$> ps
  where
    flds (QN p y) = [ s | ms      <- maybeToList $ qenvFindTy p mod
                        , TD _ es <- maybeToList $ envFindTy  y $ m_types ms
                        , s       <- map fst $ F.toListSEnv $ tm_prop es ]

    ps            = [ n | cur <- maybeToList $ HM.lookup x m
                        , anc <- reachable cur g
                        , cur /= anc      -- only gather parents
                        , TS k (BGen n _) _ <- maybeToList $ lab g anc
                        , k == ClassTDK ]

---------------------------------------------------------------------------
classAncestors     :: ClassHierarchy t -> AbsName -> [AbsName]
interfaceAncestors :: ClassHierarchy t -> AbsName -> [AbsName]
allAncestors       :: ClassHierarchy t -> AbsName -> [AbsName]
---------------------------------------------------------------------------
classAncestors      = ancestors ClassTDK
interfaceAncestors  = ancestors InterfaceTDK
allAncestors γ s    = classAncestors γ s ++ interfaceAncestors γ s

---------------------------------------------------------------------------
isAncestor :: ClassHierarchy t -> AbsName -> AbsName -> Bool
---------------------------------------------------------------------------
isAncestor cha c p = p `elem` allAncestors cha c

---------------------------------------------------------------------------
boundKeys :: (ExprReftable Int r, F.Reftable r)
          => ClassHierarchy r -> RType r -> [F.Symbol]
---------------------------------------------------------------------------
boundKeys cha t@(TRef _ _) | Just t <- expandType Coercive cha t = boundKeys cha t
                           | otherwise                         = []
boundKeys _ (TObj es _)    = fst <$> F.toListSEnv (tm_prop es)
boundKeys _ _              = []

---------------------------------------------------------------------------
immFields :: (ExprReftable Int r, F.Reftable r)
          => ClassHierarchy r -> RType r -> [(F.Symbol, RType r)]
---------------------------------------------------------------------------
immFields cha t | Just (TObj es _) <- expandType Coercive cha t
                = [ (x,t) | (x, FI _ m t) <- F.toListSEnv $ tm_prop es, isImm m ]
                | otherwise
                = []

-------------------------------------------------------------------------------
getImmediateSuperclass :: F.Reftable r => TypeSig r -> Maybe (RType r)
-------------------------------------------------------------------------------
getImmediateSuperclass (TS _ _ ([Gen p ps], _)) = Just $ TRef (Gen p ps) fTop
getImmediateSuperclass _ = Nothing

-------------------------------------------------------------------------------
getSuperType :: F.Reftable r => ClassHierarchy r -> RType r -> Maybe (RType r)
-------------------------------------------------------------------------------
getSuperType cha (TRef (Gen nm ts) _)
  | Just (TD (TS _ (BGen _ bs) ([p],_)) _) <- resolveType cha nm
  = let θ = fromList $ zip (btvToTV <$> bs) ts in
    Just $ apply θ $ TRef p fTop
  | otherwise
  = Nothing

