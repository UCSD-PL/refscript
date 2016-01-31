{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}


module Language.Rsc.ClassHierarchy (

      ClassHierarchy (..)
    , CoercionKind (..)

    -- * Build CHA
    , mkCHA

    -- * Queries
    , isClassType, isEnumType, isAncestorOf, ancestors
    , nonStaticFields, inheritedNonStaticFields
    , classAncestors, interfaceAncestors
    , getImmediateSuperclass, getSuperType
    , boundKeys, immFields
    , getMutability

    -- * Type Transformations
    , weaken, expandType

    , typeMemersOfTDecl, typeMembersOfType

    -- * Resolve symbols
    , resolveType, resolveModule, resolveEnum
    ) where

import           Control.Applicative               hiding (empty)
import           Control.Exception                 (throw)
import           Control.Monad                     (liftM, void)
import           Data.Default
import           Data.Foldable                     (foldlM)
import           Data.Generics
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree
import           Data.Graph.Inductive.Query.BFS
import           Data.Graph.Inductive.Query.DFS
import           Data.Graph.Inductive.Query.Monad  ((><))
import qualified Data.HashMap.Strict               as HM
import qualified Data.HashSet                      as HS
import           Data.List                         (find, findIndex, partition)
import qualified Data.Map.Strict                   as M
import           Data.Maybe                        (fromMaybe, isJust, listToMaybe, maybeToList)
import           Data.Monoid                       hiding ((<>))
import qualified Data.Traversable                  as T
import           Data.Tuple                        (swap)
import           Language.Fixpoint.Misc            (intersperse, safeZip)
import qualified Language.Fixpoint.Types           as F
import           Language.Fixpoint.Types.Errors
import           Language.Rsc.Annotations          hiding (err)
import           Language.Rsc.AST
import           Language.Rsc.Core.Env
import           Language.Rsc.Errors
import           Language.Rsc.Locations
import           Language.Rsc.Misc                 (concatMapM, single)
import           Language.Rsc.Module
import           Language.Rsc.Names
import           Language.Rsc.Pretty.Annotations
import           Language.Rsc.Pretty.Common
import           Language.Rsc.Pretty.Errors
import           Language.Rsc.Pretty.Types
import           Language.Rsc.Program
import           Language.Rsc.Symbols
import           Language.Rsc.Traversals
import           Language.Rsc.Typecheck.Subst
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
import           Text.PrettyPrint.HughesPJ
import           Text.Printf


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


--------------------------------------------------------------------------------
mkCHA :: (PPR r, Typeable r, Data r) => BareRsc r -> Either FError (ClassHierarchy r)
--------------------------------------------------------------------------------
mkCHA rsc = moduleEnv rsc >>= pure . fromModuleDef


--------------------------------------------------------------------------------
fromModuleDef :: QEnv (ModuleDef r) -> ClassHierarchy r
--------------------------------------------------------------------------------
fromModuleDef ms = CHA graph nk ms
  where
    graph      = mkGraph ns es
    ns         = zip ([0..] :: [Int]) $ fst <$> data_
    es         = (`concatMap` data_) $ \(TS _ (BGen s _) _, ts) ->
                   [ (σ, τ, g) | (t, g) <- ts
                               , σ <- maybeToList $ HM.lookup s nk
                               , τ <- maybeToList $ HM.lookup t nk ]
    nk         = HM.fromList $ (\(TS _ (BGen n _) _, b) -> (n, b)) . swap <$> ns

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
-- RESolveModule :: ClassHierarchy r -> AbsPath -> Maybe (ModuleDef r)
-- resolveType   :: ClassHierarchy r -> AbsName -> Maybe (TypeDeclQ AK r)
-- resolveEnum   :: ClassHierarchy r -> AbsName -> Maybe EnumDef
--------------------------------------------------------------------------------
resolveModule (cModules -> ms) = (`qenvFindTy` ms)
resolveType cha (QN p s) = resolveModule cha p >>= envFindTy s . m_types
resolveEnum cha (QN p s) = resolveModule cha p >>= envFindTy s . m_enums

--------------------------------------------------------------------------------
typeMembersOfType :: (ExprReftable Int r, PPR r) => ClassHierarchy r -> RType r -> TypeMembers r
typeMemersOfTDecl :: PPR r => ClassHierarchy r -> TypeDecl r -> TypeMembers r
--------------------------------------------------------------------------------
typeMembersOfType cha t
  | Just (TObj ms _) <- expandType Coercive cha t
  = ms
  | otherwise
  = mempty

typeMemersOfTDecl cha (TD (TS k _ (h,_)) es) = es `mappend` heritage h
  where
    expd = expandWithSubst cha
    res  = resolveType cha
    heritage (Gen p ys: _) = fromMaybe mempty $ (`expd` ys) <$> res p
    heritage _             = mempty

---------------------------------------------------------------------------
expandWithSubst :: (SubstitutableQ q r (TypeMembers r), PPR r)
                => ClassHierarchy r -> TypeDeclQ AK r -> [RTypeQ q r] -> TypeMembers r
---------------------------------------------------------------------------
expandWithSubst cha t@(TD (TS _ (BGen _ bvs) _) _) ts
  = apply (fromList $ zip (btvToTV <$> bvs) ts) (typeMemersOfTDecl cha t)

data CoercionKind = Coercive | NonCoercive

-- | `expandType c γ t` expands type @t@ to an object type (TObj)
--  * It is not intended to be called with mutability types (returns `Nothing`)
--  * If @c@ is `Coercive`, then primitive types will be treated as their
--    object counterparts, i.e. String, Number, Boolean.
---------------------------------------------------------------------------
expandType :: (ExprReftable Int r, PPR r)
           => CoercionKind -> ClassHierarchy r -> RType r -> Maybe (RType r)
---------------------------------------------------------------------------
expandType _ _ t@(TObj _ _) = Just t

-- | Enumeration
--
expandType _ cha (TRef (Gen n []) _)
  | Just e <- resolveEnum cha n
  = Just $ TObj (ms e) fTop
  where
    ms  = typeMembersFromList . concatMap mkField . envToList . e_mapping
    -- TODO
    mkField (k, IntLit _ i) = [FI (F.symbol k) Req tIM (tNum `strengthen` exprReft i)]
    mkField (k, HexLit _ s) | Just e <- bitVectorValue s
                            = [FI (F.symbol k) Req tIM (tBV32 `strengthen` e)]
    mkField _               = []

expandType _ _ t@(TRef _ _) | mutRelated t = Nothing

-- | Type Reference
--
--  TODO: revisit !!!
--
expandType _ cha t@(TRef (Gen n ts@(mut:_)) r)
  | isClassType cha t = (\m -> TObj m r) . fltInst <$> ms
  | otherwise         = (\m -> TObj m r)           <$> ms
  where
    ms      =  expandWithSubst cha
           <$> resolveType cha n
           <*> return ts
    fltInst = \(TM m _ _ _ s n) -> TM m mempty Nothing Nothing s n

-- | Ambient type: String, Number, etc.
--
expandType _ cha t@(TRef (Gen n []) r)
  | isClassType cha t = (\m -> TObj m r) . fltInst <$> ms
  | otherwise         = (\m -> TObj m r)           <$> ms
  where
    ms = typeMemersOfTDecl cha <$> resolveType cha n
    fltInst (TM m _ _ _ s n) = TM m mempty Nothing Nothing s n

expandType _ cha (TClass (BGen n ts))
  = (\m -> TObj m fTop) . fltStat <$> ms
  where
    ms  = expandWithSubst cha <$> resolveType cha n <*> return ts'
    ts' = [ tVar $ TV x s | BTV x s _ <- ts ] -- these shouldn't matter anyway
    fltStat (TM _ m c k _ _) = TM mempty m c k Nothing Nothing

expandType _ cha (TMod n)
  = (\m -> TObj m fTop) <$> typeMembers
                             .  fmap (symToField . val)
                             .  m_variables
                            <$> resolveModule cha n
  where

-- Common cases end here. The rest are only valid if non-coercive
expandType NonCoercive _ _ = Nothing

expandType _ cha (TPrim TNumber _)
  = (\m -> TObj m fTop) <$> (typeMemersOfTDecl cha <$> resolveType cha numberInterface)
expandType _ cha (TPrim TString _)
  = (\m -> TObj m fTop) <$> (typeMemersOfTDecl cha <$> resolveType cha stringInterface)
expandType _ cha (TPrim TBoolean _)
  = (\m -> TObj m fTop) <$> (typeMemersOfTDecl cha <$> resolveType cha booleanInterface)

expandType _ _ t  = Just t



-- | `weaken γ A B T..`: Given a relative type name @A@  distinguishes two
-- cases:
--
-- * If A<V..> extends B<S..> (i.e. type B is an ancestor of A), then returns
--   B applied to T.., substituted accordingly to match B's type arguments.
--
-- * If A </: B then return @Nothing@.
--
--------------------------------------------------------------------------------
weaken :: F.Reftable r => ClassHierarchy r -> TGen r -> AbsName -> Maybe (TGen r)
--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
ancestors :: TypeDeclKind -> ClassHierarchy t -> AbsName -> [AbsName]
--------------------------------------------------------------------------------
ancestors k (CHA g m _) s =
    [ n | cur <- maybeToList $ HM.lookup s m
        , anc <- reachable cur g
        , TS k' (BGen n _) _ <- maybeToList $ lab g anc
        , k' == k ]

--------------------------------------------------------------------------------
nonStaticFields :: ClassHierarchy r -> AbsName -> [F.Symbol]
--------------------------------------------------------------------------------
nonStaticFields (CHA g m modules) x
  = HS.toList . HS.unions $ HS.fromList . flds <$> ps
  where
    flds (QN p y) = [ s | ms        <- maybeToList  $ qenvFindTy p modules
                        , TD _ es   <- maybeToList  $ envFindTy y  $ m_types ms
                        , (s, FI{}) <- F.toListSEnv $ i_mems  es ]

    ps            = [ n | cur <- maybeToList $ HM.lookup x m
                        , anc <- reachable cur g
                        , TS k (BGen n _) _ <- maybeToList $ lab g anc
                        , k == ClassTDK ]

--------------------------------------------------------------------------------
inheritedNonStaticFields :: ClassHierarchy r -> AbsName -> [F.Symbol]
--------------------------------------------------------------------------------
inheritedNonStaticFields (CHA g m mod) x
  = HS.toList . HS.unions $ HS.fromList . flds <$> ps
  where
    flds (QN p y) = [ s | ms      <- maybeToList $ qenvFindTy p mod
                        , TD _ es <- maybeToList $ envFindTy  y $ m_types ms
                        , s       <- map fst     $ F.toListSEnv $ i_mems  es ]

    ps            = [ n | cur <- maybeToList $ HM.lookup x m
                        , anc <- reachable cur g
                        , cur /= anc      -- only gather parents
                        , TS k (BGen n _) _ <- maybeToList $ lab g anc
                        , k == ClassTDK ]

--------------------------------------------------------------------------------
classAncestors     :: ClassHierarchy t -> AbsName -> [AbsName]
interfaceAncestors :: ClassHierarchy t -> AbsName -> [AbsName]
allAncestors       :: ClassHierarchy t -> AbsName -> [AbsName]
--------------------------------------------------------------------------------
classAncestors      = ancestors ClassTDK
interfaceAncestors  = ancestors InterfaceTDK
allAncestors γ s    = classAncestors γ s ++ interfaceAncestors γ s


-- | A `isAncestorOf` B <==> class B extends A ... (or transitively)
--------------------------------------------------------------------------------
isAncestorOf :: ClassHierarchy t -> AbsName -> AbsName -> Bool
--------------------------------------------------------------------------------
isAncestorOf cha a b = a `elem` allAncestors cha a

--------------------------------------------------------------------------------
boundKeys :: (ExprReftable Int r, PPR r)
          => ClassHierarchy r -> RType r -> [F.Symbol]
--------------------------------------------------------------------------------
boundKeys cha t@(TRef _ _) | Just t <- expandType Coercive cha t = boundKeys cha t
                           | otherwise                         = []
boundKeys _ (TObj es _)    = fst <$> F.toListSEnv (i_mems es)
boundKeys _ _              = []

--------------------------------------------------------------------------------
immFields :: (ExprReftable Int r, PPR r)
          => ClassHierarchy r -> RType r -> [(F.Symbol, RType r)]
--------------------------------------------------------------------------------
immFields cha t | Just (TObj es _) <- expandType Coercive cha t
                = [ (x, t) | (x, FI _ _ tIM t) <- F.toListSEnv $ i_mems es ]
                | otherwise
                = []

--------------------------------------------------------------------------------
getImmediateSuperclass :: F.Reftable r => TypeSig r -> Maybe (RType r)
--------------------------------------------------------------------------------
getImmediateSuperclass (TS _ _ ([Gen p ps], _)) = Just $ TRef (Gen p ps) fTop
getImmediateSuperclass _ = Nothing

--------------------------------------------------------------------------------
-- getSuperType :: F.Reftable r => ClassHierarchy r -> RType r -> Maybe (RType r)
--------------------------------------------------------------------------------
getSuperType cha (TRef (Gen nm ts) _)
  | Just (TD (TS _ (BGen _ bs) ([p],_)) _) <- resolveType cha nm
  = let θ = fromList $ zip (btvToTV <$> bs) ts in
    Just $ apply θ $ TRef p fTop
  | otherwise
  = Nothing



-- | IGJ's I(..) function
--
--------------------------------------------------------------------------------
getMutability :: (PP r, ExprReftable Int r, F.Reftable r)
              => ClassHierarchy r -> RType r -> Maybe (MutabilityQ AK r)
--------------------------------------------------------------------------------
getMutability _ (TRef (Gen _ (m:_)) _) = Just m
getMutability _ _ = Nothing


-- getMutability cha t | Just (TObj m _ _) <- expandType Coercive cha t
--                     = Just m
--                     | otherwise
--                     = Nothing

