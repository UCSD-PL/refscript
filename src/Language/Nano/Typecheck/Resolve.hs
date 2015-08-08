{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Nano.Typecheck.Resolve ( 
  
  -- * Resolve names
    resolveTypeInEnv, resolveEnumInEnv, resolveModuleInEnv
  , resolveModuleInPgm, resolveTypeInPgm, resolveEnumInPgm

  -- * expand a type definition applying subs
  , expand, expand', CoercionKind(..), expandType

  -- * Ancestors
  , weaken, allAncestors, classAncestors, interfaceAncestors, isAncestor
  , onlyInheritedFields
  , fieldSymbols

  -- * Field Access
  , boundKeys, immFields

  , isClassType

  ) where 

import           Control.Applicative                 ((<$>), (<|>), (<*>))
import           Data.Generics
import qualified Data.HashMap.Strict              as HM
import           Data.Maybe                          (maybeToList, catMaybes, fromMaybe)
import           Data.Foldable                       (foldlM)
import           Data.List                           (find)
import           Data.Monoid
import qualified Data.HashSet                    as  HS
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.Query.DFS
import           Data.Graph.Inductive.Query.BFS
import           Data.Function                       (on)
import qualified Data.Map.Strict                  as M
import qualified Language.Fixpoint.Types          as F
import           Language.Nano.Env
import           Language.Nano.Environment
import           Language.Nano.Names
import           Language.Nano.Locations             (val)
import           Language.Nano.Types
import           Language.Nano.Program
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Subst

import           Language.Nano.Syntax


-- import           Debug.Trace
 
--------------------------------------------------------------------------------
resolveModuleInEnv  :: EnvLike r t => t r -> AbsPath -> Maybe (ModuleDef r)
resolveTypeInEnv    :: EnvLike r t => t r -> AbsName -> Maybe (TypeDecl r)
resolveEnumInEnv    :: EnvLike r t => t r -> AbsName -> Maybe EnumDef
resolveTypeInPgm    :: NanoBareR r -> AbsName -> Maybe (TypeDecl r)
resolveEnumInPgm    :: NanoBareR r -> AbsName -> Maybe EnumDef
resolveModuleInPgm  :: NanoBareR r -> AbsPath -> Maybe (ModuleDef r)
--------------------------------------------------------------------------------
resolveModuleInEnv γ s = qenvFindTy s (modules γ)
resolveTypeInEnv γ (QN AK_ l ss s) = resolveModuleInEnv γ (QP AK_ l ss) >>= envFindTy s . m_types
resolveEnumInEnv γ (QN AK_ l ss s) = resolveModuleInEnv γ (QP AK_ l ss) >>= envFindTy s . m_enums
resolveTypeInPgm p (QN AK_ l ss s) = resolveModuleInPgm p (QP AK_ l ss) >>= envFindTy s . m_types
resolveEnumInPgm p (QN AK_ l ss s) = resolveModuleInPgm p (QP AK_ l ss) >>= envFindTy s . m_enums
resolveModuleInPgm p s = qenvFindTy s $ pModules p

--------------------------------------------------------------------------------
isClassType :: EnvLike r g => g r -> RType r -> Bool
--------------------------------------------------------------------------------
isClassType γ (TRef (Gen x ts) _) | Just (TD k _ _ _) <- resolveTypeInEnv γ x
                                  = k == ClassKind
                                  | otherwise
                                  = False
isClassType _ _                   = False


numberInterface      = mkAbsName [] $ F.symbol "Number"
stringInterface      = mkAbsName [] $ F.symbol "String"
booleanInterface     = mkAbsName [] $ F.symbol "Boolean"
objectInterface      = mkAbsName [] $ F.symbol "Object"
functionInterface    = mkAbsName [] $ F.symbol "Function"
emptyObjectInterface = mkAbsName [] $ F.symbol "EmptyObject"

---------------------------------------------------------------------------
expand :: (EnvLike r g, PPR r) => g r -> TypeDecl r -> TypeMembers r
---------------------------------------------------------------------------
expand γ (TD _ _ (h,_) es)      = es `mappend` heritage h
  where
    exp                         = expand' γ
    res                         = resolveTypeInEnv γ
    heritage (Just (Gen p ys))  = fromMaybe mempty $ (`exp` ys) <$> res p
    heritage _                  = mempty

---------------------------------------------------------------------------
expand' :: (EnvLike r g, PPR r) => g r -> TypeDecl r -> [RType r] -> TypeMembers r
---------------------------------------------------------------------------
expand' γ t@(TD _ (BGen _ bvs) _ _) ts = apply θ $ expand γ t
  where 
    θ = fromList $ zip (btvToTV <$> bvs) ts

-- | expand'' also returns the interface's type parameters
-- expand'' γ d@(ID _ _ vs _ _)    = (vs,) <$> expand st γ d (tVar <$> vs)


data CoercionKind = Coercive | NonCoercive
 
-- | `expandType c γ t` expands type @t@ to an object type (TObj)
--
--  * It is not intended to be called with mutability types (returns `Nothing`)
--
--  * If @c@ is `Coercive`, then primitive types will be treated as their
--    object counterparts, i.e. String, Number, Boolean. 
--
---------------------------------------------------------------------------
expandType :: (PPR r, EnvLike r g, Data r) => CoercionKind -> g r -> RType r -> Maybe (RType r)
---------------------------------------------------------------------------
expandType _ _ t@(TObj _ _) = Just t 

expandType _ γ t@(TRef _ _) | mutRelated t = Nothing 

expandType _ γ (TRef (Gen n ts) r) 
  = (`TObj` r)    <$> (expand' γ <$> resolveTypeInEnv γ n <*> return ts)

expandType _ γ (TType ClassK (Gen n ts))
  = (`TObj` fTop) <$> (expand' γ <$> resolveTypeInEnv γ n <*> return ts)

expandType _ γ (TMod n)
  = (`TObj` fTop) <$> tmFromFields
                   .  fmap toFieldInfo
                   .  m_variables 
                  <$> resolveModuleInEnv γ n
  where
    toFieldInfo (val -> VI _ _ t) = FI [] tImm t

expandType _ γ (TType EnumK (Gen x _))
  = (`TObj` fTop) <$> tmFromFieldList
                   .  concatMap  mkField 
                   .  envToList 
                   .  e_mapping 
                  <$> resolveEnumInEnv γ x
  where
    -- TODO 
    mkField (k, IntLit _ i) = [(k, FI [] tImm (tNum `strengthen` exprReft i))]
    mkField (k, HexLit _ s) | Just e <- bitVectorValue s
                            = [(k, FI [] tImm (tBV32 `strengthen` e))]
    mkField _               = []

expandType NonCoercive γ t = Nothing

-- FIXME: Even these should inherit from Object
expandType _ γ (TPrim TNumber _)  = (`TObj` fTop) <$> (expand γ <$> resolveTypeInEnv γ numberInterface)
expandType _ γ (TPrim TString _)  = (`TObj` fTop) <$> (expand γ <$> resolveTypeInEnv γ stringInterface)
expandType _ γ (TPrim TBoolean _) = (`TObj` fTop) <$> (expand γ <$> resolveTypeInEnv γ booleanInterface)

expandType _ _ t  = Just t

-- | `weaken γ A B T..`: Given a relative type name @A@  distinguishes two
--   cases:
--
--    * If A<V..> extends B<S..> (i.e. type B is an ancestor of A), then returns
--      B applied to T.., substituted accordingly to match B's type arguments.
--
--    * If A </: B then return @Nothing@.
--
---------------------------------------------------------------------------
weaken :: (PPR r, EnvLike r g) => g r -> TGen r -> AbsName -> Maybe (TGen r)
---------------------------------------------------------------------------
weaken γ tr@(Gen s _) t
  | s == t                    = Just tr
  | otherwise 
  = do n1                    <- HM.lookup s m
       n2                    <- HM.lookup t m

       case unwrap $ lesp n1 n2 g of
         []                  -> Nothing
         path                -> foldlM (doEdge ch) tr $ map toNodes (toEdges path)
  where
    ch@(ClassHierarchy g m)   = cha γ
    unwrap (LP lpath)         = lpath
    toEdges xs                = zip (init xs) (tail xs)
    toNodes ((n1,_),(n2,_))   = (n1,n2)

---------------------------------------------------------------------------
doEdge :: PPR r => ClassHierarchy r -> TGen r -> Edge -> Maybe (TGen r)
---------------------------------------------------------------------------
doEdge (ClassHierarchy g _) (Gen _ t1) (n1, n2)
  = do  TD _ (BGen c1 v1) (e1,i1) _ <- lab g n1
        TD _ (BGen c2 v2) _       _ <- lab g n2
        let θ                        = fromList $ zip (btvToTV <$> v1) t1
        Gen n2 t2             <-  find ((c2 ==) . g_name) (maybeToList e1)
                              <|> find ((c2 ==) . g_name) i1
        return                 $  Gen n2 $ apply θ t2

---------------------------------------------------------------------------
ancestors :: EnvLike r t => TypeDeclKind -> t r -> AbsName -> [AbsName]
---------------------------------------------------------------------------
ancestors k (cha -> ClassHierarchy g m) s = [ n | cur <- maybeToList $ HM.lookup s m
                      , anc <- reachable cur g
                      , TD k' (BGen n _) _ _ <- maybeToList $ lab g anc
                      , k' == k ]

-- XXX : only strict parents 
---------------------------------------------------------------------------
strictAncestorsFromPgm :: Nano a r -> AbsName -> [AbsName]
---------------------------------------------------------------------------
strictAncestorsFromPgm (pCHA -> ClassHierarchy g m) s 
  = [ n | cur <- maybeToList $ HM.lookup s m
        , anc <- reachable cur g
        , cur /= anc      -- only gather parents
        , TD k (BGen n _) _ _ <- maybeToList $ lab g anc
        , k == ClassKind ]

-- XXX : includes search in all parent classes (including the current one)
---------------------------------------------------------------------------
classAncestorsFromPgm :: Nano a r -> AbsName -> [AbsName]
---------------------------------------------------------------------------
classAncestorsFromPgm (pCHA -> ClassHierarchy g m) s 
  = [ n | cur <- maybeToList (HM.lookup s m)
        , anc <- reachable cur g
        , TD k (BGen n _) _ _ <- maybeToList $ lab g anc
        , k == ClassKind ]

---------------------------------------------------------------------------
fieldSymbols :: StaticKind -> NanoBareR r -> AbsName -> [F.Symbol]
---------------------------------------------------------------------------
fieldSymbols k p a   = HS.toList . HS.unions 
                                $ HS.fromList . flds 
                               <$> classAncestorsFromPgm p a 
  where
    flds a = [ s | TD _ _ _ es <- maybeToList (resolveTypeInPgm p a)
                 , s <- map fst $ F.toListSEnv $ (props k) es ]
    props StaticMember         = tm_sprop
    props InstanceMember        = tm_prop

---------------------------------------------------------------------------
onlyInheritedFields :: StaticKind -> NanoBareR r -> AbsName -> [F.Symbol]
---------------------------------------------------------------------------
onlyInheritedFields k p a = HS.toList . HS.unions 
                                     $ HS.fromList . flds 
                                    <$> strictAncestorsFromPgm p a 
  where
    flds a = [ s | TD _ _ _ es <- maybeToList (resolveTypeInPgm p a)
                 , s <- map fst $ F.toListSEnv $ (props k) es ] 
    props StaticMember         = tm_sprop
    props InstanceMember        = tm_prop

---------------------------------------------------------------------------
classAncestors     :: EnvLike r t => t r -> AbsName -> [AbsName]
interfaceAncestors :: EnvLike r t => t r -> AbsName -> [AbsName]
allAncestors       :: EnvLike r t => t r -> AbsName -> [AbsName]
---------------------------------------------------------------------------
classAncestors      = ancestors ClassKind
interfaceAncestors  = ancestors InterfaceKind
allAncestors γ s    = classAncestors γ s ++ interfaceAncestors γ s 

---------------------------------------------------------------------------
isAncestor :: (PPR r, EnvLike r g) => g r -> AbsName -> AbsName -> Bool
---------------------------------------------------------------------------
isAncestor γ c p = p `elem` allAncestors γ c

---------------------------------------------------------------------------
boundKeys :: (PPR r, EnvLike r g) => g r -> RType r -> [F.Symbol]
---------------------------------------------------------------------------
boundKeys γ t@(TRef _ _) | Just t <- expandType Coercive γ t = boundKeys γ t
                         | otherwise                         = []
boundKeys _ (TObj es _)  = fst <$> F.toListSEnv (tm_prop es)
boundKeys _ _            = []

---------------------------------------------------------------------------
immFields :: (PPR r, EnvLike r g) => g r -> RType r -> [(F.Symbol, RType r)]
---------------------------------------------------------------------------
immFields γ t 
  | Just (TObj es _) <- expandType Coercive γ t 
  = [ (x,t) | (x, FI _ m t) <- F.toListSEnv $ tm_prop es, isImm m ]
  | otherwise
  = []

