{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Nano.Typecheck.Resolve ( 
  
  -- * Resolve names
    resolveTypeInEnv, resolveEnumInEnv, resolveModuleInEnv
  , resolveModuleInPgm, resolveTypeInPgm, resolveEnumInPgm

  -- * expand a type definition applying subs
  , expand, expand', expand'', CoercionKind(..), expandType

  -- * Ancestors
  , weaken, allAncestors, classAncestors, interfaceAncestors, isAncestor
  , onlyInheritedFields
  , allFields

  -- * Keys
  , boundKeys

  -- * Constructors
  , Constructor, {- toConstructor, isConstSubtype, -} sameTypeof, getTypeof
  , emptyObjectInterface

  , isClassType


  ) where 

import           Control.Applicative                 ((<$>), (<|>))
import           Data.Generics
import qualified Data.HashMap.Strict              as HM
import           Data.Maybe                          (maybeToList, catMaybes)
import           Data.Foldable                       (foldlM)
import           Data.List                           (find)
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
import           Language.Nano.Types
import           Language.Nano.Program
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Subst

import           Language.ECMAScript3.Syntax


-- import           Debug.Trace

resolveTypeInEnv :: EnvLike r t => t r -> AbsName -> Maybe (IfaceDef r)
resolveTypeInEnv γ (QN AK_ l ss s) = resolveModuleInEnv γ (QP AK_ l ss) 
                                 >>= envFindTy s . m_types

resolveEnumInEnv :: EnvLike r t => t r -> AbsName -> Maybe EnumDef
resolveEnumInEnv γ (QN AK_ l ss s) = resolveModuleInEnv γ (QP AK_ l ss) 
                                 >>= envFindTy s . m_enums
 
resolveModuleInEnv :: EnvLike r t => t r -> AbsPath -> Maybe (ModuleDef r)
resolveModuleInEnv γ s = qenvFindTy s (modules γ)


resolveTypeInPgm :: NanoBareR r -> AbsName -> Maybe (IfaceDef r)
resolveTypeInPgm p (QN AK_ l ss s) = resolveModuleInPgm p (QP AK_ l ss) 
                                 >>= envFindTy s . m_types

resolveEnumInPgm :: NanoBareR r -> AbsName -> Maybe EnumDef
resolveEnumInPgm p (QN AK_ l ss s) = resolveModuleInPgm p (QP AK_ l ss) 
                                 >>= envFindTy s . m_enums
 
resolveModuleInPgm :: NanoBareR r -> AbsPath -> Maybe (ModuleDef r)
resolveModuleInPgm p s = qenvFindTy s $ pModules p

--------------------------------------------------------------------------------
isClassType :: EnvLike r g => g r -> RType r -> Bool
--------------------------------------------------------------------------------
isClassType γ (TRef x _ _ )
  | Just (ID _ k _ _ _ ) <-resolveTypeInEnv γ x 
  = k == ClassKind
  | otherwise
  = False
isClassType _ _ = False


-- | expandning 

numberInterface      = mkAbsName [] $ F.symbol "Number"
stringInterface      = mkAbsName [] $ F.symbol "String"
booleanInterface     = mkAbsName [] $ F.symbol "Boolean"
objectInterface      = mkAbsName [] $ F.symbol "Object"
functionInterface    = mkAbsName [] $ F.symbol "Function"

emptyObjectInterface = mkAbsName [] $ F.symbol "EmptyObject"


-- 
-- | `expand m b γ d ts` 
--
--   * epands a type reference to a structural type 
--
--   * Output includes all elements of the the named type and its ancestors
--
--   * @b@ determines if static or non-static elements should be included
-- 
--   * @m@ is the top-level enforced mutability Top-level 
--
--   * When no parent is found 'Object' is used
--
---------------------------------------------------------------------------
expand :: (EnvLike r g, PPR r) 
       => StaticKind -> g r -> IfaceDef r -> [RType r] -> Maybe (TypeMembers r)
---------------------------------------------------------------------------
expand s γ (ID c _ vs h es) ts  =  M.map (apply θ) 
                                .  M.unions 
                                .  (current:)
                               <$> heritage h
  where
    current                     = M.filterWithKey (\(_,s') _ -> s == s') es

    θ                           = fromList $ zip vs ts

    -- All object-types inherit from the Object interface
    heritage ([],_)             | c == objectInterface 
                                -- The object does not have a __proto__ field 
                                = Just []
                                | otherwise
                                -- Other objects have an Object-typed __proto__ field
                                = Just [M.fromList [(proto_key,proto_fld)]] 
                                -- = mapM fields [(objectInterface,[])]
    heritage (es,_)             = mapM fields es

    fields (p,ts)               = resolveTypeInEnv γ p >>= \d -> expand s γ d ts
    
    proto_sym                   = F.symbol "__proto__"
    proto_key                   = (proto_sym, InstanceMember)
    object_ty                   = TRef objectInterface [t_immutable] fTop
    proto_fld                   = FieldSig proto_sym f_required t_immutable object_ty


-- | expand' does not apply the top-level type substitution
expand' st γ d@(ID _ _ vs _ _)  = expand st γ d (tVar <$> vs)

-- | expand'' also returns the interface's type parameters
expand'' st γ d@(ID _ _ vs _ _) = (vs,) <$> expand st γ d (tVar <$> vs)


data CoercionKind = Coercive | NonCoercive
 
-- | `expandType` will expand *any* type (including Mutability types!)
--    It is not intended to be called with mutability types.
--    Types with zero type parameters (missing mutability field) are considered
--    invalid.
--
--    expand pushes top-level enforced mutabilities down towards the fields,
--    i.e. if the top-level is 'AssignsFields' then fields get Mutable
--    (overriding their current mutability).
--
---------------------------------------------------------------------------
expandType :: (PPR r, EnvLike r g, Data r)
           => CoercionKind -> g r -> RType r -> Maybe (RType r)
---------------------------------------------------------------------------
-- 
-- Given an object literal type, @expandType@ returns an expanded TCons containing 
-- the inherited (from Object) fields. 
-- 
expandType _ γ (TCons m es r) 
  = do  empty   <- resolveTypeInEnv γ emptyObjectInterface
        es'     <- expand' InstanceMember γ $ empty { t_elts = es }
        return   $ TCons m es' r
-- 
-- FIXME: expandType for Mutability could easiliy return the same type 
--
expandType _ γ (TRef x [] r)
  = do  d       <- resolveTypeInEnv γ x
        es      <- expand' InstanceMember γ d
        return   $ TCons t_immutable es r

expandType _ γ (TRef x ts@(m:_) r)
  = do  d       <- resolveTypeInEnv γ x
        es      <- expand InstanceMember γ d ts
        return   $ TCons (toType m) es r

expandType _ γ (TClass x)             
  = do  d       <- resolveTypeInEnv γ x
        es      <- expand' StaticMember γ d
        return   $ TCons t_immutable es fTop

expandType _ γ (TModule x)             
  = do  es      <- M.fromList . map mkField . envToList . m_variables <$> resolveModuleInEnv γ x
        return   $ TCons t_immutable es fTop
  where
    mkField (k,(_,_,t,_)) = (mod_key k, FieldSig (F.symbol k) f_required t_immutable t)
    mod_key k             = (F.symbol k, InstanceMember)

expandType _ γ (TEnum x)
  = do  es      <- M.fromList . concatMap  mkField . envToList . e_mapping <$> resolveEnumInEnv γ x
        return   $ TCons t_immutable es fTop
  where
    -- TODO 
    mkField (k, IntLit _ i) = [(key k, fld k $ tInt `strengthen` exprReft i)]
    mkField (k, HexLit _ s) | Just e <- bitVectorValue s
                            = [(key k, fld k $ tBV32 `strengthen` e)]
    mkField _               = []
    fld k = FieldSig (F.symbol k) f_required t_immutable
    key   = (,InstanceMember) . F.symbol 

expandType NonCoercive γ t = Nothing

-- FIXME: Even these should inherit from Object
expandType _ γ (TApp TInt _ _)
  = do  d     <- resolveTypeInEnv γ numberInterface 
        es    <- expand' InstanceMember γ d
        return $ TCons t_immutable es fTop

expandType _ γ (TApp TString _ _)
  = do  d     <- resolveTypeInEnv γ stringInterface 
        es    <- expand' InstanceMember γ d
        return $ TCons t_immutable es fTop

expandType _ γ (TApp TBool _ _) 
  = do  d     <- resolveTypeInEnv γ booleanInterface 
        es    <- expand' InstanceMember γ d
        return $ TCons t_immutable es fTop

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
weaken :: (PPR r, EnvLike r g) => g r -> TypeReference r -> AbsName -> Maybe (TypeReference r)
---------------------------------------------------------------------------
weaken γ tr@(s,_) t
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
doEdge :: PPR r => ClassHierarchy r -> TypeReference r -> Edge -> Maybe (TypeReference r)
---------------------------------------------------------------------------
doEdge (ClassHierarchy g _) (_, t1) (n1, n2)
  = do  ID _  _ v1 (e1,i1) _  <-  lab g n1 
        ID c2 _ _  _       _  <-  lab g n2
        let θ                  =  fromList $ zip v1 t1
        (n2,t2)               <-  find ((c2 ==) . fst) e1 
                              <|> find ((c2 ==) . fst) i1
        return                 $  (n2, apply θ t2)

---------------------------------------------------------------------------
ancestors :: EnvLike r t => IfaceKind -> t r -> AbsName -> [AbsName]
---------------------------------------------------------------------------
ancestors k γ s = [ t_name l | cur <- maybeToList (HM.lookup s m)
                             , anc <- reachable cur g
                             , l   <- maybeToList $ lab g anc
                             , t_class l == k ]
  where ClassHierarchy g m   = cha γ


-- XXX : only strict parents 
---------------------------------------------------------------------------
strictAncestorsFromPgm :: Nano a r -> AbsName -> [AbsName]
---------------------------------------------------------------------------
strictAncestorsFromPgm p s = [ t_name l | cur <- maybeToList (HM.lookup s m)
                                        , anc <- reachable cur g
                                        , cur /= anc      -- only gather parents
                                        , l   <- maybeToList $ lab g anc
                                        , t_class l == ClassKind ]
  where ClassHierarchy g m              = pCHA p

-- XXX : includes search in all parent classes (including the current one)
---------------------------------------------------------------------------
classAncestorsFromPgm :: Nano a r -> AbsName -> [AbsName]
---------------------------------------------------------------------------
classAncestorsFromPgm p s = [ t_name l | cur <- maybeToList (HM.lookup s m)
                                  , anc <- reachable cur g
                                  , l   <- maybeToList $ lab g anc
                                  , t_class l == ClassKind ]
  where ClassHierarchy g m        = pCHA p

---------------------------------------------------------------------------
allFields :: StaticKind -> NanoBareR r -> AbsName -> [F.Symbol]
---------------------------------------------------------------------------
allFields k p a = HS.toList . HS.unions 
                $ HS.fromList . flds <$> classAncestorsFromPgm p a 
  where
    flds a = [ s | ID _ _ _ _ es    <- maybeToList $ resolveTypeInPgm p a
                 , ((_,k'),FieldSig s _ _ _) <- M.toList es, k == k' ] 


---------------------------------------------------------------------------
onlyInheritedFields :: StaticKind -> NanoBareR r -> AbsName -> [F.Symbol]
---------------------------------------------------------------------------
onlyInheritedFields k p a = HS.toList . HS.unions 
                        $ HS.fromList . flds <$> strictAncestorsFromPgm p a 
  where
    flds a = [ s | ID _ _ _ _ es    <- maybeToList $ resolveTypeInPgm p a
                 , ((_,k'),FieldSig s _ _ _) <- M.toList es, k == k' ] 

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
boundKeys γ t@(TRef _ _ _) | Just t <- expandType Coercive γ t = boundKeys γ t
                           | otherwise                         = []
boundKeys _ (TCons _ es _) = fst <$> M.keys es 
boundKeys _ _              = []


-----------------------------------------------------------------------
-- | Constructors
-----------------------------------------------------------------------

type Constructor = Type 

instance F.Symbolic Constructor where
  symbol (TRef x _ _)                  = F.symbol x
  symbol (TClass _ )                   = F.symbol "Function"
  symbol (TModule _ )                  = F.symbol "Object"
  symbol (TFun _ _ _ _ )               = F.symbol "Function"
  symbol (TCons _ _ _)                 = F.symbol "Object"
  symbol (TAnd _)                      = F.symbol "Function"
  symbol _                             = F.symbol "ConstructorERROR"

instance F.Expression Constructor where
  expr = F.expr . F.symbol


getTypeof (TApp TInt _ _     )         = Just "number"
getTypeof (TRef _  _ _       )         = Just "object"
getTypeof (TSelf _           )         = Just "object"
getTypeof (TApp TBool _ _    )         = Just "boolean"
getTypeof (TApp TString _ _  )         = Just "string"
getTypeof (TApp TUndef _ _   )         = Just "undefined"
getTypeof (TApp TNull  _ _   )         = Just "undefined"
getTypeof (TFun _ _ _ _      )         = Just "function"
getTypeof (TCons _ _ _       )         = Just "object"
getTypeof (TClass _          )         = Just "function"
getTypeof (TModule _         )         = Just "object"
getTypeof _                            = Nothing

sameTypeof = (==) `on` getTypeof

