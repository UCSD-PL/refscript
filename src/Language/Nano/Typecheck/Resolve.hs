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

  -- * Flatten a type definition applying subs
  , flatten, flatten', flatten'', flattenType

  -- * Ancestors
  , weaken, ancestors, isAncestor

  -- * Keys
  , boundKeys

  -- * Constructors
  , Constructor, {- toConstructor, isConstSubtype, -} sameTypeof, getTypeof

  , isClassType

  ) where 

import           Control.Applicative                 ((<$>), (<*>), (<|>))
import           Data.Generics
import           Data.Tuple

import qualified Data.IntMap.Strict               as I
import qualified Data.HashMap.Strict              as HM
import           Data.Maybe                          (catMaybes, maybeToList, listToMaybe)
import           Data.Foldable                       (foldlM)
import           Data.List                           (nub, find)
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.Query.DFS
import           Data.Graph.Inductive.PatriciaTree
import           Data.Graph.Inductive.Query.BFS
import qualified Data.HashSet                     as S 
import           Data.Function                       (on)
import qualified Data.Map.Strict                  as M
import qualified Language.Fixpoint.Types          as F
import           Language.Nano.Env
import           Language.Nano.Errors
import           Language.Fixpoint.Misc              (mapPair, snd3, fst3, thd3, mapFst)
import           Language.Nano.Environment
import           Language.Nano.Names
import           Language.Nano.Types
import           Language.Nano.Program
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Subst

import           Control.Applicative ((<$>))

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
isClassType γ (TRef x _ _ ) = 
  case resolveTypeInEnv γ x of
    Just (ID _ k _ _ _ ) -> k == ClassKind
    _                    -> False
isClassType _ _ = False


-- | Flattenning 
--
-- 
-- | `flatten m b γ (d,ts)` epands a type reference to a structural type that 
--   includes all elements of the the named type and its ancestors. Argument
--   @b@ determines if static or non-static elements should be included.
--   @m@ is the top-level enforced mutability Top-level 
--
---------------------------------------------------------------------------
flatten :: (EnvLike r g, PPR r) 
        => Maybe Mutability 
        -> StaticKind 
        -> g r 
        -> SIfaceDef r 
        -> Maybe (TypeMembers r)
---------------------------------------------------------------------------
flatten m s γ (ID _ _ vs h es, ts) =  M.map (apply θ . fmut) 
                                   .  M.unions 
                                   .  (current:)
                                  <$> heritage h
  where 
    current                        = M.filterWithKey (\(_,s') _ -> s == s') es
    θ                              = fromList $ zip vs ts
    fmut                           = maybe id setMut m
    setMut m (FieldSig x o mf t)   | isInheritedMutability mf 
                                   = FieldSig x o m t
    setMut _ f                     = f
    heritage (es,_)                = mapM fields es
    fields (p,ts)                  = resolveTypeInEnv γ p >>= flatten m s γ . (,ts) 

-- | flatten' does not apply the top-level type substitution
---------------------------------------------------------------------------
flatten' :: (PPR r, EnvLike r g) 
         => Maybe Mutability 
         -> StaticKind
         -> g r 
         -> IfaceDef r 
         -> Maybe (TypeMembers r)
---------------------------------------------------------------------------
flatten' m st γ d@(ID _ _ vs _ _) = flatten m st γ (d, tVar <$> vs)

---------------------------------------------------------------------------
flatten'' :: (PPR r, EnvLike r g) 
          => Maybe Mutability 
          -> StaticKind
          -> g r 
          -> IfaceDef r 
          -> Maybe ([TVar], (TypeMembers r))
---------------------------------------------------------------------------
flatten'' m st γ d@(ID _ _ vs _ _) = (vs,) <$> flatten m st γ (d, tVar <$> vs)


-- | `flattenType` will flatten *any* type (including Mutability types!)
--    It is not intended to be called with mutability types.
--    Types with zero type parameters (missing mutability field) are considered
--    invalid.
--
--    Flatten pushes top-level enforced mutabilities down towards the fields,
--    i.e. if the top-level is 'AssignsFields' then fields get Mutable
--    (overriding their current mutability).
--
---------------------------------------------------------------------------
flattenType :: (PPR r, EnvLike r g, Data r) => g r -> RType r -> Maybe (RType r)
---------------------------------------------------------------------------
flattenType γ (TRef x [] r)    -- This case is for Mutability types 
  = do  d       <- resolveTypeInEnv γ x
        es      <- flatten Nothing InstanceMember γ (d, [])
        return   $ TCons t_immutable es r

flattenType γ (TRef x (mut:ts) r)
  = do  d       <- resolveTypeInEnv γ x
        es      <- flatten (Just $ toType mut) InstanceMember γ (d,mut:ts) 
        return   $ TCons (toType mut) es r

flattenType γ (TClass x)             
  = do  d       <- resolveTypeInEnv γ x
        es      <- flatten' Nothing StaticMember γ d
        return   $ TCons t_immutable es fTop

flattenType γ (TModule x)             
  = do  es      <- M.fromList . map mkField . envToList . m_variables <$> resolveModuleInEnv γ x
        return   $ TCons t_immutable es fTop
  where
    mkField (k,(_,_,t,_)) = ((F.symbol k, InstanceMember), FieldSig (F.symbol k) f_required t_immutable t)

flattenType γ (TEnum x)
  = do  es      <- M.fromList . map  mkField . envToList . e_symbols <$> resolveEnumInEnv γ x
        return   $ TCons t_immutable es fTop
  where
    mkField (k, Just i ) = ((F.symbol k, InstanceMember), FieldSig (F.symbol k) f_required t_immutable $ tInt `strengthen` exprReft i)
    mkField (k, Nothing) = ((F.symbol k, InstanceMember), FieldSig (F.symbol k) f_required t_immutable $ tInt)

flattenType γ (TApp TInt _ r) 
  = do  es      <- t_elts <$> resolveTypeInEnv γ (mkAbsName [] $ F.symbol "Number")
        return   $ TCons t_immutable es fTop

flattenType γ (TApp TString _ r) 
  = do  es      <- t_elts <$> resolveTypeInEnv γ (mkAbsName [] $ F.symbol "String")
        return   $ TCons t_immutable es fTop

flattenType γ (TApp TBool _ r) 
  = do  es      <- t_elts <$> resolveTypeInEnv γ (mkAbsName [] $ F.symbol "Boolean")
        return   $ TCons t_immutable es fTop

flattenType _ t  = Just t

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
weaken γ tr@(s,ts) t
  | s == t                    = Just tr
  | otherwise 
  = do n1                    <- HM.lookup s m
       n2                    <- HM.lookup t m

       case unwrap $ lesp n1 n2 g of
         []                  -> Nothing
         path                -> -- tracePP ("weakening from " ++ ppshow tr ++ " to " ++ ppshow t) $
                                foldlM (doEdge ch) tr $ map toNodes (toEdges path)
  where
    ch@(ClassHierarchy g m)   = cha γ
    unwrap (LP lpath)         = lpath
    toEdges xs                = zip (init xs) (tail xs)
    toNodes ((n1,_),(n2,_))   = (n1,n2)

---------------------------------------------------------------------------
doEdge :: PPR r => ClassHierarchy r -> TypeReference r -> Edge -> Maybe (TypeReference r)
---------------------------------------------------------------------------
doEdge cha@(ClassHierarchy g m) (_, t1) (n1, n2)
  = do  ID _  _ v1 (e1,i1) _  <-  lab g n1 
        ID c2 _ _  _       _  <-  lab g n2
        let θ                  =  fromList $ zip v1 t1
        (n2,t2)               <-  find ((c2 ==) . fst) e1 
                              <|> find ((c2 ==) . fst) i1
        return                 $  (n2, apply θ t2)

---------------------------------------------------------------------------
ancestors :: (PPR r, EnvLike r g) => g r -> AbsName -> [AbsName]
---------------------------------------------------------------------------
ancestors γ s = [ t_name l     | cur <- maybeToList (HM.lookup s m)
                               , anc <- reachable cur g
                               , l   <- maybeToList (lab g anc)]
  where
    ClassHierarchy g m         = cha γ

---------------------------------------------------------------------------
isAncestor :: (PPR r, EnvLike r g) => g r -> AbsName -> AbsName -> Bool
---------------------------------------------------------------------------
isAncestor γ c p = p `elem` ancestors γ c

---------------------------------------------------------------------------
boundKeys :: (PPR r, EnvLike r g) => g r -> RType r -> [F.Symbol]
---------------------------------------------------------------------------
boundKeys γ t@(TRef _ _ _) = case flattenType γ t of
                               Just t  -> boundKeys γ t
                               Nothing -> []
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

