{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Nano.Typecheck.Resolve ( 
  
    absolutePathInEnv, absolutePath, absoluteName
  , absoluteNameInEnv
  , resolveRelPath, resolveRelPathInEnv
  , resolveRelType, resolveRelTypeInEnv
  , resolveRelEnum, resolveRelEnumInEnv
  , relativePath, relativeName, extendPath
  , renameRelative

  -- * Flatten a type definition applying subs
  , flatten, flatten', flatten'', flattenType

  -- * Ancestors
  , weaken, ancestors, isAncestor

  -- * Keys
  , boundKeys

  -- * Constructors
  , Constructor, {- toConstructor, isConstSubtype, -} sameTypeof, getTypeof

  ) where 

import           Control.Applicative                 ((<$>), (<*>))
import           Data.Generics
import           Data.Foldable                       (foldlM)
import           Data.Tuple

import qualified Data.IntMap.Strict               as I
import qualified Data.HashMap.Strict              as HM
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree
import           Data.Graph.Inductive.Query.BFS

import           Data.Maybe                          (catMaybes, maybeToList, listToMaybe)
import           Data.List                           (nub)
import qualified Data.HashSet                     as S 
import           Data.Function                       (on)
import qualified Data.Map.Strict                  as M
import qualified Language.Fixpoint.Types          as F
import           Language.Nano.Env
import           Language.Fixpoint.Misc              (mapPair, snd3, fst3, thd3, mapFst)
import           Language.Nano.Environment
import           Language.Nano.Names
import           Language.Nano.Types
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Subst

import           Control.Applicative ((<$>))

-- import           Debug.Trace


---------------------------------------------------------------------------
-- | Namespace substitutions
---------------------------------------------------------------------------

-- | `renameRelative mods base tgt` transforms all relative paths (and names), 
--   originally assumed to be expressed in terms of absolute path @base@ to paths
--   (and names) that are relative to the absolute path @tgt@.
--
---------------------------------------------------------------------------
renameRelative :: Data a => QEnv (ModuleDef r) -> AbsPath -> AbsPath -> a -> Maybe a
--------------------------------------------------------------------------
renameRelative mods base tgt a = 
    everywhereM (mkM paths) a >>= everywhereM (mkM names)
  where
    paths :: RelPath -> Maybe RelPath
    paths r             = relativePath tgt <$> absolutePath mods base r

    names :: RelName -> Maybe RelName
    names n             = relativeName tgt <$> absoluteName mods base n

 

-- | `relativePath base tgt` expresses path @tgt@ in relative terms of path
--   @base@.
---------------------------------------------------------------------------
relativePath :: AbsPath -> AbsPath -> RelPath
---------------------------------------------------------------------------
relativePath (AP (QPath _ x)) (AP (QPath ly y)) = RP $ QPath ly $ dropCommonPref x y

---------------------------------------------------------------------------
relativeName :: AbsPath -> AbsName -> RelName
---------------------------------------------------------------------------
relativeName (AP (QPath _ x)) (AN (QName ly y s)) = RN $ QName ly (dropCommonPref x y) s


dropCommonPref _      []      = []
dropCommonPref []     ys      = ys
dropCommonPref _      [s]     = [s]
dropCommonPref (x:xs) (y:ys)  | x == y    = dropCommonPref xs ys
                              | otherwise = y:ys



-- | `absolutePathInEnv env a r` returns the absolute path that corresponds to 
--   a path @r@ expressed in terms of environment @env@.
--
---------------------------------------------------------------------------
absolutePathInEnv :: EnvLike r t => t r -> RelPath -> Maybe AbsPath
---------------------------------------------------------------------------
absolutePathInEnv env r = absolutePath (modules env) (absPath env) r


-- | `absolutePath env a r` returns the absolute path that corresponds to the 
--   a path @r@ that is relative to an absolute namespace @a@.
--
---------------------------------------------------------------------------
absolutePath    :: QEnv (ModuleDef r) -> AbsPath -> RelPath -> Maybe AbsPath
---------------------------------------------------------------------------
absolutePath env a r    = m_path <$> resolveRelPath env a r 

-- | `absolutePath env a r` returns the absolute path that corresponds to the 
--   a path @r@ that is relative to an absolute namespace @a@.
--
---------------------------------------------------------------------------
absoluteName    :: QEnv (ModuleDef r) -> AbsPath -> RelName -> Maybe AbsName
---------------------------------------------------------------------------
absoluteName env a r@(RN (QName _ _ s)) = g <$> absolutePath env a (f r)
  where
    f (RN (QName l p _)) = RP (QPath l p)
    g (AP (QPath l p))   = AN (QName l p s)


absoluteNameInEnv env r = absoluteName (modules env) (absPath env) r


typeForAbsPath :: EnvLike r t => t r -> AbsName -> Maybe (IfaceDef r)
typeForAbsPath γ (AN (QName l ss s)) = qenvFindTy (AP $ QPath l ss) (modules γ) 
                                   >>= envFindTy s . m_types 


-- | `resolveRelPath γ a r` returns the environment referenced by the relative 
--   path @r@ expressed in terms of namespace @a@.
--
--   FIXME: check visibility
--
---------------------------------------------------------------------------
resolveRelPath :: QEnv (ModuleDef r) -> AbsPath -> RelPath -> Maybe (ModuleDef r)
---------------------------------------------------------------------------
resolveRelPath env a   (RP (QPath _ []    )) = qenvFindTy a env
resolveRelPath env a r@(RP (QPath l (m:ms))) = do
    curM <-qenvFindTy a env
    case envFindTy m (m_variables curM) of
      Just (_, _,TModule _) -> resolveRelPath env (extendPath a m) (RP (QPath l ms))
      Just _                -> Nothing
      Nothing               -> do prP <- parentOf a
                                  resolveRelPath env prP r

---------------------------------------------------------------------------
resolveRelPathInEnv :: Data r => EnvLike r g => g r -> RelPath -> Maybe (ModuleDef r)
---------------------------------------------------------------------------
resolveRelPathInEnv γ = resolveRelPath (modules γ) (absPath γ)


-- | `resolveRelName γ a r`
--
--   All relative qualified names have been made relative to absolute path `a`.
--
--   FIXME: check visibility
--
---------------------------------------------------------------------------
resolveRelName :: Data b => (ModuleDef r -> Env b) -> QEnv (ModuleDef r) -> AbsPath -> RelName -> Maybe b
---------------------------------------------------------------------------
resolveRelName f env curP (RN qn) = do
    curM        <- qenvFindTy curP env
    (dfn, remP) <- aux curM qn
    renameRelative env remP curP dfn 
  where
    aux curM qn@(QName _ [] s) = do
      case envFindTy s (f curM) of 
        Just dfn -> Just (dfn, m_path curM)
        Nothing  -> do prP <- parentOf $ m_path curM
                       prM <- qenvFindTy prP env
                       aux prM qn
    aux curM (QName l ns s)   = do 
      remM <- resolveRelPath env (m_path curM) (RP (QPath l ns))
      aux remM (QName l [] s)

resolveRelType :: Data r => QEnv (ModuleDef r) -> AbsPath -> RelName -> Maybe (IfaceDef r)
resolveRelType = resolveRelName m_types

resolveRelEnum :: QEnv (ModuleDef r) -> AbsPath -> RelName -> Maybe EnumDef
resolveRelEnum = resolveRelName m_enums

---------------------------------------------------------------------------
resolveRelTypeInEnv :: PPR r => EnvLike r t => t r -> RelName -> Maybe (IfaceDef r)
---------------------------------------------------------------------------
resolveRelTypeInEnv γ = resolveRelType (modules γ) (absPath γ)

---------------------------------------------------------------------------
resolveRelEnumInEnv :: Data r => EnvLike r g => g r -> RelName -> Maybe EnumDef
---------------------------------------------------------------------------
resolveRelEnumInEnv γ = resolveRelEnum (modules γ) (absPath γ)

---------------------------------------------------------------------------
extendPath :: F.Symbolic x => AbsPath -> x -> AbsPath
---------------------------------------------------------------------------
extendPath (AP (QPath l p)) s = AP $ QPath l $ p ++ [F.symbol s]


---------------------------------------------------------------------------
parentOf :: AbsPath -> Maybe AbsPath
---------------------------------------------------------------------------
parentOf (AP (QPath _ [])) = Nothing
parentOf (AP (QPath l m )) = Just (AP (QPath l (init m)))




-- | Flattenning 
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
flatten m s γ (ID _ _ _ vs h es, ts) =  M.map (apply θ . fmut) 
                                     .  M.unions 
                                     .  (current:)                                      
                                    <$> mapM fields h
  where 
    current                          =  M.filterWithKey (\(_,s') _ -> s == s') es
    θ                                =  fromList $ zip vs ts
    fmut                             =  maybe id setMut m
    setMut m (FieldSig x _ t)        =  FieldSig x m t
    setMut _ e                       =  e

    fields (p,ts) = undefined -- resolveRelTypeInEnv γ p >>= flatten m s γ . (,ts)


-- | flatten' does not apply the top-level type substitution
---------------------------------------------------------------------------
flatten' :: (PPR r, EnvLike r g) 
         => Maybe Mutability 
         -> StaticKind
         -> g r 
         -> IfaceDef r 
         -> Maybe (TypeMembers r)
---------------------------------------------------------------------------
flatten' m st γ d@(ID _ _ _ vs _ _) = flatten m st γ (d, tVar <$> vs)

---------------------------------------------------------------------------
flatten'' :: (PPR r, EnvLike r g) 
          => Maybe Mutability 
          -> StaticKind
          -> g r 
          -> IfaceDef r 
          -> Maybe ([TVar], (TypeMembers r))
---------------------------------------------------------------------------
flatten'' m st γ d@(ID _ _ _ vs _ _) = (vs,) <$> flatten m st γ (d, tVar <$> vs)


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
flattenType γ (TApp (TRef x) [] r)    -- This case is for Mutability types 
  = do  d       <- resolveRelTypeInEnv γ x
        es      <- flatten Nothing InstanceMember γ (d, [])
        return   $ TCons t_immutable es r

flattenType γ (TApp (TRef x) (mut:ts) r)
  = do  d       <- resolveRelTypeInEnv γ x
        es      <- flatten (Just $ toType mut) InstanceMember γ (d,mut:ts) 
        return   $ TCons (toType mut) es r

flattenType γ (TClass x)             
  = do  d       <- resolveRelTypeInEnv γ x
        es      <- flatten' Nothing StaticMember γ d
        return   $ TCons t_immutable es fTop

flattenType γ (TModule x)             
  = do  es      <- M.fromList . map mkField . envToList . m_variables <$> resolveRelPathInEnv γ x
        return   $ TCons t_immutable es fTop
  where
    mkField (k,(_,_,t)) = ((F.symbol k, InstanceMember), FieldSig (F.symbol k) t_immutable t)

flattenType γ (TEnum x)
  = do  es      <- M.fromList . map  mkField . envToList . e_symbols <$> resolveRelEnumInEnv γ x
        return   $ TCons t_immutable es fTop
  where
    mkField (k,i) = ((F.symbol k, InstanceMember), FieldSig (F.symbol k) t_immutable $ tInt `strengthen` exprReft i)

flattenType γ (TApp TInt _ r) 
  = do  es      <- t_elts <$> resolveRelTypeInEnv γ (mkRelName [] $ F.symbol "Number")
        return   $ TCons t_immutable es fTop

flattenType γ (TApp TString _ r) 
  = do  es      <- t_elts <$> resolveRelTypeInEnv γ (mkRelName [] $ F.symbol "String")
        return   $ TCons t_immutable es fTop

flattenType γ (TApp TBool _ r) 
  = do  es      <- t_elts <$> resolveRelTypeInEnv γ (mkRelName [] $ F.symbol "Boolean")
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
--
--    FIXME: Works for classes, but interfaces could have multiple ancestors.
--           What about common elements in parent class?
--
---------------------------------------------------------------------------
weaken :: (PPR r, EnvLike r g) 
       => g r 
       -> RelName 
       -> RelName 
       -> [RType r] 
       -> Maybe (SIfaceDef r)
---------------------------------------------------------------------------
weaken γ pa pb ts
  | on (==) (absoluteNameInEnv γ) pa pb = (,ts) <$> resolveRelTypeInEnv γ pa
  | otherwise
  = do  z <- resolveRelTypeInEnv γ pa
        case z of
          ID _ _ _ _ _ _ -> undefined
-- FIXME
--           ID _ _ vs (Just (p,ps)) _ -> weaken γ p pb $ apply (fromList $ zip vs ts) ps
--           ID _ _ _  Nothing       _ -> Nothing


---------------------------------------------------------------------------
path :: EnvLike r t => t r -> TypeReference r -> AbsName -> Maybe (TypeReference r)
---------------------------------------------------------------------------
path γ tr@(t1,ts) t2 
  = do n1                  <- HM.lookup t1 m
       n2                  <- HM.lookup t2 m
       foldlM (doEdge γ) tr $ map toNodes $ toEdges $ unwrap $ lesp n1 n2 g
  where
    ClassHierarchy g m = cha γ 
    unwrap (LP lpath) = lpath
    bar (s,t,_) = (,) <$> lab g s <*> lab g t
    toEdges xs = zip (init xs) (tail xs)
    toNodes ((n1,_),(n2,_)) = (n1,n2)

doEdge :: EnvLike r t => t r -> TypeReference r -> (Node, Node) -> Maybe (TypeReference r)
doEdge γ (_, ts) (n1, n2)
  = listToMaybe [ (s1, ss) | t1       <- maybeToList $ lab g n1
                           , t2       <- maybeToList $ lab g n2  
                           , (s1, ss) <- parentTypeReferences γ (t_path t1,ts)
                           , s1 == t_path t2 ]
  where
    ClassHierarchy g m     = cha γ 

---------------------------------------------------------------------------
buildCHA           :: EnvLike r t => t r -> ClassHierarchy r
---------------------------------------------------------------------------
buildCHA γ          = ClassHierarchy graph namesToKeys

  where 
    graph           = mkGraph nodes edges
    nodes           = zip [0..] $ fst3 <$> data_
    keysToTypes     = I.fromList $ zip [0..] (fst3 <$> data_)
    namesToKeys     = HM.fromList $ mapFst t_path . swap <$> I.toList keysToTypes
    edges           = concatMap toEdge data_  
    toEdge (_,s,ts) = [ (σ,τ,()) | t <- ts
                                 , σ <- maybeToList $ HM.lookup s namesToKeys
                                 , τ <- maybeToList $ HM.lookup t namesToKeys ]
    data_           = concatMap foo $ snd <$> qenvToList (modules γ)
    foo m           = bar . snd <$>  envToList (m_types m)
    bar d           = (d, t_path d, parentNames γ (t_path d))


-- parents :: EnvLike r t => t r -> TypeReference r -> [TypeReference r]
-- parents = undefined 
-- 

parentTypeReferences :: EnvLike r t => t r -> TypeReference r -> [TypeReference r]
parentTypeReferences = undefined 
 
parentNames :: EnvLike r t => t r -> AbsName -> [AbsName]
parentNames = undefined

typeReferenceToDefinition :: EnvLike r t => t r -> TypeReference r -> SIfaceDef r
typeReferenceToDefinition = undefined 


---------------------------------------------------------------------------
ancestors :: (PPR r, EnvLike r g) => g r -> RelName -> [RelName]
---------------------------------------------------------------------------
ancestors γ s = nub $ go $ resolveRelTypeInEnv γ s
  where
    -- FIXME
    -- go (Just (ID {t_base = p})) = [s] ++ concatMap (ancestors γ) (fst <$> p)
    go _                        = [s]

---------------------------------------------------------------------------
isAncestor :: (PPR r, EnvLike r g) => g r -> RelName -> RelName -> Bool
---------------------------------------------------------------------------
isAncestor γ c p = p `elem` ancestors γ c

---------------------------------------------------------------------------
boundKeys :: (PPR r, EnvLike r g) => g r -> RType r -> [F.Symbol]
---------------------------------------------------------------------------
boundKeys γ t@(TApp (TRef _) _ _) = case flattenType γ t of
                                      Just t  -> boundKeys γ t
                                      Nothing -> []
boundKeys _ (TCons _ es _)        = fst <$> M.keys es 
boundKeys _ _                     = []


-----------------------------------------------------------------------
-- Constructors
-----------------------------------------------------------------------

type Constructor = Type 

instance F.Symbolic Constructor where
  symbol (TApp (TRef x) _ _)           = F.symbol x
  symbol (TClass _ )                   = F.symbol "Function"
  symbol (TModule _ )                  = F.symbol "Object"
  symbol (TFun _ _ _ _ )               = F.symbol "Function"
  symbol (TCons _ _ _)                 = F.symbol "Object"
  symbol (TAnd _)                      = F.symbol "Function"
  symbol _                             = F.symbol "ConstructorERROR"

instance F.Expression Constructor where
  expr = F.expr . F.symbol


getTypeof (TApp TInt _ _     )         = Just "number"
getTypeof (TApp TBool _ _    )         = Just "boolean"
getTypeof (TApp TString _ _  )         = Just "string"
getTypeof (TApp TUndef _ _   )         = Just "undefined"
getTypeof (TApp TNull  _ _   )         = Just "undefined"
getTypeof (TFun _ _ _ _      )         = Just "function"
getTypeof (TCons _ _ _       )         = Just "object"
getTypeof (TApp (TRef _) _ _ )         = Just "object"
getTypeof (TClass _          )         = Just "function"
getTypeof (TModule _         )         = Just "object"
getTypeof _                            = Nothing

sameTypeof = (==) `on` getTypeof

