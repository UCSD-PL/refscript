{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Nano.Typecheck.Resolve ( 
  
    absolutePathInEnv, absolutePath, absoluteName
  , resolveRelPath, resolveRelPathInEnv
  , resolveRelName, resolveRelNameInEnv
  , relativePath, relativeName, extendPath
  , renameRelative

  -- * Flatten a type definition applying subs
  , flatten, flatten', flatten'', flattenType

  -- * Ancestors
  , weaken, ancestors

  -- * Constructors
  , Constructor, {- toConstructor, isConstSubtype, -} sameTypeof, getTypeof

  ) where 

import           Data.Generics
import           Data.Function                  (on)
import           Language.ECMAScript3.PrettyPrint
import qualified Language.Fixpoint.Types as F
import           Language.Nano.Env
-- import           Language.Nano.Errors
import           Language.Nano.Environment
import           Language.Nano.Names
import           Language.Nano.Types
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Subst

import           Control.Applicative ((<$>))
import qualified Data.List as L

-- import           Debug.Trace

type PPR r = (PP r, F.Reftable r, Data r)


---------------------------------------------------------------------------
-- | Namespace substitutions
---------------------------------------------------------------------------

-- | `renameRelative mods base tgt` transforms all relative paths (and names), 
--   originally assumed to be expressed in terms of absolute path @base@ to paths
--   (and names) that are relative to the absolute path @tgt@.
--
--   FIXME: Is extM working right?
--
---------------------------------------------------------------------------
renameRelative :: Data a => QEnv (ModuleDef r) -> AbsPath -> AbsPath -> a -> Maybe a
--------------------------------------------------------------------------
renameRelative mods base tgt = everywhereM $ mkM $ paths `extM` names
  where
    paths :: RelPath -> Maybe RelPath
    paths r             = relativePath tgt <$> absolutePath mods base r
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


-- | `resolveRelName γ a r` returns the type defined at name @r@ relative to the 
--   absolute path `a`, or Nothing if this fails.
--   
--   All relative qualified names have been made relative to absolute path `a`.
--
--   FIXME: check visibility
--
---------------------------------------------------------------------------
resolveRelName :: PPR r => QEnv (ModuleDef r) -> AbsPath -> RelName -> Maybe (IfaceDef r)
---------------------------------------------------------------------------
resolveRelName env curP (RN qn) = do
    curM        <- qenvFindTy curP env
    (dfn, remP) <- aux curM qn
    renameRelative env remP curP dfn 
  where
    aux curM qn@(QName _ [] s) = do
      case envFindTy s (m_types curM) of 
        Just dfn -> Just (dfn, m_path curM)
        Nothing  -> do prP <- parentOf $ m_path curM
                       prM <- qenvFindTy prP env
                       aux prM qn
    aux curM (QName l ns s)   = do 
      remM <- resolveRelPath env (m_path curM) (RP (QPath l ns))
      aux remM (QName l [] s)


---------------------------------------------------------------------------
resolveRelNameInEnv :: PPR r => EnvLike r g => g r -> RelName -> Maybe (IfaceDef r)
---------------------------------------------------------------------------
resolveRelNameInEnv γ = resolveRelName (modules γ) (absPath γ)


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

-- | `flatten b γ (d,ts)` epands a type reference to a structural type that 
--   includes all elements of the the named type and its ancestors. Argument
--   @b@ determines if static or non-static elements should be included.
--
---------------------------------------------------------------------------
flatten :: (EnvLike r g, PPR r, Data r) => Bool -> g r -> (SIfaceDef r) -> Maybe [TypeMember r]
---------------------------------------------------------------------------
flatten b γ (ID _ _ vs h es, ts) =
    case h of 
      Just (p, ts') -> 
          do  parent        <- resolveRelNameInEnv γ p
              inherited     <- flatten b γ (parent, ts')
              return         $ apply θ $ L.unionBy sameBinder current inherited
      Nothing -> 
          return             $ apply θ current
  where 
    current        = filter (fn b) es
    θ              = fromList $ zip vs ts
    fn True        = isStaticSig
    fn False       = nonStaticSig


-- | flatten' does not apply the top-level type substitution
---------------------------------------------------------------------------
flatten' :: (PPR r, EnvLike r g) => Bool -> g r -> IfaceDef r -> Maybe [TypeMember r]
---------------------------------------------------------------------------
flatten' st γ d@(ID _ _ vs _ _) = flatten st γ (d, tVar <$> vs)

---------------------------------------------------------------------------
flatten'' :: (PPR r, EnvLike r g) =>  Bool -> g r -> IfaceDef r -> Maybe ([TVar], [TypeMember r])
---------------------------------------------------------------------------
flatten'' st γ d@(ID _ _ vs _ _) = (vs,) <$> flatten st γ (d, tVar <$> vs)


-- | `flattenType` will flatten *any* type (including Mutability types!)
--    It is not intended to be called with mutability types.
--    Types with zero type parameters (missing mutability field) are considered
--    invalid.
--
--  FIXME: Return an `Either Error (RType r)` for better error reporting.
--
--
---------------------------------------------------------------------------
flattenType :: (PPR r, EnvLike r g, Data r) => g r -> RType r -> Maybe (RType r)
---------------------------------------------------------------------------
-- -- This case is for Mutability types 
flattenType γ (TApp (TRef x) [] r)
  = do  es      <- flatten False γ . (, []) =<< resolveRelNameInEnv γ x
        return   $ TCons es t_immutable r

flattenType γ (TApp (TRef x) (mut:ts) r)
  = do  es      <- flatten False γ . (,mut:ts) =<< resolveRelNameInEnv γ x
        return   $ TCons es (toType mut) r

flattenType γ (TClass x)             = do
    es                <- flatten' True γ =<< resolveRelNameInEnv γ x
    return             $ TCons es mut fTop
  where
    mut                = t_readOnly -- toType $ t_ReadOnly $ get_common_ts γ 

flattenType _ t = Just t


-- | `weaken γ a b ts` moves a name @a@ by moving it upwards in the class 
--   hierarchy. This function does the necessary type argument substitutions.
--
--    FIXME: Works for classes, but interfaces could have multiple ancestors.
--           What about common elements in parent class?
--
---------------------------------------------------------------------------
weaken :: (PPR r, EnvLike r g) => g r -> RelName -> RelName -> [RType r] -> Maybe (SIfaceDef r)
---------------------------------------------------------------------------
weaken γ pa pb ts
  | pa == pb = (,ts) <$> resolveRelNameInEnv γ pa
  | otherwise
  = do  z <- resolveRelNameInEnv γ pa
        case z of
          ID _ _ vs (Just (p,ps)) _ -> weaken γ p pb $ apply (fromList $ zip vs ts) ps
          ID _ _ _  Nothing       _ -> Nothing

-- FIXME: revisit these
---------------------------------------------------------------------------
ancestors :: (PPR r, EnvLike r g) => g r -> RelName -> [RelName]
---------------------------------------------------------------------------
ancestors γ s = 
  case resolveRelNameInEnv γ s of 
    Just (ID {t_proto = p }) -> 
      case p of 
        Just (par,_) ->  s : ancestors γ par
        _ -> [s]
    _ -> [s]


-----------------------------------------------------------------------
-- Constructors
-----------------------------------------------------------------------

type Constructor = Type 


-- FIXME: Use common_ts

-- funcConstr                            :: Constructor
-- funcConstr                             = TApp (TRef (QN [] (F.symbol "Function"))) [] ()
-- 
-- objectConstr                          :: Constructor
-- objectConstr                           = TApp (TRef (QN [] (F.symbol "Object"))) [] ()
-- 
-- -- Primitive types don't have constructor
-- toConstructor                         :: RType r -> Maybe Constructor
-- toConstructor  (TApp (TRef  x) _ _)    = Just $ TApp (TRef  x) [] ()
-- toConstructor  (TClass _)              = Just $ funcConstr
-- toConstructor  (TModule _)             = Just $ objectConstr
-- toConstructor  (TFun _ _ _ )           = Just $ funcConstr
-- toConstructor  (TCons _ _ _)           = Just $ objectConstr
-- toConstructor  (TAnd _)                = Just $ funcConstr 
-- toConstructor  _                       = Nothing

instance F.Symbolic Constructor where
  symbol (TApp (TRef x) _ _)           = F.symbol x
  symbol (TClass _ )                   = F.symbol "Function"
  symbol (TModule _ )                  = F.symbol "Object"
  symbol (TFun _ _ _ )                 = F.symbol "Function"
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
getTypeof (TFun _ _ _        )         = Just "function"
getTypeof (TCons _ _ _       )         = Just "object"
getTypeof (TApp (TRef _) _ _ )         = Just "object"
getTypeof (TClass _          )         = Just "function"
getTypeof (TModule _         )         = Just "object"
getTypeof _                            = Nothing

sameTypeof = (==) `on` getTypeof

