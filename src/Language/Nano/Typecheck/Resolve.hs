{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Nano.Typecheck.Resolve ( 
  
  resolveRelPath, resolveRelPathInEnv, resolveRelName, resolveRelNameInEnv

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
import           Language.Fixpoint.Misc (traceShow)
import           Language.Nano.Env
import           Language.Nano.Errors
import           Language.Nano.Names
import           Language.Nano.Types
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Environment
import           Language.Nano.Typecheck.Subst

import           Control.Applicative ((<$>))
import qualified Data.List as L
import           Data.Function (fix)

-- import           Debug.Trace

type PPR r = (PP r, F.Reftable r)


---------------------------------------------------------------------------
-- | Namespace substitutions
---------------------------------------------------------------------------

-- | `renameRelative env from to a` transforms all relative paths names in @a@ 
--   from being relative to absolute path @from@ to being relative to absolute 
--   path @to@.
--
--   FIXME: Is extM working right?
--
---------------------------------------------------------------------------
renameRelative :: Data a => QEnv (ModuleDef r) -> AbsPath -> AbsPath -> a -> Maybe a
--------------------------------------------------------------------------
renameRelative env a b = everywhereM $ mkM $ t1 `extM` t2
  where

    t1                :: RelPath -> Maybe RelPath
    t1                 = maybe Nothing ff . absolutePath env a

    t2                :: RelName -> Maybe RelName
    t2                 = maybe Nothing gg . absoluteName env a

    same (x,y)         = x == y

    ff (AP q)          = Just $ RP $ ffPref b q

    gg (AN q)          = Just $ RN $ ggPref b q

    ffPref (AP (QPath _ x)) (QPath ly y)   = QPath ly (snd <$> dropWhile same (zip x y))
    ggPref (AP (QPath _ x)) (QName ly y s) = QName ly (snd <$> dropWhile same (zip x y)) s


-- | `absolutePath env a r` returns the absolute path that corresponds to the 
--   a path @r@ that is relative to an absolute namespace @a@.
--
---------------------------------------------------------------------------
absolutePath    :: QEnv (ModuleDef r) -> AbsPath -> RelPath -> Maybe AbsPath
---------------------------------------------------------------------------
absolutePath env a r = m_path <$> resolveRelPath env a r 

-- | `absolutePath env a r` returns the absolute path that corresponds to the 
--   a path @r@ that is relative to an absolute namespace @a@.
--
---------------------------------------------------------------------------
absoluteName    :: QEnv (ModuleDef r) -> AbsPath -> RelName -> Maybe AbsName
---------------------------------------------------------------------------
absoluteName env a r@(RN (QName l ps s)) = g <$> absolutePath env a (f r)
  where
    f (RN (QName l p _)) = RP (QPath l p)
    g (AP (QPath l p))   = AN (QName l p s)


-- | `resolveRelPath γ a r` returns the environment referenced by the relative 
--   path @r@ when expressed in terms of namespace @a@.
--
--   FIXME: check visibility
--
---------------------------------------------------------------------------
resolveRelPath :: QEnv (ModuleDef r) -> AbsPath -> RelPath -> Maybe (ModuleDef r)
---------------------------------------------------------------------------
resolveRelPath env a (RP (QPath _ [])) = qenvFindTy a env
resolveRelPath env a r@(RP (QPath l (m:ms))) = do
    curM <- qenvFindTy a env
    case envFindTy m (m_contents curM) of
      Just (ModModule x _) -> resolveRelPath env (extendPath a x) (RP (QPath l ms))
      Just _               -> Nothing
      Nothing              -> do prP <- parentOf a
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
---------------------------------------------------------------------------
resolveRelName :: (PPR r, Data r) => QEnv (ModuleDef r) -> AbsPath -> RelName -> Maybe (IfaceDef r)
---------------------------------------------------------------------------
resolveRelName env curP (RN qn) = do
    curM        <- qenvFindTy (traceShow ("curP in keys " ++ show (qenvKeys env)) curP) env
    (dfn, remP) <- aux curM qn
    renameRelative env remP curP dfn 
  where
    aux curM qn@(QName _ [] s) = do
      case envFindTy s (m_contents curM) of 
        Just (ModType _ _ d) -> Just (d, m_path curM)
        Just _               -> Nothing
        Nothing              -> do prP <- parentOf $ m_path curM
                                   prM <- qenvFindTy prP env
                                   aux prM qn

    aux curM (QName l ns s)   = do remM <- resolveRelPath env (m_path curM) (RP (QPath l ns))
                                   aux remM (QName l [] s)


---------------------------------------------------------------------------
resolveRelNameInEnv :: (PPR r, Data r) => EnvLike r g => g r -> RelName -> Maybe (IfaceDef r)
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

-- | flattening type to include all fields inherited by ancestors
---------------------------------------------------------------------------
flatten :: (EnvLike r g, PPR r, Data r) => Bool -> g r -> (SIfaceDef r) -> Maybe [TypeMember r]
---------------------------------------------------------------------------
flatten b              = fix . ff fn
  where
    fn | b             = isStaticSig
       | otherwise     = nonStaticSig

    ff flt γ rec (ID _ _ vs (Just (p, ts')) es, ts) = do  
        parent        <- resolveRelNameInEnv γ p
        inherited     <- rec (parent, ts')
        return         $ apply θ $ L.unionBy sameBinder current inherited
      where 
        current        = filter flt es
        θ              = fromList $ zip vs ts

    ff flt _ _ (ID _ _ vs Nothing es, ts) = 
        return $ apply θ $ filter flt es
      where 
        θ              = fromList $ zip vs ts

-- | flatten' does not apply the top-level type substitution
---------------------------------------------------------------------------
flatten' :: (Data r, PPR r, EnvLike r g) 
         => Bool -> g r -> IfaceDef r -> Maybe [TypeMember r]
---------------------------------------------------------------------------
flatten' st γ d@(ID _ _ vs _ _) = flatten st γ (d, tVar <$> vs)

---------------------------------------------------------------------------
flatten'' :: (Data r, PPR r, EnvLike r g) 
          =>  Bool -> g r -> IfaceDef r -> Maybe ([TVar], [TypeMember r])
---------------------------------------------------------------------------
flatten'' st γ d@(ID _ _ vs _ _) = (vs,) <$> flatten st γ (d, tVar <$> vs)


-- | `flattenType` will flatten *any* type (including Mutability types!)
--    It is not intended to be called with mutability types.
--    Types with zero type parameters (missing mutability field) are considered
--    invalid.
---------------------------------------------------------------------------
flattenType :: (PPR r, EnvLike r g, Data r) => g r -> RType r -> Maybe (RType r)
---------------------------------------------------------------------------
flattenType γ (TApp (TRef x) ts r) = do 
    d                 <- resolveRelNameInEnv γ x
    es                <- flatten False γ (d, ts)
    case ts of 
      mut : _         -> Just $ TCons es (toType mut) r
      _               -> Nothing

flattenType γ (TClass x)             = do
    d                 <- resolveRelNameInEnv γ x
    es                <- flatten' True γ d
    return             $ TCons es mut fTop
  where
    mut                = t_readOnly -- toType $ t_ReadOnly $ get_common_ts γ 

flattenType _ t = Just t


-- | `weaken` a named type, by moving upwards in the class hierarchy. This
--   function does the necessary type argument substitutions. 
--
-- FIXME: Works for classes, but interfaces could have multiple ancestors.
--        What about common elements in parent class?
---------------------------------------------------------------------------
weaken :: (Data r, PPR r, EnvLike r g) 
       => g r -> RelName -> RelName -> [RType r] -> Maybe (SIfaceDef r)
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
ancestors :: (PPR r, Data r, EnvLike r g) => g r -> RelName -> [RelName]
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

