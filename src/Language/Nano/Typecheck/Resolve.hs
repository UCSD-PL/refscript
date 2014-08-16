{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Nano.Typecheck.Resolve ( 
  
  resolveModuleEnv, resolveQName

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
import           Language.Nano.Types
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Environment
import           Language.Nano.Typecheck.Subst

import           Control.Applicative ((<$>))
import qualified Data.List as L
import           Data.Function (fix)

-- import           Debug.Trace

type PPR r = (PP r, F.Reftable r)


-- Namespace substitutions

-- | `rebaseNamespaces`: express all namespaces mentioned in `a` with
--   (originally expressed in the namespace of environment `from`) in terms of
--   the namespace of environment `to`.
---------------------------------------------------------------------------
rebaseNamespaces :: (EnvLike r g, Data a) => g r -> g r -> a -> Maybe a
---------------------------------------------------------------------------
rebaseNamespaces from to = everywhereM $ mkM tx
  where
    tx         :: NameSpacePath -> Maybe NameSpacePath
    tx path     = case fromAbsPath path of
                    Just absP -> Just $ fst <$> dropWhile same (zip absP baseAbsP)
                    _         -> Nothing
    same (a,b)  = a == b
    fromAbsPath = absolutePath from
    baseAbsP    = get_nspace to


-- | `absolutePath`: expresses a namespace in terms of the global namespace
---------------------------------------------------------------------------
absolutePath    :: EnvLike r g => g r -> NameSpacePath -> Maybe AbsolutePath
---------------------------------------------------------------------------
absolutePath γ p = get_nspace <$> resolveModuleEnv γ p


-- | `resolveModuleEnv`: resolve the environment referenced by the second
--   argument, expressed in terms of the environment in the first argument.
---------------------------------------------------------------------------
resolveModuleEnv     :: EnvLike r g => g r -> NameSpacePath -> Maybe (g r)
---------------------------------------------------------------------------
resolveModuleEnv γ [] = Just γ

-- FIXME !!!
 
-- resolveModuleEnv γ (m:ms) = 
--   case envFindTy m (get_mod γ) of
--     Just γ' -> foldM (\g -> (`envFindTy` get_mod g)) γ' ms
--     Nothing -> case get_parent γ of
--                  Just γ' -> resolveModuleEnv γ' (m:ms)
--                  Nothing -> Nothing


-- | `resolveQName`: 
--   * Returns the interface definition `d` that resides at path `xs` 
--     using `g` as point of reference. 
--   * All qualified names in `d` have been translated to terms of 
--     environment `g`. 
---------------------------------------------------------------------------
resolveQName :: (Data r, EnvLike r g) => g r -> QName -> Maybe (IfaceDef r)
---------------------------------------------------------------------------
resolveQName γ qn = do
    (γ',d) <- resolveIfaceAux γ qn
    rebaseNamespaces γ' γ d
  where
  -- FIXME 
    resolveIfaceAux γ qn@(QN _ [] s) = 
      case envFindTy s (error "resolveQName") of -- (get_types γ) of 
        Just d  -> Just (γ,d)
--         Nothing -> case get_parent γ of
--                     Just γ' -> resolveIfaceAux γ' qn
--                     Nothing -> Nothing
    resolveIfaceAux γ (QN l ns s) = do
        γ' <- resolveModuleEnv γ ns
        resolveIfaceAux γ' (QN l [] s) 


-- | flattening type to include all fields inherited by ancestors
---------------------------------------------------------------------------
flatten :: (EnvLike r g, PPR r, Data r) => Bool -> g r -> (SIfaceDef r) -> Maybe [TypeMember r]
---------------------------------------------------------------------------
flatten b              = fix . ff fn
  where
    fn | b             = isStaticSig
       | otherwise     = nonStaticSig

    ff flt γ rec (ID _ _ vs (Just (p, ts')) es, ts) = do  
        parent        <- resolveQName γ p
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
    d                 <- resolveQName γ x
    es                <- flatten False γ (d, ts)
    case ts of 
      mut : _         -> Just $ TCons es (toType mut) r
      _               -> Nothing

flattenType γ (TClass x)             = do
    d                 <- resolveQName γ x
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
       => g r -> QName -> QName -> [RType r] -> Maybe (SIfaceDef r)
---------------------------------------------------------------------------
weaken γ pa pb ts
  | pa == pb = (,ts) <$> resolveQName γ pa
  | otherwise
  = do  z <- resolveQName γ pa
        case z of
          ID _ _ vs (Just (p,ps)) _ -> weaken γ p pb $ apply (fromList $ zip vs ts) ps
          ID _ _ _  Nothing       _ -> Nothing


---------------------------------------------------------------------------
ancestors :: (Data r, EnvLike r g) => g r -> QName -> [QName]
---------------------------------------------------------------------------
ancestors γ s = 
  case resolveQName γ s of 
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
  symbol (TApp (TRef (QN _ _ x)) _ _)  = x
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

