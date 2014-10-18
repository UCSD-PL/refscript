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

  -- * Flatten a type definition applying subs
  , flatten, flatten', flatten'', flattenType

  -- * Ancestors
  , weaken, ancestors, isAncestor

  -- * Keys
  , boundKeys

  -- * Constructors
  , Constructor, {- toConstructor, isConstSubtype, -} sameTypeof, getTypeof

  ) where 

import           Data.Generics
import           Data.Function                       (on)
import qualified Data.Map.Strict                  as M
import qualified Language.Fixpoint.Types          as F
import           Language.Nano.Env
import           Language.Nano.Errors
import           Language.Nano.Environment
import           Language.Nano.Names
import           Language.Nano.Types
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
flatten m s γ (ID _ _ vs h es, ts) =
    case h of 
      Just (p, ts') -> do pdef  <- resolveTypeInEnv γ p
                          inh   <- flatten m s γ $ (, ts') pdef
                          return $ M.map (apply θ . fmut) $ M.union current inh
      Nothing       ->    return $ M.map (apply θ . fmut) $ current 
  where 
    current                      = M.filterWithKey (\(_,s') _ -> s == s') es
    θ                            = fromList $ zip vs ts
    fmut                         = maybe id setMut m
    setMut m (FieldSig x _ t)    = FieldSig x m t
    setMut _ e                   = e


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
    mkField (k,(_,_,t,_)) = ((F.symbol k, InstanceMember), FieldSig (F.symbol k) t_immutable t)

flattenType γ (TEnum x)
  = do  es      <- M.fromList . map  mkField . envToList . e_symbols <$> resolveEnumInEnv γ x
        return   $ TCons t_immutable es fTop
  where
    mkField (k,i) = ((F.symbol k, InstanceMember), FieldSig (F.symbol k) t_immutable $ tInt `strengthen` exprReft i)

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
--
--    FIXME: Works for classes, but interfaces could have multiple ancestors.
--           What about common elements in parent class?
--
---------------------------------------------------------------------------
weaken :: (PPR r, EnvLike r g) => g r -> AbsName -> AbsName -> [RType r] -> Maybe (SIfaceDef r)
---------------------------------------------------------------------------
weaken γ a b ts
  | a == b = (,ts) <$> resolveTypeInEnv γ a
  | otherwise
  = do  z <- resolveTypeInEnv γ a
        case z of
          ID _ _ vs (Just (p,ps)) _ -> weaken γ p b $ apply (fromList $ zip vs ts) ps
          ID _ _ _  Nothing       _ -> Nothing


-- FIXME: revisit these
---------------------------------------------------------------------------
ancestors :: (PPR r, EnvLike r g) => g r -> AbsName -> [AbsName]
---------------------------------------------------------------------------
ancestors γ s = 
  case resolveTypeInEnv γ s of 
    Just (ID {t_proto = p }) -> 
      case p of 
        Just (par,_) ->  s : ancestors γ par
        _ -> [s]
    _ -> [s]

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
-- Constructors
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

