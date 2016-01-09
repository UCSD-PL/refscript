{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Rsc.Lookup (
    getProp
  , getFieldAssignability
  , extractCall
  , extractCtor
  ) where

import           Control.Applicative            ((<$>))
import           Data.Either                    (rights)
import qualified Language.Fixpoint.Types        as F
import           Language.Fixpoint.Types.Errors
import           Language.Rsc.AST
import           Language.Rsc.ClassHierarchy
import           Language.Rsc.Core.Env
import           Language.Rsc.Environment
import           Language.Rsc.Errors
import           Language.Rsc.Locations
import           Language.Rsc.Names
import           Language.Rsc.Pretty
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types

-- import           Debug.Trace


-- | Excluded fields from string index lookup
--
excludedFieldSymbols = F.symbol <$> [ "hasOwnProperty", "prototype", "__proto__" ]

type PPRD r   = (ExprReftable Int r, PP r, F.Reftable r)
type CEnv r t = (CheckingEnvironment () t, CheckingEnvironment r t , Functor t)

-- | `getProp γ b x s t` performs the access `x.f`, where `x: t` and returns:
--
--   (a) the subtype of @t@ for which the access of field @f@ is successful, and
--
--   (b) possible accessed members (mutliple for union types)
--
--------------------------------------------------------------------------------
getProp ::
  (CEnv r t, PP r, PP f, ExprReftable Int r, IsLocated l, F.Symbolic f, F.Reftable r) =>
  l -> t r -> f -> RType r -> Either Error [(RType r, TypeMember r)]
--------------------------------------------------------------------------------
getProp l γ f t@(TPrim _ _) = getPropPrim l γ f t

getProp l γ f (TOr ts _) = getPropUnion l γ f ts

-- | TODO: Chain up to 'Object'
getProp l γ f t@(TObj _ _ _)
  = fmap (map (t,)) (accessMember l γ InstanceK f t)

-- | Enumeration
-- TODO: Instead of the actual integer value, assign unique symbolic values:
--        E.g. A_B_C_1 ...
getProp l γ f t@(TRef (Gen n []) _)
  | Just e  <- resolveEnumInEnv γ n
  , Just io <- envFindTy f $ e_mapping e
  = case io of
      IntLit _ i ->
                    Right [(t, FI Req Final (tNum `strengthen` exprReft i))]
      -- XXX : is 32-bit going to be enough ???
      -- XXX: Invalid BV values will be dropped
      HexLit _ s -> case bitVectorValue s of
                      Just v -> Right [(t, FI Req Final (tBV32 `strengthen` v))]
                      _      -> Left (errorEnumLookup l f t)
      _          -> Left (errorEnumLookup l f t)

getProp l γ f t@(TRef _ _)
  = fmap (map (t,)) (accessMember l γ InstanceK f t)

getProp l γ f t@(TClass _)
  = fmap (map (t,)) (accessMember l γ StaticK f t)

getProp _ γ f t@(TMod m)
  | Just m' <- resolveModuleInEnv γ m
  , Just v' <- envFindTy f (m_variables m')
  = Right [(t, FI Req Final $ v_type v')]

getProp l _ f t = Left (errorGenericLookup l f t)


--------------------------------------------------------------------------------
getPropPrim ::
  (CEnv r t, PP f, PP r, ExprReftable Int r, IsLocated l, F.Symbolic f, F.Reftable r) =>
  l -> t r -> f -> RType r -> Either Error [(RType r, TypeMember r)]
--------------------------------------------------------------------------------
getPropPrim l γ f t@(TPrim c _) =
  case c of
    TNumber    -> fmap (map (t,)) (lookupAmbientType l γ f "Number")
    TString    -> fmap (map (t,)) (lookupAmbientType l γ f "String")
    TStrLit _  -> fmap (map (t,)) (lookupAmbientType l γ f "String")
    TBV32      -> fmap (map (t,)) (lookupAmbientType l γ f "Number")
    _          -> Left (errorPrimLookup l f t)

getPropPrim _ _ _ _ = error "getPropPrim should only be applied to TApp"


-- | `extractCtor γ t` extracts a contructor signature from a type @t@
--
-- TODO: Is fixRet necessary?
--
--------------------------------------------------------------------------------
extractCtor :: (PPRD r, CheckingEnvironment r g) => g r -> RType r -> Maybe (RType r)
--------------------------------------------------------------------------------
extractCtor γ t = go t
  where
    go (TClass (BGen x _)) | Just (TD _ ms) <- resolveTypeInEnv γ x
                           = tm_ctor ms
    go (TRef _ _)          = expandType Coercive (envCHA γ) t >>= go
    go (TObj _ ms _)       = tm_ctor ms
    go _                   = Nothing

-- fixRet x vs = fmap (mkAnd . (mkAll vs . mkFun . fixOut vs <$>)) . bkFuns
--   where fixOut vs (a,b,_) = (a,b,retT x vs)

-- retT x vs  = TRef (Gen x (tVar <$> vs)) fTop
-- defCtor x vs = mkAll vs $ TFun Nothing [] (retT x vs) fTop

--------------------------------------------------------------------------------
extractCall :: (CheckingEnvironment r g, PPRD r) => g r -> RType r -> [IOverloadSig r]
--------------------------------------------------------------------------------
extractCall γ             = zip [0..] . go []
  where
    go αs   (TFun ts t _) = [(αs, ts, t)]
    go αs   (TAnd ts)     = concatMap (go αs) (map snd ts)
    go αs   (TAll α t)    = go (αs ++ [α]) t
    go αs t@(TRef _ _)    | Just t' <- expandType Coercive (envCHA γ) t
                          = go αs t'
    go αs   (TObj _ ms _) | Just t <- tm_call ms
                          = go αs t
    go _  _               = []

--------------------------------------------------------------------------------
accessMember ::
  (CEnv r t, PP r, PP f, ExprReftable Int r, IsLocated l, F.Symbolic f, F.Reftable r) =>
  l -> t r -> StaticKind -> f -> RType r -> Either Error [TypeMember r]
--------------------------------------------------------------------------------
accessMember l γ static m t
  | Just (TObj _ es _) <- expandType Coercive (envCHA γ) t
  , Just mem <- F.lookupSEnv (F.symbol m) (mems es)
  = Right [mem]
  -- In the case of string indexing, build up an optional and assignable field
  | Just (TObj _ es _) <- expandType Coercive (envCHA γ) t
  , Just tIdx               <- tm_sidx es
  , validFieldName m
  = Right [FI Opt Assignable tIdx]
  | otherwise
  = Left $ errorMemLookup l m t
  where
    mems | static == StaticK  = s_mems
         | otherwise          = i_mems

--------------------------------------------------------------------------------
validFieldName  :: F.Symbolic f => f -> Bool
--------------------------------------------------------------------------------
validFieldName f = F.symbol f `notElem` excludedFieldSymbols

--------------------------------------------------------------------------------
lookupAmbientType ::
  (CEnv r t, PP f, PP r, PP b, ExprReftable Int r, IsLocated l, F.Symbolic f, F.Symbolic b, F.Reftable r) =>
  l -> t r -> b -> f -> Either Error [TypeMemberQ AK r]
--------------------------------------------------------------------------------
lookupAmbientType l γ f amb
  | Just (TD _ ms) <- resolveTypeInEnv γ nm
  = accessMember l γ InstanceK f (TObj tIM ms fTop)
  | otherwise
  = Left (errorAmbientLookup l f (F.symbol amb))
  where
    nm = mkAbsName [] (F.symbol amb)

-- | Accessing the @f@ field of the union type with @ts@ as its parts, returns
-- "Nothing" if accessing all parts return error, or "Just (ts, tfs)" if
-- accessing @ts@ returns type @tfs@. @ts@ is useful for adding casts later on.
--------------------------------------------------------------------------------
getPropUnion ::
  (CEnv r t, PP r, PP f, ExprReftable Int r, IsLocated l, F.Symbolic f, F.Reftable r) =>
  l -> t r -> f -> [RType r] -> Either Error [(RType r, TypeMember r)]
--------------------------------------------------------------------------------
getPropUnion l γ f ts =
  case rights (map (getProp l γ f) ts) of
    [ ] -> Left (errorUnionLookup l f (TOr ts fTop))
    tfs -> Right (concat tfs)

--------------------------------------------------------------------------------
getFieldAssignability ::
  (ExprReftable Int r, PPR r) =>
  ClassHierarchy r -> RType r -> F.Symbol -> Maybe FieldAsgn
--------------------------------------------------------------------------------
getFieldAssignability cha t f
  | Just (TObj _ ms _) <- expandType Coercive cha t
  , Just (FI _ m _   ) <- F.lookupSEnv f (i_mems ms)
  = Just m
  | otherwise
  = Nothing

