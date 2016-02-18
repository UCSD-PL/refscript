{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Rsc.Lookup (
    getProp
  , getFieldMutability
  , extractCtor
  ) where

import           Data.Default
import           Data.Either                    (rights)
import qualified Language.Fixpoint.Types        as F
import           Language.Fixpoint.Types.Errors
import           Language.Rsc.AST
import           Language.Rsc.ClassHierarchy
import           Language.Rsc.Core.Env
import           Language.Rsc.Environment
import           Language.Rsc.Errors
import           Language.Rsc.Liquid.Types
import           Language.Rsc.Locations
import           Language.Rsc.Module
import           Language.Rsc.Names
import           Language.Rsc.Pretty
import           Language.Rsc.Symbols
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types

type PPRE r = (ExprReftable Int r, PPR r)

-- | Excluded fields from string index lookup
--
excludedFieldSymbols = F.symbol <$> [ "hasOwnProperty", "prototype", "__proto__" ]

type PPRD r   = (ExprReftable Int r, PP r, F.Reftable r)
type CEnv r t = (CheckingEnvironment r t , Functor t)

-- | `getProp l γ x f t` performs the access `x.f`, where `x: t` and returns a
--   list of pairs `(tBase, tMember)` where
--
--   * tBase  : the part of `t` for which the access of field `f` is successful
--
--   * tMember: the accessed type member.
--
--------------------------------------------------------------------------------
getProp :: (CEnv r t, PPRD r, IsLocated l, F.Expression x, F.Symbolic x, F.Symbolic f)
        => l -> t r -> x -> f -> RType r -> Either Error [(RType r, TypeMember r)]
--------------------------------------------------------------------------------
getProp l γ x f t@(TPrim _ _) = getPropPrim l γ x f t

getProp l γ x f (TOr ts _) = getPropUnion l γ x f ts

-- | TODO: Chain up to 'Object'
getProp l γ x f t@TObj{}
  = fmap (map (t,)) (accessMember l γ InstanceK x f t)

-- | Enumeration
--
--   TODO: Instead of the actual integer value, assign unique symbolic values:
--         E.g. A_B_C_1 ...
--
getProp l γ _ f t@(TRef (Gen n []) _)
  | Just e  <- resolveEnumInEnv γ n
  , Just io <- envFindTy f (e_mapping e)
  = case io of
      IntLit _ i ->
          Right [(t, FI fSym Req tIM (tNum `strengthen` exprReft i))]
      HexLit _ s ->
          case bitVectorValue s of
            Just v -> Right [(t, FI fSym Req tIM (tBV32 `strengthen` v))]
            _      -> Left (errorEnumLookup l fSym t)
      _ ->  Left (errorEnumLookup l fSym t)
  where
    fSym = F.symbol f

getProp l γ x f t@(TRef _ _)
  = fmap (map (t,)) (accessMember l γ InstanceK x f t)

getProp l γ x f t@(TClass _)
  = fmap (map (t,)) (accessMember l γ StaticK x f t)

getProp _ γ _ f t@(TMod m)
  | Just m' <- resolveModuleInEnv γ m
  , Just v' <- envFindTy f (m_variables m')
  = Right [(t, symToField v')]

getProp l _ _ f t = Left (errorGenericLookup l (F.symbol f) t)


--------------------------------------------------------------------------------
getPropPrim
  :: (CEnv r t, PPRD r, IsLocated l, F.Expression x, F.Symbolic x, F.Symbolic f)
  => l -> t r -> x -> f -> RType r -> Either Error [(RType r, TypeMember r)]
--------------------------------------------------------------------------------
getPropPrim l γ x f t@(TPrim c _) =
  case c of
    TNumber    -> fmap (map (t,)) (lookupAmbientType l γ x f numberSym)
    TString    -> fmap (map (t,)) (lookupAmbientType l γ x f stringSym)
    TStrLit _  -> fmap (map (t,)) (lookupAmbientType l γ x f stringSym)
    TBV32      -> fmap (map (t,)) (lookupAmbientType l γ x f numberSym)
    _          -> Left (errorPrimLookup l (F.symbol f) t)

getPropPrim _ _ _ _ _ = error "getPropPrim should only be applied to TApp"


-- | `extractCtor γ t` extracts a contructor signature from a type @t@
--
--------------------------------------------------------------------------------
extractCtor :: (PPRD r, CEnv r g) => g r -> RType r -> Maybe (RType r)
--------------------------------------------------------------------------------
extractCtor γ t = go t
  where
    go (TClass (BGen x _)) | Just (TD _ ms) <- resolveTypeInEnv γ x
                           = tm_ctor ms
    go (TRef _ _)          = expandType def (envCHA γ) t >>= go
    go (TObj _ ms _)       = tm_ctor ms
    go _                   = Nothing


--------------------------------------------------------------------------------
accessMember
  :: (CEnv r t, PPRE r, IsLocated l, F.Expression x, F.Symbolic x, F.Symbolic f)
  => l -> t r -> StaticKind -> x -> f -> RType r -> Either Error [TypeMember r]
--------------------------------------------------------------------------------
accessMember l γ InstanceK x m t
  | Just (TObj _ es _) <- expandType econf (envCHA γ) t
  , Just mem           <- F.symbol m `F.lookupSEnv` i_mems es
  = Right [substThis x mem]

  -- In the case of string indexing, build up an optional and assignable field
  | Just (TObj _ es _) <- expandType econf (envCHA γ) t
  , Just (mIdx, tIdx)  <- tm_sidx es
  , validFieldName m
  = Right [substThis x $ FI (F.symbol m) Opt mIdx tIdx]

  | otherwise
  = Left $ errorMemLookup l (F.symbol m) t
  where
    econf = EConf True True -- Lookup members inherited from built-in objects

accessMember l γ StaticK x m t
  | Just (TObj _ es _) <- expandType econf (envCHA γ) t
  , Just mem           <- F.symbol m `F.lookupSEnv` i_mems es
  = Right [substThis x mem]
  | otherwise
  = Left $ errorMemLookup l (F.symbol m) t
  where
    econf = EConf True True -- Lookup members inherited from built-in objects


--------------------------------------------------------------------------------
validFieldName  :: F.Symbolic f => f -> Bool
--------------------------------------------------------------------------------
validFieldName f = F.symbol f `notElem` excludedFieldSymbols

--------------------------------------------------------------------------------
lookupAmbientType
  :: (CEnv r t, PPRD r, IsLocated l, F.Expression x, F.Symbolic x, F.Symbolic f)
  => l -> t r -> x -> f -> F.Symbol -> Either Error [TypeMemberQ AK r]
--------------------------------------------------------------------------------
lookupAmbientType l γ x f amb
  | Just (TD _ ms) <- resolveTypeInEnv γ (mkAbsName [] amb)
  = accessMember l γ InstanceK x f (TObj tIM ms fTop)
  | otherwise
  = Left (errorAmbientLookup l (F.symbol f) (F.symbol x))

-- | Accessing the @f@ field of the union type with @ts@ as its parts, returns
--   "Nothing" if accessing all parts return error, or "Just (ts, tfs)" if
--   accessing @ts@ returns type @tfs@. @ts@ is useful for adding casts later on.
--------------------------------------------------------------------------------
getPropUnion
  :: (CEnv r t, PPRD r, IsLocated l, F.Expression x, F.Symbolic x, F.Symbolic f)
  => l -> t r -> x -> f -> [RType r] -> Either Error [(RType r, TypeMember r)]
--------------------------------------------------------------------------------
getPropUnion l γ x f ts =
  case rights (map (getProp l γ x f) ts) of
    [ ] -> Left (errorUnionLookup l (F.symbol f) (tOrR ts fTop))
    tfs -> Right (concat tfs)

--------------------------------------------------------------------------------
getFieldMutability :: (PPRD r, F.Symbolic s)
  => ClassHierarchy r -> RType r -> s -> Maybe (MutabilityR r)
--------------------------------------------------------------------------------
getFieldMutability cha t f
  | Just (TObj _ ms _i) <- expandType def cha t
  , Just (FI _ _ m _)   <- F.lookupSEnv (F.symbol f) (i_mems ms)
  = Just m
  | otherwise
  = Nothing

