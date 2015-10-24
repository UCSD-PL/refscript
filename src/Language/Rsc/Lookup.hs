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
  , extractCall
  , extractCtor
  , AccessKind(..)
  ) where

import           Control.Applicative          (pure, (<$>))
import           Control.Monad                (liftM)
import           Data.Either                  (rights)
import           Data.Generics
import           Data.Maybe                   (catMaybes)
import           Language.Fixpoint.Errors
import qualified Language.Fixpoint.Types      as F
import           Language.Rsc.AST
import           Language.Rsc.ClassHierarchy
import           Language.Rsc.Core.Env
import           Language.Rsc.Environment
import           Language.Rsc.Errors
import           Language.Rsc.Locations
import           Language.Rsc.Names
import           Language.Rsc.Pretty
import           Language.Rsc.Typecheck.Sub
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types

-- import           Debug.Trace


-- | Excluded fields from string index lookup
--
excludedFieldSymbols = F.symbol <$> [ "hasOwnProperty", "prototype", "__proto__" ]

type PPRD r = (ExprReftable Int r, PP r, F.Reftable r)

data AccessKind = MethodAccess | FieldAccess

instance PP AccessKind where
  pp MethodAccess = pp "MethodAccess"
  pp FieldAccess  = pp "FieldAccess"


-- | `getProp γ b x s t` performs the access `x.f`, where @t@ is the type
--   assigned to @x@ and returns a triplet containing:
--
--   (a) the subtype of @t@ for which the access of field @f@ is successful, and
--
--   (b) the accessed type.
--
--------------------------------------------------------------------------------
-- getProp :: (IsLocated l, PPRD r, CheckingEnvironment r g, F.Symbolic f, PP f)
--         => l -> g r -> AccessKind -> f -> RType r -> Either Error (RType r, RType r)
--------------------------------------------------------------------------------
getProp l γ b f t@(TPrim _ _) = getPropPrim l γ b f t

getProp l γ b f (TOr ts) = getPropUnion l γ b f ts

-- | TODO: Chain up to 'Object'
getProp l γ b f t@(TObj _ _ _)
  = (t,) <$> accessMember l γ b InstanceK f t

-- | Enumeration
-- TODO: Instead of the actual integer value, assign unique symbolic values:
--        E.g. A_B_C_1 ...
getProp l γ _ f t@(TRef (Gen n []) _)
  | Just e  <- resolveEnumInEnv γ n
  , Just io <- envFindTy f $ e_mapping e
  = case io of
      IntLit _ i -> return (t, tNum `strengthen` exprReft i)
      -- XXX : is 32-bit going to be enough ???
      -- XXX: Invalid BV values will be dropped
      HexLit _ s -> case bitVectorValue s of
                      Just v -> Right (t, tBV32 `strengthen` v)
                      _      -> Left (errorEnumLookup l f t)
      _          -> Left (errorEnumLookup l f t)

getProp l γ b f t@(TRef _ _)
  = (t,) <$> accessMember l γ b InstanceK f t

getProp l γ b f t@(TClass _)
  = (t,) <$> accessMember l γ b StaticK f t

getProp l γ _ f t@(TMod m)
  | Just m' <- resolveModuleInEnv γ m
  , Just v' <- envFindTy f (m_variables m')
  = Right (t, v_type v')

getProp l _ _ f t = Left (errorGenericLookup l f t)


--------------------------------------------------------------------------------
-- getPropPrim :: (IsLocated l, PPRD r, CheckingEnvironment r g, F.Symbolic f, PP f)
--             => l -> g r -> AccessKind -> f -> RType r -> Either Error (RType r, RType r)
--------------------------------------------------------------------------------
getPropPrim l γ b f t@(TPrim c _) =
  case c of
    TNumber    -> (t,) <$> lookupAmbientType l γ b f "Number"
    TString    -> (t,) <$> lookupAmbientType l γ b f "String"
    TStrLit _  -> (t,) <$> lookupAmbientType l γ b f "String"
    TBV32      -> (t,) <$> lookupAmbientType l γ b f "Number"
    _          -> Left (errorPrimLookup l f t)

getPropPrim l _ _ _ _ = error "getPropPrim should only be applied to TApp"


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
    go s@(TRef _ _)        = expandType Coercive (envCHA γ) t >>= go
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
-- accessMember :: (IsLocated l, PPRD r, CheckingEnvironment r g, F.Symbolic f, PP f)
--              => l -> g r -> AccessKind -> StaticKind -> f -> RType r -> Either Error (RType r)
--------------------------------------------------------------------------------
accessMember l γ MethodAccess static m t
  | Just (TObj mut es _) <- expandType Coercive (envCHA γ) t
  , Just (MI _ mts)      <- F.lookupSEnv (F.symbol m) (meths es)
  = Right (mkAnd (catMaybes (map (select mut) mts)))

  | otherwise
  = Left $ errorMethLookup l m t

  where
    meths | static == StaticK  = tm_smeth
          | otherwise          = tm_meth
    -- select method signatures with valid mutabilities
    select mObj (mMeth, tMeth)
          | isSubtype γ mObj mMeth = Just tMeth
          | otherwise = Nothing

accessMember l γ FieldAccess static f t
  | Just (TObj mut es _) <- expandType Coercive (envCHA γ) t
  , Just (FI o _ t)      <- F.lookupSEnv (F.symbol f) $ props es
  = if o == Opt then Right $ orUndef t
                else Right t

  | Just (TObj mut es _) <- expandType Coercive (envCHA γ) t
  , Just t               <- tm_sidx es
  , validFieldName f
  = Right t

  | otherwise
  = Left (errorFieldLookup l t f)

  where
    props | static == StaticK = tm_sprop
          | otherwise         = tm_prop

--------------------------------------------------------------------------------
validFieldName  :: F.Symbolic f => f -> Bool
--------------------------------------------------------------------------------
validFieldName f = F.symbol f `notElem` excludedFieldSymbols

--------------------------------------------------------------------------------
-- lookupAmbientType :: (IsLocated l, PPRD r, CheckingEnvironment r g, F.Symbolic f, F.Symbolic s, PP f)
--                   => l -> g r -> AccessKind -> f -> s -> Either Error (RType r)
--------------------------------------------------------------------------------
lookupAmbientType l γ b f amb
  | Just (TD _ ms) <- resolveTypeInEnv γ nm
  = accessMember l γ b InstanceK f (TObj tIM ms fTop)
  | otherwise
  = Left (errorAmbientLookup l f (F.symbol amb))
  where
    nm = mkAbsName [] (F.symbol amb)

-- | Accessing the @f@ field of the union type with @ts@ as its parts, returns
-- "Nothing" if accessing all parts return error, or "Just (ts, tfs)" if
-- accessing @ts@ returns type @tfs@. @ts@ is useful for adding casts later on.
--------------------------------------------------------------------------------
-- getPropUnion :: (IsLocated l, PPRD r, CheckingEnvironment r g, F.Symbolic f, PP f)
--              => l -> g r -> AccessKind -> f -> [RType r] -> Either Error (RType r, RType r)
--------------------------------------------------------------------------------
getPropUnion l γ b f ts =
  case unzip (rights $ getProp l γ b f <$> ts) of
    ([ ], [ ]) -> Left (errorUnionLookup l f (TOr ts))
    (t1s, t2s) -> Right (mkUnion t1s, mkUnion t2s)

--------------------------------------------------------------------------------
getFieldMutability ::
  (ExprReftable Int r, PPR r) =>
  ClassHierarchy r -> RType r -> F.Symbol -> Maybe (RType r)
--------------------------------------------------------------------------------
getFieldMutability cha t f | Just (TObj _ ms _) <- expandType Coercive cha t
                           , Just (FI _ m _)  <- F.lookupSEnv f $ tm_prop ms
                           = Just m
                           | otherwise
                           = Nothing

