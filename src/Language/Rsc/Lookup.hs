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
import           Data.Generics
import           Data.Maybe                   (catMaybes)
import qualified Language.Fixpoint.Types      as F
import           Language.Rsc.AST
import           Language.Rsc.ClassHierarchy
import           Language.Rsc.Core.Env
import           Language.Rsc.Environment
import           Language.Rsc.Names
import           Language.Rsc.Pretty
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
-- assigned to @x@ and returns a triplet containing:
--  (a) the subtype of @t@ for which the access of field @f@ is successful,
--  (b) the accessed type, and
--  [ (c) the mutability associcated with the accessed element ]
--
-------------------------------------------------------------------------------
getProp :: (PPRD r, EnvLike r g, F.Symbolic f, PP f)
        => g r -> AccessKind -> f -> RType r -> Maybe (RType r, RType r)
-------------------------------------------------------------------------------
getProp γ b f t@(TPrim _ _) = getPropPrim γ b f t

getProp γ b f (TOr ts) = getPropUnion γ b f ts

-- | TODO: Chain up to 'Object'
getProp γ b f t@(TObj _ es _)
  = (t,) <$> accessMember γ b InstanceK f es

-- | Enumeration
-- TODO: Instead of the actual integer value, assign unique symbolic values:
--        E.g. A_B_C_1 ...
getProp γ _ f t@(TRef (Gen n []) _)
  | Just e  <- resolveEnumInEnv γ n
  , Just io <- envFindTy f $ e_mapping e
  = case io of
      IntLit _ i -> return (t, tNum `strengthen` exprReft i)
      -- XXX : is 32-bit going to be enough ???
      -- XXX: Invalid BV values will be dropped
      HexLit _ s -> liftM ((t,) . (tBV32 `strengthen`)) (bitVectorValue s)
      _          -> Nothing

getProp γ b f t@(TRef _ _)
  = expandType Coercive (envCHA γ) t >>= \case
      TObj _ ms _ -> (t,) <$> accessMember γ b InstanceK f ms
      _           -> Nothing


getProp γ b f t@(TClass _)
  = expandType Coercive (envCHA γ) t >>= \case
      TObj _ ms _ -> (t,) <$> accessMember γ b StaticK f ms
      _           -> Nothing

getProp γ _ f t@(TMod m)
  = do  m'          <- resolveModuleInEnv γ m
        VI _ _ _ t' <- envFindTy f $ m_variables m'
        return         (t,t')

getProp _ _ _ _ = Nothing


-------------------------------------------------------------------------------
getPropPrim :: (PPRD r, EnvLike r g, F.Symbolic f, PP f)
            => g r -> AccessKind -> f -> RType r -> Maybe (RType r, RType r)
-------------------------------------------------------------------------------
getPropPrim γ b f t@(TPrim c _) =
  case c of
    TBoolean   -> Nothing
    TUndefined -> Nothing
    TNull      -> Nothing
    TNumber    -> (t,) <$> lookupAmbientType γ b f "Number"
    TString    -> (t,) <$> lookupAmbientType γ b f "String"
    TStrLit _  -> (t,) <$> lookupAmbientType γ b f "String"
    TBV32      -> (t,) <$> lookupAmbientType γ b f "Number"
    TVoid      -> Nothing
    TTop       -> Nothing
    TBot       -> Nothing
    TFPBool    -> Nothing
getPropPrim _ _ _ _ = error "getPropPrim should only be applied to TApp"


-- | `extractCtor γ t` extracts a contructor signature from a type @t@
--
-- TODO: Is fixRet necessary?
--
-------------------------------------------------------------------------------
extractCtor :: (PPRD r, EnvLike r g) => g r -> RType r -> Maybe (RType r)
-------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
extractCall :: (EnvLike r g, PPRD r) => g r -> RType r -> [IOverloadSig r]
-------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
accessMember :: (PPRD r, EnvLike r g, F.Symbolic f, PP f)
             => g r -> AccessKind -> StaticKind -> f -> TypeMembers r -> Maybe (RType r)
-------------------------------------------------------------------------------
accessMember _ MethodAccess static m ms
  | Just (MI _ mts) <- F.lookupSEnv (F.symbol m) $ meths ms
  = Just $ mkAnd $ snd <$> mts
  | otherwise
  = Nothing
  where
    meths | static == StaticK = tm_smeth
          | otherwise         = tm_meth

accessMember _ FieldAccess static f ms
  | Just (FI o _ t) <- F.lookupSEnv (F.symbol f) $ props ms
  = if o == Opt then Just $ orUndef t
                else Just t
  | Just t <- tm_sidx ms
  , validFieldName f
  = Just t
  | otherwise
  = error $ "Not found: " ++ ppshow f
  where
    props | static == StaticK = tm_sprop
          | otherwise         = tm_prop

-------------------------------------------------------------------------------
validFieldName  :: F.Symbolic f => f -> Bool
-------------------------------------------------------------------------------
validFieldName f = F.symbol f `notElem` excludedFieldSymbols

-------------------------------------------------------------------------------
lookupAmbientType :: (PPRD r, EnvLike r g, F.Symbolic f, F.Symbolic s, PP f)
                  => g r -> AccessKind -> f -> s -> Maybe (RType r)
-------------------------------------------------------------------------------
lookupAmbientType γ b f amb
  = do TD _ ms <- resolveTypeInEnv γ nm
       accessMember γ b InstanceK f ms
  where
    nm = mkAbsName [] (F.symbol amb)

-- | Accessing the @f@ field of the union type with @ts@ as its parts, returns
-- "Nothing" if accessing all parts return error, or "Just (ts, tfs)" if
-- accessing @ts@ returns type @tfs@. @ts@ is useful for adding casts later on.
-------------------------------------------------------------------------------
getPropUnion :: (PPRD r, EnvLike r g, F.Symbolic f, PP f)
             => g r -> AccessKind -> f -> [RType r] -> Maybe (RType r, RType r)
-------------------------------------------------------------------------------
getPropUnion γ b f ts =
  case unzip (catMaybes $ getProp γ b f <$> ts) of
    ([],[]) -> Nothing
    (t1s,t2s) -> Just (mkUnion t1s, mkUnion t2s)

-------------------------------------------------------------------------------
getFieldMutability ::
  (ExprReftable Int r, PPR r) =>
  ClassHierarchy r -> RType r -> F.Symbol -> Maybe (RType r)
-------------------------------------------------------------------------------
getFieldMutability cha t f | Just (TObj _ ms _) <- expandType Coercive cha t
                           , Just (FI _ m _)  <- F.lookupSEnv f $ tm_prop ms
                           = Just m
                           | otherwise
                           = Nothing

