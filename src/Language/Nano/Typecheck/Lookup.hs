{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Nano.Typecheck.Lookup (
    getProp
  , extractCall
  , extractCtor
  , extractParent, extractParent', extractParent''
  , AccessKind(..)
  ) where 

import           Data.Generics
import qualified Data.Map.Strict as M

import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Syntax

import qualified Language.Fixpoint.Types as F
import qualified Language.Fixpoint.Bitvector as BV

import           Language.Nano.Names
import           Language.Nano.Types
import           Language.Nano.Env
import           Language.Nano.Errors
import           Language.Nano.Environment
import           Language.Nano.Typecheck.Types
import           Language.Nano.Liquid.Types
import           Language.Nano.Typecheck.Resolve
import           Language.Nano.Typecheck.Subst
import           Control.Applicative ((<$>))

-- import           Debug.Trace


-- | Excluded fields from string index lookup
--
excludedFieldSymbols = F.symbol <$> [ "hasOwnProperty", "prototype", "__proto__" ]


type PPRD r = (ExprReftable Int r, PP r, F.Reftable r, Data r)


data AccessKind = MethodAccess | FieldAccess

instance PP AccessKind where
  pp MethodAccess = pp "MethodAccess"
  pp FieldAccess  = pp "FieldAccess"


-- | `getProp γ b s t`: returns  a triplet containing:
--
--   * The subtype of @t@ for which the access of field @s@ is successful.
--
--   * The corresponding accessed type.
--
--   * The mutability associcated with the accessed element 
--
--  FIXME: Fix visibility
--
-------------------------------------------------------------------------------
getProp :: (PPRD r, EnvLike r g, F.Symbolic f, PP f) 
        => g r -> AccessKind -> f -> RType r -> Maybe (RType r, RType r, Mutability)
-------------------------------------------------------------------------------
getProp γ b s t@(TApp _ _ _  ) = getPropApp γ b s t

getProp γ b s t@(TCons m es _) 
  = do  (t',m')    <- accessMember True γ b InstanceMember s 
                    $ M.union es objProto
        return      $ (t,t',combMut m m')
  where 
  -- Add Object as the prototype type
    objProto = M.singleton (protoSym, InstanceMember) 
             $ FieldSig protoSym f_required t_immutable t_object
    protoSym = F.symbol "prototype"
    
getProp γ b s t@(TRef x ts@(m:ts') r)  
  = do  d          <- resolveTypeInEnv γ x
        es         <- flatten Nothing InstanceMember γ (d,ts)
        (t',m')    <- accessMember True γ b InstanceMember s es
        t''        <- fixMethType t'
        return      $ (t, t'',combMut (toType m) m')
  where
    fixMethType ft | isTFun ft                  = mkAnd . (replaceSelf <$>) <$> bkFuns ft
                   | otherwise                  = Just $ ft
    replaceSelf (vs, Just (TSelf m'), bs, ot)   = mkFun (vs, Just (TRef x (m':ts') r), bs, ot)
    replaceSelf a                               = mkFun a

getProp γ b s t@(TClass c) 
  = do  d          <- resolveTypeInEnv γ c
        es         <- flatten Nothing StaticMember γ (d,[])
        (t', m)    <- accessMember True γ b StaticMember s es
        return      $ (t,t',m)

getProp γ _ s t@(TModule m) 
  = do  m'         <- resolveModuleInEnv γ m
        (_,_,t',_) <- envFindTy s $ m_variables m'
        return      $ (t,t', t_readOnly) 

-- FIXME: Instead of the actual integer value, assign unique symbolic values: 
--        E.g. A_B_C_1 ... 
getProp γ _ s t@(TEnum e     ) 
  = do e'         <- resolveEnumInEnv γ e
       io         <- envFindTy (F.symbol s) (e_mapping e')
       case io of
         IntLit _ i -> return (t, tInt `strengthen` exprReft i, t_immutable)
         -- XXX : is 32-bit going to be enough ???
         HexLit _ s -> return (t, tBV32 `strengthen` exprReft (BV.Bv BV.S32 s), t_immutable)
         _          -> Nothing

getProp _ _ _ _                = Nothing


-------------------------------------------------------------------------------
getPropApp :: (PPRD r, EnvLike r g, F.Symbolic f, PP f) 
           => g r 
           -> AccessKind
           -> f 
           -> RType r 
           -> Maybe (RType r, RType r, Mutability)
-------------------------------------------------------------------------------
getPropApp γ b s t@(TApp c ts _) = 
  case c of 
    TBool    -> Nothing
    TUndef   -> Nothing
    TNull    -> Nothing
    TUn      -> getPropUnion γ b s ts
    TInt     -> do  (t',m) <- lookupAmbientType γ b s "Number"
                    return  $ (t,t',m)
    TString  -> do  (t',m) <- lookupAmbientType γ b s "String"
                    return  $ (t,t',m)
    TFPBool  -> Nothing
    TTop     -> Nothing
    TVoid    -> Nothing
getPropApp _ _ _ _ = error "getPropApp should only be applied to TApp"


-------------------------------------------------------------------------------
extractCtor :: (PPRD r, EnvLike r g) => g r -> RType r -> Maybe (RType r)
-------------------------------------------------------------------------------
extractCtor γ (TClass x) 
  = do  d        <- resolveTypeInEnv γ x
        (vs, es) <- flatten'' Nothing InstanceMember γ d
        case M.lookup (ctorSymbol, InstanceMember) es of
          Just (ConsSig t) -> fixRet x vs t
          _                -> return $ defCtor x vs
  where

extractCtor γ (TRef x ts _) 
  = do  d        <- resolveTypeInEnv γ x
        (vs, es) <- flatten'' Nothing InstanceMember γ d
        case M.lookup (ctorSymbol, InstanceMember) es of
          Just (ConsSig t) -> apply (fromList $ zip vs ts) <$> fixRet x vs t
          _                -> return $ apply (fromList $ zip vs ts) $ defCtor x vs

extractCtor _ (TCons _ es _ )
  = case M.lookup (ctorSymbol, InstanceMember) es of
      Just (ConsSig t) -> return $ t
      _                -> Nothing
       
extractCtor _ _ = Nothing

fixRet x vs = fmap (mkAnd . (mkAll vs . mkFun . fixOut vs <$>)) . bkFuns
  where fixOut vs (a,b,c,_) = (a,b,c,retT x vs)

retT x vs  = TRef x (tVar <$> vs) fTop
defCtor x vs = mkAll vs $ TFun Nothing [] (retT x vs) fTop


-------------------------------------------------------------------------------
extractParent   :: (PPR r, EnvLike r g) => g r -> RType r -> Maybe (RType r)
extractParent'  :: (PPR r, EnvLike r g) => g r -> [RType r] -> IfaceDef r -> Maybe (RType r)
extractParent'' :: (PPR r, EnvLike r g) => g r -> IfaceDef r -> Maybe (RType r)
-------------------------------------------------------------------------------
extractParent γ (TRef x ts _) = extractParent' γ ts =<< resolveTypeInEnv γ x
extractParent _ _ = Nothing

extractParent' _ ts (ID _ ClassKind vs ([(p,ps)],_) _) = 
  Just $ TRef p (apply (fromList $ zip vs ts) ps) fTop
extractParent' _ _ _ = Nothing

extractParent'' γ i = extractParent' γ ((`TVar` fTop) <$> t_args i) i


-------------------------------------------------------------------------------
extractCall :: (EnvLike r g, PPRD r) => g r -> RType r -> [RType r]
-------------------------------------------------------------------------------
extractCall γ t             = uncurry mkAll <$> foo [] t
  where
    foo αs t@(TFun _ _ _ _) = [(αs, t)]
    foo αs   (TAnd ts)      = concatMap (foo αs) ts 
    foo αs   (TAll α t)     = foo (αs ++ [α]) t
    foo αs   (TRef s _ _  ) | Just d <- resolveTypeInEnv γ s
                            = getCallSig αs $ t_elts d
                            | otherwise = []
    foo αs   (TCons _ es _) = getCallSig αs es
    foo _  _                = []
    getCallSig αs es        | Just (CallSig t) <- M.lookup (callSymbol, InstanceMember) es
                            = foo αs t
                            | otherwise = []

-- | `accessMember b s es` extracts field @s@ from type members @es@. If @b@ is
--   True then this means that the extracted field is for a call, in which case
--   we allow extraction of methods, otherwise extracting methods is disallowed.
-------------------------------------------------------------------------------
accessMember :: (PPRD r, EnvLike r g, F.Symbolic f, PP f)
             => Bool
             -> g r
             -> AccessKind 
             -> StaticKind 
             -> f
             -> TypeMembers r 
             -> Maybe (RType r, Mutability)
-------------------------------------------------------------------------------
-- 
-- Only consider methods with `MethodAccess`
--
accessMember proto γ b@MethodAccess sk f es 
  | Just (MethSig _ t)              <- M.lookup (F.symbol f, sk) es
  = Just (t,t_immutable)
  | Just (IndexSig _ StringIndex t) <- M.lookup (stringIndexSymbol, sk) es
  , validFieldName f 
  = Just (t, t_mutable) 
  | proto
  = accessMemberProto γ b sk f es
  | otherwise 
  = Nothing
-- 
-- DO NOT consider methods with `FieldAccess`
--
accessMember proto γ b@FieldAccess sk f es
  | Just f@(FieldSig _ _ m t) <- M.lookup (F.symbol f, sk) es
  = if optionalField f then Just (orUndef t,m) else Just (t,m)  
  | Just (IndexSig _ StringIndex t) <- M.lookup (stringIndexSymbol, sk) es 
  , validFieldName f 
  = Just (t, t_mutable) 
  | proto
  = accessMemberProto γ b sk f es
  | otherwise 
  = Nothing

accessMemberProto γ b sk f es
  | Just (FieldSig _ _ _ pt) <- M.lookup (F.symbol "prototype", sk) es
  = (\(_,t,m) -> (t,m)) <$> getProp γ b f pt

  -- Everybody inherits Object
  | Just t               <- envLikeFindTy "Object" γ
  , Just (TCons _ es' _) <- flattenType γ t
  = accessMember False γ b sk f es'

  | otherwise 
  = Nothing

-------------------------------------------------------------------------------
validFieldName  :: F.Symbolic f => f -> Bool
-------------------------------------------------------------------------------
validFieldName f = not $ F.symbol f `elem` excludedFieldSymbols

-------------------------------------------------------------------------------
lookupAmbientType :: (PPRD r, EnvLike r g, F.Symbolic f, F.Symbolic s, PP f) 
                  => g r -> AccessKind -> f -> s -> Maybe (RType r, Mutability)
-------------------------------------------------------------------------------
lookupAmbientType γ b fld amb
  = t_elts <$> resolveTypeInEnv γ nm >>= accessMember True γ b InstanceMember fld
  where
    nm = mkAbsName [] (F.symbol amb)

-- Accessing the @x@ field of the union type with @ts@ as its parts, returns
-- "Nothing" if accessing all parts return error, or "Just (ts, tfs)" if
-- accessing @ts@ returns type @tfs@. @ts@ is useful for adding casts later on.
-------------------------------------------------------------------------------
getPropUnion :: (PPRD r, EnvLike r g, F.Symbolic f, PP f) 
             => g r 
             -> AccessKind 
             -> f 
             -> [RType r] 
             -> Maybe (RType r, RType r, Mutability)
-------------------------------------------------------------------------------
getPropUnion γ b f ts = 
  case unzip3 [ttm | Just ttm <- getProp γ b f <$> ts] of
    ([],[] ,[])                           -> Nothing
    (ts,ts',ms)  | all isImmutable     ms -> Just (mkUnion ts, mkUnion ts', t_immutable)
                 | all isMutable       ms -> Just (mkUnion ts, mkUnion ts', t_mutable)
                 | all isAssignsFields ms -> Just (mkUnion ts, mkUnion ts', t_assignsFields)
                 | otherwise              -> Just (mkUnion ts, mkUnion ts', t_readOnly)

