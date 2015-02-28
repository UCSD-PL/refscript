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
import           Language.Nano.Typecheck.Resolve
import           Language.Nano.Typecheck.Subst
import           Control.Applicative ((<$>))

-- import           Debug.Trace


-- | Excluded fields from string index lookup
--
excludedFieldSymbols = F.symbol <$> [ "hasOwnProperty", "prototype", "__proto__" ]


type PPRD r = (ExprReftable F.Symbol r, ExprReftable Int r, PP r, F.Reftable r, Data r)


data AccessKind = MethodAccess | FieldAccess

instance PP AccessKind where
  pp MethodAccess = pp "MethodAccess"
  pp FieldAccess  = pp "FieldAccess"


-- | `getProp γ b x s t`: 

--   Performs the access `x.f`, where γ |- x :: t 
-- 
--   Returns  a triplet containing:
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
-- getProp :: (PPRD r, EnvLike r g, F.Symbolic f, PP f) 
--         => g r -> AccessKind -> x -> f -> RType r -> Maybe (RType r, RType r, Mutability)
-------------------------------------------------------------------------------
getProp γ b x f t@(TApp _ _ _  ) = getPropApp γ b x f t

-- 
-- Raw object containing fields `es` plus a "__proto__" field linking to 'Object'
-- 
getProp γ b x f t@(TCons m es _)
  = do  empty      <- resolveTypeInEnv γ emptyObjectInterface
        (t',m')    <- accessMember γ b InstanceMember x f $ es `M.union` t_elts empty
        return      $ (t,t',m')
    
getProp γ b x f t@(TRef n ts@(m0:ts') r)  
  = do  d          <- resolveTypeInEnv γ n
        es         <- expand InstanceMember γ d ts
        (t',m')    <- accessMember γ b InstanceMember x f es
        t''        <- fixMethType t'
        return      $ (t, t'',m')
  where
    m               = toType m0
    fixMethType ft  | isTFun ft                  
                    = mkAnd . (replaceSelf <$>) <$> bkFuns ft
                    | otherwise                  
                    = Just $ ft
    replaceSelf (vs, Just (TSelf m'), bs, ot) = mkFun (vs, Just (TRef n (m':ts') r), bs, ot)
    replaceSelf a                             = mkFun a

getProp γ b x f t@(TClass c) 
  = do  d          <- resolveTypeInEnv γ c
        es         <- expand StaticMember γ d []
        (t', m)    <- accessMember γ b StaticMember x f es
        return      $ (t,t',m)

getProp γ _ _ f t@(TModule m) 
  = do  m'         <- resolveModuleInEnv γ m
        (_,_,t',_) <- envFindTy f $ m_variables m'
        return      $ (t,t', t_readOnly) 

-- FIXME: Instead of the actual integer value, assign unique symbolic values: 
--        E.g. A_B_C_1 ... 
getProp γ _ x f t@(TEnum e     ) 
  = do e'         <- resolveEnumInEnv γ e
       io         <- envFindTy f (e_mapping e')
       case io of
         IntLit _ i -> return (t, tInt `strengthen` exprReft i, t_immutable)
         --
         -- XXX : is 32-bit going to be enough ???
         --
         -- XXX: Invalid BV values will be dropped
         --
         HexLit _ s -> bitVectorValue s >>= return . (t,,t_immutable) . (tBV32 `strengthen`)
         _          -> Nothing

getProp _ _ _ _ _ = Nothing


-------------------------------------------------------------------------------
-- getPropApp :: (PPRD r, EnvLike r g, F.Symbolic f, PP f) 
--            => g r 
--            -> AccessKind
--            -> f 
--            -> RType r 
--            -> Maybe (RType r, RType r, Mutability)
-------------------------------------------------------------------------------
getPropApp γ b x f t@(TApp c ts _) = 
  case c of 
    TBool    -> Nothing
    TUndef   -> Nothing
    TNull    -> Nothing
    TUn      -> getPropUnion γ b x f ts
    TInt     -> do  (t',m) <- lookupAmbientType γ b x f "Number"
                    return  $ (t,t',m)
    TString  -> do  (t',m) <- lookupAmbientType γ b x f "String"
                    return  $ (t,t',m)
    TFPBool  -> Nothing
    TTop     -> Nothing
    TVoid    -> Nothing
getPropApp _ _ _ _ _ = error "getPropApp should only be applied to TApp"


-------------------------------------------------------------------------------
extractCtor :: (PPRD r, EnvLike r g) => g r -> RType r -> Maybe (RType r)
-------------------------------------------------------------------------------
extractCtor γ (TClass x) 
  = do  d        <- resolveTypeInEnv γ x
        (vs, es) <- expand'' InstanceMember γ d
        case M.lookup (ctorSymbol, InstanceMember) es of
          Just (ConsSig t) -> fixRet x vs t
          _                -> return $ defCtor x vs
  where

extractCtor γ (TRef x ts _) 
  = do  d        <- resolveTypeInEnv γ x
        (vs, es) <- expand'' InstanceMember γ d
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

-------------------------------------------------------------------------------
accessMember :: (PPRD r, EnvLike r g, F.Symbolic f, PP f)
             => g r
             -> AccessKind 
             -> StaticKind 
             -> Maybe x
             -> f
             -> TypeMembers r 
             -> Maybe (RType r, Mutability)
-------------------------------------------------------------------------------
-- 
-- Only consider methods with `MethodAccess`
--
accessMember γ b@MethodAccess sk x f es 
  | Just (MethSig _ t)              <- M.lookup (F.symbol f, sk) es
  = Just (t,t_immutable)
  | Just (IndexSig _ StringIndex t) <- M.lookup (stringIndexSymbol, sk) es
  , validFieldName f 
  = Just (t, t_mutable) 
  | otherwise 
  = accessMemberProto γ b sk x f es
-- 
-- DO NOT consider methods with `FieldAccess`
--
accessMember γ b@FieldAccess sk x f es
  | Just f@(FieldSig _ _ m t) <- M.lookup (F.symbol f, sk) es
  = if optionalField f then Just (orUndef t,m) else Just (t,m)  
  | Just (IndexSig _ StringIndex t) <- M.lookup (stringIndexSymbol, sk) es 
  , validFieldName f 
  = Just (t, t_mutable) 
  | otherwise 
  = accessMemberProto γ b sk x f es

accessMemberProto γ b sk x f es
  | Just (FieldSig _ _ _ pt) <- M.lookup (F.symbol "__proto__",sk) es
  , Just (_,t,m) <- getProp γ b x f pt
  = return (t,m)
  | otherwise 
  = Nothing

-------------------------------------------------------------------------------
validFieldName  :: F.Symbolic f => f -> Bool
-------------------------------------------------------------------------------
validFieldName f = not $ F.symbol f `elem` excludedFieldSymbols

-------------------------------------------------------------------------------
-- lookupAmbientType :: (PPRD r, EnvLike r g, F.Symbolic f, F.Symbolic s, PP f) 
--                   => g r -> AccessKind -> f -> s -> Maybe (RType r, Mutability)
-------------------------------------------------------------------------------
lookupAmbientType γ b x f amb
  = t_elts <$> resolveTypeInEnv γ nm >>= accessMember γ b InstanceMember x f
  where
    nm = mkAbsName [] (F.symbol amb)

-- Accessing the @x@ field of the union type with @ts@ as its parts, returns
-- "Nothing" if accessing all parts return error, or "Just (ts, tfs)" if
-- accessing @ts@ returns type @tfs@. @ts@ is useful for adding casts later on.
-------------------------------------------------------------------------------
-- getPropUnion :: (PPRD r, EnvLike r g, F.Symbolic f, PP f) 
--              => g r 
--              -> AccessKind 
--              -> f 
--              -> [RType r] 
--              -> Maybe (RType r, RType r, Mutability)
-------------------------------------------------------------------------------
getPropUnion γ b x f ts = 
  case unzip3 [ttm | Just ttm <- getProp γ b x f <$> ts] of
    ([],[] ,[])                           -> Nothing
    (ts,ts',ms)  | all isImmutable     ms -> Just (mkUnion ts, mkUnion ts', t_immutable)
                 | all isMutable       ms -> Just (mkUnion ts, mkUnion ts', t_mutable)
                 | otherwise              -> Just (mkUnion ts, mkUnion ts', t_readOnly)

