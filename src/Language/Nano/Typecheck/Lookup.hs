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
  , extractParent
  ) where 

import           Data.Generics
import qualified Data.Map.Strict as M

import           Language.ECMAScript3.PrettyPrint

import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Misc

import           Language.Nano.Names
import           Language.Nano.Types
import           Language.Nano.Errors
import           Language.Nano.Env
import           Language.Nano.Environment
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Resolve
import           Language.Nano.Typecheck.Subst
import           Control.Applicative ((<$>))

-- import           Debug.Trace

type PPRD r = (ExprReftable Int r, PP r, F.Reftable r, Data r)


-- | `getProp γ b s t`: returns  a pair containing:
--
--   * The subtype of @t@ for which the access of field @s@ is successful.
--
--   * The corresponding accessed type.
--
--  If @b@ is True then the access is for a call.
--
--  FIXME: Fix visibility
--
-------------------------------------------------------------------------------
getProp :: (PPRD r, EnvLike r g, F.Symbolic f) 
        => g r -> Bool -> f -> RType r -> Maybe (RType r, RType r, Mutability)
-------------------------------------------------------------------------------
getProp γ b s t@(TApp _ _ _  ) =                  getPropApp γ b s t

getProp _ b s t@(TCons _ es _) = do (t',m)     <- accessMember b InstanceMember s es
                                    return      $ (t,t',m)
    
getProp γ b s t@(TRef x ts _)  = do d          <- resolveTypeInEnv γ x
                                    es         <- flatten Nothing InstanceMember γ (d,ts)
                                    (t',m)     <- accessMember b InstanceMember s es
                                    return      $ (t,t',m)

getProp γ b s t@(TClass c    ) = do d          <- resolveTypeInEnv γ c
                                    es         <- flatten Nothing StaticMember γ (d,[])
                                    (t', m)    <- accessMember b StaticMember s es
                                    return      $ (t,t',m)

getProp γ _ s t@(TModule m   ) = do m'         <- resolveModuleInEnv γ m
                                    (_,_,t',_) <- envFindTy s $ m_variables m'
                                    return      $ (t,t', t_readOnly) 
                                    -- FIXME: perhaps something else here as mutability

getProp γ _ s t@(TEnum e     ) = do e'         <- resolveEnumInEnv γ e
                                    i          <- envFindTy (F.symbol s) (e_symbols e')
                                    return      $ (t, tInt `strengthen` exprReft i, t_immutable)

getProp _ _ _ _ = Nothing


-------------------------------------------------------------------------------
getPropApp :: (PPRD r, EnvLike r g, F.Symbolic f) 
           => g r 
           -> Bool 
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
extractParent :: (PPR r, PP r, EnvLike r g, Substitutable r (RType r)) 
              => g r -> RType r -> Maybe (RType r)
-------------------------------------------------------------------------------
extractParent γ (TRef x ts _) 
  = do  ID _ k vs (es,_) _ <- resolveTypeInEnv γ x
        case (k,es) of 
          (ClassKind, [(p,ps)]) -> 
              Just $ TRef p (apply (fromList $ zip vs ts) ps) fTop
          _  -> Nothing
extractParent _ _ = Nothing


-------------------------------------------------------------------------------
extractCall :: (EnvLike r g, PPRD r) => g r -> RType r -> [RType r]
-------------------------------------------------------------------------------
extractCall γ t             = uncurry mkAll <$> foo [] t
  where
    foo αs t@(TFun _ _ _ _) = [(αs, t)]
    foo αs   (TAnd ts)      = concatMap (foo αs) ts 
    foo αs   (TAll α t)     = foo (αs ++ [α]) t
    foo αs   (TRef s _ _  ) = case resolveTypeInEnv γ s of 
                                Just d  -> getCallSig αs $ t_elts d
                                Nothing -> []
    foo αs   (TCons _ es _) = getCallSig αs es
    foo _  _                = []

    getCallSig αs es        = case M.lookup (callSymbol, InstanceMember) es of
                                Just (CallSig t) -> [(αs, t)]
                                _                -> []


-- | `accessMember b s es` extracts field @s@ from type members @es@. If @b@ is
--   True then this means that the extracted field is for a call, in which case
--   we allow extraction of methods, otherwise extracting methods is disallowed.
--
--   Invariant: each field appears at most once.
--
--   FIXME: Mutability: enforcing the field's mutability for now -- use a
--   combinator ...
--
--
-------------------------------------------------------------------------------
accessMember :: F.Symbolic f 
             => Bool 
             -> StaticKind 
             -> f 
             -> TypeMembers r 
             -> Maybe (RType r, Mutability)
-------------------------------------------------------------------------------
-- Get member for a call
accessMember True sk s es =    
  case M.lookup (F.symbol s, sk) es of
    Just (FieldSig _ m t) -> Just (t,m)
    Just (MethSig _ m t)  -> Just (t,m)
    _ -> case M.lookup (stringIndexSymbol, sk) es of 
           Just (IndexSig _ StringIndex t) -> Just (t, t_mutable)
           _ -> Nothing

-- Extract member: cannot extract methods
accessMember False sk s es =
  case M.lookup (F.symbol s, sk) es of
    Just (FieldSig _ m t) -> Just (t,m)
    _ -> case M.lookup (stringIndexSymbol, sk) es of 
           Just (IndexSig _ StringIndex t) -> Just (t, t_mutable)
           _ -> Nothing

-------------------------------------------------------------------------------
lookupAmbientType :: (PPRD r, EnvLike r g, F.Symbolic f, F.Symbolic s) 
                  => g r -> Bool -> f -> s -> Maybe (RType r, Mutability)
-------------------------------------------------------------------------------
lookupAmbientType γ b fld amb
  = t_elts <$> resolveTypeInEnv γ nm >>= accessMember b InstanceMember fld
  where
    nm = mkAbsName [] (F.symbol amb)

-- Accessing the @x@ field of the union type with @ts@ as its parts, returns
-- "Nothing" if accessing all parts return error, or "Just (ts, tfs)" if
-- accessing @ts@ returns type @tfs@. @ts@ is useful for adding casts later on.
-------------------------------------------------------------------------------
getPropUnion :: (PPRD r, EnvLike r g, F.Symbolic f) 
             => g r -> Bool -> f -> [RType r] -> Maybe (RType r, RType r, Mutability)
-------------------------------------------------------------------------------
getPropUnion γ b f ts = 
  case unzip3 [ttm | Just ttm <- getProp γ b f <$> ts] of
    ([],[] ,[])                           -> Nothing
    (ts,ts',ms)  | all isImmutable     ms -> Just (mkUnion ts, mkUnion ts', t_immutable)
                 | all isMutable       ms -> Just (mkUnion ts, mkUnion ts', t_mutable)
                 | all isAssignsFields ms -> Just (mkUnion ts, mkUnion ts', t_assignsFields)
                 | otherwise              -> Just (mkUnion ts, mkUnion ts', t_readOnly)

