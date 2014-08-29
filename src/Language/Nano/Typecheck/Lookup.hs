{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Nano.Typecheck.Lookup (
    getProp
  , getElt
  , extractCall
  , extractCtor
  , extractParent
  , getPropTDef
  ) where 

import           Data.Generics
import           Data.Maybe (listToMaybe)

import           Language.ECMAScript3.PrettyPrint

import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Misc

import           Language.Nano.Types
import           Language.Nano.Env
import           Language.Nano.Errors
import           Language.Nano.Environment
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Resolve
import           Language.Nano.Typecheck.Subst
import           Control.Applicative ((<$>))

import           Debug.Trace

type PPRD r = (PP r, F.Reftable r, Data r)


-- | `getProp`: given an environment `γ`, a field `s` and a type `t`, returns 
--   a pair containing:
--
--   * The subtype of @t@ for which the access is successful.
--
--   * The corresponding accessed type.
--
--  FIXME: Fix visibility
--
-------------------------------------------------------------------------------
getProp :: (PPRD r, EnvLike r g) => g r -> F.Symbol -> RType r -> Maybe (RType r, RType r)
-------------------------------------------------------------------------------
getProp γ s t@(TApp _ _ _  ) = getPropApp γ s t
getProp _ s t@(TCons es _ _) = (t,) <$> accessMember s es
getProp γ s t@(TClass c    ) 
  = do  d   <- resolveRelNameInEnv γ c
        es  <- flatten True γ (d,[])
        (t,) <$> accessMember s es
getProp γ s t@(TModule m   ) 
  = do  m'        <- resolveRelPathInEnv γ m
        (_,_,ty)  <- envFindTy s $ m_variables m'
        (t,)     <$> renameRelative (modules γ) (m_path m') (absPath γ) ty

getProp _ _ _ = Nothing


-------------------------------------------------------------------------------
getPropApp :: (PPRD r, EnvLike r g) => g r -> F.Symbol -> RType r -> Maybe (RType r, RType r)
-------------------------------------------------------------------------------
getPropApp γ s t@(TApp c ts _) = 
  case c of 
    TBool    -> Nothing
    TUndef   -> Nothing
    TNull    -> Nothing
    TUn      -> getPropUnion γ s ts
    TInt     -> (t,) <$> lookupAmbientVar γ s "Number"
    TString  -> (t,) <$> lookupAmbientVar γ s "String"
    TRef x   -> do  d      <- resolveRelNameInEnv γ x
                    es     <- flatten False γ (d,ts)
                    p      <- accessMember s es
                    return  $ (t,p)
    TFPBool  -> Nothing
    TTop     -> Nothing
    TVoid    -> Nothing
getPropApp _ _ _ = error "getPropApp should only be applied to TApp"


-------------------------------------------------------------------------------
extractCtor :: (PPRD r, EnvLike r g) => g r -> RType r -> Maybe (RType r)
-------------------------------------------------------------------------------
extractCtor γ (TClass x) 
  = do  d        <- resolveRelNameInEnv γ x
        (vs, es) <- flatten'' False γ d
        case [ mkAll vs (TFun bs (retT vs) r) | ConsSig (TFun bs _ r) <- es ] of
          [] -> return $ defCtor vs
          ts -> return $ mkAnd ts
    where
        retT vs    = TApp (TRef x) (tVar <$> vs) fTop
        defCtor vs = mkAll vs $ TFun [] (retT vs) fTop

extractCtor γ (TApp (TRef x) ts _) 
  = do  d        <- resolveRelNameInEnv γ x
        (vs, es) <- flatten'' False γ d
        case [ mkAll vs (TFun bs (retT vs) r) | ConsSig (TFun bs _ r) <- es ] of
          [] -> return $ apply (fromList $ zip vs ts) $ defCtor vs
          ts -> return $ apply (fromList $ zip vs ts) $ mkAnd ts
    where
        retT vs    = TApp (TRef x) (tVar <$> vs) fTop
        defCtor vs = mkAll vs $ TFun [] (retT vs) fTop

extractCtor γ (TCons es _ _ )
  = do  case [ tf | ConsSig tf <- es ] of
          [] -> Nothing 
          ts -> return $ mkAnd ts
       
extractCtor _ _ = Nothing


-------------------------------------------------------------------------------
extractParent :: (Data r, F.Reftable r, PP r, EnvLike r g, Substitutable r (RType r)) 
              => g r -> RType r -> Maybe (RType r)
-------------------------------------------------------------------------------
extractParent γ (TApp (TRef x) ts _) 
  = do  d <- resolveRelNameInEnv γ x
        case t_proto d of
          Just (p,ps) -> Just $ TApp (TRef p) (tArgs d ts ps) fTop
          _           -> Nothing
  where
    tArgs d ts ps = apply (fromList $ zip (t_args d) ts) ps
parentDef _ _ = Nothing



-- | `getElt`: return elements associated with a symbol @s@. The return list 
-- is empty if the binding was not found or @t@ is an invalid type.
-------------------------------------------------------------------------------
getElt :: (F.Symbolic s, PPRD r, EnvLike r g) => g r -> s -> RType r -> [TypeMember r]
-------------------------------------------------------------------------------
-- getElt γ  s (TModule r) = 
--     case resolveRelPathInEnv r of
--       Just m  -> 
--       Nothing -> []

getElt γ  s t = case flattenType γ t of
                  Just t  -> fromCons t
                  Nothing -> []
  where   
    fromCons (TCons es _ _) = [ e | e <- es, F.symbol e == F.symbol s ]
    fromCons _              = []


-------------------------------------------------------------------------------
extractCall :: (EnvLike r g, PPRD r) => g r -> RType r -> [RType r]
-------------------------------------------------------------------------------
extractCall γ t             = uncurry mkAll <$> foo [] t
  where
    foo αs t@(TFun _ _ _)   = [(αs, t)]
    foo αs   (TAnd ts)      = concatMap (foo αs) ts 
    foo αs   (TAll α t)     = foo (αs ++ [α]) t
    foo αs   (TApp (TRef s) _ _ )
                            = case resolveRelNameInEnv γ s of 
                                Just d  -> [ (αs, t) | CallSig t <- t_elts d ]
                                Nothing -> []
    foo αs   (TCons es _ _) = [ (αs, t) | CallSig t <- es  ]
    foo _  _                = []


-------------------------------------------------------------------------------
accessMember :: F.Symbol -> [TypeMember r] -> Maybe (RType r)
-------------------------------------------------------------------------------
accessMember s es = 
  case [ t | FieldSig x _ t <- es, x == s ] of
    t:_ -> Just t
    _   -> listToMaybe [ t | IndexSig _ True t <- es]


-- Access the property from the relevant ambient object but return the 
-- original accessed type instead of the type of the ambient object. 
-- FIXME !!!
-------------------------------------------------------------------------------
lookupAmbientVar :: (PPRD r, EnvLike r g, F.Symbolic s) => g r -> F.Symbol -> s -> Maybe (RType r)
-------------------------------------------------------------------------------
lookupAmbientVar γ s amb
  = do  (t, _ )   <- envFindTy (F.symbol amb) (names γ)
        (_, t')   <- getProp γ s t
        return     $ t'


-- FIXME: Probably should get rid of this and just use getField, etc...
-------------------------------------------------------------------------------
getPropTDef :: (EnvLike r g, PPRD r) => g r -> F.Symbol -> [RType r] -> IfaceDef r -> Maybe (RType r)
-------------------------------------------------------------------------------
getPropTDef γ f ts d = accessMember f =<< flatten False γ (d,ts)


-- Accessing the @x@ field of the union type with @ts@ as its parts, returns
-- "Nothing" if accessing all parts return error, or "Just (ts, tfs)" if
-- accessing @ts@ returns type @tfs@. @ts@ is useful for adding casts later on.
-------------------------------------------------------------------------------
getPropUnion :: (PPRD r, EnvLike r g) => g r -> F.Symbol -> [RType r] -> Maybe (RType r, RType r)
-------------------------------------------------------------------------------
getPropUnion γ f ts = 
  case [tts | Just tts <- getProp γ f <$> ts] of
    [] -> Nothing
    ts -> Just $ mapPair mkUnion $ unzip ts

