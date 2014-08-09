{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Nano.Typecheck.Lookup (getProp, getElt, getCallable, getPropTDef) where 

import           Data.Maybe (listToMaybe)
import           Language.ECMAScript3.PrettyPrint
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc
import           Language.Nano.Types
import           Language.Nano.Errors 
import           Language.Nano.Env
import           Language.Nano.Typecheck.Types
import qualified Language.Nano.Typecheck.Subst as S
import           Control.Applicative ((<$>))

-- import           Debug.Trace

type PPR r = (PP r, F.Reftable r)

type TDR r = IfaceEnv r
type TER r = Env (RType r)

-- Naming convention for the following:
--
--  α: the variable declaration environment
--
--  δ: the type definition environment
--

-- Given an environment @γ@, a (string) field @s@ and a type @t@, `getProp` 
-- returns a tuple with elements:
-- * The subtype of @t@ for which the access is successful.
-- * The corresponding accessed type.
-------------------------------------------------------------------------------
getProp ::  (IsLocated l, PPR r) => 
  l -> TER r -> TDR r -> F.Symbol -> RType r -> Maybe (RType r, RType r)
-------------------------------------------------------------------------------
getProp l α δ s t@(TApp _ _ _  ) = getPropApp l α δ s t 
getProp _ _ _ _   (TFun _ _ _  ) = Nothing
getProp _ _ _ s a@(TCons es _ _) = (a,) <$> lookupElt s es
getProp _ _ δ s t@(TClass c    ) = do d <- findSym c δ 
                                      p <- lookupElt s $ S.flatten True δ (d,[])
                                      return (t,p)
getProp l _ _ _ t                = die $ bug (srcPos l) 
                                      $ "Using getProp on type: " ++ ppshow t 


-- | `getElt`: return elements associated with a symbol @s@. The return list 
-- is empty if the binding was not found or @t@ is an invalid type.
-------------------------------------------------------------------------------
getElt :: (F.Symbolic s, PPR r) => TDR r -> s -> RType r -> [TypeMember r]
-------------------------------------------------------------------------------
getElt δ s t                = fromCons $ S.flattenType δ t
  where   
    fromCons (TCons es _ _) = [ e | e <- es, F.symbol e == F.symbol s ]
    fromCons _              = []


-------------------------------------------------------------------------------
getCallable :: PPR r => IfaceEnv r -> RType r -> [RType r]
-------------------------------------------------------------------------------
getCallable δ t             = uncurry mkAll <$> foo [] t
  where
    foo αs t@(TFun _ _ _)   = [(αs, t)]
    foo αs   (TAnd ts)      = concatMap (foo αs) ts 
    foo αs   (TAll α t)     = foo (αs ++ [α]) t
    foo αs   (TApp (TRef s) _ _ )
                            = case findSym s δ of 
                                Just d  -> [ (αs, t) | CallSig t <- t_elts d ]
                                Nothing -> []
    foo αs   (TCons es _ _) = [ (αs, t) | CallSig t <- es  ]
    foo _    t              = error $ "getCallable: " ++ ppshow t


-------------------------------------------------------------------------------
getPropApp :: (PPR r, IsLocated a) 
           => a -> TER r -> TDR r -> F.Symbol -> RType r -> Maybe (RType r, RType r)
-------------------------------------------------------------------------------
getPropApp l α γ s t@(TApp c ts _) = 
  case c of 
    TBool    -> Nothing
    TUndef   -> Nothing
    TNull    -> Nothing
    TUn      -> getPropUnion l α γ s ts
    TInt     -> lookupAmbientVar l α γ s "Number" t
    TString  -> lookupAmbientVar l α γ s "String" t
    TRef x   -> do  d      <- findSym x γ 
                    p      <- lookupElt s $ S.flatten False γ (d,ts)
                    return  $ (t,p)
    TFPBool  -> Nothing
    TTop     -> Nothing
    TVoid    -> Nothing

getPropApp _ _ _ _ _ = error "getPropApp should only be applied to TApp"



lookupElt s es = 
    case lookupField of
      Just t  -> Just t
      Nothing -> lookupIndex 
  where
    -- FIXME: for the moment only supporting string index signature
    lookupField = listToMaybe [ eltType e | e <- es, nonStaticSig e, F.symbol e == s ]
    lookupIndex = listToMaybe [ t         | IndexSig _ True t <- es]

-- getPropCons _ _ _ = error "BUG: Cannot call getPropCons on non TCons"

-- Access the property from the relevant ambient object but return the 
-- original accessed type instead of the type of the ambient object. 
-------------------------------------------------------------------------------
lookupAmbientVar :: (PPR r, IsLocated l) => 
  l -> TER r -> TDR r -> F.Symbol -> String -> RType r -> Maybe (RType r, RType r)
-------------------------------------------------------------------------------
lookupAmbientVar l α γ s amb t = 
  envFindTy amb α >>= getProp l α γ s >>= return . (t,) . snd


-- FIXME: Probably should get rid of this and just use getField, etc...
-------------------------------------------------------------------------------
getPropTDef :: PPR r => t -> TDR r -> F.Symbol -> [RType r] -> InterfaceDefinition r -> Maybe (RType r)
-------------------------------------------------------------------------------
getPropTDef _ γ f ts d = lookupElt f $ S.flatten False γ (d,ts)


-- Accessing the @x@ field of the union type with @ts@ as its parts, returns
-- "Nothing" if accessing all parts return error, or "Just (ts, tfs)" if
-- accessing @ts@ returns type @tfs@. @ts@ is useful for adding casts later on.
-------------------------------------------------------------------------------
getPropUnion :: (IsLocated l, PPR r) => 
  l -> TER r -> TDR r -> F.Symbol -> [RType r] -> Maybe (RType r, RType r)
-------------------------------------------------------------------------------
getPropUnion l α γ f ts = 
  case [tts | Just tts <- getProp l α γ f <$> ts] of
    [] -> Nothing
    ts -> Just $ mapPair mkUnion $ unzip ts

