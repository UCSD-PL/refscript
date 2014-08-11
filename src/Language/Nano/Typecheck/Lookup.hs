{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Nano.Typecheck.Lookup (getProp, getElt, getCallable, getPropTDef) where 

import           Data.Generics
import           Data.Maybe (listToMaybe)
import           Language.ECMAScript3.PrettyPrint
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc
import           Language.Nano.Types
import           Language.Nano.Errors 
import           Language.Nano.Env
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Resolve
import qualified Language.Nano.Typecheck.Subst as S
import           Control.Applicative ((<$>))

-- import           Debug.Trace

type PPR r = (PP r, F.Reftable r)


-- | `getProp`: given an environment `γ`, a field `s` and a type `t`, returns 
--   a pair containing:
--   * The subtype of @t@ for which the access is successful.
--   * The corresponding accessed type.
-------------------------------------------------------------------------------
getProp :: (IsLocated l, PPR r, EnvLike r g, Data r) 
        => l -> g r -> F.Symbol -> RType r -> Maybe (RType r, RType r)
-------------------------------------------------------------------------------
getProp l γ s t@(TApp _ _ _  ) = getPropApp l γ s t
getProp _ _ _   (TFun _ _ _  ) = Nothing
getProp _ _ s t@(TCons es _ _) = (t,) <$> lookupElt s es
getProp _ γ s t@(TClass c    ) = do d     <- resolveIface γ c
                                    es    <- flatten True γ (d,[])
                                    p     <- lookupElt s es
                                    return $ (t,p)
getProp l _ _ t                = die $ bug (srcPos l)
                                     $ "Using getProp on type: " ++ ppshow t 


-------------------------------------------------------------------------------
getPropApp :: (PPR r, IsLocated a, EnvLike r g, Data r) 
           => a -> g r -> F.Symbol -> RType r -> Maybe (RType r, RType r)
-------------------------------------------------------------------------------
getPropApp l γ s t@(TApp c ts _) = 
  case c of 
    TBool    -> Nothing
    TUndef   -> Nothing
    TNull    -> Nothing
    TUn      -> getPropUnion l γ s ts
    TInt     -> (t,) <$> lookupAmbientVar l γ s "Number"
    TString  -> (t,) <$> lookupAmbientVar l γ s "String"
    TRef x   -> do  d      <- resolveIface γ x
                    es     <- flatten False γ (d,ts)
                    p      <- lookupElt s es
                    return  $ (t,p)
    TFPBool  -> Nothing
    TTop     -> Nothing
    TVoid    -> Nothing

getPropApp _ _ _ _ = error "getPropApp should only be applied to TApp"


-- | `getElt`: return elements associated with a symbol @s@. The return list 
-- is empty if the binding was not found or @t@ is an invalid type.
-------------------------------------------------------------------------------
getElt :: (F.Symbolic s, PPR r, EnvLike r g, Data r) 
       => g r -> s -> RType r -> Maybe [TypeMember r]
-------------------------------------------------------------------------------
getElt γ  s t                = fromCons <$> flattenType γ t
  where   
    fromCons (TCons es _ _) = [ e | e <- es, F.symbol e == F.symbol s ]
    fromCons _              = []


-------------------------------------------------------------------------------
getCallable :: PPR r => IfaceEnv r -> RType r -> [RType r]
-------------------------------------------------------------------------------
getCallable γ t             = uncurry mkAll <$> foo [] t
  where
    foo αs t@(TFun _ _ _)   = [(αs, t)]
    foo αs   (TAnd ts)      = concatMap (foo αs) ts 
    foo αs   (TAll α t)     = foo (αs ++ [α]) t
-- FIXME !!!
--     foo αs   (TApp (TRef s) _ _ )
--                             = case resolveIface γ s of 
--                                 Just d  -> [ (αs, t) | CallSig t <- t_elts d ]
--                                 Nothing -> []
--     foo αs   (TCons es _ _) = [ (αs, t) | CallSig t <- es  ]
--     foo _    t              = error $ "getCallable: " ++ ppshow t


-- FIXME: CLEAN UP (CODE DUPLICATION)
-- FIXME: for the moment only supporting string index signature
-------------------------------------------------------------------------------
lookupElt :: F.Symbol -> [TypeMember r] -> Maybe (RType r)
-------------------------------------------------------------------------------
lookupElt s es = 
  case [ eltType e | e <- es, nonStaticSig e, F.symbol e == s ] of
    t:_ -> Just t
    _   -> listToMaybe [ t | IndexSig _ True t <- es]


-- Access the property from the relevant ambient object but return the 
-- original accessed type instead of the type of the ambient object. 
-------------------------------------------------------------------------------
lookupAmbientVar :: (PPR r, IsLocated l, EnvLike r g, F.Symbolic s, Data r) 
                 => l -> g r -> F.Symbol -> s -> Maybe (RType r)
-------------------------------------------------------------------------------
lookupAmbientVar l γ s amb
  = do  a <- envFindTy (F.symbol amb) (get_env γ)
        snd <$> getProp l γ s a


-- FIXME: Probably should get rid of this and just use getField, etc...
-------------------------------------------------------------------------------
getPropTDef :: (EnvLike r g, PPR r, Data r) 
            => g r -> F.Symbol -> [RType r] -> IfaceDef r -> Maybe (RType r)
-------------------------------------------------------------------------------
getPropTDef γ f ts d = lookupElt f =<< flatten False γ (d,ts)


-- Accessing the @x@ field of the union type with @ts@ as its parts, returns
-- "Nothing" if accessing all parts return error, or "Just (ts, tfs)" if
-- accessing @ts@ returns type @tfs@. @ts@ is useful for adding casts later on.
-------------------------------------------------------------------------------
getPropUnion :: (IsLocated l, PPR r, EnvLike r g, Data r) 
             => l -> g r -> F.Symbol -> [RType r] -> Maybe (RType r, RType r)
-------------------------------------------------------------------------------
getPropUnion l γ f ts = 
  case [tts | Just tts <- getProp l γ f <$> ts] of
    [] -> Nothing
    ts -> Just $ mapPair mkUnion $ unzip ts

