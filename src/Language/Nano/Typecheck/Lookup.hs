{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Nano.Typecheck.Lookup (getProp, getIdx) where 

import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Syntax
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc
import           Language.Nano.Types
import           Language.Nano.Errors 
import           Language.Nano.Env
import           Language.Nano.Typecheck.Types
import qualified Language.Nano.Typecheck.Subst as SU

import           Control.Applicative ((<$>))
import           Data.List                      (find)

-- import           Debug.Trace

type PPR r = (PP r, F.Reftable r)


-- Given an environment @γ@, a (string) field @s@ and a type @t@, `getProp` 
-- returns a tuple with elements:
-- ∙ The subtype of @t@ for which the access does not throw an error.
-- ∙ The type the corresponds to the access of exactly that type that does not
--   throw an error.
-------------------------------------------------------------------------------
getProp ::  (IsLocated l, PPR r) => 
  l -> TDefEnv (RType r) -> F.Symbol -> RType r -> Maybe (RType r, RType r)
-------------------------------------------------------------------------------
{-getProp l γ s t@(TObj bs _) = -}
{-  do  case find (match $ F.symbol s) bs of-}
{-        Just b -> Just (t, b_type b)-}
{-        _      -> case find (match $ F.stringSymbol "*") bs of-}
{-                    Just b' -> Just (t, b_type b')-}
{-                    _       -> lookupProto l specs defs s t-}
{-  where match s (B f _)  = s == f-}

-- FIXME: apply type args !!!
getProp l γ f t@(TApp (TRef i) _ _) = 
  case find (match f) $ t_elts $ findTyIdOrDie i γ of 
    Just b -> Just (t, f_type b)
    _      -> lookupParent l γ f t
  where
    match f (TE s _ _) = s == f

getProp l γ s t@(TApp _ _ _)  = getPropApp l γ s t
getProp _ _ _   (TFun _ _ _ ) = Nothing     -- FIXME
getProp l γ s a@(TArr _ _)    = undefined -- getPropArr l γ s a
getProp l _ _ t               = die $ bug (srcPos l) $ "Using getProp on type: " ++ (show $ toType t) 


-- TODO
lookupParent = undefined

{---------------------------------------------------------------------------------}
{-lookupProto :: (Ord r, PPR r, IsLocated a, F.Symbolic s) =>-}
{-  a -> TDefEnv (RType r) -> s -> RType r -> Maybe (RType r, RType r)-}
{---------------------------------------------------------------------------------}
{-lookupProto l γ s (TObj bs _) = -}
{-    case find (match $ F.stringSymbol "__proto__") bs of-}
{-      Just (B _ t) -> getProp l specs defs s t-}
{-      Nothing -> Nothing -- Just (t, tUndef)-}
{-  where match s (B f _)  = s == f-}
{-lookupProto l _ _ _ _ = die $ bug (srcPos l) -}
{-  "lookupProto can only unfold the prototype chain for object types"-}

-- Access the property from the relevant ambient object but return the 
-- original accessed type instead of the type of the ambient object. 
-------------------------------------------------------------------------------
lookupAmbientVar :: (PPR r, IsLocated l) => 
  l -> TDefEnv (RType r) -> F.Symbol -> String -> RType r -> Maybe (RType r, RType r)
-------------------------------------------------------------------------------
lookupAmbientVar l γ s amb t = 
-- TODO: FIXME: oops need to keep specs around or ...
      envFindTy amb undefined
  >>= getProp l γ s 
  >>= return . mapFst (const t)


getPropApp l γ s t@(TApp c ts _) 
  = case c of 
      TUn      -> getPropUnion l γ s ts
      TInt     -> lookupAmbientVar l γ s "Number" t
      TBool    -> Nothing
      TString  -> lookupAmbientVar l γ  s "String" t
      TUndef   -> Nothing
      TNull    -> Nothing
      -- FIXME !!!
      -- (TDef _) -> getProp l specs defs s $ unfoldSafe defs t
      TTop     -> die $ bug (srcPos l) "getProp top"
      TVoid    -> die $ bug (srcPos l) "getProp void"
getPropApp _ _ _ _ = error "getPropArr should only be applied to TApp"

getPropArr l γ s a@(TArr t _) = 
        envFindTy "Array" γ 
   >>=  return undefined
-- TODO: FIXME: getProp γ ...
    {->>= \to -> (getProp l γ s (su t to)) -}
    {->>= \tr -> return ((mapFst (const a) tr))-}
  where
    su s (TD _ [v] tdef _) = SU.apply (SU.fromList [(v, s)]) tdef
    su _ _                 = die $ bug (srcPos l) "Array needs to be defined as a generic type in prelude.js"

getPropArr _ _ _ _ = error "getPropArr should only be applied to arrays"
 
  -- Array has been defined as a generic data type


-- Accessing the @x@ field of the union type with @ts@ as its parts, returns
-- "Nothing" if accessing all parts return error, or "Just (ts, tfs)" if
-- accessing @ts@ returns type @tfs@. @ts@ is useful for adding casts later on.
-------------------------------------------------------------------------------
getPropUnion :: (IsLocated l, PPR r) => 
  l -> TDefEnv (RType r) -> F.Symbol -> [RType r] -> Maybe (RType r, RType r)
-------------------------------------------------------------------------------
getPropUnion l γ f ts = 
  -- Gather all the types that do not throw errors, and the type of 
  -- the accessed expression that yields them
  case [tts | Just tts <- getProp l γ f <$> ts] of
    [] -> Nothing
    ts -> Just $ mapPair mkUnion $ unzip ts


-------------------------------------------------------------------------------
getIdx ::  (IsLocated l, PPR r) => 
  l -> TDefEnv (RType r) -> Int -> RType r -> Maybe (RType r, RType r)
-------------------------------------------------------------------------------
getIdx _ _ _ a@(TArr t _)  = Just (a,t)
-- FIXME
-- getIdx l γ i t             = getProp l γ (F.symbol i) t 

