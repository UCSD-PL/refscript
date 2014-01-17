{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Nano.Typecheck.Lookup (getProp, getIdx) where 

import           Text.PrettyPrint.HughesPJ
import           Language.ECMAScript3.PrettyPrint
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Parse as P
import           Language.Nano.Types
import           Language.Nano.Errors 
import           Language.Nano.Env
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Unfold

import           Control.Exception   (throw)
import           Control.Applicative ((<$>))
import qualified Data.HashSet as S
import           Data.List                      (find)
import qualified Data.HashMap.Strict as M 
import           Data.Monoid
import           Text.Parsec

import           Text.Printf 
import           Debug.Trace
-- import           Language.Nano.Misc (mkEither)

-- Given an environment @γ@, a (string) field @s@ and a type @t@, `getProp` 
-- returns a tuple with elements:
-- ∙ The subtype of @t@ for which the access does not throw an error.
-- ∙ The type the corresponds to the access of exactly that type that does not
--   throw an error.
-------------------------------------------------------------------------------
getProp ::  (IsLocated l, Ord r, PP r, F.Reftable r) => 
  l -> Env (RType r) -> Env (RType r) -> String -> RType r -> Maybe (RType r, RType r)
-------------------------------------------------------------------------------
getProp l specs defs s t@(TObj bs _) = 
  do  case find (match $ F.symbol s) bs of
        Just b -> Just (t, b_type b)
        _      -> case find (match $ F.stringSymbol "*") bs of
                    Just b' -> Just (t, b_type b')
                    _       -> lookupProto l specs defs s t
  where match s (B f _)  = s == f

getProp l specs defs s t@(TApp _ _ _)  = getPropApp l specs defs s t 
getProp _ _     _    _ t@(TFun _ _ _ ) = Just (t, tUndef)
getProp l specs defs s a@(TArr _ _)    = getPropArr l specs defs s a
getProp l _     _    _ t               = die $ bug (srcPos l) $ "getProp: " ++ (ppshow t) 


-------------------------------------------------------------------------------
lookupProto :: (Ord r, PP r, F.Reftable r, IsLocated a) =>
  a -> Env (RType r) -> Env (RType r) -> String -> RType r -> Maybe (RType r, RType r)
-------------------------------------------------------------------------------
lookupProto l specs defs s t@(TObj bs _) = 
    case find (match $ F.stringSymbol "__proto__") bs of
      Just (B _ t) -> getProp l specs defs s t
      Nothing -> Nothing -- Just (t, tUndef)
  where match s (B f _)  = s == f
lookupProto l _ _ _ _ = die $ bug (srcPos l) 
  "lookupProto can only unfold the prototype chain for object types"

-- Access the property from the relevant ambient object but return the 
-- original accessed type instead of the type of the ambient object. 
-------------------------------------------------------------------------------
lookupAmbient :: (Ord r, F.Reftable r, F.Symbolic a, PP r, IsLocated l) =>
  l -> Env (RType r) -> Env (RType r) -> String -> a -> RType r -> Maybe (RType r, RType r)
-------------------------------------------------------------------------------
lookupAmbient l specs defs s amb t = 
      envFindTy amb specs 
  >>= getProp l specs defs s 
  >>= return . mapFst (const t)

getPropApp l specs defs s t@(TApp c ts _) 
  = case c of 
      TUn      -> getPropUnion l specs defs s ts
      TInt     -> Nothing -- Just (t, tUndef)
      TBool    -> Nothing -- Just (t, tUndef)
      TString  -> lookupAmbient l specs defs s "String" t
      TUndef   -> Nothing
      TNull    -> Nothing
      (TDef _) -> getProp l specs defs s $ unfoldSafe defs t
      TTop     -> die $ bug (srcPos l) "getProp top"
      TVoid    -> die $ bug (srcPos l) "getProp void"

getPropArr l specs defs s a@(TArr _ _) 
  = case s of
    -- TODO: make more specific, add refinements 
    "length" -> Just (a, tInt) 
    _        -> case stringToInt s of
                  -- Implicit coersion of numieric strings:
                  -- x["0"] = x[0], x["1"] = x[1], etc.
                  Just i  -> getIdx l specs defs i a 
                  -- The rest of the cases are undefined
                  Nothing -> Just (a, tUndef) 

-------------------------------------------------------------------------------
stringToInt :: String -> Maybe Int
-------------------------------------------------------------------------------
stringToInt s = 
  case runParser P.integer 0 "" s of
    Right i -> Just $ fromInteger i
    Left _  -> Nothing


-- Accessing the @x@ field of the union type with @ts@ as its parts, returns
-- "Nothing" if accessing all parts return error, or "Just (ts, tfs)" if
-- accessing @ts@ returns type @tfs@. @ts@ is useful for adding casts later on.
-------------------------------------------------------------------------------
getPropUnion :: (IsLocated l, Ord r, PP r, F.Reftable r) 
             => l -> Env (RType r) -> Env (RType r) -> String -> [RType r] -> Maybe (RType r, RType r)
-------------------------------------------------------------------------------
getPropUnion l specs defs f ts = 
  -- Gather all the types that do not throw errors, and the type of 
  -- the accessed expression that yields them
  case [tts | Just tts <- getProp l specs defs f <$> ts] of
    [] -> Nothing
    ts -> Just $ mapPair mkUnion $ unzip ts


-------------------------------------------------------------------------------
getIdx ::  (IsLocated l, Ord r, PP r, F.Reftable r) => 
  l -> Env (RType r) -> Env (RType r) -> Int -> RType r -> Maybe (RType r, RType r)
-------------------------------------------------------------------------------
getIdx _ _ _ _ a@(TArr t _)  = Just (a,t)
getIdx l specs defs i t             = getProp l specs defs (show i) t 
--error $ "Unimplemented: getIdx on" ++ (ppshow t) 


