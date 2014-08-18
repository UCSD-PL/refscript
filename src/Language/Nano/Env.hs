-- | Type Environments and Related Operations

{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}

module Language.Nano.Env (
  -- * Env definitions
    Env, QEnv
 
  -- * Env API
  , envFromList 
  , envToList
  , envAdd, envAdds 
  , envDel, envDels
  , envFindTy
  , envAddReturn
  , envFindReturn
  , envFilter
  , envMem
  , envMap
  , envLefts
  , envRights
  , envUnion
  , envUnionList
  , envDiff
  , envIntersectWith
  , envEmpty
  , envSEnv
  , envIds

  , qenvFromList
  , qenvToEnv
  , qenvEmpty
  , qenvFindTy
  ) where 

import           Control.Applicative 
import           Control.Exception (throw)
import           Data.Maybe             (isJust)
import           Data.Hashable          
import           Data.Monoid            (Monoid (..))
import qualified Data.HashMap.Strict as M
import           Data.Data
import qualified Data.List               as L

import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.PrettyPrint
import           Language.Nano.Locations
import           Language.Nano.Errors
import           Language.Nano.Names
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Misc
import           Language.Fixpoint.PrettyPrint
import           Text.PrettyPrint.HughesPJ


--------------------------------------------------------------------------
-- | Environment Definitions
--------------------------------------------------------------------------

type Env t = F.SEnv (Located t) 

newtype QEnv t = QE (M.HashMap AbsPath (Located t)) deriving (Eq, Data, Typeable, Functor)


--------------------------------------------------------------------------------
-- | Env API  
--------------------------------------------------------------------------------

envIds          = map fst . envToList
envEmpty        = F.emptySEnv
envMap    f     = F.mapSEnv (fmap f) 
envFilter f     = F.filterSEnv (f . val) 
envMem i γ      = isJust $ envFind i γ
envFind    i γ  = F.lookupSEnv (F.symbol i) γ
envFindLoc i γ  = fmap loc $ envFind i γ 
envFindTy  i γ  = fmap val $ envFind i γ
envAdd   i t γ  = F.insertSEnv (F.symbol i) (Loc (srcPos i) t) γ
envAdds  xts γ  = L.foldl' (\γ (x,t) -> envAdd x t γ) γ xts
envDel   i   γ  = F.deleteSEnv (F.symbol i) γ
envDels  is  γ  = L.foldl' (\γ x -> envDel x γ) γ is
envToList  γ    = [ (Id l (F.symbolString x), t) | (x, Loc l t) <- F.toListSEnv γ]
envAddReturn f  = envAdd (returnId (srcPos f))
envFindReturn   = maybe msg val . F.lookupSEnv returnSymbol  
  where 
    msg = errorstar "bad call to envFindReturn"

envSEnv           :: Env a -> F.SEnv a
envSEnv            = F.fromListSEnv . map (mapFst F.symbol) . envToList

envFromList       :: (PP x, IsLocated x, F.Symbolic x) =>  [(x, t)] -> Env t
envFromList        = L.foldl' step envEmpty
  where 
    step γ (i, t)  = case envFindLoc i γ of
                       Nothing -> envAdd i t γ 
                       Just l' -> throw $ errorDuplicate i (srcPos i) l'

envIntersectWith  :: (a -> b -> c) -> Env a -> Env b -> Env c
envIntersectWith f = F.intersectWithSEnv (\v1 v2 -> Loc (loc v1) (f (val v1) (val v2)))

-- | Favors the bindings in the second environment
envUnion          :: Env a -> Env a -> Env a
envUnion           = envAdds . envToList

envUnionList       = go envEmpty 
  where
    go acc (y:ys)  = go (envUnion acc y) ys
    go acc []      = acc

envRights         :: Env (Either a b) -> Env b
envRights          = envMap (\(Right z) -> z) . envFilter isRight

envLefts          :: Env (Either a b) -> Env a
envLefts           = envMap (\(Left z) -> z) . envFilter isLeft

envDiff           :: Env a -> Env b -> Env a
envDiff m1 m2      = envFromList [(x, t) | (x, t) <- envToList m1, not (x `envMem` m2)] 

isRight (Right _)  = True
isRight (_)        = False
isLeft             = not . isRight


-- --------------------------------------------------------------------------------
-- -- | QEnv API  
-- --------------------------------------------------------------------------------
-- 
-- qenvFromList       :: [(QName, t)] -> QEnv t
-- qenvFromList        = L.foldl' step qenvEmpty
--   where 
--     step γ (q, t)   = case qenvFindLoc q γ of
--                         Nothing -> qenvAdd q t γ 
--                         Just l' -> throw $ errorDuplicate q (srcPos q) l'
-- 
-- qenvEmpty           = QE M.empty
-- qenvFindLoc i γ     = fmap loc $ qenvFind i γ 
-- qenvFind q (QE γ)   = M.lookup q γ 
-- 
-- qenvAdd :: QName -> t -> QEnv t -> QEnv t
-- qenvAdd q t (QE γ)  = QE (M.insert q (Loc (srcPos q) t) γ)
-- 
-- -- Create dot separated symbols as keys
-- qenvToEnv (QE γ)    = envFromList [ (Id l (ppshow x),t) | (x, Loc l t) <- M.toList γ]


--------------------------------------------------------------------------------
-- | QEnv API  
--------------------------------------------------------------------------------

qenvFromList       :: [(AbsPath, t)] -> QEnv t
qenvFromList        = L.foldl' step qenvEmpty
  where 
    step γ (q, t)   = case qenvFindLoc q γ of
                        Nothing -> qenvAdd q t γ 
                        Just l' -> throw $ errorDuplicate q (srcPos q) l'

qenvEmpty           = QE M.empty
qenvFindLoc i γ     = fmap loc $ qenvFind i γ 
qenvFind q (QE γ)   = M.lookup q γ

qenvFindTy  i γ     = fmap val $ qenvFind i γ

qenvAdd :: AbsPath -> t -> QEnv t -> QEnv t
qenvAdd q t (QE γ)  = QE (M.insert q (Loc (srcPos q) t) γ)

-- Create dot separated symbols as keys
qenvToEnv (QE γ)    = envFromList [ (Id l (ppshow x),t) | (x, Loc l t) <- M.toList γ]



--------------------------------------------------------------------------------
-- | Monoid Instance
--------------------------------------------------------------------------------

instance Monoid (Env t) where
  mempty  = envEmpty
  mappend = envUnion 


--------------------------------------------------------------------------------
-- | Printing Instance 
--------------------------------------------------------------------------------

instance PP t => PP (Env t) where
  pp = vcat . (ppBind <$>) . F.toListSEnv . fmap val 

instance PP t => PP (QEnv t) where
  pp = pp . qenvToEnv

ppBind (x, t) = pprint x <+> dcolon <+> pp t

