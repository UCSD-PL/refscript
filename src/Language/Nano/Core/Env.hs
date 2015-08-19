-- | Type Environments and Related Operations

{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Nano.Core.Env (
  -- * Env definitions
    Env, QEnv

  -- * Env API
  , envFromList, envFromList', envFromListWithKey
  , envToList
  , envAdd, envAdds
  , envDel, envDels
  , envFindTy
  , envAddReturn
  , envFindReturn
  , envFilter
  , envFindLoc
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
  , envKeys
  , envAddWith

  , qenvToList
  , qenvFromList
  , qenvEmpty
  , qenvFindTy
  , qenvKeys
  , qenvMap
  ) where

import           Control.Applicative
import           Data.Data
import qualified Data.HashMap.Strict      as M
import qualified Data.List                as L
import           Data.Maybe               (isJust)
import           Data.Monoid              (Monoid (..))
import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Types  as F
import           Language.Nano.AST.Syntax
import           Language.Nano.Locations
import           Language.Nano.Names


--------------------------------------------------------------------------
-- | Environment Definitions
--------------------------------------------------------------------------

type Env t = F.SEnv (Located t)

newtype QEnv t = QE (M.HashMap AbsPath (Located t)) deriving (Eq, Functor, Data, Typeable)


--------------------------------------------------------------------------------
-- | Env API
--------------------------------------------------------------------------------

envIds             = map fst . envToList
envEmpty           = F.emptySEnv
envMap    f        = fmap (fmap f)
envFilter f        = F.filterSEnv (f . val)
envMem i γ         = isJust $ envFind i γ
envFind    i γ     = F.lookupSEnv (F.symbol i) γ
envFindLoc i γ     = loc <$> envFind i γ
envFindTy  i γ     = val <$> envFind i γ
envAdd   i t γ     = F.insertSEnv (F.symbol i) (Loc (srcPos i) t) γ
envAdds  xts γ     = L.foldl' (\γ (x,t) -> envAdd x t γ) γ xts
envDel   i   γ     = F.deleteSEnv (F.symbol i) γ
envDels  is  γ     = L.foldl' (\γ x -> envDel x γ) γ is
envToList  γ       = [ (Id l (F.symbolString x), t) | (x, Loc l t) <- F.toListSEnv γ]
envAddReturn f     = envAdd (returnId (srcPos f))
envFindReturn      = maybe msg val . F.lookupSEnv returnSymbol
  where
    msg = errorstar "bad call to envFindReturn"

envAddWith f i t γ = case envFind i γ of
                       Just t' -> envAdd i (t `f` t') γ
                       Nothing -> envAdd i t γ

envSEnv           :: Env a -> F.SEnv a
envSEnv            = F.fromListSEnv . map (mapFst F.symbol) . envToList

envFromList       :: (IsLocated x, F.Symbolic x) =>  [(x, t)] -> Env t
envFromList        = L.foldl' step envEmpty
  where
    step γ (i, t)  = case envFindLoc i γ of
                       Nothing -> envAdd i t γ
                       Just l' | l' /= srcPos i -> error "Duplicate specification" -- TODO: remove this error
                               -- | l' /= srcPos i -> throw $ errorDuplicate i (srcPos i) l'
                               | otherwise      -> γ

-- Allows duplicates in list -- last one in list will be used
envFromList'      :: (IsLocated x, F.Symbolic x) =>  [(x, t)] -> Env t
envFromList'       = L.foldl' step envEmpty
  where
    step γ (i, t)  = envAdd i t γ

envFromListWithKey :: (IsLocated x, F.Symbolic x) => (x -> a -> a -> a) -> [(x, a)] -> Env a
envFromListWithKey merge = L.foldl' step envEmpty
  where
    step γ (i, t)  = case envFindTy i γ of
                       Nothing -> envAdd i t γ
                       Just t' -> envAdd i (merge i t t') γ

envIntersectWith  :: (a -> b -> c) -> Env a -> Env b -> Env c
envIntersectWith f = F.intersectWithSEnv (\v1 v2 -> Loc (loc v1) (f (val v1) (val v2)))

-- | Favors environment on the left
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

envKeys γ          = fst <$> envToList γ

isRight (Right _)  = True
isRight (_)        = False
isLeft             = not . isRight


--------------------------------------------------------------------------------
-- | QEnv API
--------------------------------------------------------------------------------

qenvToList :: QEnv t -> [(AbsPath, t)]
qenvToList  (QE γ)  = [ (x, t) | (x, Loc _ t) <- M.toList γ]

qenvFromList       :: Monoid t => [(AbsPath, t)] -> QEnv t
qenvFromList        = L.foldl' step qenvEmpty
  where
    step γ (q, t)   = case qenvFindTy q γ of
                        Nothing -> qenvAdd q t γ
                        Just t' -> qenvAdd q (t `mappend` t') γ

qenvEmpty           = QE M.empty
qenvFind q (QE γ)   = M.lookup q γ

qenvFindTy  i γ     = fmap val $ qenvFind i γ

qenvAdd :: AbsPath -> t -> QEnv t -> QEnv t
qenvAdd q t (QE γ)  = QE (M.insert q (Loc (srcPos q) t) γ)

qenvMap f (QE γ)    = QE $ M.map (fmap f) γ

qenvKeys (QE γ)     = M.keys γ

