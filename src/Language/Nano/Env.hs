-- | Type Environments and Related Operations

{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}

module Language.Nano.Env (
    Env
  , Var 

  , envFromList 
  , envToList
  , envAdd, envAdds 
  , envDel, envDels
  , envFindTy
  , envAddReturn
  , envFindReturn
  , envMem
  , envMap
  , envLefts
  , envRights
  , envUnion
  , envDiff
  , envIntersectWith
  , envEmpty
  , envSEnv
  , envIds
  ) where 

import           Data.Maybe             (isJust)
import           Data.Hashable          ()
import           Data.Monoid            (Monoid (..))
import qualified Data.List               as L
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.PrettyPrint
import           Language.Nano.Types
import           Language.Nano.Errors
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Misc
import           Language.Fixpoint.PrettyPrint
import           Text.PrettyPrint.HughesPJ
import           Control.Applicative 
import           Control.Monad.Error ()
import           Control.Exception (throw)

--------------------------------------------------------------------------
-- | Environments
--------------------------------------------------------------------------

type Env t      = F.SEnv (Located t) 
type Var        = Id SourceSpan 

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

envSEnv         :: Env a -> F.SEnv a
envSEnv         = F.fromListSEnv . map (mapFst F.symbol) . envToList

envFromList       :: (PP x, IsLocated x, F.Symbolic x) =>  [(x, t)] -> Env t
envFromList       = L.foldl' step envEmpty
  where 
    step γ (i, t) = case envFindLoc i γ of
                      Nothing -> envAdd i t γ 
                      Just l' -> throw $ errorDuplicate i (srcPos i) l'

envIntersectWith :: (a -> b -> c) -> Env a -> Env b -> Env c
envIntersectWith f = F.intersectWithSEnv (\v1 v2 -> Loc (loc v1) (f (val v1) (val v2)))

-- | Favors the bindings in the second environment
envUnion :: Env a -> Env a -> Env a
envUnion = envAdds . envToList

envRights :: Env (Either a b) -> Env b
envRights = envMap (\(Right z) -> z) . envFilter isRight

envLefts :: Env (Either a b) -> Env a
envLefts = envMap (\(Left z) -> z) . envFilter isLeft

envDiff :: Env a -> Env b -> Env a
envDiff m1 m2 = envFromList [(x, t) | (x, t) <- envToList m1, not (x `envMem` m2)] 

isRight (Right _) = True
isRight (_)       = False
isLeft            = not . isRight

--------------------------------------------------------------------------------
-- Monoid Instance -------------------------------------------------------------
--------------------------------------------------------------------------------

instance Monoid (Env t) where
  mempty  = envEmpty
  mappend = envUnion 

--------------------------------------------------------------------------------
-- Printing Instance -----------------------------------------------------------
--------------------------------------------------------------------------------

instance PP t => PP (Env t) where
  pp = vcat . (ppBind <$>) . F.toListSEnv . fmap val 

ppBind (x, t) = pprint x <+> dcolon <+> pp t

