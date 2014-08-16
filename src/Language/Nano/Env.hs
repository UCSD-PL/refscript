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
  , Var, QName(..), NameSpacePath, AbsolutePath 

  -- * Deconstructing Id
  , idName, idLoc 
  , returnId
  
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
  ) where 

import           Data.Maybe             (isJust)
import           Data.Hashable          
import           Data.Monoid            (Monoid (..))
import qualified Data.HashMap.Strict as M
-- import qualified Data.Foldable       as FO
-- import           Data.Traversable
import           Data.Data
import qualified Data.List               as L
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.PrettyPrint
import           Language.Nano.Locations
import           Language.Nano.Errors
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Misc
import           Language.Fixpoint.PrettyPrint
import           Text.PrettyPrint.HughesPJ
import           Control.Applicative 
import           Control.Exception (throw)

--------------------------------------------------------------------------
-- | Environment Definitions
--------------------------------------------------------------------------

type Env t      = F.SEnv (Located t) 

newtype QEnv t  = QE { qe_binds :: M.HashMap QName (Located t) } 
    deriving (Eq, Data, Typeable, Functor)

type NameSpacePath = [F.Symbol]
type AbsolutePath  = NameSpacePath

data QName = QN { q_ss    :: SourceSpan
                , q_path  :: NameSpacePath
                , q_name  :: F.Symbol }
    deriving (Eq, Ord, Show, Data, Typeable)


instance Hashable QName where
  hashWithSalt i (QN _ n s) = hashWithSalt i (s:n)

instance IsLocated QName where
  srcPos (QN s _ _) = s

qnameList (QN _ n s) = n ++ [s]

type Var        = Id SourceSpan 

instance F.Symbolic   (Id a) where
  symbol (Id _ x)   = F.symbol x 

instance Hashable a => Hashable (Id a) where 
  hashWithSalt i x = hashWithSalt i (idLoc x, idName x)

idName (Id _ x) = x
idLoc  (Id l _) = l

instance F.Fixpoint String where
  toFix = text 


--------------------------------------------------------------------------------
-- | Printing
--------------------------------------------------------------------------------

instance PP F.Symbol where 
  pp = pprint

instance PP QName where
  pp (QN _ [] s) = pp s
  pp (QN _ ms s) = pp ms <> dot <> pp s

instance PP NameSpacePath where
  pp = hcat . punctuate dot . map pp

instance (Ord a, F.Fixpoint a) => PP (F.FixResult a) where
  pp = F.resultDoc

instance PP F.Pred where 
  pp = pprint

instance PP (Id a) where
  pp (Id _ x) = text x

instance PP a => PP (Located a) where
  pp x = pp (val x) <+> text "at:" <+> pp (loc x)



returnName :: String
returnName = "$result"

symbolId :: (IsLocated l, F.Symbolic x) => l -> x -> Id l
symbolId l x = Id l $ F.symbolString $ F.symbol x

returnId   :: a -> Id a
returnId x = Id x returnName 

returnSymbol :: F.Symbol
returnSymbol = F.symbol returnName


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


--------------------------------------------------------------------------------
-- | QEnv API  
--------------------------------------------------------------------------------

qenvFromList       :: [(QName, t)] -> QEnv t
qenvFromList        = L.foldl' step qenvEmpty
  where 
    step γ (q, t)   = case qenvFindLoc q γ of
                        Nothing -> qenvAdd q t γ 
                        Just l' -> throw $ errorDuplicate q (srcPos q) l'

qenvEmpty           = QE M.empty
qenvFindLoc i γ     = fmap loc $ qenvFind i γ 
qenvFind q (QE γ)   = M.lookup q γ 

qenvAdd :: QName -> t -> QEnv t -> QEnv t
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

