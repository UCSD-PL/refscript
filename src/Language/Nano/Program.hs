
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE OverlappingInstances   #-}

module Language.Nano.Program (

  -- * Programs
    Nano (..)
  , UNanoBare, UNanoSSA, NanoBareR, NanoBareRelR, NanoSSAR, NanoRefType
  , NanoTypeR, UNanoType, ExprSSAR, StmtSSAR
  , Source (..)

  -- * Nano Transformations
  , flattenStmt

  -- * SSA Ids
  , mkNextId, isNextId, mkSSAId , mkKeysId, mkKeysIdxId, mkCtorStr, mkCtorId

  ) where

import           Control.Applicative              hiding (empty)
import           Control.Exception                (throw)
import           Data.Monoid                      hiding ((<>))
import           Data.Default
import           Data.Maybe                       (maybeToList)
import           Data.List                        (stripPrefix)
import qualified Data.HashMap.Strict              as HM
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree
import qualified Data.HashSet                     as H
import           Data.Generics
import qualified Data.IntMap                      as I
import           Text.PrettyPrint.HughesPJ

import           Language.Nano.AST
import           Language.Nano.Annots
import           Language.Nano.Env
import           Language.Nano.Errors
import           Language.Nano.Locations
import           Language.Nano.Names
import           Language.Nano.Types

import           Language.Fixpoint.Misc           hiding (mkGraph)
import qualified Language.Fixpoint.Types          as F

-- import           Debug.Trace                        hiding (traceShow)


---------------------------------------------------------------------------------
-- | Nano Program
---------------------------------------------------------------------------------
--
-- consts, aliases, invariants refer to absolute names
-- (hence the use of RType r)

data Nano a r = Nano {
  --
  -- Source AST
  --
    code      :: !(Source a)
  --
  -- Measure Signatures
  --
  , consts    :: !(Env (RType r))
  --
  -- Type aliases
  --
  , tAlias    :: !(TAliasEnv (RTypeQ RK r))
  --
  -- Predicate aliases
  --
  , pAlias    :: !(PAliasEnv)
  --
  -- Qualifiers
  --
  , pQuals    :: ![F.Qualifier]
  --
  -- Type Invariants
  --
  , invts     :: ![Located (RType r)]
  --
  -- Maximum id
  --
  , max_id    :: NodeId
  --
  -- Options
  --
  , pOptions  :: [RscOption]

  } deriving (Functor) -- , Data, Typeable)


newtype Source a = Src [Statement a]
  deriving (Data, Typeable)


type NanoBareRelR r = Nano  (AnnRel  r) r       -- ^ After parse (relative names)
type NanoBareR r    = Nano  (AnnBare r) r       -- ^ After Parse
type NanoSSAR r     = Nano  (AnnSSA  r) r       -- ^ After SSA
type NanoTypeR r    = Nano  (AnnType r) r       -- ^ After TC
type NanoRefType    = NanoTypeR F.Reft          -- ^ After Liquid

type ExprSSAR r     = Expression (AnnSSA r)
type StmtSSAR r     = Statement  (AnnSSA r)

type UNanoBare      = NanoBareR ()
type UNanoSSA       = NanoSSAR  ()
type UNanoType      = NanoTypeR ()


---------------------------------------------------------------------------
-- | Instances
---------------------------------------------------------------------------

instance Monoid (Source a) where
  mempty                    = Src []
  mappend (Src s1) (Src s2) = Src $ s1 ++ s2

instance Functor Source where
  fmap f (Src zs) = Src (map (fmap f) zs)


flattenStmt (BlockStmt _ ss) = concatMap flattenStmt ss
flattenStmt s                = [s]


--------------------------------------------------------------------------------
-- | Manipulating SSA Ids
--------------------------------------------------------------------------------

mkSSAId :: (F.Symbolic x, IsLocated a) => a -> x -> Int -> Id a
mkSSAId l x n = Id l (F.symbolString (F.symbol x) ++ ssaStr ++ show n)

mkNextId :: Id a -> Id a
mkNextId (Id a x) =  Id a $ nextStr ++ x

isNextId :: Id a -> Maybe (Id a)
isNextId (Id a s) = Id a <$> stripPrefix nextStr s

mkKeysId :: Id a -> Id a
mkKeysId (Id a x) =  Id a $ keysStr ++ x

mkKeysIdxId :: Id a -> Id a
mkKeysIdxId (Id a x) =  Id a $ keysIdxStr ++ x

mkCtorId l (Id _ x) = Id l $ mkCtorStr x
mkCtorStr x         = x ++ ctorStr

nextStr    = "_NEXT_"
ssaStr     = "_SSA_"
keysIdxStr = "_KEYS_IDX_"
keysStr    = "_KEYS_"
ctorStr    = "_CTOR_"


