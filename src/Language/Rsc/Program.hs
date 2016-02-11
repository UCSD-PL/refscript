{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Rsc.Program (

  -- * Programs
    Rsc (..)
  , FRsc (..)
--   , RscQ
  , BareRelRsc
  , BareRsc
  , RRsc
  , SsaRsc
  , TcRsc
  , RefScript
  , RelRefScript
  , UBareRsc
  , USsaRsc
  , ExprSSAR, StmtSSAR
  , Source (..)

  -- * SSA Ids
  , mkNextId, isNextId, mkSSAId , mkKeysId, mkKeysIdxId, mkCtorStr, mkCtorId

  ) where

import           Data.Generics
import           Data.List                     (stripPrefix)
import qualified Language.Fixpoint.Types       as F
import           Language.Fixpoint.Types.Names (symbolString)
import           Language.Rsc.Annotations
import           Language.Rsc.AST
import           Language.Rsc.Core.Env
import           Language.Rsc.Locations
import           Language.Rsc.Names
import           Language.Rsc.Types


---------------------------------------------------------------------------------
-- | Rsc Program
---------------------------------------------------------------------------------
--
-- consts, aliases, invariants refer to absolute names
-- (hence the use of RType r)

data Rsc a r = Rsc {
  --
  -- | Source AST
  --
    code     :: !(Source a)
  --
  -- | Measure Signatures
  --
  , consts   :: !(Env (RType r))
  --
  -- | Type aliases
  --
  , tAlias   :: !(TAliasEnv (RTypeQ RK r))
  --
  -- | Predicate aliases
  --
  , pAlias   :: !PAliasEnv
  --
  -- | Qualifiers
  --
  , pQuals   :: ![F.Qualifier]
  --
  -- | Type Invariants
  --
  , invts    :: ![Located (RType r)]
  --
  -- | Maximum node id
  --
  , maxId    :: NodeId
  --
  -- | Options
  --
  , pOptions :: [Located String]

  } deriving (Functor) -- , Data, Typeable)


newtype Source a = Src [Statement a]
  deriving (Data, Typeable)

data FRsc q r = FRsc (Rsc (FAnnQ q r) r)

-- type BareRelRsc r = FRsc RK r           -- After parse (relative names)
-- type BareRsc r    = FRsc AK r           -- After parse (absolute names)
-- type RRsc r       = FRsc AK r           -- After parse (absolute names)
-- type SsaRsc r     = FRsc AK r           -- SSA
-- type TcRsc r      = FRsc AK r           -- Raw checking
-- type RefScript    = RRsc F.Reft         -- Liquid
-- type RelRefScript = BareRelRsc F.Reft
-- type UBareRsc     = BareRsc ()
-- type USsaRsc      = SsaRsc  ()


type BareRelRsc r = Rsc (AnnRel  r) r         -- After parse (relative names)
type BareRsc r    = Rsc (AnnBare r) r         -- After parse (absolute names)
type RRsc r       = Rsc (AnnBare r) r         -- After parse (absolute names)
type SsaRsc r     = Rsc (AnnSSA  r) r         -- SSA
type TcRsc r      = Rsc (AnnTc   r) r         -- Raw checking
type RefScript    = RRsc F.Reft               -- Liquid
type RelRefScript = BareRelRsc F.Reft
type UBareRsc     = BareRsc ()
type USsaRsc      = SsaRsc  ()

type ExprSSAR r   = Expression (AnnSSA r)
type StmtSSAR r   = Statement  (AnnSSA r)



---------------------------------------------------------------------------
-- | Instances
---------------------------------------------------------------------------

instance Monoid (Source a) where
  mempty                    = Src []
  mappend (Src s1) (Src s2) = Src $ s1 ++ s2

instance Functor Source where
  fmap f (Src zs) = Src (map (fmap f) zs)


--------------------------------------------------------------------------------
-- | Manipulating SSA Ids
--------------------------------------------------------------------------------

mkSSAId :: (F.Symbolic x, IsLocated a) => a -> x -> Int -> Id a
mkSSAId l x n = Id l (symbolString (F.symbol x) ++ ssaStr ++ show n)

mkNextId :: F.Symbolic x => a -> x -> Id a
mkNextId a x = Id a $ nextStr ++ (F.symbolString (F.symbol x))

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

