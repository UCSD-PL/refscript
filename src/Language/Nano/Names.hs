
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Language.Nano.Names (
 
    QN(..)
  , QP(..)
  , AK(..)
  , RK(..)
  , RelName, AbsName
  , RelPath, AbsPath
  , NameSpacePath

  -- * Deconstructing Id
  , idName
  , idLoc
  , symbolId
  , returnId
  , returnSymbol 
  , extendAbsPath
  , nameInPath
  , pathInPath

  , mkRelName
  , mkAbsName
  , mkAbsPath

  , absoluteName
  , absolutePath

  , toAbsoluteName
  , toLocSym 
  , extClassSym
  , extInterfaceSym
  , offsetLocSym 
  , offsetSym
  , ttagSym
  , hasPropertySym
  , protoSym
  , thisSym
  , thisId
  , undefinedId

  ) where 

import qualified Data.HashSet                   as H
import           Data.List                      (find)
import           Data.Default
import           Control.Applicative            ((<$>))
import           Data.Hashable          
import           Data.Data
import           Data.Traversable
import           Data.Foldable                      (Foldable()) 
import           Language.Nano.AST
import           Language.Nano.Locations
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Misc
import           Language.Fixpoint.PrettyPrint
import           Text.PrettyPrint.HughesPJ

--------------------------------------------------------------------------
-- | Names
--------------------------------------------------------------------------

type NameSpacePath = [F.Symbol]

type AbsName = QN AK
type AbsPath = QP AK

type RelName = QN RK
type RelPath = QP RK

-- Qualified Kind: Absolute or Relative
data AK = AK_ deriving (Eq, Data, Typeable, Show)
data RK = RK_ deriving (Eq, Data, Typeable, Show)

-- Qualified Name
data QN l = QN (QP l) F.Symbol deriving (Show, Ord, Data, Typeable, Functor, Traversable, Foldable)

-- Qualified Path
data QP l = QP l SrcSpan NameSpacePath deriving (Show, Ord, Data, Typeable, Functor, Traversable, Foldable)

instance Eq l => Eq (QN l) where
  QN p1 s1 == QN p2 s2 = (p1,s1) == (p2,s2)

instance Eq l => Eq (QP l) where
  QP k1 _ n1 == QP k2 _ n2 = (k1,n1) == (k2,n2)

instance F.Symbolic (QN l) where
  symbol (QN _ s) = s
 
instance Hashable (QN l) where
  hashWithSalt i (QN p s) = hashWithSalt i (p,s)
 
instance Hashable (QP l) where
  hashWithSalt i (QP _ _ n) = hashWithSalt i n
 
instance IsLocated (QN l) where
  srcPos (QN p _) = srcPos p

instance IsLocated (QP l) where
  srcPos (QP _ a _) = a

instance Default l => Default (QP l) where
  def = QP def (srcPos dummySpan) []

instance Default SrcSpan where
  def = srcPos dummySpan

instance Default AK where
  def = AK_

instance F.Symbolic (Id a) where
  symbol (Id _ x)   = F.symbol x 
 
instance Hashable a => Hashable (Id a) where 
  hashWithSalt i x = hashWithSalt i (idLoc x, idName x)
 
idName (Id _ x) = x
idLoc  (Id l _) = l
 
instance F.Fixpoint String where
  toFix = text 
 

extendAbsPath :: F.Symbolic s => AbsPath -> s -> AbsPath
extendAbsPath (QP _ l ps) s = QP AK_ l $ ps ++ [F.symbol s]

nameInPath  :: (IsLocated l, F.Symbolic s) => l -> AbsPath -> s -> AbsName
nameInPath l (QP _ _ ps) s = QN (QP AK_ (srcPos l) ps) (F.symbol s)

pathInPath  :: (IsLocated l, F.Symbolic s) => l -> AbsPath -> s -> AbsPath
pathInPath l (QP _ _ ps) s = QP AK_ (srcPos l) $ ps ++ [F.symbol s]

returnName :: String
returnName = "$result"

symbolId :: (IsLocated l, F.Symbolic x) => l -> x -> Id l
symbolId l x = Id l $ F.symbolString $ F.symbol x

returnId   :: a -> Id a
returnId x = Id x returnName 

returnSymbol :: F.Symbol
returnSymbol = F.symbol returnName

mkRelName :: F.Symbolic s => NameSpacePath -> s -> RelName
mkRelName n = QN (QP RK_ def n) . F.symbol

mkAbsName n = QN (QP AK_ def n) . F.symbol

mkAbsPath = QP AK_ $ srcPos dummySpan


--------------------------------------------------------------------------
-- | Name transformation
--------------------------------------------------------------------------

-- | `absoluteName env p r` returns `Just a` where `a` is the absolute path of
--   the relative name `r` when referenced in the context of the absolute path 
--   `p`; `Nothing` otherwise.
--
--   If p = A.B.C and r = C.D.E then the paths that will be checked in this
--   order are: 
--
--    A.B.C.C.D.E
--    A.B.C.D.E
--    A.C.D.E
--    C.D.E
--
---------------------------------------------------------------------------------
absoluteName :: H.HashSet AbsName -> AbsPath -> RelName -> Maybe AbsName
---------------------------------------------------------------------------------
absoluteName ns (QP AK_ _ p) (QN (QP RK_ _ ss) s) = 
    find (`H.member` ns) $ (`mkAbsName` s) . (++ ss) <$> prefixes p
  where
    prefixes        = map reverse . suffixes . reverse
    suffixes []     = [[]]
    suffixes (x:xs) = (x:xs) : suffixes xs
    
---------------------------------------------------------------------------------
absolutePath :: H.HashSet AbsPath -> AbsPath -> RelPath -> Maybe AbsPath
---------------------------------------------------------------------------------
absolutePath ps (QP AK_ _ p) (QP RK_ _ ss) = 
    find (`H.member` ps) $ mkAbsPath . (++ ss) <$> prefixes p
  where
    prefixes        = map reverse . suffixes . reverse
    suffixes []     = [[]]
    suffixes (x:xs) = (x:xs) : suffixes xs

toAbsoluteName (QN (QP RK_ l ss) s) = QN (QP AK_ l ss) s

toLocSym        = F.dummyLoc . F.symbol 
extClassSym     = toLocSym "extends_class"
extInterfaceSym = toLocSym "extends_interface"
offsetLocSym    = toLocSym "offset"
offsetSym       = F.symbol "offset"
ttagSym         = toLocSym "ttag" 
hasPropertySym  = toLocSym "hasProperty"

undefinedId     = Id (srcPos dummySpan) "undefined"
thisId          = Id (srcPos dummySpan) "this"
thisSym         = F.symbol "this"
protoSym        = F.symbol "__proto__"

