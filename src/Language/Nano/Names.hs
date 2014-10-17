
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveTraversable    #-}

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

  , mkRelName
  , mkAbsName
  , mkAbsPath

  ) where 

import qualified Data.HashSet                   as H
import           Data.List                      (find)
import           Control.Applicative            ((<$>), (<*>))
import           Data.Hashable          
import           Data.Data
import           Data.Traversable
import           Data.Foldable                      (Foldable()) 
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.PrettyPrint
import           Language.Nano.Locations
import           Language.Nano.Errors()
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
data QN l = QN l SourceSpan NameSpacePath F.Symbol deriving (Show, Ord, Data, Typeable, Functor, Traversable, Foldable)

-- Qualified Path
data QP l = QP l SourceSpan NameSpacePath deriving (Show, Ord, Data, Typeable, Functor, Traversable, Foldable)

instance Eq l => Eq (QN l) where
  QN k1 _ n1 s1 == QN k2 _ n2 s2 = (k1,n1,s1) == (k2,n2,s2)

instance Eq l => Eq (QP l) where
  QP k1 _ n1 == QP k2 _ n2 = (k1,n1) == (k2,n2)

instance F.Symbolic (QN l) where
  symbol (QN _ _ _ s) = s
 
instance Hashable (QN l) where
  hashWithSalt i (QN _ _ n s) = hashWithSalt i (s:n)
 
instance Hashable (QP l) where
  hashWithSalt i (QP _ _ n) = hashWithSalt i n
 
instance IsLocated (QN l) where
  srcPos (QN _ a _ _) = a

instance IsLocated (QP l) where
  srcPos (QP _ a _) = a

instance F.Symbolic (Id a) where
  symbol (Id _ x)   = F.symbol x 
 
instance Hashable a => Hashable (Id a) where 
  hashWithSalt i x = hashWithSalt i (idLoc x, idName x)
 
idName (Id _ x) = x
idLoc  (Id l _) = l
 
instance F.Fixpoint String where
  toFix = text 
 
instance PP F.Symbol where 
  pp = pprint
 
instance PP (QN l) where
  pp (QN _ _ [] s) = pp s
  pp (QN _ _ ms s) = (hcat $ punctuate dot $ map pp ms) <> dot <> pp s
 
instance PP (QP l) where
  pp (QP _ _ []) = pp "<global>"
  pp (QP _ _ ms) = hcat $ punctuate dot $ map pp ms

-- instance PP NameSpacePath where
--   pp = hcat . punctuate dot . map pp
 
instance (Ord a, F.Fixpoint a) => PP (F.FixResult a) where
  pp = F.resultDoc
 
instance PP F.Pred where 
  pp = pprint
 
instance PP (Id a) where
  pp (Id _ x) = text x

instance PP a => PP (Located a) where
  pp x = pp (val x) <+> text "at:" <+> pp (loc x)

extendAbsPath :: F.Symbolic s => AbsPath -> s -> AbsPath
extendAbsPath (QP _ l ps) s = QP AK_ l $ ps ++ [F.symbol s]


returnName :: String
returnName = "$result"

symbolId :: (IsLocated l, F.Symbolic x) => l -> x -> Id l
symbolId l x = Id l $ F.symbolString $ F.symbol x

returnId   :: a -> Id a
returnId x = Id x returnName 

returnSymbol :: F.Symbol
returnSymbol = F.symbol returnName

mkRelName :: NameSpacePath -> F.Symbol -> RelName
mkRelName = QN RK_ $ srcPos dummySpan

mkAbsName = QN AK_ $ srcPos dummySpan
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
absoluteName ns (QP AK_ _ p) (QN RK_ _ ss s) = 
    find (`H.member` ns) $ (`mkAbsName` s) . (++ ss) <$> prefixes p
  where
    prefixes        = map reverse . suffixes . reverse
    suffixes []     = [[]]
    suffixes (x:xs) = (x:xs) : suffixes xs
    

absolutePath :: H.HashSet AbsPath -> AbsPath -> RelPath -> Maybe AbsPath
absolutePath ps (QP AK_ _ p) (QP RK_ _ ss) = 
    find (`H.member` ps) $ mkAbsPath . (++ ss) <$> prefixes p
  where
    prefixes        = map reverse . suffixes . reverse
    suffixes []     = [[]]
    suffixes (x:xs) = (x:xs) : suffixes xs


