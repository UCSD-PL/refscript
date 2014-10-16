
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}

module Language.Nano.Names (
 
    QN(..)
  , QP(..)
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

  ) where 

import           Data.Hashable          
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
data QK = AK | RK 

-- Qualified Name
data QN :: QK -> * where
  AN :: SourceSpan -> NameSpacePath -> F.Symbol -> QN AK
  RN :: SourceSpan -> NameSpacePath -> F.Symbol -> QN RK

-- Qualified Path
data QP :: QK -> * where
  AP :: SourceSpan -> NameSpacePath -> QP AK
  RP :: SourceSpan -> NameSpacePath -> QP RK

instance Eq (QN l) where
  AN _ n1 s1 == AN _ n2 s2 = (n1,s1) == (n2,s2)
  RN _ n1 s1 == RN _ n2 s2 = (n1,s1) == (n2,s2)
  _          == _          = False

instance Eq (QP l) where
  AP _ n1 == AP _ n2 = n1 == n2
  RP _ n1 == RP _ n2 = n1 == n2
  _       == _       = False

instance F.Symbolic (QN l) where
  symbol (AN _ _ s) = s
  symbol (RN _ _ s) = s
 
instance Hashable (QN l) where
  hashWithSalt i (AN _ n s) = hashWithSalt i (s:n)
  hashWithSalt i (RN _ n s) = hashWithSalt i (s:n)
 
instance Hashable (QP l) where
  hashWithSalt i (AP _ n) = hashWithSalt i n
  hashWithSalt i (RP _ n) = hashWithSalt i n
 
instance IsLocated (QN l) where
  srcPos (AN a _ _) = a
  srcPos (RN a _ _) = a

instance IsLocated (QP l) where
  srcPos (AP a _) = a
  srcPos (RP a _) = a

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
  pp (AN _ [] s) = pp s
  pp (AN _ ms s) = (hcat $ punctuate dot $ map pp ms) <> dot <> pp s
  pp (RN _ [] s) = pp s
  pp (RN _ ms s) = (hcat $ punctuate dot $ map pp ms) <> dot <> pp s

 
instance PP (QP l) where
  pp (AP _ []) = pp "<global>"
  pp (AP _ ms) = hcat $ punctuate dot $ map pp ms
  pp (RP _ []) = pp "<global>"
  pp (RP _ ms) = hcat $ punctuate dot $ map pp ms

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
extendAbsPath (AP l ps) s = AP l $ ps ++ [F.symbol s]


returnName :: String
returnName = "$result"

symbolId :: (IsLocated l, F.Symbolic x) => l -> x -> Id l
symbolId l x = Id l $ F.symbolString $ F.symbol x

returnId   :: a -> Id a
returnId x = Id x returnName 

returnSymbol :: F.Symbol
returnSymbol = F.symbol returnName

mkRelName :: NameSpacePath -> F.Symbol -> RelName
mkRelName ss s = RN (srcPos dummySpan) ss s 


