
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}

module Language.Nano.Names (

    Var
  , QName(..)
  , QPath(..)
  , RelName(..), AbsName(..)
  , RelPath(..), AbsPath(..)
  , NameSpacePath
  , AbsolutePath 

  -- * Deconstructing Id
  , idName
  , idLoc
  , symbolId
  , returnId
  , returnSymbol 
  , extendAbsPath

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
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Misc
import           Language.Fixpoint.PrettyPrint
import           Text.PrettyPrint.HughesPJ


--------------------------------------------------------------------------
-- | Names
--------------------------------------------------------------------------

type NameSpacePath = [F.Symbol]

type AbsolutePath  = NameSpacePath

-- | A qualified name (used for qualified variables, classes, functions, etc.)
--
data QName = QName { 
    qn_ss    :: SourceSpan
  , qn_path  :: NameSpacePath
  , qn_name  :: F.Symbol 
  } deriving (Eq, Ord, Show, Data, Typeable)

-- | Relative and absolute version of a qualafied name
--
newtype RelName = RN QName deriving (Eq, Ord, Show, Data, Typeable)
newtype AbsName = AN QName deriving (Eq, Ord, Show, Data, Typeable)

-- | A qualified path (used for qualified namespaces, i.e. modules)
--
data QPath = QPath { 
    qp_ss    :: SourceSpan
  , qp_path  :: NameSpacePath 
  } deriving (Eq, Ord, Show, Data, Typeable)

-- | Relative and absolute version of a qualafied path
--
newtype RelPath = RP QPath deriving (Eq, Ord, Show, Data, Typeable)
newtype AbsPath = AP QPath deriving (Eq, Ord, Show, Data, Typeable)

instance F.Symbolic RelName where
  symbol (RN (QName _ _ s)) = s

instance Hashable QName where
  hashWithSalt i (QName _ n s) = hashWithSalt i (s:n)

instance Hashable RelName where
  hashWithSalt i (RN a) = hashWithSalt i a

instance Hashable RelPath where
  hashWithSalt i (RP a) = hashWithSalt i a

instance Hashable AbsName where
  hashWithSalt i (AN a) = hashWithSalt i a

instance Hashable AbsPath where
  hashWithSalt i (AP a) = hashWithSalt i a

instance IsLocated AbsPath where
  srcPos (AP a) = srcPos a

instance Hashable QPath where
  hashWithSalt i (QPath _ n) = hashWithSalt i n

instance IsLocated QName where
  srcPos (QName s _ _) = s

instance IsLocated QPath where
  srcPos (QPath s _) = s

type Var = Id SourceSpan 

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

instance PP QName where
  pp (QName _ [] s) = pp s
  pp (QName _ ms s) = pp ms <> dot <> pp s

instance PP QPath where
  pp (QPath _ []) = pp "<global>"
  pp (QPath _ ms) = pp ms

instance PP RelPath where
  pp (RP p) = pp p

instance PP AbsPath where
  pp (AP p) = pp p

instance PP RelName where
  pp (RN p) = pp p

instance PP AbsName where
  pp (AN p) = pp p

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

extendAbsPath :: F.Symbolic s => AbsPath -> s -> AbsPath
extendAbsPath (AP (QPath l ps)) s = AP $ QPath l $ ps ++ [F.symbol s]


returnName :: String
returnName = "$result"

symbolId :: (IsLocated l, F.Symbolic x) => l -> x -> Id l
symbolId l x = Id l $ F.symbolString $ F.symbol x

returnId   :: a -> Id a
returnId x = Id x returnName 

returnSymbol :: F.Symbol
returnSymbol = F.symbol returnName

