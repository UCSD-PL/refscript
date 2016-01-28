{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Rsc.Names (

    QN(..)
  , QP(..)
  , AK(..)
  , RK(..)
  , RelName, AbsName
  , RelPath, AbsPath
  , NameSpacePath
  , Id(..)

  -- * Id operations
  , idName
  , idLoc
  , unId
  , symbolId
  , returnId
  , lenId
  , argId
  , argIdInit

  -- * Path operations
  , extendAbsPath
  , nameInPath
  , pathInPath

  -- * Name constructors
  , mkRelName
  , mkAbsName
  , mkAbsPath
  , emptyPath

  -- * Hard-coded Symbols
  , returnSymbol
  , extClassSym
  , extInterfaceSym
  , offsetLocSym
  , protoSym
  , thisSym
  , argSym
  , thisId
  , undefinedId

  -- * Hard-coded names
  , arrayName
  , boolName
  , voidName
  , unionName
  , intersName
  , topName
  , anyName
  , nullName
  , undefName
  , objectName
  , selfName
  , className
  , moduleName
  , enumName
  , extendsClassName
  , extendsInterfaceName
  , offsetName

  ) where

import           Control.Applicative           ((<$>))
import           Data.Data
import           Data.Default
import           Data.Foldable                 (Foldable ())
import           Data.Hashable
import qualified Data.HashSet                  as H
import           Data.Traversable
import           GHC.Generics                  (Generic ())
import qualified Language.Fixpoint.Types       as F
import           Language.Fixpoint.Types.Names (symbolString)
import           Language.Rsc.Locations
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
data QN l = QN (QP l) F.Symbol
            deriving (Show, Ord, Data, Typeable, Functor, Traversable, Foldable)

-- Qualified Path
data QP l = QP l SrcSpan NameSpacePath
            deriving (Show, Ord, Data, Typeable, Functor, Traversable, Foldable)

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

instance IsLocated (QP l) where
  srcPos (QP _ a _) = a

instance IsLocated (QN l) where
  srcPos (QN a _) = srcPos a

instance Default l => Default (QP l) where
  def = QP def (srcPos dummySpan) []

instance Default SrcSpan where
  def = srcPos dummySpan

instance Default AK where
  def = AK_

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

data Id a = Id a String
          deriving (Show,Data,Typeable,Functor,Foldable,Traversable, Generic)

unId :: Id a -> String
unId (Id _ s) = s

idName (Id _ x) = x
idLoc  (Id l _) = l

instance {-# OVERLAPPING #-} IsLocated a => IsLocated (Id a) where
  srcPos (Id l _) = srcPos l

symbolId :: (IsLocated l, F.Symbolic x) => l -> x -> Id l
symbolId l x = Id l $ symbolString $ F.symbol x

returnId   :: a -> Id a
returnId x  = Id x returnName
lenId l     = Id l "length"
argIdInit l = Id l $ "arguments"
argId l i   = Id l $ "arguments_" ++ show i


instance Eq (Id a) where
  Id _ x1 == Id _ x2 = x1 == x2

instance Ord (Id a) where
  Id _ x1 `compare` Id _ x2 = x1 `compare` x2

instance F.Symbolic (Id a) where
  symbol (Id _ x)   = F.symbol x

instance Hashable a => Hashable (Id a) where
  hashWithSalt i x = hashWithSalt i (idLoc x, idName x)


returnSymbol :: F.Symbol
returnSymbol = F.symbol returnName

mkRelName :: F.Symbolic s => NameSpacePath -> s -> RelName
mkRelName n = QN (QP RK_ def n) . F.symbol

mkAbsName n = QN (QP AK_ def n) . F.symbol

mkAbsPath = QP AK_ $ srcPos dummySpan

emptyPath = mkAbsPath []



-- toLocSym        = F.dummyLoc . F.symbol
extClassSym     = F.dummyLoc extendsClassName     -- "extends_class"
extInterfaceSym = F.dummyLoc extendsInterfaceName --  "extends_interface"
offsetLocSym    = F.dummyLoc offsetName

-- ttagSym         = F.dummyLoc "ttag"
-- hasPropertySym  = F.dummyLoc "hasProperty"

undefinedId     = Id (srcPos dummySpan) "undefined"
thisId l        = Id l "this"

offsetName, thisSym, protoSym, argSym :: F.Symbol
thisSym    = "this"
protoSym   = "__proto__"
argSym     = "arguments"
offsetName = "offset"

----------------------------------------------------------------------
-- | Global Names
----------------------------------------------------------------------

arrayName, boolName, voidName, unionName, topName, nullName, undefName :: F.Symbol
arrayName = "Array"
boolName  = "Boolean"
voidName  = "Void"
unionName = "Union"
topName   = "Top"
anyName   = "Any"
nullName  = "Tull"
undefName = "Undefined"

objectName, selfName, className, moduleName, enumName, intersName:: F.Symbol
objectName = "Object"
selfName   = "Self"
className  = "class"
moduleName = "module"
enumName   = "enum"
intersName = "intersection"

extendsClassName, extendsInterfaceName :: F.Symbol
extendsClassName     = "extends_class"
extendsInterfaceName = "extends_interface"

