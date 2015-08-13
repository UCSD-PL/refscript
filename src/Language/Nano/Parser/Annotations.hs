{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE DeriveGeneric             #-}


module Language.Nano.Typecheck.Parse ( 
    RawSpec(..)
  ) where

import           Prelude                          hiding ( mapM)

import           Data.Either                             (partitionEithers)
import           Data.Default
import           Data.Traversable                        (mapAccumL)
import           Data.Monoid                             (mempty, mconcat)
import           Data.Maybe                              (listToMaybe, catMaybes, maybeToList, fromMaybe)
import           Data.Generics                    hiding (Generic)
import           Data.Aeson                              (eitherDecode)
import           Data.Aeson.Types                 hiding (Parser, Error, parse)
import qualified Data.Aeson.Types                 as     AI
import qualified Data.ByteString.Lazy.Char8       as     B
import           Data.Char                               (isLower)
import qualified Data.List                        as     L
import qualified Data.IntMap.Strict               as I
import qualified Data.HashMap.Strict              as HM
import           Data.Tuple
import qualified Data.HashSet                     as HS

import           Text.PrettyPrint.HughesPJ               (text)
import qualified Data.Foldable                    as     FO
import           Data.Vector                             ((!))
import           Data.Graph.Inductive.Graph

import           Control.Monad
import           Control.Monad.Trans                     (MonadIO,liftIO)
import           Control.Applicative                     ((<$>), (<*>) , (<*) , (*>))

import           Language.Fixpoint.Types          hiding (quals, Loc, Expression)
import           Language.Fixpoint.Parse
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc                  (mapEither, mapSnd, fst3, mapFst)

import           Language.Nano.Annots
import           Language.Nano.Errors
import           Language.Nano.Env
import           Language.Nano.Locations hiding (val)
import           Language.Nano.Names
import           Language.Nano.Misc                      (fst4)
import           Language.Nano.Program
import           Language.Nano.Types              hiding (Exported)
import           Language.Nano.Visitor
import           Language.Nano.Typecheck.Types
import           Language.Nano.Liquid.Types
import           Language.Nano.Liquid.Alias
import           Language.Nano.Liquid.Qualifiers
import           Language.Nano.Typecheck.Resolve

import           Language.Nano.Parser.Common

import           Language.Nano.Syntax
import           Language.Nano.Syntax.PrettyPrint
import           Language.Nano.Syntax.Annotations

import           Text.Parsec                      hiding (parse, State)
import           Text.Parsec.Pos                         (newPos, SourcePos)
import           Text.Parsec.Error                       (errorMessages, showErrorMessages)
import qualified Text.Parsec.Token                as     T
import qualified Data.Text                        as     DT
import           Text.Parsec.Token                       (identStart, identLetter)
-- import           Text.Parsec.Prim                        (stateUser)
import           Text.Parsec.Language                    (emptyDef)

import           GHC.Generics

-- import           Debug.Trace                             ( trace, traceShow)


---------------------------------------------------------------------------------
-- | Specifications
---------------------------------------------------------------------------------

data RawSpec
  = RawMeas     (SrcSpan, String)   -- Measure
  | RawBind     (SrcSpan, String)   -- Named function or var bindings
  | RawAmbBind  (SrcSpan, String)   -- Ambient bamed function or var bindings
  | RawFunc     (SrcSpan, String)   -- Anonymouns function type
  | RawIface    (SrcSpan, String)   -- Interface annots
  | RawClass    (SrcSpan, String)   -- Class annots
  | RawField    (SrcSpan, String)   -- Field annots
  | RawMethod   (SrcSpan, String)   -- Method annots
  | RawConstr   (SrcSpan, String)   -- Constructor annots
  | RawTAlias   (SrcSpan, String)   -- Type aliases
  | RawPAlias   (SrcSpan, String)   -- Predicate aliases
  | RawQual     (SrcSpan, String)   -- Qualifiers
  | RawOption   (SrcSpan, String)   -- Options
  | RawInvt     (SrcSpan, String)   -- Invariants
  | RawCast     (SrcSpan, String)   -- Casts
  | RawExported (SrcSpan, String)   -- Exported
  | RawReadOnly (SrcSpan, String)   -- ReadOnly
  deriving (Show,Eq,Ord,Data,Typeable,Generic)

data PSpec l r
  = Meas    (Id l, RTypeQ RK r)
  | Bind    (Id l, Assignability, Maybe (RTypeQ RK r))
  | AmbBind (Id l, RTypeQ RK r)
  | AnFunc  (RTypeQ RK r)
  | Field   (TypeMemberQ RK r)
  | Constr  (TypeMemberQ RK r)
  | Method  (TypeMemberQ RK r)
  | Static  (TypeMemberQ RK r)
  | Iface   (Id l, IfaceDefQ RK r)
  | Class   (Id l, ClassSigQ RK r)
  | TAlias  (Id l, TAlias (RTypeQ RK r))
  | PAlias  (Id l, PAlias)
  | Qual    Qualifier
  | Option  RscOption
  | Invt    l (RTypeQ RK r)
  | CastSp  l (RTypeQ RK r)
  | Exported l
  | RdOnly l

  -- Used only for parsing specs
  | ErrorSpec
  deriving (Eq, Show, Data, Typeable)

type Spec = PSpec SrcSpan Reft

parseAnnot :: RawSpec -> Parser Spec
parseAnnot = go
  where
    go (RawMeas     (ss, _)) = Meas    <$> patch2 ss <$> idBindP
    go (RawBind     (ss, _)) = Bind    <$> patch3 ss <$> idBindP'
    go (RawAmbBind  (ss, _)) = AmbBind <$> patch2 ss <$> idBindP
    go (RawFunc     (_ , _)) = AnFunc  <$>               anonFuncP
    go (RawField    (_ , _)) = Field   <$>               fieldEltP
    go (RawMethod   (_ , _)) = Method  <$>               methEltP
    go (RawConstr   (_ , _)) = Constr  <$>               consEltP
    go (RawIface    (ss, _)) = Iface   <$> patch2 ss <$> iFaceP
    go (RawClass    (ss, _)) = Class   <$> patch2 ss <$> classDeclP
    go (RawTAlias   (ss, _)) = TAlias  <$> patch2 ss <$> tAliasP
    go (RawPAlias   (ss, _)) = PAlias  <$> patch2 ss <$> pAliasP
    go (RawQual     (_ , _)) = Qual    <$>               (qualifierP sortP)
    go (RawOption   (_ , _)) = Option  <$>               optionP
    go (RawInvt     (ss, _)) = Invt               ss <$> bareTypeP
    go (RawCast     (ss, _)) = CastSp             ss <$> bareTypeP
    go (RawExported (ss, _)) = return  $ Exported ss
    go (RawReadOnly (ss, _)) = return  $ RdOnly ss


patch2 ss (id,t)   = (fmap (const ss) id ,t)
patch3 ss (id,a,t) = (fmap (const ss) id ,a,t)

getSpecString :: RawSpec -> String
getSpecString = go
  where
    go (RawMeas     (_, s)) = s
    go (RawBind     (_, s)) = s
    go (RawAmbBind  (_, s)) = s
    go (RawFunc     (_, s)) = s
    go (RawIface    (_, s)) = s
    go (RawField    (_, s)) = s
    go (RawMethod   (_, s)) = s
    go (RawConstr   (_, s)) = s
    go (RawClass    (_, s)) = s
    go (RawTAlias   (_, s)) = s
    go (RawPAlias   (_, s)) = s
    go (RawQual     (_, s)) = s
    go (RawOption   (_, s)) = s
    go (RawInvt     (_, s)) = s
    go (RawCast     (_, s)) = s
    go (RawExported (_, s)) = s
    go (RawReadOnly (_, s)) = s

instance IsLocated RawSpec where
  srcPos (RawMeas     (s,_)) = s
  srcPos (RawBind     (s,_)) = s
  srcPos (RawAmbBind  (s,_)) = s
  srcPos (RawFunc     (s,_)) = s
  srcPos (RawIface    (s,_)) = s
  srcPos (RawField    (s,_)) = s
  srcPos (RawMethod   (s,_)) = s
  srcPos (RawConstr   (s,_)) = s
  srcPos (RawClass    (s,_)) = s
  srcPos (RawTAlias   (s,_)) = s
  srcPos (RawPAlias   (s,_)) = s
  srcPos (RawQual     (s,_)) = s
  srcPos (RawOption   (s,_)) = s
  srcPos (RawInvt     (s,_)) = s
  srcPos (RawCast     (s,_)) = s
  srcPos (RawExported (s,_)) = s
  srcPos (RawReadOnly (s,_)) = s


