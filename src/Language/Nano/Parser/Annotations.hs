{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}


module Language.Nano.Parser.Annotations
    (
      RawSpec(..), Spec, PSpec(..)
    , parseAnnot
    , getSpecString
    ) where

import           Control.Applicative        ((<$>))
import           Control.Monad
import           Data.Generics              hiding (Generic)
import           GHC.Generics
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Parse
import           Language.Fixpoint.Types    hiding (Expression, FI, Loc, quals)
import           Language.Nano.Annots
import           Language.Nano.AST
import           Language.Nano.Locations    hiding (val)
import           Language.Nano.Names
import           Language.Nano.Parser.Sorts
import           Language.Nano.Parser.Types
import           Language.Nano.Pretty
import           Language.Nano.Types
import           Prelude                    hiding (mapM)
import           Text.PrettyPrint.HughesPJ  (text)

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

instance PP (RawSpec) where
  pp = text . getSpecString

data PSpec l r
  = Meas     (Id l, RTypeQ RK r)
  | Bind     (Id l, Assignability, Maybe (RTypeQ RK r))
  | AmbBind  (Id l, RTypeQ RK r)
  | AnFunc   (RTypeQ RK r)
  | Field    (StaticKind, FieldInfoQ RK r)
  | Method   (StaticKind, MethodInfoQ RK r)
  | Constr   (RTypeQ RK r)
  | Iface    (TypeDeclQ RK r)
  | Class    (TypeSigQ RK r)
  | TAlias   (Id l, TAlias (RTypeQ RK r))
  | PAlias   (Id l, PAlias)
  | Qual     Qualifier
  | Option   RscOption
  | Invt     l (RTypeQ RK r)
  | CastSp   l (RTypeQ RK r)
  | Exported l
  | RdOnly   l

  -- Used only for parsing specs
  | ErrorSpec
  deriving (Data, Typeable)

type Spec = PSpec SrcSpan Reft

parseAnnot :: RawSpec -> Parser Spec
parseAnnot = go
  where
    go (RawMeas     (ss, _)) = Meas    <$> patch2 ss <$> idBindP
    go (RawBind     (ss, _)) = Bind    <$> patch3 ss <$> idBindP'
    go (RawAmbBind  (ss, _)) = AmbBind <$> patch2 ss <$> idBindP
    go (RawFunc     (_ , _)) = AnFunc  <$>               anonFuncP
    go (RawField    (_ , _)) = Field   <$>  (propP >>= \(x,s,o,m,t) -> return (s, FI o m t))
    go (RawMethod   (_ , _)) = Method  <$>  (methP >>= \(x,s,o,m,t) -> return (s, MI o m t))
    go (RawConstr   (_ , _)) = Constr  <$>               ctorP
    go (RawIface    (ss, _)) = Iface   <$>               interfaceP
    go (RawClass    (ss, _)) = Class   <$>               classDeclP
    go (RawTAlias   (ss, _)) = TAlias  <$> patch2 ss <$> tAliasP
    go (RawPAlias   (ss, _)) = PAlias  <$> patch2 ss <$> pAliasP
    go (RawQual     (_ , _)) = Qual    <$>               qualifierP sortP
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

