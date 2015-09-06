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


module Language.Rsc.Parser.Annotations
    ( RawSpec(..), Spec, PSpec(..)
    , parseSpec
    , getSpecString
    ) where

import           Control.Applicative       ((<$>))
import           Control.Monad
import           Data.Generics             hiding (Generic)
import           GHC.Generics
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Parse
import           Language.Fixpoint.Types   hiding (Expression, FI, Loc, quals)
import           Language.Rsc.AST
import           Language.Rsc.Locations    hiding (val)
import           Language.Rsc.Names
import           Language.Rsc.Options
import           Language.Rsc.Parser.Sorts
import           Language.Rsc.Parser.Types
import           Language.Rsc.Pretty
import           Language.Rsc.Types
import           Prelude                   hiding (mapM)
import           Text.PrettyPrint.HughesPJ (text)


---------------------------------------------------------------------------------
-- | Specifications
---------------------------------------------------------------------------------

data RawSpec
  -- Specifications
  = FunctionDeclarationRawSpec (SrcSpan, String)
  | VariableDeclarationRawSpec (SrcSpan, String)
  | FunctionExpressionRawSpec  (SrcSpan, String)
  | InterfaceRawSpec           (SrcSpan, String)
  | ClassRawSpec               (SrcSpan, String)
  | FieldRawSpec               (SrcSpan, String)
  | MethodRawSpec              (SrcSpan, String)
  | ConstructorRawSpec         (SrcSpan, String)
  | CastRawSpec                (SrcSpan, String)
  -- Global
  | MeasureRawSpec             (SrcSpan, String)
  | TypeAliasRawSpec           (SrcSpan, String)
  | PredicateAliasRawSpec      (SrcSpan, String)
  | QualifierRawSpec           (SrcSpan, String)
  | InvariantRawSpec           (SrcSpan, String)
  | OptionRawSpec              (SrcSpan, String)
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

instance PP (RawSpec) where
  pp = text . getSpecString

data PSpec l r
  = FunctionDeclarationSpec (Id l, RTypeQ RK r)
  | VariableDeclarationSpec (Id l, Assignability, Maybe (RTypeQ RK r))
  | FunctionExpressionSpec  (RTypeQ RK r)
  | InterfaceSpec           (TypeDeclQ RK r)
  | ClassSpec               (TypeSigQ RK r)
  | FieldSpec               (FieldInfoQ RK r)
  | MethodSpec              (MethodInfoQ RK r)
  | ConstructorSpec         (RTypeQ RK r)
  | CastSpec                l (RTypeQ RK r)
  -- Global
  | MeasureSpec             (Id l, RTypeQ RK r)
  | TypeAliasSpec           (Id l, TAlias (RTypeQ RK r))
  | PredicateAliasSpec      (Id l, PAlias)
  | QualifierSpec           Qualifier
  | InvariantSpec           l (RTypeQ RK r)
  | OptionSpec              RscOption
  -- Used only for parsing specs
  | ErrorSpec
  deriving (Data, Typeable)

type Spec = PSpec SrcSpan Reft

parseSpec :: RawSpec -> Parser Spec
parseSpec = go
  where
    go (FunctionDeclarationRawSpec (ss, _)) = FunctionDeclarationSpec <$> patch2 ss <$> idBindP
    go (VariableDeclarationRawSpec (ss, _)) = VariableDeclarationSpec <$> patch3 ss <$> idBindP'
    go (FunctionExpressionRawSpec  (ss, _)) = FunctionExpressionSpec  <$> functionExpressionP
    go (InterfaceRawSpec           (ss, _)) = InterfaceSpec           <$> interfaceP
    go (ClassRawSpec               (ss, _)) = ClassSpec               <$> classDeclP
    go (FieldRawSpec               (ss, _)) = FieldSpec               <$> (propP >>= \(_,_,o,m,t) -> return (FI o m t))
    go (MethodRawSpec              (ss, _)) = MethodSpec              <$> (methP >>= \(_,_,o,m,t) -> return (MI o m t))
    go (ConstructorRawSpec         (ss, _)) = ConstructorSpec         <$> ctorP
    go (CastRawSpec                (ss, _)) = CastSpec ss             <$> bareTypeP
    go (MeasureRawSpec             (ss, _)) = MeasureSpec             <$> patch2 ss <$> idBindP
    go (TypeAliasRawSpec           (ss, _)) = TypeAliasSpec           <$> patch2 ss <$> tAliasP
    go (PredicateAliasRawSpec      (ss, _)) = PredicateAliasSpec      <$> patch2 ss <$> pAliasP
    go (QualifierRawSpec           (ss, _)) = QualifierSpec           <$> qualifierP sortP
    go (InvariantRawSpec           (ss, _)) = InvariantSpec ss        <$> bareTypeP
    go (OptionRawSpec              (ss, _)) = OptionSpec              <$> optionP

    patch2 ss (id,t)   = (fmap (const ss) id ,t)
    patch3 ss (id,a,t) = (fmap (const ss) id ,a,t)

getSpecString :: RawSpec -> String
getSpecString = go
  where
    go (FunctionDeclarationRawSpec (_, s)) = s
    go (VariableDeclarationRawSpec (_, s)) = s
    go (FunctionExpressionRawSpec  (_, s)) = s
    go (InterfaceRawSpec           (_, s)) = s
    go (ClassRawSpec               (_, s)) = s
    go (FieldRawSpec               (_, s)) = s
    go (MethodRawSpec              (_, s)) = s
    go (ConstructorRawSpec         (_, s)) = s
    go (CastRawSpec                (_, s)) = s
    go (MeasureRawSpec             (_, s)) = s
    go (TypeAliasRawSpec           (_, s)) = s
    go (PredicateAliasRawSpec      (_, s)) = s
    go (QualifierRawSpec           (_, s)) = s
    go (InvariantRawSpec           (_, s)) = s
    go (OptionRawSpec              (_, s)) = s

instance IsLocated RawSpec where
  srcPos (FunctionDeclarationRawSpec (s,_)) = s
  srcPos (VariableDeclarationRawSpec (s,_)) = s
  srcPos (FunctionExpressionRawSpec  (s,_)) = s
  srcPos (InterfaceRawSpec           (s,_)) = s
  srcPos (ClassRawSpec               (s,_)) = s
  srcPos (FieldRawSpec               (s,_)) = s
  srcPos (MethodRawSpec              (s,_)) = s
  srcPos (ConstructorRawSpec         (s,_)) = s
  srcPos (CastRawSpec                (s,_)) = s
  srcPos (MeasureRawSpec             (s,_)) = s
  srcPos (TypeAliasRawSpec           (s,_)) = s
  srcPos (PredicateAliasRawSpec      (s,_)) = s
  srcPos (QualifierRawSpec           (s,_)) = s
  srcPos (InvariantRawSpec           (s,_)) = s
  srcPos (OptionRawSpec              (s,_)) = s
