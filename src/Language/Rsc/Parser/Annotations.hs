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


module Language.Rsc.Parser.Annotations (
      RawSpec(..), Spec, PSpec(..)
    , PContext
    , parseSpec
    , getSpecString
    ) where

import           Control.Applicative       ((<$>))
import           Control.Monad
import           Data.Generics             hiding (Generic)
import           GHC.Generics
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Parse
import           Language.Fixpoint.Types   hiding (Expression, FI, Loc, Located, quals)
import           Language.Rsc.AST
import           Language.Rsc.Locations    hiding (val)
import           Language.Rsc.Names
import           Language.Rsc.Options
-- import           Language.Rsc.Parser.Sorts
import           Language.Rsc.Parser.Types
import           Language.Rsc.Pretty
import           Language.Rsc.Types
import           Prelude                   hiding (mapM)
import           Text.PrettyPrint.HughesPJ (text)


--------------------------------------------------------------------------------
-- | Specifications
--------------------------------------------------------------------------------

data RawSpec
  -- Specifications
  = FunctionDeclarationRawSpec (SrcSpan, String)
  | VariableDeclarationRawSpec (SrcSpan, String)
  | FunctionExpressionRawSpec  (SrcSpan, String)
  | InterfaceRawSpec           (SrcSpan, String)
  | ClassRawSpec               (SrcSpan, String)
  | ModuleRawSpec              (SrcSpan, String)
  | FieldRawSpec               (SrcSpan, String)
  | MethodRawSpec              (SrcSpan, String)
  | ConstructorRawSpec         (SrcSpan, String)
  | CastRawSpec                (SrcSpan, String)
  | ExportRawSpec              (SrcSpan, String)
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
  | ModuleSpec              RelPath
  | FieldSpec               (TypeMemberQ RK r)
  | MethodSpec              (TypeMemberQ RK r)
  | ConstructorSpec         (RTypeQ RK r)
  | CastSpec                l (RTypeQ RK r)
  | ExportedSpec
  -- Global
  | MeasureSpec             (Id l, RTypeQ RK r)
  | TypeAliasSpec           (Id l, TAlias (RTypeQ RK r))
  | PredicateAliasSpec      (Id l, PAlias)
  | QualifierSpec           Qualifier
  | InvariantSpec           l (RTypeQ RK r)
  | OptionSpec              (Located String)
  -- Used only for parsing specs
  | ErrorSpec
  deriving (Data, Typeable)

type Spec = PSpec SrcSpan Reft

instance PP Spec where
  pp FunctionDeclarationSpec{} = text "FunctionDeclarationSpec"
  pp VariableDeclarationSpec{} = text "VariableDeclarationSpec"
  pp FunctionExpressionSpec{}  = text "FunctionExpressionSpec"
  pp InterfaceSpec{}           = text "InterfaceSpec"
  pp ClassSpec  {}             = text "ClassSpec"
  pp FieldSpec{}               = text "FieldSpec"
  pp MethodSpec{}              = text "MethodSpec"
  pp ConstructorSpec{}         = text "ConstructorSpec"
  pp CastSpec{}                = text "CastSpec"
  pp ExportedSpec{}            = text "ExportedSpec"
  pp MeasureSpec{}             = text "MeasureSpec"
  pp TypeAliasSpec{}           = text "TypeAliasSpec"
  pp PredicateAliasSpec{}      = text "PredicateAliasSpec"
  pp QualifierSpec{}           = text "QualifierSpec"
  pp InvariantSpec{}           = text "InvariantSpec"
  pp OptionSpec{}              = text "OptionSpec"
  pp ErrorSpec{}               = text "ErrorSpec"

parseSpec :: PContext -> RawSpec -> Parser Spec
parseSpec ctx = go
  where
    go (FunctionDeclarationRawSpec (ss, _)) = FunctionDeclarationSpec <$> patch2 ss <$> idBindP ctx
    go (VariableDeclarationRawSpec (ss, _)) = VariableDeclarationSpec <$> patch3 ss <$> idBindP' ctx
    go (FunctionExpressionRawSpec  (_ , _)) = FunctionExpressionSpec  <$> functionExpressionP ctx
    go (InterfaceRawSpec           (_ , _)) = InterfaceSpec <$> interfaceP
    go (ClassRawSpec               (_ , _)) = ClassSpec     <$> classDeclP
    go (ModuleRawSpec              (_ , _)) = ModuleSpec    <$> moduleDeclP
    go (FieldRawSpec               (_ , _)) = FieldSpec     <$> (propP ctx >>= \(_,_,o,m,t) -> return (FI o m t))
    go (MethodRawSpec              (_ , _)) = MethodSpec    <$> (methP ctx >>= \(_,_,o,m,t) -> return (MI o [(m, t)]))
    go (ConstructorRawSpec         (_ , _)) = ConstructorSpec    <$> ctorP ctx
    go (CastRawSpec                (ss, _)) = CastSpec ss        <$> bareTypeP ctx
    go (ExportRawSpec              (_ , _)) = return ExportedSpec
    go (MeasureRawSpec             (ss, _)) = MeasureSpec        <$> patch2 ss <$> idBindP ctx
    go (TypeAliasRawSpec           (ss, _)) = TypeAliasSpec      <$> patch2 ss <$> tAliasP
    go (PredicateAliasRawSpec      (ss, _)) = PredicateAliasSpec <$> patch2 ss <$> pAliasP
    go (QualifierRawSpec           (_ , _)) = QualifierSpec      <$> qualifierP sortP
    go (InvariantRawSpec           (ss, _)) = InvariantSpec ss   <$> bareTypeP ctx
    go (OptionRawSpec              (ss, o)) = -- OptionSpec         <$> optionP
                                              return   $ OptionSpec         (Loc ss o)

    patch2 ss (i,t)   = (fmap (const ss) i,t)
    patch3 ss (i,a,t) = (fmap (const ss) i,a,t)

getSpecString :: RawSpec -> String
getSpecString = go
  where
    go (FunctionDeclarationRawSpec (_, s)) = s
    go (VariableDeclarationRawSpec (_, s)) = s
    go (FunctionExpressionRawSpec  (_, s)) = s
    go (InterfaceRawSpec           (_, s)) = s
    go (ClassRawSpec               (_, s)) = s
    go (ModuleRawSpec              (_, s)) = s
    go (FieldRawSpec               (_, s)) = s
    go (MethodRawSpec              (_, s)) = s
    go (ConstructorRawSpec         (_, s)) = s
    go (CastRawSpec                (_, s)) = s
    go (ExportRawSpec              (_, s)) = s
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
  srcPos (ModuleRawSpec              (s,_)) = s
  srcPos (FieldRawSpec               (s,_)) = s
  srcPos (MethodRawSpec              (s,_)) = s
  srcPos (ConstructorRawSpec         (s,_)) = s
  srcPos (CastRawSpec                (s,_)) = s
  srcPos (ExportRawSpec              (s,_)) = s
  srcPos (MeasureRawSpec             (s,_)) = s
  srcPos (TypeAliasRawSpec           (s,_)) = s
  srcPos (PredicateAliasRawSpec      (s,_)) = s
  srcPos (QualifierRawSpec           (s,_)) = s
  srcPos (InvariantRawSpec           (s,_)) = s
  srcPos (OptionRawSpec              (s,_)) = s

