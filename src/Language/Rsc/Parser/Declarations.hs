{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}


module Language.Rsc.Parser.Declarations (
    RawSpec(..)
  ) where

import           Control.Monad
import           Data.Aeson                      ()
import           Data.Aeson.Types                hiding (Error, Parser, parse)
import qualified Data.Aeson.Types                as AI
import           Data.Vector                     ((!))
import           Language.Fixpoint.Types.Errors
import           Language.Rsc.AST
import           Language.Rsc.Names
import           Language.Rsc.Parser.Annotations
import           Prelude                         hiding (mapM)
import           Text.Parsec                     hiding (State, parse)
import           Text.Parsec.Pos                 (newPos)

instance FromJSON SourcePos where
  parseJSON (Array v) = do
    v0 <- parseJSON (v!0) :: AI.Parser String
    v1 <- parseJSON (v!1) :: AI.Parser Int
    v2 <- parseJSON (v!2) :: AI.Parser Int
    return $ newPos v0 v1 v2
  parseJSON _ = error "SourcePos should only be an A.Array"

instance FromJSON (Expression  (SrcSpan, [RawSpec]))
instance FromJSON (Statement   (SrcSpan, [RawSpec]))
instance FromJSON (EnumElt     (SrcSpan, [RawSpec]))
instance FromJSON (LValue      (SrcSpan, [RawSpec]))
instance FromJSON (JavaScript  (SrcSpan, [RawSpec]))
instance FromJSON (ClassElt    (SrcSpan, [RawSpec]))
instance FromJSON (CaseClause  (SrcSpan, [RawSpec]))
instance FromJSON (CatchClause (SrcSpan, [RawSpec]))
instance FromJSON (ForInit     (SrcSpan, [RawSpec]))
instance FromJSON (ForInInit   (SrcSpan, [RawSpec]))
instance FromJSON (VarDecl     (SrcSpan, [RawSpec]))
instance FromJSON (Id          (SrcSpan, [RawSpec]))
instance FromJSON (Prop        (SrcSpan, [RawSpec]))
instance FromJSON (SrcSpan, [RawSpec])
instance FromJSON InfixOp
instance FromJSON AssignOp
instance FromJSON PrefixOp
instance FromJSON UnaryAssignOp
instance FromJSON SrcSpan
instance FromJSON RawSpec

instance ToJSON (Expression  (SrcSpan, [RawSpec])) where
  toJSON = genericToJSON defaultOptions
instance ToJSON (Statement   (SrcSpan, [RawSpec])) where
  toJSON = genericToJSON defaultOptions
instance ToJSON (EnumElt     (SrcSpan, [RawSpec])) where
  toJSON = genericToJSON defaultOptions
instance ToJSON (LValue      (SrcSpan, [RawSpec])) where
  toJSON = genericToJSON defaultOptions
instance ToJSON (JavaScript  (SrcSpan, [RawSpec])) where
  toJSON = genericToJSON defaultOptions
instance ToJSON (ClassElt    (SrcSpan, [RawSpec])) where
  toJSON = genericToJSON defaultOptions
instance ToJSON (CaseClause  (SrcSpan, [RawSpec])) where
  toJSON = genericToJSON defaultOptions
instance ToJSON (CatchClause (SrcSpan, [RawSpec])) where
  toJSON = genericToJSON defaultOptions
instance ToJSON (ForInit     (SrcSpan, [RawSpec])) where
  toJSON = genericToJSON defaultOptions
instance ToJSON (ForInInit   (SrcSpan, [RawSpec])) where
  toJSON = genericToJSON defaultOptions
instance ToJSON (VarDecl     (SrcSpan, [RawSpec])) where
  toJSON = genericToJSON defaultOptions
instance ToJSON (Id          (SrcSpan, [RawSpec])) where
  toJSON = genericToJSON defaultOptions
instance ToJSON (Prop        (SrcSpan, [RawSpec])) where
  toJSON = genericToJSON defaultOptions
instance ToJSON (SrcSpan, [RawSpec]) where
  toJSON = genericToJSON defaultOptions
instance ToJSON InfixOp where
  toJSON = genericToJSON defaultOptions
instance ToJSON AssignOp where
  toJSON = genericToJSON defaultOptions
instance ToJSON PrefixOp where
  toJSON = genericToJSON defaultOptions
instance ToJSON UnaryAssignOp where
  toJSON = genericToJSON defaultOptions
instance ToJSON SrcSpan where
  toJSON = genericToJSON defaultOptions
instance ToJSON RawSpec where
  toJSON = genericToJSON defaultOptions

instance ToJSON SourcePos where
  toJSON sp = toJSON (sourceName sp, sourceLine sp, sourceColumn sp)
