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

instance ToJSON (Expression  (SrcSpan, [RawSpec]))
instance ToJSON (Statement   (SrcSpan, [RawSpec]))
instance ToJSON (EnumElt     (SrcSpan, [RawSpec]))
instance ToJSON (LValue      (SrcSpan, [RawSpec]))
instance ToJSON (JavaScript  (SrcSpan, [RawSpec]))
instance ToJSON (ClassElt    (SrcSpan, [RawSpec]))
instance ToJSON (CaseClause  (SrcSpan, [RawSpec]))
instance ToJSON (CatchClause (SrcSpan, [RawSpec]))
instance ToJSON (ForInit     (SrcSpan, [RawSpec]))
instance ToJSON (ForInInit   (SrcSpan, [RawSpec]))
instance ToJSON (VarDecl     (SrcSpan, [RawSpec]))
instance ToJSON (Id          (SrcSpan, [RawSpec]))
instance ToJSON (Prop        (SrcSpan, [RawSpec]))
instance ToJSON (SrcSpan, [RawSpec])
instance ToJSON InfixOp
instance ToJSON AssignOp
instance ToJSON PrefixOp
instance ToJSON UnaryAssignOp
instance ToJSON SrcSpan
instance ToJSON RawSpec

-- instance ToJSON (Expression  (SrcSpan, [RawSpec])) where
--   toJSON = genericToJSON defaultOptions



instance ToJSON SourcePos where
  toJSON sp = toJSON (sourceName sp, sourceLine sp, sourceColumn sp)
