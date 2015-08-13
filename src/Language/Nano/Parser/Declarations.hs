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



instance FromJSON SourcePos where
  parseJSON (Array v) = do
    v0 <- parseJSON (v!0) :: AI.Parser String
    v1 <- parseJSON (v!1) :: AI.Parser Int
    v2 <- parseJSON (v!2) :: AI.Parser Int
    return $ newPos v0 v1 v2
  parseJSON _ = error "SourcePos should only be an A.Array"

instance FromJSON (Expression (SrcSpan, [RawSpec]))
instance FromJSON (Statement (SrcSpan, [RawSpec]))
instance FromJSON (EnumElt (SrcSpan, [RawSpec]))
instance FromJSON (LValue (SrcSpan, [RawSpec]))
instance FromJSON (JavaScript (SrcSpan, [RawSpec]))
instance FromJSON (ClassElt (SrcSpan, [RawSpec]))
instance FromJSON (CaseClause (SrcSpan, [RawSpec]))
instance FromJSON (CatchClause (SrcSpan, [RawSpec]))
instance FromJSON (ForInit (SrcSpan, [RawSpec]))
instance FromJSON (ForInInit (SrcSpan, [RawSpec]))
instance FromJSON (VarDecl (SrcSpan, [RawSpec]))
instance FromJSON (Id (SrcSpan, [RawSpec]))
instance FromJSON (Prop (SrcSpan, [RawSpec]))
instance FromJSON (SrcSpan, [RawSpec])
instance FromJSON InfixOp
instance FromJSON AssignOp
instance FromJSON PrefixOp
instance FromJSON UnaryAssignOp
instance FromJSON SrcSpan
instance FromJSON RawSpec

instance ToJSON (Expression (SrcSpan, [RawSpec]))
instance ToJSON (Statement (SrcSpan, [RawSpec]))
instance ToJSON (EnumElt (SrcSpan, [RawSpec]))
instance ToJSON (LValue (SrcSpan, [RawSpec]))
instance ToJSON (JavaScript (SrcSpan, [RawSpec]))
instance ToJSON (ClassElt (SrcSpan, [RawSpec]))
instance ToJSON (CaseClause (SrcSpan, [RawSpec]))
instance ToJSON (CatchClause (SrcSpan, [RawSpec]))
instance ToJSON (ForInit (SrcSpan, [RawSpec]))
instance ToJSON (ForInInit (SrcSpan, [RawSpec]))
instance ToJSON (VarDecl (SrcSpan, [RawSpec]))
instance ToJSON (Id (SrcSpan, [RawSpec]))
instance ToJSON (Prop (SrcSpan, [RawSpec]))
instance ToJSON (SrcSpan, [RawSpec])
instance ToJSON InfixOp
instance ToJSON AssignOp
instance ToJSON PrefixOp
instance ToJSON UnaryAssignOp
instance ToJSON SrcSpan
instance ToJSON RawSpec

instance ToJSON SourcePos where
  toJSON sp = toJSON (sourceName sp, sourceLine sp, sourceColumn sp)



