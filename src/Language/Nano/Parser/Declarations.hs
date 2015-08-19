{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}


module Language.Nano.Parser.Declarations (
    RawSpec(..)
  ) where

import           Control.Applicative              ((*>), (<$>), (<*), (<*>))
import           Control.Monad
import           Control.Monad.Trans              (MonadIO, liftIO)
import           Data.Aeson                       (eitherDecode)
import           Data.Aeson.Types                 hiding (Error, Parser, parse)
import qualified Data.Aeson.Types                 as AI
import qualified Data.ByteString.Lazy.Char8       as B
import           Data.Char                        (isLower)
import           Data.Default
import           Data.Either                      (partitionEithers)
import qualified Data.Foldable                    as FO
import           Data.Generics                    hiding (Generic)
import           Data.Graph.Inductive.Graph
import qualified Data.HashMap.Strict              as HM
import qualified Data.HashSet                     as HS
import qualified Data.IntMap.Strict               as I
import qualified Data.List                        as L
import           Data.Maybe                       (catMaybes, fromMaybe, listToMaybe, maybeToList)
import           Data.Monoid                      (mconcat, mempty)
import qualified Data.Text                        as DT
import           Data.Traversable                 (mapAccumL)
import           Data.Tuple
import           Data.Vector                      ((!))
import           GHC.Generics
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc           (fst3, mapEither, mapFst, mapSnd)
import           Language.Fixpoint.Parse
import           Language.Fixpoint.Types          hiding (Expression, Loc, quals)
import           Language.Nano.Annots
import           Language.Nano.AST
import           Language.Nano.Core.Env
import           Language.Nano.Errors
import           Language.Nano.Liquid.Alias
import           Language.Nano.Liquid.Types
import           Language.Nano.Locations          hiding (val)
import           Language.Nano.Misc               (fst4)
import           Language.Nano.Names
import           Language.Nano.Parser.Annotations
import           Language.Nano.Parser.Common
import           Language.Nano.Program
import           Language.Nano.Typecheck.Types
import           Language.Nano.Types              hiding (Exported)
import           Language.Nano.Visitor
import           Prelude                          hiding (mapM)
import           Text.Parsec                      hiding (State, parse)
import           Text.Parsec.Error                (errorMessages, showErrorMessages)
import           Text.Parsec.Language             (emptyDef)
import           Text.Parsec.Pos                  (SourcePos, newPos)
import           Text.Parsec.Token                (identLetter, identStart)
import qualified Text.Parsec.Token                as T
import           Text.PrettyPrint.HughesPJ        (text)
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



