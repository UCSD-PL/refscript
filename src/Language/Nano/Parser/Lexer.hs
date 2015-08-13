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


module Language.Nano.Parser.Lexer

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

jsLexer    = T.makeTokenParser $ emptyDef { identStart  = letter   <|> oneOf "$_"
                                          , identLetter = alphaNum <|> oneOf "$_" }

identifier = T.identifier jsLexer
