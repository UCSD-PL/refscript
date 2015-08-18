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


module Language.Nano.Parser.Lexer where

import           Control.Applicative             ((*>), (<$>), (<*), (<*>))
import           Control.Monad
import           Control.Monad.Trans             (MonadIO, liftIO)
import           Data.Aeson                      (eitherDecode)
import           Data.Aeson.Types                hiding (Error, Parser, parse)
import qualified Data.Aeson.Types                as AI
import qualified Data.ByteString.Lazy.Char8      as B
import           Data.Char                       (isLower)
import           Data.Default
import           Data.Either                     (partitionEithers)
import qualified Data.Foldable                   as FO
import           Data.Generics                   hiding (Generic)
import           Data.Graph.Inductive.Graph
import qualified Data.HashMap.Strict             as HM
import qualified Data.HashSet                    as HS
import qualified Data.IntMap.Strict              as I
import qualified Data.List                       as L
import           Data.Maybe                      (catMaybes, fromMaybe, listToMaybe, maybeToList)
import           Data.Monoid                     (mconcat, mempty)
import qualified Data.Text                       as DT
import           Data.Traversable                (mapAccumL)
import           Data.Tuple
import           Data.Vector                     ((!))
import           GHC.Generics
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc          (fst3, mapEither, mapFst, mapSnd)
import           Language.Fixpoint.Parse
import           Language.Fixpoint.Types         hiding (Expression, Loc, quals)
import           Language.Nano.Annots
import           Language.Nano.AST
import           Language.Nano.Env
import           Language.Nano.Errors
import           Language.Nano.Liquid.Alias
import           Language.Nano.Liquid.Qualifiers
import           Language.Nano.Liquid.Types
import           Language.Nano.Locations         hiding (val)
import           Language.Nano.Misc              (fst4)
import           Language.Nano.Names
import           Language.Nano.Program
import           Language.Nano.Typecheck.Types
import           Language.Nano.Types             hiding (Exported)
import           Language.Nano.Visitor
import           Prelude                         hiding (mapM)
import           Text.Parsec                     hiding (State, parse)
import           Text.Parsec.Error               (errorMessages, showErrorMessages)
import           Text.Parsec.Language            (emptyDef)
import           Text.Parsec.Pos                 (SourcePos, newPos)
import           Text.Parsec.Token               (identLetter, identStart)
import qualified Text.Parsec.Token               as T
import           Text.PrettyPrint.HughesPJ       (text)

jsLexer    = T.makeTokenParser $ emptyDef { identStart  = letter   <|> oneOf "$_"
                                          , identLetter = alphaNum <|> oneOf "$_" }

identifier = T.identifier jsLexer
