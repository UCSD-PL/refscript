{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}


module Language.Nano.Parser.Lexer where

import           Prelude              hiding (mapM)
import           Text.Parsec          hiding (State, parse)
import           Text.Parsec.Language (emptyDef)
import           Text.Parsec.Token    (identLetter, identStart)
import qualified Text.Parsec.Token    as T

jsLexer    = T.makeTokenParser $ emptyDef { identStart  = letter   <|> oneOf "$_"
                                          , identLetter = alphaNum <|> oneOf "$_" }

identifier = T.identifier jsLexer
