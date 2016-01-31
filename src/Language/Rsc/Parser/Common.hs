{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}


module Language.Rsc.Parser.Common (
    xyP, withSpan, postP
  , dot, plus, question
  , withinSpacesP
  ) where

import           Control.Applicative            ((<$>), (<*>))
import qualified Data.HashSet                   as HS
import           Data.Maybe                     (listToMaybe)
import           Language.Fixpoint.Parse
import qualified Language.Fixpoint.Types        as F
import           Language.Fixpoint.Types.Errors
import           Language.Rsc.Annotations
import           Language.Rsc.AST
import           Language.Rsc.Locations         hiding (val)
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
import           Text.Parsec                    hiding (State, parse)
import qualified Text.Parsec.Token              as T

----------------------------------------------------------------------------------
dot :: Parser String
plus :: Parser String
question :: Parser String
----------------------------------------------------------------------------------
dot        = T.dot        lexer
plus       = T.symbol     lexer "+"
question   = T.symbol     lexer "?"

----------------------------------------------------------------------------------
withinSpacesP :: Parser a -> Parser a
----------------------------------------------------------------------------------
withinSpacesP p = do { spaces; a <- p; spaces; return a }


xyP lP sepP rP
  = (\x _ y -> (x, y)) <$> lP <*> (spaces >> sepP) <*> rP


withSpan f p = do pos   <- getPosition
                  x     <- p
                  pos'  <- getPosition
                  return $ f (SS pos pos') x

postP p post = const <$> p <*> post

