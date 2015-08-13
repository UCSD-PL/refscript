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


module Language.Nano.Parser.Common where


import           Language.Fixpoint.Types          hiding (quals, Loc, Expression)
import           Language.Fixpoint.Parse
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc                  (mapEither, mapSnd, fst3, mapFst)


import           Text.Parsec                      hiding (parse, State)
import           Text.Parsec.Error                       (errorMessages, showErrorMessages)
import qualified Text.Parsec.Token                as     T
import           Text.Parsec.Token                       (identStart, identLetter)

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

