{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}


module Language.Rsc.Parser.Common
    (
      xyP, axyP, withSpan, postP
    , dot, plus, question
    , withinSpacesP
    ) where

import           Control.Applicative          ((<$>), (<*>))
import qualified Data.HashSet                 as HS
import           Data.Maybe                   (listToMaybe)
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Parse
import qualified Language.Fixpoint.Types      as F
import           Language.Rsc.Annots
import           Language.Rsc.AST
import           Language.Rsc.Locations       hiding (val)
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
import           Text.Parsec                  hiding (State, parse)
import qualified Text.Parsec.Token            as T

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


assignabilityP
  =  try (withinSpacesP (reserved "global"  ) >> return WriteGlobal)
 <|> try (withinSpacesP (reserved "local"   ) >> return WriteLocal )
 <|> try (withinSpacesP (reserved "readonly") >> return Ambient    )
 <|>     (return WriteGlobal)

axyP lP sepP rP
  = do  a <- assignabilityP
        i <- withinSpacesP lP
        spaces >> sepP
        r <- rP
        return (i,a,r)

withSpan f p = do pos   <- getPosition
                  x     <- p
                  pos'  <- getPosition
                  return $ f (SS pos pos') x

postP p post = const <$> p <*> post


