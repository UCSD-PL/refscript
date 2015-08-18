{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}


module Language.Nano.Parser.Sorts (

    sortP

) where

import           Control.Applicative     ((<$>), (<*>))
import           Control.Monad
import           Language.Fixpoint.Parse
import           Language.Fixpoint.Types hiding (Expression, Loc, quals)
import           Prelude                 hiding (mapM)
import           Text.Parsec             hiding (State, parse)

-- import           Debug.Trace                             ( trace, traceShow)


sortP :: Parser Sort
sortP
  =   try (parens $ sortP)
  <|> try (string "@"    >> varSortP)
  -- <|> try (string "func" >> funcSortP)
 --  <|> try (fApp (Left listFTyCon) . single <$> brackets sortP)
  <|> try bvSortP
  -- <|> try baseSortP
  <|> try (fApp' <$> locLowerIdP)
  <|> try (fApp  <$> (Left <$> fTyConP) <*> sepBy sortP blanks)
  <|> (FObj . symbol <$> lowerIdP)

varSortP :: Parser Sort
varSortP  = FVar  <$> parens intP

intP :: Parser Int
intP = fromInteger <$> integer

fTyConP :: Parser FTycon
fTyConP = symbolFTycon <$> locUpperIdP

fApp' :: LocSymbol -> Sort
fApp' ls
  | s == "int"     = intSort
  | s == "Integer" = intSort
  | s == "Int"     = intSort
  | s == "int"     = intSort
  | s == "real"    = realSort
  | s == "bool"    = boolSort
  | otherwise      = fTyconSort . symbolFTycon $ ls
  where
    s              = symbolString $ val ls


