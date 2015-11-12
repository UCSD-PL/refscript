{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}



-- TODO: throw away

module Language.Rsc.Parser.Sorts (

    sortP_

) where

import           Control.Applicative     ((<$>), (<*>))
import           Control.Monad
import           Language.Fixpoint.Parse
import           Language.Fixpoint.Types hiding (Expression, Loc, quals)
import           Prelude                 hiding (mapM)
import           Text.Parsec             hiding (State, parse)

-- import           Debug.Trace                             ( trace, traceShow)


-- OLD CODE -- sortP_ :: Parser Sort
-- OLD CODE -- sortP_
-- OLD CODE --   =   try (parens $ sortP_)
-- OLD CODE --   <|> try (string "@"    >> varSortP)
-- OLD CODE --   -- <|> try (string "func" >> funcSortP)
-- OLD CODE --   -- <|> try (fApp (Left listFTyCon) . single <$> brackets sortP_)
-- OLD CODE --   <|> try bvSortP
-- OLD CODE --   -- <|> try baseSortP
-- OLD CODE --   <|> try (fApp' <$> locLowerIdP)
-- OLD CODE --   <|> try (fApp  <$> (Left <$> fTyConP) <*> sepBy sortP_ blanks)
-- OLD CODE --   <|> (FObj . symbol <$> lowerIdP)

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
    s              = symbolSafeString $ val ls


