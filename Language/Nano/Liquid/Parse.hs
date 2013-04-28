{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances         #-} 
{-# LANGUAGE UndecidableInstances      #-} 
{-# LANGUAGE TypeSynonymInstances      #-} 
{-# LANGUAGE TupleSections             #-}

module Language.Haskell.Liquid.Parse () where

import Control.Monad
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as Token
import qualified Data.HashMap.Strict as M

import Control.Applicative ((<$>), (<*), (<*>))
import Data.Char (toLower, isLower, isSpace, isAlpha)
import Data.List (partition)
import Data.Monoid (mempty)

import Language.Fixpoint.Types
import Language.Fixpoint.Parse 

import Language.Nano.Types
import Language.Nano.Liquid.Types

dot        = Token.dot        lexer
braces     = Token.braces     lexer
angles     = Token.angles     lexer

----------------------------------------------------------------------------------
-- | RefTypes -------------------------------------------------------------------
----------------------------------------------------------------------------------

-- Top-level parser for "bare" types. If refinements not supplied, then "top" refinement is used.

bareTypeP :: Parser RefType 

bareTypeP   
  =  try bareFunP
 <|> try bareAllP
 <|> bareAtomP 

bareFunP  
  = do args   <- parens $ sepBy bareTypeP comma
       reserved "=>" 
       ret    <- bareTypeP 
       return $ TFun args ret

bareAtomP 
  =  refP bbaseP 
 <|> try (dummyP (bbaseP <* spaces))

bbaseP :: Parser (Reft -> RefType)
bbaseP 
  =  try (TVar <$> tvarP)
 <|> TApp <$> tconP <*> (brackets $ sepBy bareTypeP comma)

tvarP :: Parser TVar
tvarP = TV <$> locParserP (stringSymbol <$> upperIdP) 

tconP :: Parser TCon
tconP =  try (reserved "int"  >> return TInt)
     <|> try (reserved "bool" >> return TBool)
     <|> try (reserved "void" >> return TVoid)
     <|> (TDef . stringSymbol) <$> lowerIdP

bareAllP 
  = do reserved "forall"
       as <- many tvarP
       dot
       t  <- bareTypeP
       return $ foldr TAll t as


locParserP :: Parser a -> Parser (Located a)
locParserP p = liftM2 Loc getPosition p
  
dummyP ::  Monad m => m (Reft -> b) -> m b
dummyP fm 
  = fm `ap` return top 

refP :: Parser (Reft -> a) -> Parser a
refP kindP
  = braces $ do
      v   <- symbolP 
      colon
      t   <- kindP
      reserved "|"
      ras <- refasP 
      return $ t (Reft (v, ras))

refasP :: Parser [Refa]
refasP  =  (try (brackets $ sepBy (RConc <$> predP) semi)) 
       <|> liftM ((:[]) . RConc) predP

------------------------------------------------------------------------
----------------------- Wrapped Constructors ---------------------------
------------------------------------------------------------------------

-- filePathP :: Parser FilePath
-- filePathP = angles $ many1 pathCharP
--   where pathCharP = choice $ char <$> pathChars 
--         pathChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['.', '/']
-- 
-- xyP lP sepP rP
--   = liftM3 (\x _ y -> (x, y)) lP (spaces >> sepP) rP
-- 
-- tyBindP    :: Parser (LocSymbol, RefType)
-- tyBindP    = xyP (locParserP binderP) dcolon bareTypeP
-- 
-- embedP     = xyP upperIdP (reserved "as") fTyConP
-- 
-- binderP :: Parser Symbol
-- binderP =  try $ liftM stringSymbol (idP badc)
--        <|> liftM pwr (parens (idP bad))
--        where idP p  = many1 (satisfy (not . p))
--              badc c = (c == ':') ||  bad c
--              bad c  = isSpace c || c `elem` "()"
--              pwr s  = stringSymbol $ "(" ++ s ++ ")" 
--              
-- grabs p = try (liftM2 (:) p (grabs p)) 
--        <|> return []

---------------------------------------------------------------------
------------ Interacting with Fixpoint ------------------------------
---------------------------------------------------------------------

grabUpto p  
  =  try (lookAhead p >>= return . Just)
 <|> try (eof         >> return Nothing)
 <|> (anyChar >> grabUpto p)

betweenMany leftP rightP p 
  = do z <- grabUpto leftP
       case z of
         Just _  -> liftM2 (:) (between leftP rightP p) (betweenMany leftP rightP p)
         Nothing -> return []

specWraps = betweenMany (string "/*@" >> spaces) (spaces >> string "@*/")

---------------------------------------------------------------------------------

instance Inputable RefType where 
  rr' = doParse' bareTypeP

instance Inputable Type where 
  rr' = doParse' (fmap (const ()) <$> bareTypeP)

