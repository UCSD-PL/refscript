{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances         #-} 
{-# LANGUAGE UndecidableInstances      #-} 
{-# LANGUAGE TypeSynonymInstances      #-} 
{-# LANGUAGE TupleSections             #-}

module Language.Haskell.Liquid.Parse (
  hsSpecificationP
) where

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

import Language.Nano.Liquid.Types

dot        = Token.dot        lexer
braces     = Token.braces     lexer
angles     = Token.angles     lexer

----------------------------------------------------------------------------------
-- | BareTypes -------------------------------------------------------------------
----------------------------------------------------------------------------------

-- Top-level parser for "bare" types. If refinements not supplied, then "top" refinement is used.

bareTypeP :: Parser BareType 

bareTypeP   
  =  try bareFunP
 <|> bareAllP
 <|> bareAtomP 

bareFunP  
  = do args   <- parens $ sepBy bareTypeP comma
       reserved "=>" 
       ret    <- bareTypeP 
       return $ TFun args ret

bareAtomP 
  =  refP bbaseP 
 <|> try (dummyP (bbaseP <* spaces))

bbaseP :: Parser (Reft -> BareType)
bbaseP 
  =  try (liftM2 bAppTy lowerIdP bareTyArgP)
 <|> try (liftM2 bRVar lowerIdP monoPredicateP)
 <|> liftM3 bCon upperIdP predicatesP (sepBy bareTyArgP blanks)

bbaseNoAppP :: Parser (Reft -> BareType)
bbaseNoAppP
  =  liftM2 bLst (brackets bareTypeP) predicatesP
 <|> liftM2 bTup (parens $ sepBy bareTypeP comma) predicatesP
 <|> try (liftM3 bCon upperIdP predicatesP (return []))
 <|> liftM2 bRVar lowerIdP monoPredicateP 

bareTyArgP 
  =  try (braces $ liftM RExprArg exprP)
 <|> try bareAtomNoAppP
 <|> try (parens bareTypeP)

bareAtomNoAppP 
  =  refP bbaseNoAppP 
 <|> try (dummyP (bbaseNoAppP <* spaces))

bareAllP 
  = do reserved "forall"
       as <- many tyVarIdP
       ps <- predVarDefsP
       dot
       t  <- bareTypeP
       return $ foldr RAllT (foldr RAllP t ps) as

tyVarIdP :: Parser String
tyVarIdP = condIdP alphanums (isLower . head) 
           where alphanums = ['a'..'z'] ++ ['0'..'9']

xyP lP sepP rP
  = liftM3 (\x _ y -> (x, y)) lP (spaces >> sepP) rP

data ArrowSym = ArrowFun | ArrowPred

positionNameP = dummyNamePos <$> getPosition
  
dummyNamePos pos  = "dummy." ++ name ++ ['.'] ++ line ++ ['.'] ++ col
    where name    = san <$> sourceName pos
          line    = show $ sourceLine pos  
          col     = show $ sourceColumn pos  
          san '/' = '.'
          san c   = toLower c

dummyBindP 
  = stringSymbol <$> positionNameP

bbindP = lowerIdP <* dcolon 

bindP  = liftM stringSymbol (lowerIdP <* colon)

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


filePathP :: Parser FilePath
filePathP = angles $ many1 pathCharP
  where pathCharP = choice $ char <$> pathChars 
        pathChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['.', '/']

tyBindP    :: Parser (LocSymbol, BareType)
tyBindP    = xyP (locParserP binderP) dcolon bareTypeP

locParserP :: Parser a -> Parser (Located a)
locParserP p = liftM2 Loc getPosition p

embedP 
  = xyP upperIdP (reserved "as") fTyConP

binderP :: Parser Symbol
binderP =  try $ liftM stringSymbol (idP badc)
       <|> liftM pwr (parens (idP bad))
       where idP p  = many1 (satisfy (not . p))
             badc c = (c == ':') ||  bad c
             bad c  = isSpace c || c `elem` "()"
             pwr s  = stringSymbol $ "(" ++ s ++ ")" 
             
grabs p = try (liftM2 (:) p (grabs p)) 
       <|> return []

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

-- specWrap  = between     (string "{-@" >> spaces) (spaces >> string "@-}")
specWraps = betweenMany (string "{-@" >> spaces) (spaces >> string "@-}")

