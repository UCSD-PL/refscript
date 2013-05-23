{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances         #-} 
{-# LANGUAGE UndecidableInstances      #-} 
{-# LANGUAGE TypeSynonymInstances      #-} 
{-# LANGUAGE TupleSections             #-}

module Language.Nano.Typecheck.Parse (
    parseNanoFromFile 
  ) where

import           Data.Maybe (fromMaybe)
import           Data.Generics.Aliases
import           Data.Generics.Schemes
import           Control.Monad
import           Text.Parsec
import           Text.Parsec.String hiding (parseFromFile)
import qualified Text.Parsec.Token as Token
import           Control.Applicative ((<$>), (<*), (<*>))
import           Data.Char (toLower, isLower) 
import           Data.Monoid (mconcat)

import           Language.Fixpoint.Names (propConName)
import           Language.Fixpoint.Misc (errorstar, mapSnd)
import           Language.Fixpoint.Types hiding (quals)
import           Language.Fixpoint.Parse 

import           Language.Nano.Errors
import           Language.Nano.Files
import           Language.Nano.Types
import           Language.Nano.Typecheck.Types
import           Language.Nano.Liquid.Types
import           Language.Nano.Env

import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Parser        (parseJavaScriptFromFile)

dot        = Token.dot        lexer
braces     = Token.braces     lexer
-- angles     = Token.angles     lexer

----------------------------------------------------------------------------------
-- | Type Binders ----------------------------------------------------------------
----------------------------------------------------------------------------------

idBindP :: Parser (Id SourcePos, RefType)
idBindP = xyP identifierP dcolon bareTypeP

identifierP :: Parser (Id SourcePos)
identifierP = Id <$> getPosition <*> lowerIdP -- Lexer.identifier

annotP      :: Parser AnnType 
annotP      = (`Ann` []) <$> getPosition

xyP lP sepP rP
  = (\x _ y -> (x, y)) <$> lP <*> (spaces >> sepP) <*> rP


----------------------------------------------------------------------------------
-- | RefTypes -------------------------------------------------------------------
----------------------------------------------------------------------------------

-- Top-level parser for "bare" types. If refinements not supplied, then "top" refinement is used.

bareTypeP :: Parser RefType 

bareTypeP   
  =  try bareAllP
 <|> try bareFunP
 <|> bareAtomP 

bareFunP  
  = do args   <- parens $ sepBy bareTypeP comma
       reserved "=>" 
       ret    <- bareTypeP 
       return $ TFun args ret

bareAtomP 
  =  refP bbaseP 
 <|> try (bindRefP bbaseP)
 <|> try (dummyP (bbaseP <* spaces))

bbaseP :: Parser (Reft -> RefType)
bbaseP 
  =  try (TVar <$> tvarP)
 <|> try (TApp <$> tconP <*> (brackets $ sepBy bareTypeP comma))
 <|> try ((`TApp` []) <$> tconP)

tvarP :: Parser TVar
tvarP = TV <$> (stringSymbol <$> upperWordP) 

upperWordP :: Parser String
upperWordP = condIdP nice (not . isLower . head)
  where 
    nice   = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0'..'9']

tconP :: Parser TCon
tconP =  try (reserved "int"       >> return TInt)
     <|> try (reserved "boolean"   >> return TBool)
     <|> try (reserved "void"      >> return TVoid)
     <|> (TDef . stringSymbol)  <$> lowerIdP

bareAllP 
  = do reserved "forall"
       as <- many1 tvarP
       dot
       t  <- bareTypeP
       return $ foldr TAll t as


locParserP :: Parser a -> Parser (Located a)
locParserP p = liftM2 Loc getPosition p
  
dummyP ::  Monad m => m (Reft -> b) -> m b
dummyP fm 
  = fm `ap` return top 

bindRefP :: Parser (Reft -> a) -> Parser a
bindRefP kindP
  = do v <- symbolP 
       colon
       t <- kindP
       return $ t (Reft (v, []))

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

specWraps :: Parser a -> Parser [a] 
specWraps = betweenMany start stop
  where 
    start = string "/*@" >> spaces
    stop  = spaces >> string "*/"

---------------------------------------------------------------------------------
-- | Specifications
---------------------------------------------------------------------------------
data PSpec l t 
  = Meas (Id l, t)
  | Bind (Id l, t) 
  | Qual Qualifier
  deriving (Show)

specP :: Parser (PSpec SourcePos RefType)
specP 
  = try (reserved "measure"   >> (Meas <$> idBindP   ))
    <|> (reserved "qualif"    >> (Qual <$> qualifierP))
    <|> ({- DEFAULT -}           (Bind <$> idBindP   ))


--------------------------------------------------------------------------------------
parseSpecFromFile :: FilePath -> IO (Nano SourcePos RefType) 
--------------------------------------------------------------------------------------
parseSpecFromFile = parseFromFile $ mkSpec <$> specWraps specP  

mkSpec    ::  IsLocated l => [PSpec l t] -> Nano a t
mkSpec xs = Nano { code   = Src [] 
                 , specs  = envFromList [b | Bind b <- xs] 
                 , defs   = envEmpty
                 , consts = envFromList [(switchProp i, t) | Meas (i, t) <- xs]
                 , quals  =             [q | Qual q <- xs]  
                 }

-- YUCK. Worst hack of all time.
switchProp i@(Id l x) 
  | x == (toLower <$> propConName) = Id l propConName
  | otherwise                      = i

--------------------------------------------------------------------------------------
parseCodeFromFile :: FilePath -> IO (Nano SourcePos a) 
--------------------------------------------------------------------------------------
parseCodeFromFile = fmap mkCode . parseJavaScriptFromFile 
        
mkCode    :: [Statement SourcePos] -> Nano SourcePos a
mkCode ss = Nano { code   = Src (checkTopStmt <$> ss)
                 , specs  = envEmpty  
                 , defs   = envEmpty
                 , consts = envEmpty 
                 , quals  = []  
                 } 

-------------------------------------------------------------------------------
-- | Parse File and Type Signatures -------------------------------------------
-------------------------------------------------------------------------------

parseNanoFromFile :: FilePath-> IO (Nano SourcePos RefType)
parseNanoFromFile f 
  = do code  <- parseCodeFromFile f
       spec  <- parseSpecFromFile f
       ispec <- parseSpecFromFile =<< getPreludePath
       return $ shuffleSpecDefs $ mconcat [code, spec, ispec] 

shuffleSpecDefs pgm = pgm { specs = specγ } { defs = defγ }
  where 
    defγ            = envFromList [(fn, initFunTy fn γ) | fn <- fns]
    specγ           = envFromList [(x, t) | (x, t) <- xts, not (x `envMem` defγ)]
    γ               = specs pgm
    xts             = envToList γ
    fns             = definedFuns fs 
    Src fs          = code pgm

initFunTy fn γ = fromMaybe err $ envFindTy fn γ 
  where 
    err        = errorstar $ bugUnboundVariable (srcPos fn) fn


-- SYB examples at: http://web.archive.org/web/20080622204226/http://www.cs.vu.nl/boilerplate/#suite
definedFuns       :: [Statement SourcePos] -> [Id SourcePos]
definedFuns stmts = everything (++) ([] `mkQ` fromFunction) stmts
  where 
    fromFunction (FunctionStmt _ x _ _) = [x] 
    fromFunction _                      = []



--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

instance Inputable RefType where 
  rr' = doParse' bareTypeP

instance Inputable Type where 
  rr' = doParse' (fmap (const ()) <$> bareTypeP)

