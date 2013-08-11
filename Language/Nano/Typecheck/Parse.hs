{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances         #-} 
{-# LANGUAGE UndecidableInstances      #-} 
{-# LANGUAGE TypeSynonymInstances      #-} 
{-# LANGUAGE TupleSections             #-}

module Language.Nano.Typecheck.Parse (
    parseNanoFromFile 
  ) where

import           Data.List (sort)
import           Data.Maybe (fromMaybe)
import           Data.Generics.Aliases
import           Data.Generics.Schemes
import           Control.Monad
import           Text.Parsec
-- import           Text.Parsec.String hiding (Parser, parseFromFile)
import qualified Text.Parsec.Token as Token
import           Control.Applicative ((<$>), (<*), (<*>))
import           Data.Char (toLower, isLower, isSpace) 
import           Data.Monoid (mconcat)

import           Language.Fixpoint.Names (propConName)
import           Language.Fixpoint.Misc (errorstar)
import           Language.Fixpoint.Types hiding (quals)
import           Language.Fixpoint.Parse 

import           Language.Nano.Errors
import           Language.Nano.Files
import           Language.Nano.Types
import           Language.Nano.Typecheck.Types
import           Language.Nano.Liquid.Types
import           Language.Nano.Env

import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Parser        (parseJavaScriptFromFile, SourceSpan (..))

import           Language.ECMAScript3.PrettyPrint
import           Text.Printf                        (printf)

dot        = Token.dot        lexer
braces     = Token.braces     lexer
plus       = Token.symbol     lexer "+"
-- angles     = Token.angles     lexer

----------------------------------------------------------------------------------
-- | Type Binders ----------------------------------------------------------------
----------------------------------------------------------------------------------

idBindP :: Parser (Id SourceSpan, RefType)
idBindP = xyP identifierP dcolon bareTypeP

identifierP :: Parser (Id SourceSpan)
identifierP = withSpan Id lowerIdP -- <$> getPosition <*> lowerIdP -- Lexer.identifier

tBodyP :: Parser (Id SourceSpan, RType Reft)
tBodyP = do  id <- identifierP 
             tv <- option [] tParP
             tb <- bareTypeP
             return $ (id, TBd $ TD (TDef id) tv tb (idLoc id))

-- [A,B,C...]
tParP = brackets $ sepBy tvarP comma

withSpan f p = do pos   <- getPosition
                  x     <- p
                  pos'  <- getPosition
                  return $ f (Span pos pos') x


xyP lP sepP rP
  = (\x _ y -> (x, y)) <$> lP <*> (spaces >> sepP) <*> rP


----------------------------------------------------------------------------------
-- | RefTypes --------------------------------------------------------------------
----------------------------------------------------------------------------------

-- | Top-level parser for "bare" types. 
-- If refinements not supplied, then "top" refinement is used.

bareTypeP :: Parser RefType 
bareTypeP 
  = do  ts <- bareTypeNoUnionP `sepBy1` plus
        tr <- topP   -- unions have Top ref. type atm
        case ts of
          [ ] -> error "impossible"
          [t] -> return t
          _   -> return $ TApp TUn (sort ts) tr


bareTypeNoUnionP
  =  try bareAllP
 <|> try bareFunP
 <|>     (bareAtomP bbaseP)

-- Creating the bindings right away at bareArgP
bareFunP
  -- = do args   <- parens $ sepBy bareTypeP comma
  = do args   <- parens $ sepBy bareArgP comma
       reserved "=>" 
       ret    <- bareTypeP 
       r      <- topP
       return $ TFun args ret r

bareArgP 
  =   (try boundTypeP)
 <|>  (argBind <$> try (bareTypeP))

boundTypeP = do s <- symbolP 
                spaces
                colon
                spaces
                B s <$> bareTypeP

argBind t = B (rTypeValueVar t) t

bareAtomP p
  =  try (refP  p)
 <|> try (bRefP p)
--  <|> try (bindP p)   -- This case is taken separately at Function parser
 <|>     (dummyP (p <* spaces))

bbaseP :: Parser (Reft -> RefType)
bbaseP 
  =  try (TVar <$> tvarP)
 <|> try (TObj <$> (braces $ bindsP) )
 <|> try (TApp <$> tDefP <*> (brackets $ sepBy bareTypeP comma))  -- This is what allows: list [A], tree [A,B] etc...
 <|>     ((`TApp` []) <$> tconP)

tvarP :: Parser TVar
-- tvarP = TV <$> (stringSymbol <$> upperWordP) <*> getPosition
tvarP = withSpan (\l x -> TV x l) (stringSymbol <$> upperWordP)


upperWordP :: Parser String
upperWordP = condIdP nice (not . isLower . head)
  where 
    nice   = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0'..'9']

tconP :: Parser TCon
tconP =  try (reserved "number"    >> return TInt)
     <|> try (reserved "boolean"   >> return TBool)
     <|> try (reserved "void"      >> return TVoid)
     <|> try (reserved "top"       >> return TTop)
     <|> try (reserved "string"    >> return TString)
     <|> try (reserved "null"      >> return TNull)
     <|> tDefP

tDefP 
  = do  s <- identifierP 
        -- XXX: This list will have to be enhanced.
        if unId s `elem` ["true", "false", "number", "boolean", "string", "top", "void", "null"] 
          then parserZero
          else return $ TDef s

bareAllP 
  = do reserved "forall"
       as <- many1 tvarP
       dot
       t  <- bareTypeP
       return $ foldr TAll t as

bindsP 
  =  try (sepBy1 bareBindP comma)
 <|> (spaces >> return [])

bareBindP 
  = do  s <- binderP
        spaces      --ugly
        colon
        spaces
        t <- bareTypeP
        return $ B s t 

 
dummyP ::  Parser (Reft -> b) -> Parser b
dummyP fm = fm `ap` topP 

topP   :: Parser Reft
topP   = (Reft . (, []) . vv . Just) <$> freshIntP

-- | Parses bindings of the form: `x : kind`
bindP :: Parser (Reft -> a) -> Parser a
bindP kindP
  = do v <- symbolP 
       colon
       t <- kindP
       return $ t (Reft (v, []))

-- | Parses refined types of the form: `{ x : kind | refinement }`
bRefP :: Parser (Reft -> a) -> Parser a
bRefP kindP
  = braces $ do
      v   <- symbolP
      colon
      t   <- kindP
      reserved "|"
      ras <- refasP 
      return $ t (Reft (v, ras))

-- | Parses refined types of the form: `{ kind | refinement }`
refP :: Parser (Reft -> a) -> Parser a
refP kindP
  = braces $ do
      t   <- kindP
      reserved "|"
      ras <- refasP 
      return $ t (Reft (stringSymbol "v", ras))

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
 
binderP :: Parser Symbol
binderP =  try $ liftM stringSymbol (idP badc)
      <|> liftM pwr (parens (idP bad))
      where idP p  = many1 (satisfy (not . p))
            badc c = (c == ':') ||  bad c
            bad c  = isSpace c || c `elem` "()"
            pwr s  = stringSymbol $ "(" ++ s ++ ")" 
              
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
  | Type (Id l, t)
  | Invt l t 
  deriving (Show)

specP :: Parser (PSpec SourceSpan RefType)
specP 
  = try (reserved "measure"   >> (Meas <$> idBindP    ))
    <|> (reserved "qualif"    >> (Qual <$> qualifierP ))
    <|> (reserved "type"      >> (Type <$> tBodyP     )) 
    <|> (reserved "invariant" >> (withSpan Invt bareTypeP))
    <|> ({- DEFAULT -}           (Bind <$> idBindP    ))

--------------------------------------------------------------------------------------
parseSpecFromFile :: FilePath -> IO (Nano SourceSpan RefType) 
--------------------------------------------------------------------------------------
parseSpecFromFile = parseFromFile $ mkSpec <$> specWraps specP  

mkSpec    ::  (PP t, IsLocated l) => [PSpec l t] -> Nano a t
mkSpec xs = Nano { code   = Src [] 
                 , specs  = envFromList [b | Bind b <- xs] 
                 , defs   = envEmpty
                 , consts = envFromList [(switchProp i, t) | Meas (i, t) <- xs]
                 , tDefs  = envFromList [b         | Type b <- xs]
                 , quals  =             [q         | Qual q <- xs]  
                 , invts  =             [Loc l' t  | Invt l t <- xs, let l' = srcPos l] 
                 }

-- YUCK. Worst hack of all time.
switchProp i@(Id l x) 
  | x == (toLower <$> propConName) = Id l propConName
  | otherwise                      = i


--------------------------------------------------------------------------------------
parseCodeFromFile :: FilePath -> IO (Nano SourceSpan a) 
--------------------------------------------------------------------------------------
parseCodeFromFile = fmap mkCode . parseJavaScriptFromFile 
        
mkCode    :: [Statement SourceSpan] -> Nano SourceSpan a
mkCode ss = Nano { code   = Src (checkTopStmt <$> ss)
                 , specs  = envEmpty  
                 , defs   = envEmpty
                 , consts = envEmpty 
                 , tDefs  = envEmpty
                 , quals  = [] 
                 , invts  = [] 
                 } 

-------------------------------------------------------------------------------
-- | Parse File and Type Signatures -------------------------------------------
-------------------------------------------------------------------------------

parseNanoFromFile :: FilePath-> IO (Nano SourceSpan RefType)
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
definedFuns       :: [Statement SourceSpan] -> [Id SourceSpan]
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

