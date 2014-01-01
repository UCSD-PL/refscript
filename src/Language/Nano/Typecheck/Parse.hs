{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances         #-} 
{-# LANGUAGE UndecidableInstances      #-} 
{-# LANGUAGE TypeSynonymInstances      #-} 
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Language.Nano.Typecheck.Parse (
    parseNanoFromFile 
  ) where

import           Data.List (sort)
import qualified Data.Foldable                      as F
import           Data.Maybe (fromMaybe)
import           Data.Generics.Aliases
import           Data.Generics.Schemes
import qualified Data.HashMap.Strict                as M 
import           Data.Data
import           Data.Typeable
import           Control.Monad
import           Control.Exception (throw)
import           Text.Parsec
import           Text.PrettyPrint.HughesPJ          (text, (<+>))
import qualified Text.Parsec.Token as Token
import           Control.Applicative ((<$>), (<*), (<*>))
import           Control.Monad.Identity
import           Data.Char (toLower, isLower, isSpace) 
import           Data.Monoid (mappend, mconcat, mempty)

import           Language.Fixpoint.Names (propConName)
import           Language.Fixpoint.Types hiding (quals, Loc)
import qualified Language.Fixpoint.Types        as F
import           Language.Fixpoint.Parse 
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc (mapEither)
import           Language.Nano.Errors
import           Language.Nano.Files
import           Language.Nano.Types
import           Language.Nano.Typecheck.Types
import           Language.Nano.Liquid.Types
import           Language.Nano.Env

import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations   (getAnnotation)
import           Language.ECMAScript3.Parser        ( parseJavaScriptFromFile',
                                                      initialParserState )
import           Language.ECMAScript3.Parser.Type   ( SourceSpan (..))
import           Language.ECMAScript3.Parser.Type hiding (Parser)

import           Language.ECMAScript3.PrettyPrint


-- import           Debug.Trace                        (trace, traceShow)

dot        = Token.dot        lexer
plus       = Token.symbol     lexer "+"
star       = Token.symbol     lexer "*"

----------------------------------------------------------------------------------
-- | Type Binders ----------------------------------------------------------------
----------------------------------------------------------------------------------

idBindP :: Parser (Id SourceSpan, RefType)
idBindP = xyP identifierP dcolon bareTypeP

identifierP :: Parser (Id SourceSpan)
identifierP =   try (withSpan Id upperIdP)
           <|>      (withSpan Id lowerIdP)

pAliasP :: Parser (Id SourceSpan, PAlias) 
pAliasP = do name <- identifierP
             πs   <- pAliasVarsP -- many symbolP 
             reservedOp "="
             body <- predP 
             return  (name, Alias name [] πs body) 

pAliasVarsP = try (parens $ sepBy symbolP comma)
           <|> many symbolP

tAliasP :: Parser (Id SourceSpan, TAlias RefType) 
tAliasP = do name      <- identifierP
             (αs, πs)  <- mapEither aliasVarT <$> aliasVarsP 
             reservedOp "="
             body      <- bareTypeP
             return      (name, Alias name αs πs body) 

aliasVarsP    = try (brackets $ sepBy aliasVarP comma) <|> return []
aliasVarP     = withSpan (,) (wordP $ \_ -> True)

aliasVarT (l, x)      
  | isTvar x  = Left  $ tvar l x
  | otherwise = Right $ stringSymbol x 

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
  =  try (do  ts <- bareTypeNoUnionP `sepBy1` plus
              tr <- topP   -- unions have Top ref. type atm
              case ts of
                [ ] -> error "impossible"
                [t] -> return t
                _   -> return $ TApp TUn (sort ts) tr)
         
 <|> try (refP ( do ts <- bareTypeNoUnionP `sepBy1` plus
                    case ts of
                      [ ] -> error "impossible"
                      [_] -> error "bareTypeP parser BUG"
                      _   -> return $ TApp TUn (sort ts) 
                ))


bareTypeNoUnionP
  =  try bareAllP
 <|> try bareFun1P
 <|> try bareFunP
 <|>     (bareAtomP bbaseP)

-- | `bareFunP` parses an ordered-intersection type
bareFunP 
  = tAnd <$> many1 (reserved "/\\" >> bareFun1P)

-- | `bareFun1P` parses a single function type
bareFun1P
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
  =  try (xrefP  p)
 <|> try (refP p)
--  <|> try (bindP p)   -- This case is taken separately at Function parser
 <|>     (dummyP (p <* spaces))

bbaseP :: Parser (Reft -> RefType)
bbaseP 
  =  try (TVar <$> tvarP)                  -- A
 <|> try (TObj <$> (braces $ bindsP) )     -- { f1: T1, ... , fn: Tn} 
 <|> try (TObj <$> arrayBindsP)            -- { i1: T1, ... , in: Tn}
 <|> try (TArr <$> arrayP)                 -- [T]
 <|> try (TApp <$> tDefP <*> bareTyArgsP)  -- list[A], tree[A,B] etc...
 
 -- <|>     ((`TApp` []) <$> tconP)           -- yuck. this is wierd.

bareTyArgsP = try (brackets $ sepBy bareTyArgP comma) <|> return []

bareTyArgP  = try bareTypeP 
           <|> (TExp <$> exprP)

tvarP    :: Parser TVar
tvarP    = withSpan tvar $ wordP isTvar                  -- = withSpan {- (\l x -> TV x l) -} (flip TV) (stringSymbol <$> upperWordP)

tvar l x = TV (stringSymbol x) l

isTvar   = not . isLower . head

wordP p  = condIdP ok p
  where 
    ok   = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0'..'9']

tDefP :: Parser TCon
tDefP =  try (reserved "number"    >> return TInt)
     <|> try (reserved "boolean"   >> return TBool)
     <|> try (reserved "undefined" >> return TUndef)
     <|> try (reserved "void"      >> return TVoid)
     <|> try (reserved "top"       >> return TTop)
     <|> try (reserved "string"    >> return TString)
     <|> try (reserved "null"      >> return TNull)
     <|> (TDef <$> identifierP)

bareAllP 
  = do reserved "forall"
       αs <- many1 tvarP
       dot
       t  <- bareTypeP
       return $ foldr TAll t αs

arrayP = brackets bareTypeP


arrayBindsP 
  = do reserved "[|"
       ts    <- sepBy bareTypeP comma
       reserved "|]"
       return $ zipWith B (symbol . show <$> [0..]) ts ++ [len ts]
    where
      len ts = B (symbol "length") (eSingleton tInt $ length ts)


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


-- | Parses refined types of the form: `{ kind | refinement }`
xrefP :: Parser (Reft -> a) -> Parser a
xrefP kindP
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
binderP = try (stringSymbol <$> idP badc)
      <|> try (star >> return (stringSymbol "*"))
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
  = Meas   (Id l, t)
  | Bind   (Id l, t) 
  | Extern (Id l, t)
  | Type   (Id l, t)
  | Talias (Id l, TAlias t)
  | Palias (Id l, PAlias) 
  | Qual    Qualifier
  | Invt   l t 
  deriving (Eq, Ord, Show, Data, Typeable)

specP :: Parser (PSpec SourceSpan RefType)
specP 
  =   try (reserved "measure"   >> (Meas   <$> idBindP    ))
  <|> try (reserved "qualif"    >> (Qual   <$> qualifierP ))
  <|> try (reserved "type"      >> (Type   <$> tBodyP     )) 
  <|> try (reserved "type"      >> (Talias <$> tAliasP    ))
  <|> try (reserved "predicate" >> (Palias <$> pAliasP    ))
  <|> try (reserved "invariant" >> (withSpan Invt bareTypeP))
  <|>     (reserved "extern"    >> (Extern <$> idBindP    ))

instance (PP l, PP t) => PP (PSpec l t) where
  pp (Meas (i, t))   = text "measure: " <+> pp i
  pp (Bind (i, t))   = text "bind: " <+>  pp i <+> text " :: " <+> pp t
  pp (Extern (i, t)) = text "extern: " <+>  pp i <+> text " :: " <+> pp t
  pp (Type (i, t))   = text "Type:TODO"
  pp (Talias _)      = text "Talias:TODO"
  pp (Palias _)      = text "Palias:TODO"
  pp (Qual _)        = text "Qual:TODO"
  pp (Invt _ _)      = text "Invt:TODO"

-- | `AnnToken`: Elements that can are parsed along the source as annotations.

data AnnToken r 
  = TBind (Id SourceSpan, RType r)          -- ^ Function signature binding
  | TType (RType r)                         -- ^ Variable declaration binding
  | TSpec (PSpec SourceSpan (RType r))      -- ^ Specs: qualifiers, measures, type defs, etc.
  | EmptyToken                              -- ^ Dummy empty token
  deriving (Eq, Ord, Show, Data, Typeable)

instance (PP r, F.Reftable r) => PP (AnnToken r) where
  pp (TBind (id,t)) = pp id <+> text " :: " <+> pp t
  pp (TType t)      = pp t
  pp (TSpec s)      = pp s
  pp EmptyToken     = text "<empyt>"


-- --------------------------------------------------------------------------------------
-- parseSpecFromFile :: FilePath -> IO (Nano SourceSpan RefType) 
-- --------------------------------------------------------------------------------------
-- parseSpecFromFile f = parseFromFile (mkSpec <$> specWraps specWithDefaultP) f
-- 
-- --------------------------------------------------------------------------------------
-- mkSpec    ::  (PP t, IsLocated l) => [PSpec l t] -> Nano a t
-- --------------------------------------------------------------------------------------
-- mkSpec xs = Nano { code   = Src [] 
--                  , specs  = envFromList [b | Bind b <- xs] 
--                  , sigs   = envEmpty
--                  , consts = envFromList [(switchProp i, t) | Meas (i, t) <- xs]
--                  , defs   = envFromList [b         | Type b <- xs]
--                  , tAnns  = M.empty
--                  , quals  =             [q         | Qual q <- xs]
--                  , invts  =             [Loc l' t  | Invt l t <- xs, let l' = srcPos l]
--                  }

-- -- YUCK. Worst hack of all time.
-- switchProp i@(Id l x) 
--   | x == (toLower <$> propConName) = Id l propConName
--   | otherwise                      = i

--------------------------------------------------------------------------------------
tAnnotP :: ParserState String (AnnToken Reft) -> ExternP String (AnnToken Reft)
--------------------------------------------------------------------------------------
tAnnotP stIn = EP typeP fSigP tLevP
  where
    typeP  = TType <$> changeState fwd bwd bareTypeP
    fSigP  = TBind <$> changeState fwd bwd idBindP
    tLevP  = TSpec <$> changeState fwd bwd specP
    fwd _  = stIn  -- NOTE: need to keep the state of the language-ecmascript parser!!!
    bwd _  = 0     -- TODO: Is this adequate???

-- `changeState` taken from here:
-- http://stackoverflow.com/questions/17968784/an-easy-way-to-change-the-type-of-parsec-user-state
--------------------------------------------------------------------------------------
changeState :: forall m s u v a . (Functor m, Monad m) => 
  (u -> v) -> (v -> u) -> ParsecT s u m a -> ParsecT s v m a
--------------------------------------------------------------------------------------
changeState forward backward = mkPT . transform . runParsecT
  where
    mapState :: forall u v . (u -> v) -> State s u -> State s v
    mapState f st = st { stateUser = f (stateUser st) }

    mapReply :: forall u v . (u -> v) -> Reply s u a -> Reply s v a
    mapReply f (Ok a st err) = Ok a (mapState f st) err
    mapReply _ (Error e) = Error e

    fmap3 = fmap . fmap . fmap

    transform
      :: (State s u -> m (Consumed (m (Reply s u a))))
      -> (State s v -> m (Consumed (m (Reply s v a))))
    transform p st = fmap3 (mapReply forward) (p (mapState backward st))


-- | Parse Code along with type annotations

--------------------------------------------------------------------------------------
parseCodeFromFile :: FilePath -> IO (Nano SourceSpan RefType)
--------------------------------------------------------------------------------------
parseCodeFromFile fp = parseJavaScriptFromFile' tAnnotP fp >>= return . mkCode

--------------------------------------------------------------------------------------
mkCode :: ([Statement SourceSpan], M.HashMap SourceSpan [AnnToken Reft]) -> 
  Nano SourceSpan RefType
--------------------------------------------------------------------------------------
mkCode (ss, m) = Nano { code   = Src (checkTopStmt <$> ss)
    , specs  = envFromList [ a       | TSpec (Extern a)    <- list ] 
    , sigs   = envFromList [ a       | TBind         a     <- list ] 
    , consts = envFromList [ a       | TSpec (Meas   a)    <- list ] 
    , defs   = envFromList [ a       | TSpec (Type   a)    <- list ] 
    , tAlias = envFromList [ a       | TSpec (Talias a)    <- list ]
    , pAlias = envFromList [ p       | TSpec (Palias p)    <- list ]
    , tAnns  = foldr (envUnion . vds) envEmpty $ varDeclStmts ss
    , quals  = [q                    | TSpec (Qual   q)    <- list ]
    , invts  = [Loc l' t             | TSpec (Invt l t)    <- list, let l' = srcPos l]
    } 
  where
    list                    = concat $ M.elems m
    
    vds (VarDeclStmt l ds)  = mrg (prefixed $ M.lookup l m) (inlined ds)
    vds _                   = error "BUG: Parse.mkCode"

    prefixed (Just xs)      = envFromList [ a | TBind a <- xs ]
    prefixed Nothing        = envEmpty

    id (VarDecl _ i _)      = i

    spAn id                 = (id, M.lookup (getAnnotation id) m)
    inlined xs              = envFromList [(i, t) | (i, Just ans) <- spAn . id <$> xs
                                                  , TType t <- ans ]

    mrg e1 e2 
      | not $ null un       = (die . er . head) un
      where un              = fst <$> (envToList $ envIntersectWith const e1 e2)
            er s            = bugMultipleAnnots (srcPos s) s
    mrg e1 e2 
      | otherwise           = envUnion e1 e2  



-------------------------------------------------------------------------------
-- | Parse File and Type Signatures -------------------------------------------
-------------------------------------------------------------------------------

parseNanoFromFile :: FilePath-> IO (Nano SourceSpan RefType)
parseNanoFromFile f 
  = do spec <- parseCodeFromFile =<< getPreludePath
       code <- parseCodeFromFile f
       return $ catSpecDefs $ mconcat [spec, code] 

catSpecDefs pgm = pgm { sigs = defγ }
  where 
    defγ        = envFromList [ (x, lookupTy x γ) | x <- fs ]
    γ           = sigs pgm
    fs          = definedFuns stmts 
    Src stmts   = code pgm

lookupTy x γ   = fromMaybe err $ envFindTy x γ 
  where 
    err        = die $ bugUnboundFunction γ (srcPos x) x


-- SYB examples at: http://web.archive.org/web/20080622204226/http://www.cs.vu.nl/boilerplate/#suite
definedFuns       :: [Statement SourceSpan] -> [Id SourceSpan]
definedFuns stmts = everything (++) ([] `mkQ` fromFunction) stmts
  where 
    fromFunction (FunctionStmt _ x _ _) = [x] 
    fromFunction _                      = []

definedVars          :: [Statement SourceSpan] -> [Id SourceSpan]
definedVars stmts    = everything (++) ([] `mkQ` fromVarDecl) stmts
  where 
    fromVarDecl (VarDeclStmt _ ds) = [x | VarDecl _ x (Just _) <- ds]
    fromVarDecl _                  = []

varDeclStmts         :: [Statement SourceSpan] -> [Statement SourceSpan]
varDeclStmts stmts    = everything (++) ([] `mkQ` fromVarDecl) stmts
  where 
    fromVarDecl s@(VarDeclStmt _ _) = [s]
    fromVarDecl _                   = []

