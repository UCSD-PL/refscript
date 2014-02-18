{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances         #-} 
{-# LANGUAGE DeriveDataTypeable        #-} 
{-# LANGUAGE UndecidableInstances      #-} 
{-# LANGUAGE TypeSynonymInstances      #-} 
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE DeriveGeneric             #-}

module Language.Nano.Typecheck.Parse (
    parseNanoFromFile 
  ) where

import           Prelude                          hiding ( mapM)

import           Data.Aeson                              ( eitherDecode)
import           Data.Aeson.Types                 hiding ( Parser, Error, parse)
import qualified Data.Aeson.Types                 as     AI
import qualified Data.ByteString.Lazy.Char8       as     B
import           Data.Char                               ( isLower, isSpace)
import qualified Data.List                        as     L
import           Data.Generics.Aliases                   ( mkQ)
import           Data.Generics.Schemes
import           Data.Traversable                        ( mapAccumL)
import           Data.Data
import qualified Data.Foldable                    as     FO
import           Data.Vector                             ((!))

import           Control.Monad                    hiding ( mapM)
import           Control.Monad.Trans                     ( MonadIO,liftIO)
import           Control.Applicative                     ( (<$>), ( <*>))

import           Language.Fixpoint.Types          hiding ( quals, Loc, Expression)
import           Language.Fixpoint.Parse
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc                  ( mapEither, mapSnd)
import           Language.Nano.Misc                      ( maybeToEither)
import           Language.Nano.Errors
import           Language.Nano.Types
import           Language.Nano.Typecheck.Types
import           Language.Nano.Liquid.Types
import           Language.Nano.Env

import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.PrettyPrint

import           Text.Parsec                      hiding ( parse)
import           Text.Parsec.Pos                         ( newPos)
import qualified Text.Parsec.Token                as     Token

import           GHC.Generics

-- import           Debug.Trace                             ( trace, traceShow)

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

tBodyP :: Parser (Id SourceSpan, TyDef RefType)
tBodyP = do  id     <- identifierP 
             tv     <- option [] tParP
             reservedOp "="
             tb     <- bareTypeP
             return  $ (id, TD id tv tb (idLoc id))

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

-- | `bareTypeP` parses top-level "bare" types. If no refinements are supplied, 
-- then "top" refinement is used.

bareTypeP :: Parser RefType 
bareTypeP =       
      try bUnP
  <|> try (refP rUnP)
  <|>     (xrefP rUnP)

rUnP      = mkUn <$> bareTypeNoUnionP `sepBy1` plus

bUnP      = bareTypeNoUnionP `sepBy1` plus >>= ifSingle return (\xs -> TApp TUn xs <$> topP)
  where
    ifSingle f _ [x] = f x
    ifSingle _ g xs  = g xs

mkUn [a] = strengthen a
mkUn ts  = TApp TUn (L.sort ts)

-- | `bareTypeNoUnionP` parses a type that does not contain a union at the top-level.
bareTypeNoUnionP  = try funcSigP          <|> (bareAtomP bbaseP)

-- | `funcSigP` parses a function type that is possibly generic and/or an intersection.
funcSigP          = 
      try bareAll1P
  <|> try (intersectP bareAll1P) 
  <|> try bareFun1P
  <|>     (intersectP bareFun1P)

intersectP p      = tAnd <$> many1 (reserved "/\\" >> p)

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
                withinSpacesP colon
                B s <$> bareTypeP

argBind t = B (rTypeValueVar t) t

bareAtomP p
  =  try (xrefP  p)
 <|> try (refP p)
 <|>     (dummyP p)

bbaseP :: Parser (Reft -> RefType)
bbaseP 
  =  try (TVar <$> tvarP)                  -- A
 <|> try (TObj <$> (braces $ bindsP) )     -- { f1: T1, ... , fn: Tn} 
 <|> try (TObj <$> arrayBindsP)            -- { i1: T1, ... , in: Tn}
 <|> try (TArr <$> arrayP)                 -- [T]
 <|> try (TApp <$> tConP <*> bareTyArgsP)  -- list[A], tree[A,B] etc...

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

tConP :: Parser TCon
tConP =  try (reserved "number"    >> return TInt)
     <|> try (reserved "boolean"   >> return TBool)
     <|> try (reserved "undefined" >> return TUndef)
     <|> try (reserved "void"      >> return TVoid)
     <|> try (reserved "top"       >> return TTop)
     <|> try (reserved "string"    >> return TString)
     <|> try (reserved "null"      >> return TNull)
     <|>     (TDef <$> identifierP)

bareAll1P 
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
        withinSpacesP colon
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
 
binderP :: Parser Symbol
binderP = try (stringSymbol <$> idP badc)
      <|> try (star >> return (stringSymbol "*"))
      <|> liftM pwr (parens (idP bad))
      where idP p  = many1 (satisfy (not . p))
            badc c = (c == ':') ||  bad c
            bad c  = isSpace c || c `elem` "()"
            pwr s  = stringSymbol $ "(" ++ s ++ ")" 

withinSpacesP :: Parser a -> Parser a
withinSpacesP p = do { spaces; a <- p; spaces; return a } 
             

---------------------------------------------------------------------------------
-- | Specifications
---------------------------------------------------------------------------------

data RawSpec
  = RawMeas   String
  | RawBind   String
  | RawExtern String
  | RawType   String
  | RawTAlias String
  | RawPAlias String
  | RawQual   String 
  | RawInvt   String
  deriving (Show,Eq,Ord,Data,Typeable,Generic)

data PSpec l t 
  = Meas   (Id l, t)
  | Bind   (Id l, t) 
  | Extern (Id l, t)
  | Type   (Id l, TyDef t)
  | TAlias (Id l, TAlias t)
  | PAlias (Id l, PAlias) 
  | Qual   Qualifier
  | Invt   l t 
  deriving (Eq, Ord, Show, Data, Typeable)

type Spec = PSpec SourceSpan RefType

parseAnnot :: SourceSpan -> RawSpec -> Parser Spec
parseAnnot ss (RawMeas   _) = Meas   <$> patch ss <$> idBindP
parseAnnot ss (RawBind   _) = Bind   <$> patch ss <$> idBindP
parseAnnot ss (RawExtern _) = Extern <$> patch ss <$> idBindP
parseAnnot ss (RawType   _) = Type   <$> patch ss <$> tBodyP
parseAnnot ss (RawTAlias _) = TAlias <$> patch ss <$> tAliasP
parseAnnot ss (RawPAlias _) = PAlias <$> patch ss <$> pAliasP
parseAnnot _  (RawQual   _) = Qual   <$>              qualifierP
parseAnnot ss (RawInvt   _) = Invt             ss <$> bareTypeP

patch ss (id, t) = (fmap (const ss) id , t)

getSpecString :: RawSpec -> String 
getSpecString (RawMeas   s) = s  
getSpecString (RawBind   s) = s
getSpecString (RawExtern s) = s 
getSpecString (RawType   s) = s 
getSpecString (RawTAlias s) = s 
getSpecString (RawPAlias s) = s 
getSpecString (RawQual   s) = s 
getSpecString (RawInvt   s) = s 

instance FromJSON SourcePos where
  parseJSON (Array v) = do
    v0 <- parseJSON (v!0) :: AI.Parser String 
    v1 <- parseJSON (v!1) :: AI.Parser Int
    v2 <- parseJSON (v!2) :: AI.Parser Int
    return $ newPos v0 v1 v2
  parseJSON _ = error "SourcePos should only be an A.Array" 

instance FromJSON (Expression (SourceSpan, [RawSpec]))
instance FromJSON (Statement (SourceSpan, [RawSpec]))
instance FromJSON (LValue (SourceSpan, [RawSpec]))
instance FromJSON (JavaScript (SourceSpan, [RawSpec]))
instance FromJSON (ClassElt (SourceSpan, [RawSpec]))
instance FromJSON (CaseClause (SourceSpan, [RawSpec]))
instance FromJSON (CatchClause (SourceSpan, [RawSpec]))
instance FromJSON (ForInit (SourceSpan, [RawSpec]))
instance FromJSON (ForInInit (SourceSpan, [RawSpec]))
instance FromJSON (VarDecl (SourceSpan, [RawSpec]))
instance FromJSON (Id (SourceSpan, [RawSpec]))
instance FromJSON (Prop (SourceSpan, [RawSpec]))
instance FromJSON (SourceSpan, [RawSpec])
instance FromJSON InfixOp
instance FromJSON AssignOp
instance FromJSON PrefixOp
instance FromJSON UnaryAssignOp
instance FromJSON SourceSpan
instance FromJSON RawSpec


-------------------------------------------------------------------------------
-- | Parse File and Type Signatures -------------------------------------------
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
parseNanoFromFile :: FilePath-> IO (Either Error (NanoBareR Reft))
-------------------------------------------------------------------------------
parseNanoFromFile f 
  = do  -- spec <- parseCodeFromFile =<< getPreludePath
        code <- parseCodeFromFile f
        case msum [{-spec,-} code] of 
          Right s -> return $ catSpecDefs s
          Left  e -> return $ Left e

-------------------------------------------------------------------------------
-- collectTypes :: [Statement (SourceSpan, Maybe RefType)] -> [RefType]
-------------------------------------------------------------------------------
-- collectTypes = concatMap $ FO.foldr (\s -> (++) (maybeToList $ snd s)) []


getJSON :: MonadIO m => FilePath -> m B.ByteString
getJSON = liftIO . B.readFile

--------------------------------------------------------------------------------------
parseScriptFromJSON :: FilePath -> IO ([Statement (SourceSpan, [RawSpec])])
--------------------------------------------------------------------------------------
parseScriptFromJSON filename = decodeOrDie <$> getJSON filename
  where 
    decodeOrDie s =
      case eitherDecode s :: Either String [Statement (SourceSpan, [RawSpec])] of
        Left msg -> error $ "JSON decode error:\n" ++ msg
        Right p  -> p

--------------------------------------------------------------------------------------
parseCodeFromFile :: FilePath -> IO (Either Error (NanoBareR Reft))
--------------------------------------------------------------------------------------
parseCodeFromFile fp = parseScriptFromJSON fp >>= return . mkCode . expandAnnots

--------------------------------------------------------------------------------------
mkCode :: [Statement (SourceSpan, [Spec])] ->  Either Error (NanoBareR Reft)
--------------------------------------------------------------------------------------
mkCode ss =  do
    return $ Nano { 
        code    = Src (checkTopStmt <$> ss')
      , specs   = envFromList $ [ t | Extern t <- anns ] 
                             ++ [ t | Bind   t <- anns ] -- externs and binds
      , chSpecs = envFromList   [ t | Bind   t <- concatMap (FO.foldMap snd) vds ]
      , consts  = envFromList   [ t | Meas   t <- anns ] 
      , defs    = envFromList   [ t | Type   t <- anns ] 
      , tAlias  = envFromList   [ t | TAlias t <- anns ] 
      , pAlias  = envFromList   [ t | PAlias t <- anns ] 
      , quals   =               [ t | Qual   t <- anns ] 
      , invts   = [Loc (srcPos l) t | Invt l t <- anns ]
    } 
  where
    toBare     :: (SourceSpan, [Spec]) -> AnnBare Reft 
    toBare p    = Ann (fst p) [TAnnot t | Bind (_,t) <- snd p ]
    ss'         = (toBare <$>) <$> ss
    anns        = concatMap (FO.foldMap snd) ss
    -- ssAnns      = concatMap (FO.foldMap (\(s,l) -> (s,) <$> l)) ss
    vds         = varDeclStmts ss


-- TODO: Is there any chance we can keep the Either Monad here?
--
-- In the following `a` is meant to be Spec + RefType (the type specs for
-- functions etc.). 
type PState = Integer

--------------------------------------------------------------------------------------
expandAnnots :: [Statement (SourceSpan, [RawSpec])] -> [Statement (SourceSpan, [Spec])]
--------------------------------------------------------------------------------------
expandAnnots = snd . mapAccumL (mapAccumL f) 0
  where f st (ss,sp) = mapSnd ((ss),) $ L.mapAccumL (parse ss) st sp

--------------------------------------------------------------------------------------
parse :: SourceSpan -> PState -> RawSpec -> (PState, Spec)
--------------------------------------------------------------------------------------
parse ss st c = foo c
  where foo s = failLeft $ runParser (liftM2 (,) getState (parseAnnot ss s)) st f (getSpecString s)
        failLeft (Left s) = error $ show s
        failLeft (Right r) = r
        f = sourceName $ sp_begin ss


--------------------------------------------------------------------------------------
catSpecDefs :: NanoBareR Reft -> Either Error (NanoBareR Reft)
--------------------------------------------------------------------------------------
catSpecDefs pgm = do
    defL  <- sequence [ (x,) <$> lookupTy x (specs pgm) | x <- fs ]
    return $ pgm { chSpecs = envUnion (envFromList defL) (chSpecs pgm) }
  where 
    fs          = definedFuns stmts 
    Src stmts   = code pgm

lookupTy x γ   = maybeToEither err $ envFindTy x γ 
  where 
    err        = bugUnboundFunction γ (srcPos x) x


-- SYB examples at: http://web.archive.org/web/20080622204226/http://www.cs.vu.nl/boilerplate/#suite
--------------------------------------------------------------------------------------
definedFuns       :: [Statement (AnnBare Reft)] -> [Id (AnnBare Reft)]
--------------------------------------------------------------------------------------
definedFuns stmts = everything (++) ([] `mkQ` fromFunction) stmts
  where 
    fromFunction (FunctionStmt _ x _ _) = [x] 
    fromFunction _                      = []

--------------------------------------------------------------------------------------
varDeclStmts         :: (Data a, Typeable a) => [Statement a] -> [Statement a]
--------------------------------------------------------------------------------------
varDeclStmts stmts    = everything (++) ([] `mkQ` fromVarDecl) stmts
  where 
    fromVarDecl s@(VarDeclStmt _ _) = [s]
    fromVarDecl _                   = []

