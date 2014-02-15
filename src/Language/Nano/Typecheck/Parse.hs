{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances         #-} 
{-# LANGUAGE DeriveDataTypeable        #-} 
{-# LANGUAGE UndecidableInstances      #-} 
{-# LANGUAGE TypeSynonymInstances      #-} 
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Language.Nano.Typecheck.Parse (
    parseNanoFromFile 
  ) where

import           Data.List (sort)
import           Data.Maybe (maybeToList, fromJust)
import           Data.Generics.Aliases
import           Data.Generics.Schemes
import           Data.Traversable           (mapM, mapAccumL)
import qualified Data.HashMap.Strict                as M 
import           Data.Aeson   (encode)
import           Data.Data
import qualified Data.Foldable as FO
import           Prelude          hiding (mapM)
import           Control.Monad    hiding (mapM)
import           Text.Parsec      hiding (parse)
import           Text.PrettyPrint.HughesPJ          (text, (<+>))
import qualified Text.Parsec.Token as Token
import           Control.Applicative ((<$>), (<*>))
import           Data.Char (isLower, isSpace) 

import Control.Monad.Identity
import           Data.Monoid (Monoid, mconcat)

import           Language.Fixpoint.Types hiding (quals, Loc)
import qualified Language.Fixpoint.Types        as F
import           Language.Fixpoint.Parse 
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc (mapEither)
import           Language.Nano.Misc     (maybeToEither)
import           Language.Nano.Errors
import           Language.Nano.Files
import           Language.Nano.Types
import           Language.Nano.Typecheck.Types
import           Language.Nano.Liquid.Types
import           Language.Nano.Env

import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Parser        ( parseJavaScriptFromFile', parseScriptFromJSON)
import           Language.ECMAScript3.Parser.Type hiding (Parser)

import           Language.ECMAScript3.PrettyPrint

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Generics.Text as G

import           Debug.Trace                        (trace, traceShow)

dot        = Token.dot        lexer
plus       = Token.symbol     lexer "+"
star       = Token.symbol     lexer "*"

----------------------------------------------------------------------------------
-- | Type Binders ----------------------------------------------------------------
----------------------------------------------------------------------------------

idBindP :: Parser (Id SourceSpan, RefType)
idBindP = xyP identifierP dcolon bareTypeP

fdBindP :: Parser (Id SourceSpan, RefType)
fdBindP = xyP identifierP dcolon funcSigP

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
mkUn ts  = TApp TUn (sort ts)

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
                spaces
                colon
                spaces
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


---------------------------------------------------------------------------------
-- | Specifications
---------------------------------------------------------------------------------
data PSpec l t 
  = Meas   (Id l, t)
  | Bind   (Id l, t) 
  | Extern (Id l, t)
  | Type   (Id l, TyDef t)
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
  <|> try (reserved "alias"     >> (Talias <$> tAliasP    ))
  <|> try (reserved "predicate" >> (Palias <$> pAliasP    ))
  <|> try (reserved "invariant" >> (withSpan Invt bareTypeP))
  <|>     (reserved "extern"    >> (Extern <$> idBindP    ))

instance (PP l, PP t) => PP (PSpec l t) where
  pp (Meas (i, _))   = text "measure: " <+> pp i
  pp (Bind (i, t))   = text "bind: " <+>  pp i <+> text " :: " <+> pp t
  pp (Extern (i, t)) = text "extern: " <+>  pp i <+> text " :: " <+> pp t
  pp (Type _)        = text "Type:TODO"
  pp (Talias _)      = text "Talias:TODO"
  pp (Palias _)      = text "Palias:TODO"
  pp (Qual _)        = text "Qual:TODO"
  pp (Invt _ _)      = text "Invt:TODO"

-- | `AnnToken`: Elements that can are parsed along the source as annotations.

data AnnToken r 
  = TBind (Id SourceSpan, RType r)          -- ^ Function signature binding
  | TType (RType r)                         -- ^ Type annotation
  | TSpec (PSpec SourceSpan (RType r))      -- ^ Specs: qualifiers, measures, type defs, etc.
  | EmptyToken                              -- ^ Dummy empty token
  deriving (Eq, Ord, Show, Data, Typeable)

instance (PP r, F.Reftable r) => PP (AnnToken r) where
  pp (TBind (id,t)) = pp id <+> text " :: " <+> pp t
  pp (TType t)      = pp t
  pp (TSpec s)      = pp s
  pp EmptyToken     = text "<empyt>"

--------------------------------------------------------------------------------------
tAnnotP :: ParserState String (AnnToken Reft) -> ExternP String (AnnToken Reft)
--------------------------------------------------------------------------------------
tAnnotP stIn = EP typeP bFSigP bTypeP tLevP
  where
    typeP  = TType    <$> changeState fwd bwd bareTypeP
    bFSigP = TBind    <$> changeState fwd bwd fdBindP
    bTypeP = TBind    <$> changeState fwd bwd idBindP
    tLevP  = TSpec    <$> changeState fwd bwd specP
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
parseCodeFromFile :: FilePath -> IO (Either Error (Nano SourceSpan RefType))
--------------------------------------------------------------------------------------
parseCodeFromFile fp = parseJavaScriptFromFile' tAnnotP fp >>= return . mkCode

--------------------------------------------------------------------------------------
mkCode :: ([Statement SourceSpan], M.HashMap SourceSpan [AnnToken Reft]) -> 
  Either Error (Nano SourceSpan RefType)
--------------------------------------------------------------------------------------
mkCode (ss, m) =  do
    tas   <- annots
    return $ Nano { 
        code    = Src (checkTopStmt <$> ss)
      , specs   = envFromList ([ a | TSpec (Extern a) <- list ] ++ [ a | TBind a <- list ])
      , chSpecs = envFromList tas -- populated further with functions later
      , tAnns   = M.fromList $ annT $ M.toList m
      , consts  = envFromList  [ a | TSpec (Meas   a) <- list ] 
      , defs    = envFromList  [ a | TSpec (Type   a) <- list ] 
      , tAlias  = envFromList  [ a | TSpec (Talias a) <- list ]
      , pAlias  = envFromList  [ p | TSpec (Palias p) <- list ]
      , quals   =              [ q | TSpec (Qual   q) <- list ]
      , invts   = [Loc l' t        | TSpec (Invt l t) <- list, let l' = srcPos l]
    } 
  where
    list        = concat $ M.elems m
    vds         = varDeclStmts ss
    prefixed    = [ a     | VarDeclStmt l _  <- vds
                          , ts               <- maybeToList (M.lookup l m)
                          , TBind a <- ts ]
    varDeclAnns = [ (i,t) | VarDeclStmt _ ds <- vds
                          , VarDecl l i _    <- ds
                          , ts               <- maybeToList (M.lookup l m)
                          , TType t          <- ts ]
    annT xs     = [ (ss, t) | (ss, tt) <- xs , TType t <- tt ]
    doubleTyped = [ (l1,s1) | (Id l1 s1, _)  <- prefixed, (Id _ s2, _) <- varDeclAnns, s1 == s2 ]
    annots      | null doubleTyped = Right    $ prefixed ++ varDeclAnns
                | otherwise        = Left     $ errors
    errors      = foldl1 catError $ (uncurry bugMultipleAnnots) <$> doubleTyped 


-------------------------------------------------------------------------------
-- | Parse File and Type Signatures -------------------------------------------
-------------------------------------------------------------------------------

parseNanoFromFile :: FilePath-> IO (Either Error (Nano SourceSpan RefType))
parseNanoFromFile f 
  = do  s <-  parseScriptFromJSON f 
        let ts = fillTypes $ tracePP "parseScriptFromJSON" s
        error $ "parseNanoFromFile:\n" ++ ppshow (collectTypes ts)

  {-= do spec <- parseCodeFromFile =<< getPreludePath-}
  {-     code <- parseCodeFromFile f-}
  {-     case (spec, code) of -}
  {-      (Right s, Right c) -> return $ catSpecDefs $ mconcat [s, c]-}
  {-      (Left  e, _      ) -> return $ Left e-}
  {-      (_      , Left  e) -> return $ Left e-}


collectTypes :: [Statement (SourceSpan, Maybe RefType)] -> [RefType]
collectTypes = concatMap $ FO.foldr (\s -> (++) (maybeToList $ snd s)) []

fillTypes :: [Statement (SourceSpan, Maybe String)] -> [Statement (SourceSpan, Maybe RefType)]
fillTypes = snd . mapAccumL (mapAccumL (parse "")) 0

parse :: SourceName -> Integer -> (SourceSpan, Maybe String) -> (Integer, (SourceSpan, Maybe RefType))
parse f st (ss,c) = maybe (st, (ss,Nothing)) (\t -> failLeft (runParser pp st f (tracePP "String to parse" t))) c
  where pp = liftM2 (,) getState ((ss,) <$> (Just <$> tracePP "bareTypeP" <$> bareTypeP))
        failLeft (Left s) = error $ show s
        failLeft (Right r) = r


parseNanoFromJSON f 
  = do  s <- parseScriptFromJSON f
        return $ undefined


catSpecDefs :: PP t => Nano SourceSpan t -> Either Error (Nano SourceSpan t)
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
definedFuns       :: [Statement SourceSpan] -> [Id SourceSpan]
definedFuns stmts = everything (++) ([] `mkQ` fromFunction) stmts
  where 
    fromFunction (FunctionStmt _ x _ _) = [x] 
    fromFunction _                      = []

varDeclStmts         :: [Statement SourceSpan] -> [Statement SourceSpan]
varDeclStmts stmts    = everything (++) ([] `mkQ` fromVarDecl) stmts
  where 
    fromVarDecl s@(VarDeclStmt _ _) = [s]
    fromVarDecl _                   = []

