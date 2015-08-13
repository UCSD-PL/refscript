{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE DeriveGeneric             #-}


module Language.Nano.Typecheck.Parse
       ( parseNanoFromFiles
       , parseScriptFromJSON
       , parseIdFromJSON
       , RawSpec(..))
       where

import           Prelude                          hiding ( mapM)

import           Data.Either                             (partitionEithers)
import           Data.Default
import           Data.Traversable                        (mapAccumL)
import           Data.Monoid                             (mempty, mconcat)
import           Data.Maybe                              (listToMaybe, catMaybes, maybeToList, fromMaybe)
import           Data.Generics                    hiding (Generic)
import           Data.Aeson                              (eitherDecode)
import           Data.Aeson.Types                 hiding (Parser, Error, parse)
import qualified Data.Aeson.Types                 as     AI
import qualified Data.ByteString.Lazy.Char8       as     B
import           Data.Char                               (isLower)
import qualified Data.List                        as     L
import qualified Data.IntMap.Strict               as I
import qualified Data.HashMap.Strict              as HM
import           Data.Tuple
import qualified Data.HashSet                     as HS

import           Text.PrettyPrint.HughesPJ               (text)
import qualified Data.Foldable                    as     FO
import           Data.Vector                             ((!))
import           Data.Graph.Inductive.Graph

import           Control.Monad
import           Control.Monad.Trans                     (MonadIO,liftIO)
import           Control.Applicative                     ((<$>), (<*>) , (<*) , (*>))

import           Language.Fixpoint.Types          hiding (quals, Loc, Expression)
import           Language.Fixpoint.Parse
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc                  (mapEither, mapSnd, fst3, mapFst)

import           Language.Nano.Annots
import           Language.Nano.Errors
import           Language.Nano.Env
import           Language.Nano.Locations hiding (val)
import           Language.Nano.Names
import           Language.Nano.Misc                      (fst4)
import           Language.Nano.Program
import           Language.Nano.Types              hiding (Exported)
import           Language.Nano.Visitor
import           Language.Nano.Typecheck.Types
import           Language.Nano.Liquid.Types
import           Language.Nano.Liquid.Alias
import           Language.Nano.Liquid.Qualifiers
import           Language.Nano.Typecheck.Resolve
import           Language.Nano.Parser.Common

import           Language.Nano.Syntax
import           Language.Nano.Syntax.PrettyPrint
import           Language.Nano.Syntax.Annotations

import           Text.Parsec                      hiding (parse, State)
import           Text.Parsec.Pos                         (newPos, SourcePos)
import           Text.Parsec.Error                       (errorMessages, showErrorMessages)
import qualified Text.Parsec.Token                as     T
import qualified Data.Text                        as     DT
import           Text.Parsec.Token                       (identStart, identLetter)
-- import           Text.Parsec.Prim                        (stateUser)
import           Text.Parsec.Language                    (emptyDef)

import           GHC.Generics

-- import           Debug.Trace                             ( trace, traceShow)


----------------------------------------------------------------------------------
-- | Type Binders
----------------------------------------------------------------------------------

idBindP :: Parser (Id SrcSpan, RTypeQ RK Reft)
idBindP = withinSpacesP $ xyP identifierP dcolon bareTypeP

idBindP' :: Parser (Id SrcSpan, Assignability, Maybe (RTypeQ RK Reft))
idBindP' = withinSpacesP $ axyP identifierP dcolon typeOrHashP
  where
    typeOrHashP = try (Just <$> bareTypeP)
               <|>    (char '#' >> return Nothing)


anonFuncP :: Parser (RTypeQ RK Reft)
anonFuncP = funcSigP

identifierP :: Parser (Id SrcSpan)
identifierP = withSpan Id identifier

binderP     = withSpan Id $  try identifier
                         <|> try (show <$> integer)

pAliasP :: Parser (Id SrcSpan, PAlias)
pAliasP = do name <- identifierP
             πs   <- pAliasVarsP
             reservedOp "="
             body <- predP
             return  (name, Alias name [] πs body)

pAliasVarsP = try (parens $ sepBy symbolP comma)
           <|> many symbolP

tAliasP :: Parser (Id SrcSpan, TAlias (RTypeQ RK Reft))
tAliasP = do name      <- identifierP
             (αs, πs)  <- mapEither aliasVarT <$> aliasVarsP
             reservedOp "="
             body      <- convertTvar αs <$> bareTypeP
             return      (name, Alias name αs πs body)

aliasVarsP =  try (brackets avarsP)
          <|> try (angles   avarsP)
          <|> return []
  where
    avarsP = sepBy aliasVarP comma

aliasVarP     = withSpan (,) (wordP $ \_ -> True)

aliasVarT :: (SrcSpan, Symbol) -> Either TVar Symbol
aliasVarT (l, x)
  | isTvar x' = Left  $ tvar l x
  | otherwise = Right $ x
  where
    x'        = symbolString x

--
-- PV: Insert your option parser here
--
optionP   = string "REALS" >> return RealOption

iFaceP   :: Parser (Id SrcSpan, IfaceDefQ RK Reft)
iFaceP
  = do  name   <- identifierP
        as     <- option [] tParP
        case as of
          (m:_) -> do h   <- heritageP
                      withSpan (mkTm1 m) (braces propBindP) >>= \case
                        Left err -> fail $ ppshow err
                        Right es -> return (name, convertTvar as $ ID (mkrn name) InterfaceKind as h es)
          -- The mutability here shouldn't matter
          _     -> do h   <- heritageP
                      withSpan mkTm2 (braces propBindP) >>= \case
                        Left err -> fail $ ppshow err
                        Right es -> return (name, convertTvar as $ ID (mkrn name) InterfaceKind as h es)
  where
    mkTm1 m l = mkTypeMembers (TVar m fTop) . map (mkTup l)
    mkTm2   l = mkTypeMembers defMut        . map (mkTup l)
    mkTup l e = (l,symbol e,InstanceMember,MemDeclaration,e)
    defMut = TRef (QN RK_ (srcPos dummySpan) [] (symbol "Immutable")) [] fTop
    mkrn   = mkRelName [] . symbol


extendsGenP :: String -> Parser [TypeReferenceQ RK Reft]
extendsGenP s = option [] $ reserved s >> sepBy1 extendsGen1P comma

extendsGen1P :: Parser (TypeReferenceQ RK Reft)
extendsGen1P = do qn  <- qnameP
                  ts  <- option [] $ angles $ sepBy bareTypeP comma
                  return (qn, ts)

extendsP :: Parser [TypeReferenceQ RK Reft]
extendsP = extendsGenP "extends"

implementsP :: Parser [TypeReferenceQ RK Reft]
implementsP = extendsGenP "implements"


heritageP :: Parser (HeritageQ RK Reft)
heritageP = (,) <$> extendsP <*> implementsP


qnameP  :: Parser RelName
qnameP   = do optionMaybe (char '#')    -- backwards-compatibility fix
              withSpan qname $ sepBy1 qSymbolP (char '.')
  where
    qname s x = QN RK_ s (init x) (last x)

-- | Redefining some stuff to make the Qualified names parse right
qSymbolP    :: Parser Symbol
qSymbolP    = symbol <$> qSymCharsP
qSymCharsP  = condIdP' qSymChars (`notElem` keyWordSyms)

keyWordSyms = ["if", "then", "else", "mod"]
qSymChars   = ['a' .. 'z'] ++
              ['A' .. 'Z'] ++
              ['0' .. '9'] ++
              ['_', '%', '#'] -- omitting '.'

condIdP'  :: [Char] -> (String -> Bool) -> Parser Symbol
condIdP' chars f
  = do c  <- try letter <|> oneOf ['_']
       cs <- many (satisfy (`elem` chars))
       blanks
       if f (c:cs) then return (symbol $ DT.pack $ c:cs) else parserZero


-- [A,B,C...]
tParP    = angles $ sepBy tvarP comma

withSpan f p = do pos   <- getPosition
                  x     <- p
                  pos'  <- getPosition
                  return $ f (SS pos pos') x


xyP lP sepP rP
  = (\x _ y -> (x, y)) <$> lP <*> (spaces >> sepP) <*> rP

axyP lP sepP rP
  = do  a <- assignabilityP
        i <- withinSpacesP lP
        spaces >> sepP
        r <- rP
        return (i,a,r)
        -- (\a x _ y -> (x, a, y)) <$> aP <*> lP <*> (spaces >> sepP) <*> rP

assignabilityP
  =  try (withinSpacesP (reserved "global"  ) >> return WriteGlobal)
 <|> try (withinSpacesP (reserved "local"   ) >> return WriteLocal )
 <|> try (withinSpacesP (reserved "readonly") >> return ReadOnly   )
 <|>     (return WriteGlobal)

postP p post
  = (\x _ -> x) <$> p <*> post

----------------------------------------------------------------------------------
-- | RefTypes
----------------------------------------------------------------------------------
-- | `bareTypeP` parses top-level "bare" types. If no refinements are supplied,
--    then "top" refinement is used.
----------------------------------------------------------------------------------
bareTypeP :: Parser (RTypeQ RK Reft)
----------------------------------------------------------------------------------
bareTypeP = bareAllP $ bodyP
  where
    bodyP =  try bUnP
         <|> try (refP rUnP)
         <|>     (xrefP rUnP)

rUnP        = mkU <$> parenNullP (bareTypeNoUnionP `sepBy1` plus) toN
  where
    mkU [x] = strengthen x
    mkU xs  = flattenUnions . TApp TUn xs
    toN     = (tNull:)

bUnP        = parenNullP (bareTypeNoUnionP `sepBy1` plus) toN >>= mkU
  where
    mkU [x] = return x
    mkU xs  = flattenUnions . TApp TUn xs <$> topP
    toN     = (tNull:)

-- FIXME: disallow functions in unions?
-- | `bareTypeNoUnionP` parses a type that does not contain a union at the top-level.
bareTypeNoUnionP = try funcSigP <|> (bareAllP $ bareAtomP bbaseP)

-- | `optNullP` optionally parses "( `a` )?", where `a` is parsed by the input parser @pr@.
parenNullP p f =  try (f <$> postP p question) <|> p

-- | `funcSigP` parses a function type that is possibly generic and/or an intersection.
funcSigP =  try (bareAllP bareFunP)
        <|> try (intersectP $ bareAllP bareFunP)
  where
    intersectP p = mkAnd <$> many1 (reserved "/\\" >> withinSpacesP p)

methSigP =  try (bareAllP bareMethP)
        <|> try (intersectP $ bareAllP bareMethP)
  where
    intersectP p = mkAnd <$> many1 (reserved "/\\" >> withinSpacesP p)


-- | `bareFunP` parses a single function type
--
--  (x:t, ...) => t
--
bareFunP
  = do args   <- parens $ sepBy bareArgP comma
       reserved "=>"
       ret    <- bareTypeP
       r      <- topP
       return $ mkF args ret r
  where
    mkF as ret r = case as of
      (B s t : ts) | s == symbol "this" -> TFun (Just t) ts ret r
      ts                                -> TFun Nothing ts ret r


--  (x:t, ...): t
bareMethP
  = do args   <- parens $ sepBy bareArgP comma
       _      <- colon
       ret    <- bareTypeP
       r      <- topP
       return  $ mkF args ret r
  where
    mkF as ret r = case as of
      (B s t : ts) | s == symbol "this" -> TFun (Just t) ts ret r
      ts                                -> TFun Nothing ts ret r


bareArgP
  =   (try boundTypeP)
 <|>  (argBind <$> try (bareTypeP))

boundTypeP = do s <- symbol <$> identifierP
                withinSpacesP colon
                B s <$> bareTypeP

argBind t = B (rTypeValueVar t) t

bareAtomP p
  =  try (xrefP  p)
 <|> try (refP p)
 <|>     (dummyP p)


----------------------------------------------------------------------------------
bbaseP :: Parser (Reft -> RTypeQ RK Reft)
----------------------------------------------------------------------------------
bbaseP
  =  try objLitP                               -- {f1: T1; ... ; fn: Tn}
 <|> try (TApp  <$> tConP  <*> bareTyArgsP)    -- number, boolean, etc...
 <|> try selfP
 <|>     (TRef  <$> qnameP <*> bareTyArgsP)    -- List[A], Tree[A,B] etc...

----------------------------------------------------------------------------------
objLitP :: Parser (Reft -> RTypeQ RK Reft)
----------------------------------------------------------------------------------
objLitP
  = do  m <- fromMaybe defMut <$> optionMaybe (toType <$> mutP)
        withSpan (mkTm m) (braces propBindP) >>= \case
          Left err -> fail   $ ppshow err
          Right ms -> return $ TCons m ms
  where
    defMut     = TRef (QN RK_ (srcPos dummySpan) [] (symbol "Immutable")) [] fTop
    mkTm m l   = mkTypeMembers m . map (mkTup l)
    mkTup l e  = (l,symbol e,InstanceMember,MemDeclaration,e)

----------------------------------------------------------------------------------
selfP :: Parser (Reft -> RTypeQ RK Reft)
----------------------------------------------------------------------------------
selfP = do reserved "Self"
           m <- angles bareTypeP
           return $ \_ -> TSelf m

----------------------------------------------------------------------------------
mutP :: Parser (MutabilityQ RK)
----------------------------------------------------------------------------------
mutP = TRef <$> brackets qnameP  <*> return [] <*> return ()


bareTyArgsP
  =  try (brackets argsP)
 <|> try (angles   argsP)
 <|> return []
     where
       argsP = sepBy bareTyArgP comma

bareTyArgP
  = try bareTypeP
 <|> (TExp <$> exprP)

----------------------------------------------------------------------------------
tvarP    :: Parser TVar
----------------------------------------------------------------------------------
tvarP    = withSpan tvar $ wordP isTvar

tvar l x = TV x l

isTvar   = not . isLower . head

wordP p  = condIdP ok p
  where
    ok   = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0'..'9']

----------------------------------------------------------------------------------
tConP :: Parser TCon
----------------------------------------------------------------------------------
tConP =  try (reserved "number"      >> return TInt)
     <|> try (reserved "bitvector32" >> return TBV32)
     <|> try (reserved "boolean"     >> return TBool)
     <|> try (reserved "undefined"   >> return TUndef)
     <|> try (reserved "void"        >> return TVoid)
     <|> try (reserved "top"         >> return TTop)
     <|> try (reserved "string"      >> return TString)
     <|> try (reserved "null"        >> return TNull)
     <|> try (reserved "bool"        >> return TFPBool)

bareAllP p
  = do tvs   <- optionMaybe (reserved "forall" *> many1 tvarP <* dot)
       t     <- p
       return $ maybe t (`tAll` t) tvs
    where
       tAll αs t = foldr TAll (convertTvar αs t) αs

propBindP      =  sepEndBy propEltP semi
  where
    propEltP   =  try indexEltP
              <|> try fieldEltP
              <|> try methEltP
              <|> try callEltP
              <|>     consEltP

-- | [f: string]: t
-- | [f: number]: t
indexEltP = do ((x,it),t) <- xyP (brackets indexP) colon bareTypeP
               case it of
                 "number" -> return $ IndexSig x NumericIndex t
                 "string" -> return $ IndexSig x StringIndex t
                 _        -> error $ "Index signature can only have " ++
                                     "string or number as index."

indexP = xyP id colon sn
  where
    id = symbol <$> (try lowerIdP <|> upperIdP)
    sn = withinSpacesP (string "string" <|> string "number")

-- | <[mut]> f<?>: t
fieldEltP       = do
    x          <- symbol <$> binderP
    o          <- maybe f_requiredR (\_ -> f_optionalR)
              <$> optionMaybe (withinSpacesP $ char '?')
    _          <- colon
    m          <- option mut (toType <$> mutP)
    t          <- bareTypeP
    return      $ FieldSig x o m t
  where
    mut         = tr_inheritedMut

-- | m: mt
methEltP        = do
    x          <- symbol <$> identifierP
    _          <- colon
    t          <- methSigP
    return      $ MethSig x t
  where

-- | <forall A .> (t...) => t
callEltP = CallSig <$> withinSpacesP funcSigP

-- | new <forall A .> (t...) => t
consEltP = reserved "new" >> ConsSig <$> withinSpacesP funcSigP

----------------------------------------------------------------------------------
dummyP ::  Parser (Reft -> b) -> Parser b
----------------------------------------------------------------------------------
dummyP fm = fm `ap` topP

----------------------------------------------------------------------------------
topP   :: Parser Reft
----------------------------------------------------------------------------------
topP   = (Reft . (, mempty) . vv . Just) <$> freshIntP'

-- Using a slightly changed version of `freshIntP`
----------------------------------------------------------------------------------
freshIntP' :: Parser Integer
----------------------------------------------------------------------------------
freshIntP' = do n <- stateUser <$> getParserState
                putState $ n+1
                return n

----------------------------------------------------------------------------------
-- | Parses refined types of the form: `{ kind | refinement }`
----------------------------------------------------------------------------------
xrefP :: Parser (Reft -> a) -> Parser a
xrefP kindP
  = braces $ do
      t  <- kindP
      reserved "|"
      ra <- refaP
      return $ t (Reft (symbol "v", ra))

----------------------------------------------------------------------------------
classDeclP :: Parser (Id SrcSpan, ClassSigQ RK Reft)
----------------------------------------------------------------------------------
classDeclP = do
    reserved "class"
    id      <- identifierP
    vs      <- option [] $ angles $ sepBy tvarP comma
    (es,is) <- heritageP
    return (id, (vs,es,is))

