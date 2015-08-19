{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}


module Language.Nano.Parser.Types where

import           Control.Applicative           ((*>), (<$>), (<*), (<*>))
import           Control.Monad
import           Data.Char                     (isLower)
import           Data.Generics                 hiding (Generic)
import           Data.Monoid                   (mempty)
import qualified Data.Text                     as DT
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc        (mapEither)
import           Language.Fixpoint.Parse
import           Language.Fixpoint.Types       hiding (Expression, Loc, quals)
import           Language.Nano.Annots
import           Language.Nano.AST
import           Language.Nano.Liquid.Types
import           Language.Nano.Names
import           Language.Nano.Parser.Common
import           Language.Nano.Parser.Lexer
import           Language.Nano.Typecheck.Types
import           Language.Nano.Types
import           Prelude                       hiding (mapM)
import           Text.Parsec                   hiding (State, parse)


type RRType = RTypeQ RK Reft
type RMutability = RTypeQ RK Reft

----------------------------------------------------------------------------------
-- | Type Binders
----------------------------------------------------------------------------------

idBindP :: Parser (Id SrcSpan, RRType)
idBindP = withinSpacesP $ xyP identifierP dcolon bareTypeP

idBindP' :: Parser (Id SrcSpan, Assignability, Maybe RRType)
idBindP' = withinSpacesP $ axyP identifierP dcolon typeOrHashP
  where
    typeOrHashP = try (Just <$> bareTypeP)
               <|>    (char '#' >> return Nothing)


anonFuncP :: Parser RRType
anonFuncP = funcSigP

identifierP :: Parser (Id SrcSpan)
identifierP = withSpan Id identifier

binderP :: Parser (Id SrcSpan)
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

tAliasP :: Parser (Id SrcSpan, TAlias RRType)
tAliasP = do name      <- identifierP
             (αs, πs)  <- mapEither aliasVarT <$> aliasVarsP
             reservedOp "="
             body      <- convertTVar αs <$> bareTypeP
             return      (name, Alias name αs πs body)

aliasVarsP =  try (brackets avarsP)
          <|> try (angles   avarsP)
          <|> return []
  where
    avarsP = sepBy aliasVarP comma

aliasVarP     = withSpan (,) (wordP $ const True)

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

interfaceP :: Parser (TypeDeclQ RK Reft)
interfaceP = TD <$> typeSignatureP InterfaceKind<*> typeBodyP

classDeclP = typeSignatureP ClassKind

-- Mutability parameter should be included here.
typeSignatureP :: TypeDeclKind -> Parser (TypeSigQ RK Reft)
typeSignatureP k = TS k <$> btgenP <*> heritageP

btgenP = BGen <$> qnameP <*> option [] bTParP

-- TODO: is convertTVar necessary here?
typeBodyP = return $ TM undefined undefined undefined undefined undefined undefined undefined undefined
  where
    _ = braces propBindP

--     case as of
--       (m:_) -> do h <- heritageP
--                   withSpan (mkTm1 m) (braces propBindP) >>= \case
--                     Left err -> fail $ ppshow err
--                     Right es -> return undefined -- (name, convertTVar as $ TD (TS InterfaceKind  mkrn name) as h es)
--
--
--           -- The mutability here shouldn't matter
--       _ -> do h <- heritageP
--               withSpan mkTm2 (braces propBindP) >>= \case
--                 Left err -> fail $ ppshow err
--                 Right es -> return undefined -- (name, convertTVar as $ ID (mkrn name) InterfaceKind as h es)
--   where
--     mkTm1 m l = mkTypeMembers (TVar m fTop) . map (mkTup l)
--     mkTm2   l = mkTypeMembers defMut        . map (mkTup l)
--     mkTup l e = (l,symbol e,InstanceMember,MemDeclaration,e)
--     defMut = TRef (QN RK_ (srcPos dummySpan) [] (symbol "Immutable")) [] fTop
--     mkrn   = mkRelName [] . symbol
--

extendsGenP :: String -> Parser [TGenQ RK Reft]
extendsGenP s = option [] $ reserved s >> sepBy1 extendsGen1P comma

extendsGen1P :: Parser (TGenQ RK Reft)
extendsGen1P = do qn  <- qnameP
                  ts  <- option [] $ angles $ sepBy bareTypeP comma
                  return $ Gen qn ts

extendsP :: Parser [TGenQ RK Reft]
extendsP = extendsGenP "extends"

implementsP :: Parser [TGenQ RK Reft]
implementsP = extendsGenP "implements"


heritageP :: Parser (HeritageQ RK Reft)
heritageP = (,) <$> extendsP <*> implementsP


qnameP  :: Parser RelName
qnameP   = do optionMaybe (char '#')    -- backwards-compatibility fix
              withSpan qname $ sepBy1 qSymbolP (char '.')
  where
    qname s x = QN (QP RK_ s (init x)) (last x)

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


-- | <A, B, C ...>
tParP  = angles $ sepBy tvarP comma

-- | <A extends T, B extends S, ...>
bTParP = angles $ sepBy btvarP comma

----------------------------------------------------------------------------------
-- | RefTypes
----------------------------------------------------------------------------------
-- | `bareTypeP` parses top-level "bare" types. If no refinements are supplied,
--    then "top" refinement is used.
----------------------------------------------------------------------------------
bareTypeP :: Parser RRType
----------------------------------------------------------------------------------
bareTypeP = bareAllP bUnP

bUnP        = parenNullP (bareTypeNoUnionP `sepBy1` plus) toN >>= mkU
  where
    mkU [x] = return x
    mkU xs  = return $ TOr xs -- flattenUnions . TOr xs
    toN     = (tNull:)

-- | `bareTypeNoUnionP` parses a type that does not contain a union at the top-level.
bareTypeNoUnionP = try funcSigP <|> bareAllP (bareAtomP bbaseP)

-- | `parenNullP p f` optionally parses "( `a` )?", where `a` is parsed by the input parser @p@.
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

--  (x:t, ...) => t
bareFunP = bareArrowP $ reserved "=>"
--  (x:t, ...): t
bareMethP = bareArrowP colon

bareArrowP f = do args   <- parens $ sepBy bareArgP comma
                  _     <- f
                  ret   <- bareTypeP
                  r     <- topP
                  return $ TFun args ret r

bareArgP = try boundTypeP <|> (argBind <$> try bareTypeP)

boundTypeP = do s <- symbol <$> identifierP
                withinSpacesP colon
                B s <$> bareTypeP

argBind t = B (rTypeValueVar t) t

bareAtomP p
  =  try (xrefP  p)
 <|> try (refP p)
 <|>     (dummyP p)


----------------------------------------------------------------------------------
bbaseP :: Parser (Reft -> RRType)
----------------------------------------------------------------------------------
bbaseP
  =  try (TObj  <$> typeBodyP)  -- {f1: T1; ... ; fn: Tn}
 <|> try (TPrim <$> tPrimP)     -- number, boolean, etc...
 <|>     (TRef  <$> tGenP)      -- List<A>, Tree<A,B> etc...

tGenP = Gen <$> qnameP <*> bareTyArgsP

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

----------------------------------------------------------------------------------
btvarP    :: Parser (BTVarQ RK Reft)
----------------------------------------------------------------------------------
btvarP   = withSpan btvar $ (,) <$> wordP isTvar
                                <*> optionMaybe (reserved "extends" *> bareTypeP)

tvar l x = TV x l

btvar l (x,t) = BTV x l t

isTvar = not . isLower . head

wordP  = condIdP ok
  where
    ok = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0'..'9']

----------------------------------------------------------------------------------
tPrimP :: Parser TPrim
----------------------------------------------------------------------------------
tPrimP =  try (reserved "number"      >> return TNumber)
      <|> try (reserved "bitvector32" >> return TBV32)
      <|> try (reserved "boolean"     >> return TBoolean)
      <|> try (reserved "undefined"   >> return TUndefined)
      <|> try (reserved "void"        >> return TVoid)
      <|> try (reserved "top"         >> return TTop)
      <|> try (reserved "string"      >> return TString)
      <|> try (reserved "null"        >> return TNull)
      <|> try (reserved "bool"        >> return TFPBool)

bareAllP p
  = do tvs   <- optionMaybe (reserved "forall" *> many1 btvarP <* dot)
       t     <- p
       return $ maybe t (`tAll` t) tvs
    where
       tAll αs t = foldr TAll (convertTVar (btvToTV <$> αs) t) αs

propBindP   = sepEndBy memberP semi
  where
    memberP =  try idxP
           <|> try (propP >>= \(x,s,o,m,t) -> return $ Prop x s o m t)
           <|> try (methP >>= \(x,s,o,m,t) -> return $ Meth x s o m t)
           <|> try callP
           <|>     (Ctor <$> ctorP)

data EltKind = Prop Symbol StaticKind Optionality RMutability RRType
             | Meth Symbol StaticKind Optionality MutabilityMod RRType
             | Call RRType
             | Ctor RRType
             | SIdx RRType
             | NIdx RRType
             deriving (Data, Typeable)

-- | [f: string]: t
-- | [f: number]: t
idxP = do ((_, k), t) <- xyP (brackets indexP) colon bareTypeP
          case k of
            "number" -> return $ NIdx t
            "string" -> return $ SIdx t
            _        -> error $ "Index signature can only have " ++
                                "string or number as index."

indexP = xyP id colon sn
  where
    id = symbol <$> (try lowerIdP <|> upperIdP)
    sn = withinSpacesP (string "string" <|> string "number")

-- | [STATIC] [MUTABILITY] f[?]: t     (Default value for [MUTABILITY] is Mutable)
propP = do  s     <- (reserved "static" >> return StaticMember) <|> (return InstanceMember)
            m     <- option trMut mutabilityP
            x     <- symbol <$> binderP
            o     <- option Req (withinSpacesP (char '?') *> return Opt)
            _     <- colon
            t     <- bareTypeP
            return $ (x, s, o, m, t)

-- | [STATIC] [MUTABILITY] m[<A..>](x:t,..): t
methP = do  s     <- (reserved "static" >> return StaticMember) <|> (return InstanceMember)
            m     <- methMutabilityP
            x     <- symbol <$> identifierP
            o     <- maybe Req (\_ -> Opt) <$> optionMaybe (withinSpacesP $ char '?')
            _     <- colon
            t     <- methSigP
            return $ (x, s, o, m, t)

-- | [<A..>](t..) => t
callP = Call <$> withinSpacesP funcSigP

-- | new [<A..>](t..) => t
ctorP = reserved "new" >> withinSpacesP funcSigP

mutabilityP     =  try (reserved "Mutable" >> return trMut)
               <|> try (reserved "Immutable" >> return trImm)
               <|>     (reserved "ReadOnly" >> return trRO)

methMutabilityP =  try (reserved "Mutable" >> return Mutable)
               <|> try (reserved "Immutable" >> return Immutable)
               <|> try (reserved "ReadOnly" >> return ReadOnly)
               <|>     (reserved "AssignsFields" >> return AssignsFields)

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

