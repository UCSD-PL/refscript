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


module Language.Rsc.Parser.Types where

import           Control.Applicative          ((*>), (<$>), (<*>))
import           Control.Monad
import           Data.Char                    (isLower)
import           Data.Generics                hiding (Generic)
import           Data.List                    (foldl')
import           Data.Monoid                  (mempty)
import qualified Data.Text                    as DT
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc       (mapEither)
import           Language.Fixpoint.Names
import           Language.Fixpoint.Parse
import qualified Language.Fixpoint.Types      as F
import           Language.Rsc.Liquid.Types
import           Language.Rsc.Names
import           Language.Rsc.Options
import           Language.Rsc.Parser.Common
import           Language.Rsc.Parser.Lexer
import           Language.Rsc.Pretty
import           Language.Rsc.Transformations
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
import           Prelude                      hiding (mapM)
import           Text.Parsec                  hiding (State, parse)

-- import           Language.Rsc.Pretty


type RRType = RTypeQ RK F.Reft
type RMutability = RTypeQ RK F.Reft

--------------------------------------------------------------------------------
-- | Type Binders
--------------------------------------------------------------------------------

idBindP :: Parser (Id SrcSpan, RRType)
idBindP = withinSpacesP $ xyP identifierP dcolon bareTypeP

idBindP' :: Parser (Id SrcSpan, Assignability, Maybe RRType)
idBindP' = withinSpacesP $ axyP identifierP dcolon typeOrHashP
  where
    typeOrHashP = try (Just <$> bareTypeP)
               <|>    (char '#' >> return Nothing)


functionExpressionP :: Parser RRType
functionExpressionP = funcSigP

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

pAliasVarsP = try (parens $ symbolP `sepBy` comma)
           <|> many symbolP

tAliasP :: Parser (Id SrcSpan, TAlias RRType)
tAliasP = do withinSpacesP $ reserved "type"
             name      <- identifierP
             (αs, πs)  <- mapEither aliasVarT <$> aliasVarsP
             reservedOp "="
             body      <- convertTVar αs <$> bareTypeP
             return (name, Alias name αs πs body)

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

interfaceP :: Parser (TypeDeclQ RK F.Reft)
interfaceP = reserved "interface" >> TD <$> typeSignatureP InterfaceTDK <*> typeBodyP

classDeclP = reserved "class"     >> typeSignatureP ClassTDK

-- Mutability parameter should be included here.
typeSignatureP :: TypeDeclKind -> Parser (TypeSigQ RK F.Reft)
typeSignatureP k = TS k <$> btgenP <*> heritageP

btgenP = BGen <$> qnameP <*> option [] bTParP

-- TODO: is convertTVar necessary here?
typeBodyP :: Parser (TypeMembersQ RK F.Reft)
typeBodyP = eltKindsToTypeMembers <$> braces propBindP

extendsGenP :: String -> Parser [TGenQ RK F.Reft]
extendsGenP s = option [] $ reserved s >> sepBy1 extendsGen1P comma

extendsGen1P :: Parser (TGenQ RK F.Reft)
extendsGen1P = do qn  <- qnameP
                  ts  <- option [] $ angles $ sepBy bareTypeP comma
                  return $ Gen qn ts

extendsP :: Parser [TGenQ RK F.Reft]
extendsP = extendsGenP "extends"

implementsP :: Parser [TGenQ RK F.Reft]
implementsP = extendsGenP "implements"


heritageP :: Parser (HeritageQ RK F.Reft)
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
       if f (c:cs) then return (F.symbol $ DT.pack $ c:cs) else parserZero


-- | <A, B, C ...>
tParP  = angles $ sepBy tvarP comma

-- | <A extends T, B extends S, ...>
bTParP = angles $ sepBy btvarP comma

--------------------------------------------------------------------------------
-- | RefTypes
--------------------------------------------------------------------------------
-- | `bareTypeP` parses top-level "bare" types. If no refinements are supplied,
--    then "top" refinement is used.
bareTypeP :: Parser RRType
bareTypeP = bareAllP bUnP

bUnP        = parenNullP (bareTypeNoUnionP `sepBy1` plus) toN >>= mkU
  where
    mkU [x] = return x
    mkU xs  = return $ TOr xs -- flattenUnions . TOr xs
    toN     = (tNull:)

-- | `bareTypeNoUnionP` parses a type that does not contain a union at the
-- top-level.
bareTypeNoUnionP = try funcSigP <|> bareAllP (bareAtomP bbaseP)

-- | `parenNullP p f` optionally parses "( `a` )?", where `a` is parsed by the
-- input parser @p@.
parenNullP p f =  try (f <$> postP p question) <|> p


-- | `funcSigP` parses a function type that is possibly generic and/or an
-- intersection.
funcSigP =  try (bareAllP bareFunP)
        <|> try (intersectP $ bareAllP bareFunP)
  where
    intersectP p = mkAnd <$> many1 (reserved "/\\" >> withinSpacesP p)

-- -- PV: disabling intersect here - instead use mutliple comments
-- methSigP =  try (bareAllP bareMethP)
--         <|> try (intersectP $ bareAllP bareMethP)
--   where
--     intersectP p = mkAnd <$> many1 (reserved "/\\" >> withinSpacesP p)

methSigP  = bareAllP bareMethP
bareFunP  = bareArrowP (reserved "=>")    --  (x:t, ...) => t
bareMethP = bareArrowP colon              --  (x:t, ...): t

bareArrowP f = do args   <- parens $ sepBy bareArgP comma
                  _     <- f
                  ret   <- bareTypeP
                  r     <- topP
                  return $ TFun args ret r

bareArgP     = try boundTypeP <|> (argBind <$> try bareTypeP)

-- TODO: Take the optional argument into account
boundTypeP   = do s <- symbol <$> identifierP
                  optional (char '?')
                  withinSpacesP colon
                  B s <$> bareTypeP

argBind t = B (rTypeValueVar t) t

bareAtomP p
  =  try (xrefP  p)
 <|> try (refP p)
 <|>     (dummyP p)


--------------------------------------------------------------------------------
bbaseP :: Parser (F.Reft -> RRType)
--------------------------------------------------------------------------------
bbaseP
  =  try (TObj trImm <$> typeBodyP)  -- {f1: T1; ... ; fn: Tn}
 <|> try (TPrim      <$> tPrimP)     -- number, boolean, etc...
 <|>     (TRef       <$> tGenP)      -- List<A>, Tree<A,B> etc...

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

--------------------------------------------------------------------------------
tvarP    :: Parser TVar
--------------------------------------------------------------------------------
tvarP    = withSpan tvar $ wordP isTvar

--------------------------------------------------------------------------------
btvarP    :: Parser (BTVarQ RK F.Reft)
--------------------------------------------------------------------------------
btvarP   = withSpan btvar $ (,) <$> wordP isTvar
                                <*> optionMaybe (reserved "extends" *> bareTypeP)

tvar l x = TV x l

btvar l (x,t) = BTV x l t

isTvar = not . isLower . head

wordP  = condIdP ok
  where
    ok = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0'..'9']

--------------------------------------------------------------------------------
tPrimP :: Parser TPrim
--------------------------------------------------------------------------------
tPrimP =  try (reserved "number"      >> return TNumber)
      <|> try (reserved "bitvector32" >> return TBV32)
      <|> try (reserved "boolean"     >> return TBoolean)
      <|> try (reserved "undefined"   >> return TUndefined)
      <|> try (reserved "void"        >> return TVoid)
      <|> try (reserved "top"         >> return TTop)
      <|> try (reserved "string"      >> return TString)
      <|> try (reserved "null"        >> return TNull)
      <|> try (reserved "bool"        >> return TFPBool)
      <|> try (reserved "any"         >> return TAny)

bareAllP p
  -- = do tvs   <- optionMaybe (reserved "forall" *> many1 btvarP <* dot)
  = do tvs   <- optionMaybe $ angles $ btvarP `sepBy1` comma
       t     <- p
       return $ maybe t (`tAll` t) tvs
    where
       tAll αs t = foldr TAll (convertTVar (btvToTV <$> αs) t) αs

propBindP   = sepEndBy memberP semi
  where
    unc     = \f (a,b,c,d,e) -> f a b c d e
    memberP =  try idxP
           <|> try (    Ctor <$> ctorP) -- Ctor needs to be before Meth
           <|> try (unc Prop <$> propP)
           <|> try (unc Meth <$> methP)
           <|>     (    Call <$> callP)

data EltKind = Prop Symbol StaticKind Optionality RMutability RRType
             | Meth Symbol StaticKind Optionality MutabilityMod RRType
             | Call RRType
             | Ctor RRType
             | SIdx RRType
             | NIdx RRType
             deriving (Data, Typeable)

--------------------------------------------------------------------------------
eltKindsToTypeMembers :: [EltKind] -> TypeMembersQ RK F.Reft
--------------------------------------------------------------------------------
eltKindsToTypeMembers = foldl' go mempty
  where
    go ms (Prop f InstanceK o m t) = ms { tm_prop  = F.insertSEnv f (FI o m t) (tm_prop  ms) }
    go ms (Prop f StaticK   o m t) = ms { tm_sprop = F.insertSEnv f (FI o m t) (tm_sprop ms) }
    go ms (Meth n InstanceK o m t) = ms { tm_meth  = F.insertSEnv n (MI o m t) (tm_meth  ms) }
    go ms (Meth n StaticK   o m t) = ms { tm_smeth = F.insertSEnv n (MI o m t) (tm_smeth ms) }
    -- TODO: Multiple overloads are dropped
    go ms (Call t) = ms { tm_call = Just t }
    go ms (Ctor t) = ms { tm_ctor = Just t }
    go ms (SIdx t) = ms { tm_sidx = Just t }
    go ms (NIdx t) = ms { tm_nidx = Just t }

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

-- | [STATIC] f[?]: [MUTABILITY] t
--
--    Default value for [MUTABILITY] is Mutable
--
propP = do  s     <- option InstanceK (reserved "static" *> return StaticK)
            x     <- symbol <$> binderP
            o     <- option Req (withinSpacesP (char '?') *> return Opt)
            _     <- colon
            m     <- option trMut mutabilityP
            t     <- bareTypeP
            return $ (x, s, o, m, t)

-- | [STATIC] [MUTABILITY] m[<A..>](x:t,..): t
methP = do  s     <- option InstanceK (reserved "static" *> return StaticK)
            m     <- methMutabilityP
            x     <- symbol <$> identifierP
            o     <- option Req (withinSpacesP (char '?') *> return Opt)
            t     <- methSigP
            return $ (x, s, o, m, t)

-- | [<A..>](t..): t
callP           = withinSpacesP methSigP

-- | new [<A..>](t..): t
ctorP           = withinSpacesP (reserved "new")
               *> withinSpacesP methSigP

mutabilityP     =  brackets bareTypeP

methMutabilityP =  try (reserved "Mutable"       >> return Mutable)
               <|> try (reserved "Immutable"     >> return Immutable)
               <|> try (reserved "ReadOnly"      >> return ReadOnly)
               <|> try (reserved "AssignsFields" >> return AssignsFields)
               <|>     (                            return Mutable)

--------------------------------------------------------------------------------
dummyP ::  Parser (F.Reft -> b) -> Parser b
--------------------------------------------------------------------------------
dummyP fm = fm `ap` topP

--------------------------------------------------------------------------------
topP   :: Parser F.Reft
--------------------------------------------------------------------------------
topP   = (F.Reft . (, mempty) . vv . Just) <$> freshIntP'

-- Using a slightly changed version of `freshIntP`
--------------------------------------------------------------------------------
freshIntP' :: Parser Integer
--------------------------------------------------------------------------------
freshIntP' = do n <- stateUser <$> getParserState
                putState $ n+1
                return n

--------------------------------------------------------------------------------
-- | Parses refined types of the form: `{ kind | refinement }`
--------------------------------------------------------------------------------
xrefP :: Parser (F.Reft -> a) -> Parser a
xrefP kindP
  = braces $ do
      t  <- kindP
      reserved "|"
      ra <- refaP
      return $ t (F.Reft (symbol "v", ra))
