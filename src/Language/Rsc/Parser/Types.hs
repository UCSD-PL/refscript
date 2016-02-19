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

import           Control.Exception              (throw)
import           Control.Monad
import           Data.Char                      (isLower)
import           Data.Foldable                  (concat)
import           Data.Generics                  hiding (Generic)
import qualified Data.HashSet                   as S
import           Data.Maybe                     (catMaybes, fromMaybe)
import           Data.Monoid
import           Language.Fixpoint.Parse
import qualified Language.Fixpoint.Types        as F
import           Language.Fixpoint.Types.Errors
import           Language.Fixpoint.Types.Names
import           Language.Rsc.Liquid.Types
import           Language.Rsc.Locations
import           Language.Rsc.Misc              (mapEither)
import           Language.Rsc.Module
import           Language.Rsc.Names
import           Language.Rsc.Parser.Common
import           Language.Rsc.Parser.Lexer
import           Language.Rsc.Pretty
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
import           Prelude                        hiding (concat, mapM)
import           Text.Parsec                    hiding (State, parse)
import           Text.PrettyPrint.HughesPJ      ((<+>))

-- import           Language.Rsc.Pretty


type RMutability = RTypeQ RK F.Reft


-- TODO: Can Also take care of qualified names etc...

data PContext = PContext {
      pctx_vars :: [TVar]                         -- Type variables in scope
    , pctx_mut  :: Maybe (MutabilityQ RK F.Reft)  -- Ambient mutability
  }

instance Monoid PContext where
  mempty = PContext [] Nothing
  mappend (PContext vs m) (PContext vs' m')
    = PContext (vs `mappend` vs') (m `mappendMutOpt` m')

Nothing `mappendMutOpt` m       = m
m       `mappendMutOpt` Nothing = m
Just _  `mappendMutOpt` Just m  = Just m


pCtxFromList :: [BTVarQ q r] -> PContext
pCtxFromList bs = PContext (map btvToTV bs) Nothing

pCtxFromListWithMut :: [BTVarQ RK F.Reft] -> PContext
pCtxFromListWithMut []        = mempty
pCtxFromListWithMut bs@(b1:_)
  | mutRelatedBVar b1
  = PContext (map btvToTV bs) (Just (btVar b1))
  | otherwise
  = PContext (map btvToTV bs) Nothing

pCtxFromSig = pCtxFromListWithMut . b_args . sigTRef


instance PP PContext where
  pp (PContext vs m) = pp vs <+> pp m

--------------------------------------------------------------------------------
-- | Type Binders
--------------------------------------------------------------------------------

idBindP2 :: PContext -> Parser (Id SrcSpan, RRType)
idBindP2 ctx = withinSpacesP $ xyP identifierP dcolon (typeP0 ctx)

assignabilityP
  =  try (withinSpacesP (reserved "WriteGlobal") >> return WriteGlobal)
 <|> try (withinSpacesP (reserved "WriteLocal" ) >> return WriteLocal )
 <|> try (withinSpacesP (reserved "Ambient"    ) >> return Ambient    )
 <|>     (withinSpacesP (reserved "ReadOnly"   ) >> return RdOnly     )

idBindP3 :: PContext -> Parser (Id SrcSpan, Assignability, Maybe RRType)
idBindP3 ctx
  = do  a <- option WriteGlobal assignabilityP -- WG is default assignability
        i <- withinSpacesP identifierP
        t <- optionMaybe (withinSpacesP dcolon >> withinSpacesP (typeP0 ctx))
        return (i, a, t)

functionExpressionP :: PContext -> Parser RRType
functionExpressionP = funcSigP

identifierP :: Parser (Id SrcSpan)
identifierP = withSpan Id identifier

binderP :: Parser (Id SrcSpan)
binderP     = withSpan Id $  try identifier
                         <|> try (show <$> integer)

pAliasP :: Parser (Id SrcSpan, PAlias)
pAliasP
  = do  name <- identifierP
        πs   <- pAliasVarsP
        reservedOp "="
        body <- predP
        return  (name, Alias name [] πs body)

pAliasVarsP = try (parens $ symbolP `sepBy` comma)
           <|> many symbolP

tAliasP :: Parser (Id SrcSpan, TAlias RRType)
tAliasP
  = do  withinSpacesP $ reserved "type"
        name      <- identifierP
        (αs, πs)  <- mapEither aliasVarT <$> aliasVarsP
        reservedOp "="
        body      <- typeP1 $ pCtxFromList (map tvToBTV αs)
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

interfaceP :: Parser (TypeDeclQ RK F.Reft)
interfaceP
  = do  _     <- withinSpacesP (reserved "interface")
        s     <- typeSignatureP mempty InterfaceTDK
        bd    <- typeBodyP (pCtxFromSig s)
        return $ TD s bd

classDeclP :: Parser (TypeSigQ RK F.Reft)
classDeclP
  = do  _     <- reserved "class"
        typeSignatureP mempty ClassTDK

moduleDeclP
  = do  _     <- reserved "module"
        withSpan (QP RK_) $ sepBy1 qSymbolP (char '.')

-- Mutability parameter should be included here.
typeSignatureP :: PContext -> TypeDeclKind -> Parser (TypeSigQ RK F.Reft)
typeSignatureP c k
  = do  b     <- btgenP c
        h     <- heritageP $ c `mappend` (pCtxFromListWithMut $ b_args b)
        return $ TS k b h

btgenP c = BGen <$> qnameP <*> option [] (bTParP c)

typeBodyP :: PContext -> Parser (TypeMembersQ RK F.Reft)
typeBodyP c = do
    pos <- getPosition
    eltKindsToTypeMembers pos <$> braces (propBindP c)

extendsGenP :: String -> PContext -> Parser [TGenQ RK F.Reft]
extendsGenP s c = option [] $ reserved s >> sepBy1 (extendsGen1P c) comma

extendsGen1P :: PContext -> Parser (TGenQ RK F.Reft)
extendsGen1P c
  = do  qn    <- qnameP
        ts    <- option [] $ angles $ sepBy (typeP1 c) comma
        return $ Gen qn ts

extendsP :: PContext -> Parser [TGenQ RK F.Reft]
extendsP = extendsGenP "extends"

implementsP :: PContext -> Parser [TGenQ RK F.Reft]
implementsP = extendsGenP "implements"


heritageP :: PContext -> Parser (HeritageQ RK F.Reft)
heritageP c = (,) <$> extendsP c <*> implementsP c


qnameP  :: Parser RelName
qnameP
  = do  optionMaybe (char '#')    -- backwards-compatibility fix
        withSpan qname $ sepBy1 qSymbolP (char '.')
  where
    qname s x = QN (QP RK_ s (init x)) (last x)

-- | Redefining some stuff to make the Qualified names parse right
qSymbolP    :: Parser Symbol
qSymbolP    = symbol <$> qSymCharsP
qSymCharsP  = condIdP (S.fromList qSymChars) (`notElem` keyWordSyms)

keyWordSyms = ["if", "then", "else", "mod"]
qSymChars   = ['a' .. 'z'] ++
              ['A' .. 'Z'] ++
              ['0' .. '9'] ++
              ['_', '%', '#'] -- omitting '.'

-- OLD CODE -- condIdP'  :: [Char] -> (String -> Bool) -> Parser Symbol
-- OLD CODE -- condIdP' chars f
-- OLD CODE --   = do c  <- try letter <|> oneOf ['_']
-- OLD CODE --        cs <- many (satisfy (`elem` chars))
-- OLD CODE --        blanks
-- OLD CODE --        if f (c:cs) then return (F.symbol $ DT.pack $ c:cs) else parserZero


-- | <A extends T, B extends S, ...>
bTParP c = angles $ sepBy (btvarP c) comma


--------------------------------------------------------------------------------
-- | RefTypes
--
--      T0 ::= ∀a . T1
--           | T1
--
--      T1 ::= (x: T1) => T1
--           | T2
--
--      T2 ::= { v: T3 | r }
--           | T3
--
--      T3 ::= T4 + T4 + ...
--
--      T4 ::= { v: T5 | r }
--           | T5
--
--      T5 ::= a
--           | B
--           | { f : T1 }
--           | C<T2>
--           | ( T1 )
--
--      B  ::= number
--           | string
--           | ...
--
--------------------------------------------------------------------------------

typeP0      :: PContext -> Parser RRType
typeP0 c     = allP c typeP1

typeP1      :: PContext -> Parser RRType
typeP1 c     =  try (funP   c)
            <|> try (typeP2 c)

typeP1'     :: PContext -> Parser (F.Reft -> RRType)
typeP1' c    =  try (funP'  c)
            <|> try (typeP3 c)

typeP2      :: PContext -> Parser RRType
typeP2 c     = atomP $ typeP3 c

typeP3      :: PContext -> Parser (F.Reft -> RRType)
typeP3 c     = tOrR <$> typeP4 c `sepBy1` plus

typeP4      :: PContext -> Parser RRType
typeP4 c     = atomP (typeP5 c)

typeP5      :: PContext -> Parser (F.Reft -> RRType)
typeP5 c     =  try (tVarP  c)
            <|> try (tPrimP c)
            <|> try (tObjP  c)
            <|> try (tRefP  c)
            <|> try (parens (typeP1' c))

atomP       :: (Parser (F.Reft -> a)) -> Parser a
atomP p      =  try (dummyP p)
            <|> try (xrefP  p)
            <|>     (refP   p)

funP'       :: PContext -> Parser (F.Reft -> RRType)
funP' c      = do f <- funP c
                  return $ \_ -> f

funP        :: PContext -> Parser RRType
funP c       = bareArrowP c (reserved "=>")    --  (x:t, ...) => t

bareArrowP  :: PContext -> Parser a -> Parser RRType
bareArrowP c f
  = do  args  <- parens $ sepBy (bareArgP c) comma
        _     <- f
        ret   <- typeP1 c
        r     <- topP
        return $ TFun args ret r

funcSigP    :: PContext -> Parser RRType
funcSigP c   = allP c funP

methSigP    :: PContext -> Parser RRType
methSigP c   = allP c bareMethP

bareMethP   :: PContext -> Parser RRType
bareMethP c  = bareArrowP c colon              --  (x:t, ...): t

bareArgP :: PContext -> Parser (BindQ RK F.Reft)
bareArgP c   =  try (boundTypeP c)
            <|> try (argBind <$> typeP1 c)

boundTypeP :: PContext -> Parser (BindQ RK F.Reft)
boundTypeP c
  = do  s     <- symbol <$> identifierP
        opt   <- option Req (char '?' >> pure Opt)
        withinSpacesP colon
        t     <- typeP1 c
        return $ B s opt t

argBind :: RRType -> BindQ RK F.Reft
argBind t = B (rTypeValueVar t) Req t

tObjP c = do  m   <- option trRO (parens (dummyP (typeP3 c)))
              c'  <- pure (c { pctx_mut = Just m })
              ms  <- typeBodyP c'
              return (TObj m ms)

tRefP c = TRef <$> tGenP c
tGenP c = Gen  <$> qnameP <*> bareTyArgsP c

tUQP    = parens (reserved "Unique") >> return trUQ

bareTyArgsP c
  =  try (brackets argsP)
 <|> try (angles   argsP)
 <|>      return []
     where
       argsP = sepBy (bareTyArgP c) comma

bareTyArgP c
  =  try (typeP2 c)
 <|> try (TExp <$> exprP)

tVarP c = TVar <$> tVarP0 c

tVarP0 :: PContext -> Parser TVar
tVarP0 c = try $ do
  t <- withSpan tvar $ wordP isTvar
  if t `elem` (pctx_vars c)
    then return t
    else fail "not a tvar"

btvarP :: PContext -> Parser (BTVarQ RK F.Reft)
btvarP c = withSpan btvar
         $ (,) <$> wordP isTvar
               <*> optionMaybe (reserved "extends" *> typeP1 c)

tvar l x = TV x l

btvar l (x,t) = BTV x l t

isTvar = not . isLower . head

wordP  = condIdP ok
  where
    ok = S.fromList $ ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0'..'9']

tPrimP _ = fmap TPrim tPrimP0

tPrimP0 :: Parser TPrim
tPrimP0  =  try (reserved "number"      >> return TNumber)
        <|> try (reserved "real"        >> return TReal)
        <|> try (reserved "bitvector32" >> return TBV32)
        <|> try (reserved "boolean"     >> return TBoolean)
        <|> try (reserved "undefined"   >> return TUndefined)
        <|> try (reserved "void"        >> return TVoid)
        <|> try (reserved "top"         >> return TTop)
        <|> try (reserved "string"      >> return TString)
        <|> try (reserved "null"        >> return TNull)
        <|> try (reserved "bool"        >> return TFPBool)
        <|> try (reserved "any"         >> return TAny)


-- <A, B extends A> --> Each bounded type can reference type
--                      variables appearing to its left only.
--
allP :: PContext -> (PContext -> Parser RRType) -> Parser RRType
allP c p
  = do tvs   <- try ( do  bsTry <- lookAhead (angles $ btvarP c `sepBy1` comma)
                          let c' = c `mappend` pCtxFromList bsTry
                          bs    <- angles $ btvarP c' `sepBy1` comma
                          return (Just bs))
            <|> return Nothing

       t     <- p $ c `mappend` pCtxFromList (concat tvs)
       return $ maybe t (`tAll` t) tvs
    where
       tAll αs t = foldr TAll t αs

propBindP :: PContext -> Parser [EltKind]
propBindP c = sepEndBy memberP semi
  where
    unc f (a,b,c,d,e) = f a b c d e
    memberP =  try (idxP c)
           <|> try (    Ctor <$> ctorP c) -- Ctor needs to be before Meth
           <|> try (unc Prop <$> propP c)
           <|> try (unc Meth <$> methP c)
           <|>     (    Call <$> callP c)

data EltKind = Prop Symbol StaticKind Optionality RMutability RRType
             | Meth Symbol StaticKind Optionality RMutability RRType
             | Call RRType
             | Ctor RRType
             | SIdx RMutability RRType
             | NIdx RMutability RRType
             deriving (Data, Typeable)

eltKindsToTypeMembers :: IsLocated l => l -> [EltKind] -> TypeMembersQ RK F.Reft
eltKindsToTypeMembers l eks =
    case tmsE of
      Right tms -> tms
      Left  e   -> throw e
  where
    tmsE = mkTypeMembers l (catMaybes (map ek2itm eks))
                           (catMaybes (map ek2stm eks))
                           (catMaybes (map ek2cl eks))
                           (catMaybes (map ek2ct eks))
                           (catMaybes (map ek2si eks))
                           (catMaybes (map ek2ni eks))

    ek2itm (Prop f InstanceK o m t) = Just (f, FI f o m t)
    ek2itm (Meth n InstanceK o m t) = Just (n, MI n o [(m,t)])
    ek2itm _                        = Nothing

    ek2stm (Prop f StaticK   o m t) = Just (f, FI f o m t)
    ek2stm (Meth n StaticK   o m t) = Just (n, MI n o [(m,t)])
    ek2stm _                        = Nothing

    ek2cl (Call t)                  = Just t
    ek2cl _                         = Nothing

    ek2ct (Ctor t)                  = Just t
    ek2ct _                         = Nothing

    ek2si (SIdx m t)                = Just (m, t)
    ek2si _                         = Nothing

    ek2ni (NIdx m t)                = Just (m, t)
    ek2ni _                         = Nothing


instance PP EltKind where
  pp (Prop s _ _ m t) = pp s <+> pp m <+> pp t
  pp (Meth s _ _ m t) = pp s <+> pp m <+> pp t
  pp (Call t)         = pp t
  pp (Ctor t)         = pp "new"  <+> pp t
  pp (SIdx _ t)       = pp "sidx" <+> pp t
  pp (NIdx _ t)       = pp "nidx" <+> pp t


-- | [f: string]: t
-- | [f: number]: t
idxP c
  = do  m           <- mutP c
        ((_, k), t) <- xyP (brackets indexP) colon (typeP1 c)
        case k of
          "number" -> return $ NIdx m t
          "string" -> return $ SIdx m t
          _        -> error $ "Index signature can only have " ++
                              "string or number as index."

indexP = xyP id colon sn
  where
    id = symbol <$> (try lowerIdP <|> upperIdP)
    sn = withinSpacesP (string "string" <|> string "number")

-- | [Static] [Mutability] f[?]: t
--
--  If mutability is provided, then use that.
--  If no mutability is provided, then use the contextual one.
--  If no contextual exist (probably a bug), use ReadOnly.
--
propP c
  = do  s     <- option InstanceK (reserved "static" *> return StaticK)
        a     <- withinSpacesP (mutP c)
        x     <- symbol <$> withinSpacesP binderP
        o     <- option Req (withinSpacesP (char '?') *> return Opt)
        _     <- colon
        t     <- typeP1 c
        return $ (x, s, o, a, t)


-- Parse (Mutability) with trRO as default if not specified in the context
mutP c = optionMaybe mP >>= \case
    Just m  -> return m
    Nothing -> return (fromMaybe trRO (pctx_mut c))
 where
    mP   = parens (dummyP (typeP3 c))


-- | [Static] [Mutability] m[<A extends T,..>](x:t,..): t
--
methP c
  = do  s     <- option InstanceK (reserved "static" *> return StaticK)
        m     <- withinSpacesP methMutabilityP
        x     <- symbol <$> identifierP
        o     <- option Req (withinSpacesP (char '?') *> return Opt)
        t     <- methSigP c
        return $ (x, s, o, m, t)

-- | [<A..>](t..): t
callP c         = withinSpacesP (methSigP c)

-- | new [<A..>](t..): t
ctorP c         = withinSpacesP (reserved "new")
               *> withinSpacesP (methSigP c)

methMutabilityP =  try (reserved "@Mutable"       >> return trMU)
               <|> try (reserved "@Immutable"     >> return trIM)
               <|> try (reserved "@ReadOnly"      >> return trRO)
               <|> try (reserved "@AssignsFields" >> return trAF)
               <|>     (                             return trRO)       -- default

dummyP ::  Parser (F.Reft -> b) -> Parser b
dummyP fm = fm `ap` topP

topP   :: Parser F.Reft
topP   = (F.Reft . (, mempty) . vv . Just) <$> freshIntP'

freshIntP' :: Parser Integer
freshIntP'
  = do  n <- stateUser <$> getParserState
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

