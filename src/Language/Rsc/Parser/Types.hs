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
import           Data.Foldable                (concat)
import           Data.Generics                hiding (Generic)
import qualified Data.HashSet                 as S
import           Data.List                    (foldl')
import           Data.Maybe                   (maybeToList)
import           Data.Monoid
import qualified Data.Text                    as DT
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Names
import           Language.Fixpoint.Parse
import qualified Language.Fixpoint.Types      as F
import           Language.Rsc.Liquid.Types
import           Language.Rsc.Misc            (mapEither)
import           Language.Rsc.Names
import           Language.Rsc.Options
import           Language.Rsc.Parser.Common
import           Language.Rsc.Parser.Lexer
import           Language.Rsc.Pretty
import           Language.Rsc.Transformations
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
import           Prelude                      hiding (concat, mapM)
import           Text.Parsec                  hiding (State, parse)
import           Text.PrettyPrint.HughesPJ    ((<+>))

-- import           Language.Rsc.Pretty


type RRType = RTypeQ RK F.Reft
type RMutability = RTypeQ RK F.Reft


-- TODO: Can Also take care of qualified names etc...

data PContext = PContext { pctx_vars :: [TVar]                    -- Type variables in scope
                         , pctx_mut  :: Maybe (BTVarQ RK F.Reft)  -- Ambient mutability
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
  = PContext (map btvToTV bs) (Just b1)
  | otherwise
  = PContext (map btvToTV bs) Nothing

pCtxFromSig = pCtxFromListWithMut . b_args . sigTRef


instance PP PContext where
  pp (PContext vs m) = pp vs <+> pp m

--------------------------------------------------------------------------------
-- | Type Binders
--------------------------------------------------------------------------------

idBindP :: PContext -> Parser (Id SrcSpan, RRType)
idBindP ctx = withinSpacesP $ xyP identifierP dcolon (bareTypeP ctx)

idBindP' :: PContext -> Parser (Id SrcSpan, Assignability, Maybe RRType)
idBindP' ctx = withinSpacesP $ axyP identifierP dcolon (Just <$> bareTypeP ctx)

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
        body      <- bareTypeP $ pCtxFromList (map tvToBTV αs)
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
interfaceP
  = do  _     <- reserved "interface"
        s     <- typeSignatureP mempty InterfaceTDK
        bd    <- typeBodyP $ pCtxFromSig s
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
typeBodyP c = eltKindsToTypeMembers <$> braces (propBindP c)

extendsGenP :: String -> PContext -> Parser [TGenQ RK F.Reft]
extendsGenP s c = option [] $ reserved s >> sepBy1 (extendsGen1P c) comma

extendsGen1P :: PContext -> Parser (TGenQ RK F.Reft)
extendsGen1P c
  = do  qn    <- qnameP
        ts    <- option [] $ angles $ sepBy (bareTypeP c) comma
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
--------------------------------------------------------------------------------

bareTypeP          :: PContext -> Parser RRType
bareTypeP c         = bareAllP c bareUnionTypeP

bareUnionTypeP     :: PContext -> Parser RRType
bareUnionTypeP c    = mkUnion <$> bareUnionMemberP c `sepBy1` plus
-- bareUnionTypeP c    = mkUnion <$> parenNullP (bareUnionMemberP c `sepBy1` plus)

bareUnionMemberP   :: PContext -> Parser RRType
bareUnionMemberP c  =  try (funcSigP c)
                   <|> bareAllP c (bareAtomP bbaseP)

-- parenNullP         :: Parser a -> Parser a
-- parenNullP p        =  try ((tNull:) <$> postP p question)
--                    <|> p

funcSigP           :: PContext -> Parser RRType
funcSigP c          = bareAllP c bareFunP

methSigP           :: PContext -> Parser RRType
methSigP c          = bareAllP c bareMethP

bareFunP           :: PContext -> Parser RRType
bareFunP c          = bareArrowP c (reserved "=>")    --  (x:t, ...) => t

bareMethP          :: PContext -> Parser RRType
bareMethP c         = bareArrowP c colon              --  (x:t, ...): t

bareArrowP         :: PContext -> Parser a -> Parser RRType
bareArrowP c f
  = do  args  <- parens $ sepBy (bareArgP c) comma
        _     <- f
        ret   <- bareTypeP c
        r     <- topP
        return $ TFun args ret r

bareArgP :: PContext -> Parser (BindQ RK F.Reft)
bareArgP c          =  try (boundTypeP c)
                   <|> (argBind <$> bareTypeP c)

-- TODO: Take the optional argument into account
boundTypeP :: PContext -> Parser (BindQ RK F.Reft)
boundTypeP c
  = do  s <- symbol <$> identifierP
        optional (char '?')
        withinSpacesP colon
        t <- bareTypeP c
        return $ B s t

argBind :: RRType -> BindQ RK F.Reft
argBind t = B (rTypeValueVar t) t

-- bareAtomP :: (PContext -> Parser (F.Reft -> a)) -> PContext -> Parser a
bareAtomP p c
  =  try (xrefP  (p c))
 <|> try (refP   (p c))
 <|>     (dummyP (p c))

bbaseP :: PContext -> Parser (F.Reft -> RRType)
bbaseP c
  =  try (TVar        <$> tvarP     c)     -- V
 <|> try (TObj ambMut <$> typeBodyP c)     -- {f1: T1; ... ; fn: Tn}
 <|> try (TPrim       <$> tPrimP     )     -- number, boolean, etc...
 <|>     (TRef        <$> tGenP     c)     -- List<A>, Tree<A,B> etc...
  where
    ambMut | Just b <- pctx_mut c
           = TVar (btvToTV b) fTop
           | otherwise
           = trIM

tGenP c = Gen <$> qnameP <*> bareTyArgsP c

bareTyArgsP c
  =  try (brackets argsP)
 <|> try (angles   argsP)
 <|> return []
     where
       argsP = sepBy (bareTyArgP c) comma

bareTyArgP c
  =  try (bareTypeP c)
 <|> (TExp <$> exprP)

tvarP :: PContext -> Parser TVar
tvarP c = try (
    do  t <- withSpan tvar $ wordP isTvar
        if t `elem` (pctx_vars c)
          then return t
          else fail "not a tvar"
  )

btvarP :: PContext -> Parser (BTVarQ RK F.Reft)
btvarP c = withSpan btvar $ (,) <$> wordP isTvar
                                <*> optionMaybe (reserved "extends" *> bareTypeP c)

tvar l x = TV x l

btvar l (x,t) = BTV x l t

isTvar = not . isLower . head

wordP  = condIdP ok
  where
    ok = S.fromList $ ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0'..'9']

tPrimP :: Parser TPrim
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


bareAllP :: PContext -> (PContext -> Parser RRType) -> Parser RRType
bareAllP c p
  = do tvs   <- optionMaybe $ angles $ btvarP c `sepBy1` comma
       t     <- p $ c `mappend` pCtxFromList (concat tvs)
       return $ maybe t (`tAll` t) tvs
    where
       tAll αs t = foldr TAll t αs

propBindP :: PContext -> Parser [EltKind]
propBindP c  = sepEndBy memberP semi
  where
    unc     = \f (a,b,c,d,e) -> f a b c d e
    memberP =  try (idxP c)
           <|> try (    Ctor <$> ctorP c) -- Ctor needs to be before Meth
           <|> try (unc Prop <$> propP c)
           <|> try (unc Meth <$> methP c)
           <|>     (    Call <$> callP c)

data EltKind = Prop Symbol StaticKind Optionality FieldAsgn RRType
             | Meth Symbol StaticKind Optionality RMutability RRType
             | Call RRType
             | Ctor RRType
             | SIdx RRType
             | NIdx RRType
             deriving (Data, Typeable)

eltKindsToTypeMembers :: [EltKind] -> TypeMembersQ RK F.Reft
eltKindsToTypeMembers = foldl' go mempty
  where
    go ms (Prop f InstanceK o m t) = ms { i_mems = F.insertSEnv f (FI o m t) (i_mems ms) }
    go ms (Prop f StaticK   o m t) = ms { s_mems = F.insertSEnv f (FI o m t) (s_mems ms) }
    go ms (Meth n InstanceK o m t) = ms { i_mems = insert (i_mems ms) n o (m,t) }
    go ms (Meth n StaticK   o m t) = ms { s_mems = insert (s_mems ms) n o (m,t) }
    go ms (Call t) = ms { tm_call = Just t `mappend` tm_call ms }
    go ms (Ctor t) = ms { tm_ctor = Just t }
    go ms (SIdx t) = ms { tm_sidx = Just t }
    go ms (NIdx t) = ms { tm_nidx = Just t }

    insert g x o mt | Just (MI o' mts') <- F.lookupSEnv x g
                    = F.insertSEnv x (MI (o `mappend` o') (mts' ++ [mt])) g
                    | otherwise
                    = F.insertSEnv x (MI o [mt]) g

-- | [f: string]: t
-- | [f: number]: t
idxP c
  = do  ((_, k), t) <- xyP (brackets indexP) colon (bareTypeP c)
        case k of
          "number" -> return $ NIdx t
          "string" -> return $ SIdx t
          _        -> error $ "Index signature can only have " ++
                              "string or number as index."

indexP = xyP id colon sn
  where
    id = symbol <$> (try lowerIdP <|> upperIdP)
    sn = withinSpacesP (string "string" <|> string "number")

-- | [STATIC] [@ASSIGNABILITY] f[?]: t
--
--  e.g.
--
--      static
--
propP c
  = do  s     <- option InstanceK (reserved "static" *> return StaticK)
        a     <- withinSpacesP fieldAsgnP
        x     <- symbol <$> withinSpacesP binderP
        o     <- option Req (withinSpacesP (char '?') *> return Opt)
        _     <- colon
        t     <- bareTypeP c
        return $ (x, s, o, a, t)
  where
    ambMut | Just b <- pctx_mut c = TVar (btvToTV b) fTop
           | otherwise            = trIM


-- | [STATIC] [@MUTABILITY] m[<A..>](x:t,..): t
--
--  e.g.
--
--    static @Mutable m<A>(x: A): A
--
--    n(x: number): string
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

mutabilityP     =  brackets . bareTypeP

methMutabilityP =  try (reserved "@Mutable"       >> return trMU)
               <|> try (reserved "@Immutable"     >> return trIM)
               <|> try (reserved "@ReadOnly"      >> return trRO)
               <|> try (reserved "@AssignsFields" >> return trAF)
               <|>     (                             return trRO)       -- default

fieldAsgnP      =  try (reserved "@assignable"    >> return Assignable)
               <|> try (reserved "@final"         >> return Final     )
               <|>     (                             return Inherited ) -- default

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

