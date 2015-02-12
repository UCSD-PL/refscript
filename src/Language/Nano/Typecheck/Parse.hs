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


module Language.Nano.Typecheck.Parse (parseNanoFromFiles) where

import           Prelude                          hiding ( mapM)

import           Data.Either                             (partitionEithers)
import           Data.Default
import           Data.Traversable                        (mapAccumL)
import           Data.Monoid                             (mconcat)
import           Data.Maybe                              (listToMaybe, catMaybes, maybeToList)
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
import           Language.Nano.Locations
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

import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Syntax.Annotations

import           Text.Parsec                      hiding (parse, State)
import           Text.Parsec.Pos                         (newPos)
import           Text.Parsec.Error                       (errorMessages, showErrorMessages)
import qualified Text.Parsec.Token                as     T
import           Text.Parsec.Token                       (identStart, identLetter)
import           Text.Parsec.Prim                        (stateUser)
import           Text.Parsec.Language                    (emptyDef)

import           GHC.Generics

-- import           Debug.Trace                             ( trace, traceShow)

dot        = T.dot        lexer
plus       = T.symbol     lexer "+"
question   = T.symbol     lexer "?"

jsLexer    = T.makeTokenParser $ emptyDef { identStart  = letter   <|> oneOf "$_"
                                          , identLetter = alphaNum <|> oneOf "$_" }
identifier = T.identifier jsLexer

----------------------------------------------------------------------------------
-- | Type Binders 
----------------------------------------------------------------------------------

idBindP :: Parser (Id SourceSpan, RTypeQ RK Reft)
idBindP = withinSpacesP $ xyP identifierP dcolon bareTypeP

anonFuncP :: Parser (RTypeQ RK Reft)
anonFuncP = funcSigP

identifierP :: Parser (Id SourceSpan)
identifierP = withSpan Id identifier 

binderP     = withSpan Id $  try identifier
                         <|> try (show <$> integer)
                          
pAliasP :: Parser (Id SourceSpan, PAlias) 
pAliasP = do name <- identifierP
             πs   <- pAliasVarsP 
             reservedOp "="
             body <- predP 
             return  (name, Alias name [] πs body) 

pAliasVarsP = try (parens $ sepBy symbolP comma)
           <|> many symbolP

tAliasP :: Parser (Id SourceSpan, TAlias (RTypeQ RK Reft))
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

aliasVarT :: (SourceSpan, Symbol) -> Either TVar Symbol
aliasVarT (l, x)      
  | isTvar x' = Left  $ tvar l x
  | otherwise = Right $ x 
  where
    x'        = symbolString x

-- 
-- PV: Insert your option parser here
--
optionP   = do _ <- many anyToken 
               return "DEFAULT FLAG"
    
iFaceP   :: Parser (Id SourceSpan, IfaceDefQ RK Reft)
iFaceP   = do name   <- identifierP 
              as     <- option [] tParP
              h      <- heritageP
              es     <- mkTypeMembers . ((InstanceMember, MemDeclaration,) <$>) 
                    <$> braces propBindP
              return (name, convertTvar as $ ID (mkrn name) InterfaceKind as h es)
  where
    mkrn = mkRelName [] . symbol


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
qnameP   = withSpan qname $ optionMaybe (char '#') >> sepBy1 qSymbolP (char '.')
  where
    qname s x = QN RK_ s (init x) (last x)

-- | Redefining some stuff to make the Qualified names parse right
qSymbolP    :: Parser Symbol
qSymbolP    = symbol <$> qSymCharsP
qSymCharsP  = condIdP qSymChars (`notElem` keyWordSyms)
keyWordSyms = ["if", "then", "else", "mod"]
qSymChars   = ['a' .. 'z'] ++
              ['A' .. 'Z'] ++
              ['0' .. '9'] ++
              ['_', '%', '#'] -- omitting the the '.'

-- [A,B,C...]
tParP    = angles $ sepBy tvarP comma

withSpan f p = do pos   <- getPosition
                  x     <- p
                  pos'  <- getPosition
                  return $ f (Span pos pos') x


xyP lP sepP rP
  = (\x _ y -> (x, y)) <$> lP <*> (spaces >> sepP) <*> rP


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
  = optionMaybe (toType <$> mutP) >>= \case 
      Just m    -> TCons m <$> typeMembersP
      Nothing   -> withSpan addMVar typeMembersP
  where
    typeMembersP = mkTypeMembers . ((InstanceMember, MemDeclaration,) <$>) 
                                <$> braces propBindP
    addMVar _    = TCons defMut
    defMut       = TRef (QN RK_ (srcPos dummySpan) [] (symbol "Immutable")) [] fTop
    -- addMVar l t r = TAll v (TCons (TVar v fTop) t r) where v = tvar l ms
    -- ms            = symbol "_M_"  -- A hard-to-guess symbol

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
topP   = (Reft . (, []) . vv . Just) <$> freshIntP'

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
      t   <- kindP
      reserved "|"
      ras <- refasP 
      return $ t (Reft (symbol "v", ras))

----------------------------------------------------------------------------------
refasP :: Parser [Refa]
----------------------------------------------------------------------------------
refasP  =  (try (brackets $ sepBy (RConc <$> predP) semi)) 
       <|> liftM ((:[]) . RConc) predP
 

----------------------------------------------------------------------------------
withinSpacesP :: Parser a -> Parser a
----------------------------------------------------------------------------------
withinSpacesP p = do { spaces; a <- p; spaces; return a } 
             
----------------------------------------------------------------------------------
classDeclP :: Parser (Id SourceSpan, ClassSigQ RK Reft)
----------------------------------------------------------------------------------
classDeclP = do
    reserved "class"
    id      <- identifierP 
    vs      <- option [] $ angles $ sepBy tvarP comma
    (es,is) <- heritageP
    return (id, (vs,es,is))


---------------------------------------------------------------------------------
-- | Specifications
---------------------------------------------------------------------------------

data RawSpec
  = RawMeas     (SourceSpan, String)   -- Measure
  | RawBind     (SourceSpan, String)   -- Named function or var bindings
  | RawAmbBind  (SourceSpan, String)   -- Ambient bamed function or var bindings
  | RawFunc     (SourceSpan, String)   -- Anonymouns function type
  | RawIface    (SourceSpan, String)   -- Interface annots
  | RawClass    (SourceSpan, String)   -- Class annots
  | RawField    (SourceSpan, String)   -- Field annots
  | RawMethod   (SourceSpan, String)   -- Method annots
  | RawConstr   (SourceSpan, String)   -- Constructor annots
  | RawTAlias   (SourceSpan, String)   -- Type aliases
  | RawPAlias   (SourceSpan, String)   -- Predicate aliases
  | RawQual     (SourceSpan, String)   -- Qualifiers
  | RawOption   (SourceSpan, String)   -- Options
  | RawInvt     (SourceSpan, String)   -- Invariants
  | RawCast     (SourceSpan, String)   -- Casts
  | RawExported (SourceSpan, String)   -- Exported
  deriving (Show,Eq,Ord,Data,Typeable,Generic)

data PSpec l r
  = Meas    (Id l, RTypeQ RK r)
  | Bind    (Id l, RTypeQ RK r) 
  | AmbBind (Id l, RTypeQ RK r) 
  | AnFunc  (RTypeQ RK r) 
  | Field   (TypeMemberQ RK r)
  | Constr  (TypeMemberQ RK r)
  | Method  (TypeMemberQ RK r)
  | Static  (TypeMemberQ RK r)
  | Iface   (Id l, IfaceDefQ RK r)
  | Class   (Id l, ClassSigQ RK r)
  | TAlias  (Id l, TAlias (RTypeQ RK r))
  | PAlias  (Id l, PAlias) 
  | Qual    Qualifier
  | Option  String
  | Invt    l (RTypeQ RK r) 
  | CastSp  l (RTypeQ RK r)
  | Exported l

  -- Used only for parsing specs
  | ErrorSpec
  deriving (Eq, Show, Data, Typeable)

type Spec = PSpec SourceSpan Reft

parseAnnot :: RawSpec -> Parser Spec
parseAnnot = go
  where
    go (RawMeas     (ss, _)) = Meas    <$> patch2 ss <$> idBindP
    go (RawBind     (ss, _)) = Bind    <$> patch2 ss <$> idBindP
    go (RawAmbBind  (ss, _)) = AmbBind <$> patch2 ss <$> idBindP
    go (RawFunc     (_ , _)) = AnFunc  <$>               anonFuncP
    go (RawField    (_ , _)) = Field   <$>               fieldEltP 
    go (RawMethod   (_ , _)) = Method  <$>               methEltP
    go (RawConstr   (_ , _)) = Constr  <$>               consEltP
    go (RawIface    (ss, _)) = Iface   <$> patch2 ss <$> iFaceP
    go (RawClass    (ss, _)) = Class   <$> patch2 ss <$> classDeclP 
    go (RawTAlias   (ss, _)) = TAlias  <$> patch2 ss <$> tAliasP
    go (RawPAlias   (ss, _)) = PAlias  <$> patch2 ss <$> pAliasP
    go (RawQual     (_ , _)) = Qual    <$>               qualifierP
    go (RawOption   (_ , _)) = Option  <$>               optionP
    go (RawInvt     (ss, _)) = Invt               ss <$> bareTypeP
    go (RawCast     (ss, _)) = CastSp             ss <$> bareTypeP
    go (RawExported (ss, _)) = return  $ Exported ss


patch2 ss (id, t)    = (fmap (const ss) id , t)

getSpecString :: RawSpec -> String 
getSpecString = go
  where
    go (RawMeas     (_, s)) = s 
    go (RawBind     (_, s)) = s 
    go (RawAmbBind  (_, s)) = s 
    go (RawFunc     (_, s)) = s 
    go (RawIface    (_, s)) = s  
    go (RawField    (_, s)) = s  
    go (RawMethod   (_, s)) = s  
    go (RawConstr   (_, s)) = s  
    go (RawClass    (_, s)) = s  
    go (RawTAlias   (_, s)) = s  
    go (RawPAlias   (_, s)) = s  
    go (RawQual     (_, s)) = s  
    go (RawOption   (_, s)) = s  
    go (RawInvt     (_, s)) = s  
    go (RawCast     (_, s)) = s  
    go (RawExported (_, s)) = s  

instance IsLocated RawSpec where
  srcPos (RawMeas     (s,_)) = s 
  srcPos (RawBind     (s,_)) = s 
  srcPos (RawAmbBind  (s,_)) = s 
  srcPos (RawFunc     (s,_)) = s 
  srcPos (RawIface    (s,_)) = s  
  srcPos (RawField    (s,_)) = s  
  srcPos (RawMethod   (s,_)) = s  
  srcPos (RawConstr   (s,_)) = s  
  srcPos (RawClass    (s,_)) = s  
  srcPos (RawTAlias   (s,_)) = s  
  srcPos (RawPAlias   (s,_)) = s  
  srcPos (RawQual     (s,_)) = s  
  srcPos (RawOption   (s,_)) = s  
  srcPos (RawInvt     (s,_)) = s  
  srcPos (RawCast     (s,_)) = s  
  srcPos (RawExported (s,_)) = s  


instance FromJSON SourcePos where
  parseJSON (Array v) = do
    v0 <- parseJSON (v!0) :: AI.Parser String 
    v1 <- parseJSON (v!1) :: AI.Parser Int
    v2 <- parseJSON (v!2) :: AI.Parser Int
    return $ newPos v0 v1 v2
  parseJSON _ = error "SourcePos should only be an A.Array" 

instance FromJSON (Expression (SourceSpan, [RawSpec]))
instance FromJSON (Statement (SourceSpan, [RawSpec]))
instance FromJSON (EnumElt (SourceSpan, [RawSpec]))
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


--------------------------------------------------------------------------------------
-- | Parse File and Type Signatures 
--------------------------------------------------------------------------------------

-- Parse the contents of a FilePath list into a program structure with relative
-- qualified names.
--------------------------------------------------------------------------------------
parseNanoFromFiles :: [FilePath] -> IO (Either (FixResult Error) (NanoBareR Reft))
--------------------------------------------------------------------------------------
parseNanoFromFiles fs = 
  do  sa <- partitionEithers <$> mapM parseScriptFromJSON fs
      case sa of
        ([],ps) -> case parseAnnots $ concat ps of
                     Right ps -> return $ Right $ mkCode ps
                     Left e   -> return $ Left  $ e
        (es,_ ) -> return $ Left  $ mconcat es 

--------------------------------------------------------------------------------------
getJSON :: MonadIO m => FilePath -> m B.ByteString
--------------------------------------------------------------------------------------
getJSON = liftIO . B.readFile

--------------------------------------------------------------------------------------
parseScriptFromJSON :: FilePath -> IO (Either (FixResult a) [Statement (SourceSpan, [RawSpec])])
--------------------------------------------------------------------------------------
parseScriptFromJSON filename = decodeOrDie <$> getJSON filename
  where 
    decodeOrDie s =
      case eitherDecode s :: Either String [Statement (SourceSpan, [RawSpec])] of
        Left msg -> Left  $ Crash [] $ "JSON decode error:\n" ++ msg
        Right p  -> Right $ p


---------------------------------------------------------------------------------
mkCode :: [Statement (SourceSpan, [Spec])] -> NanoBareR Reft
---------------------------------------------------------------------------------
mkCode = --debugTyBinds
         buildCHA
       . fixEnums
       . scrapeModules
       . scrapeQuals 
       . replaceAbsolute
       . expandAliases
       . visitNano convertTvarVisitor []
       . mkCode'
    
---------------------------------------------------------------------------------
mkCode' :: [Statement (SourceSpan, [Spec])] -> NanoBareRelR Reft
---------------------------------------------------------------------------------
mkCode' ss = Nano { 
        code          = Src (checkTopStmt <$> ss')
      , consts        = envFromList [ mapSnd (ntrans f g) t | Meas t <- anns ]
      , tAlias        = envFromList [ t | TAlias t <- anns ]
      , pAlias        = envFromList [ t | PAlias t <- anns ] 
      , pQuals        =             [ t | Qual   t <- anns ] 
      , pOptions      =             [ t | Option t <- anns ]
      , invts         = [Loc (srcPos l) (ntrans f g t) | Invt l t <- anns ]
      , max_id        = ending_id
      , fullNames     = names
      , fullPaths     = paths
      , pModules      = qenvEmpty -- is populated at mkCode
      , pCHA          = def
    } 
  where
    toBare            :: Int -> (SourceSpan, [Spec]) -> AnnRel Reft 
    toBare n (l,αs)    = Ann n l $ catMaybes $ extractFact <$> αs 
    f (QN RK_ l ss s)  = QN AK_ l ss s
    g (QP RK_ l ss)    = QP AK_ l ss
    starting_id        = 0
    (ending_id, ss')   = mapAccumL (mapAccumL (\n -> (n+1,) . toBare n)) starting_id ss
    anns               = concatMap (FO.foldMap snd) ss
    (names, paths)     = extractQualifiedNames ss'

---------------------------------------------------------------------------------
extractFact :: PSpec t r -> Maybe (FactQ RK r)
---------------------------------------------------------------------------------
extractFact = go
  where
    go (Bind    (_,t)) = Just $ VarAnn    t   
    go (AmbBind (_,t)) = Just $ AmbVarAnn t
    go (Constr  c    ) = Just $ ConsAnn   c   
    go (Field   f    ) = Just $ FieldAnn  f
    go (Method  m    ) = Just $ MethAnn   m
    go (Static  m    ) = Just $ StatAnn   m
    go (Class   (_,t)) = Just $ ClassAnn  t
    go (Iface   (_,t)) = Just $ IfaceAnn  t
    go (CastSp  _ t  ) = Just $ UserCast  t
    go (Exported  _  ) = Just $ ExportedElt
    go (AnFunc  t    ) = Just $ FuncAnn   t
    go _               = Nothing

---------------------------------------------------------------------------------
scrapeQuals     :: NanoBareR Reft -> NanoBareR Reft
---------------------------------------------------------------------------------
scrapeQuals p = p { pQuals = qs ++ pQuals p}
  where
    qs        = qualifiers $ mkUq $ foldNano tbv [] [] p
    tbv       = defaultVisitor { accStmt = stmtTypeBindings
                               , accCElt = celtTypeBindings }

mkUq                  = zipWith tx ([0..] :: [Int])
  where
    tx i (Id l s, t)  = (Id l $ s ++ "_" ++ show i, t)
    

stmtTypeBindings _                = go
  where
    go (FunctionStmt l f _ _)     = [(f, t) | FuncAnn t <- ann_fact l ] ++
                                    [(f, t) | VarAnn t <- ann_fact l ]
    go (VarDeclStmt _ vds)        = [(x, t) | VarDecl l x _ <- vds, VarAnn t <- ann_fact l]   
    go _                          = []

celtTypeBindings _                = (mapSnd eltType <$>) . go 
  where
    go (Constructor l _ _)        = [(x, e) | ConsAnn  e <- ann_fact l, let x = Id l "ctor" ]
    go (MemberVarDecl l _ x _)    = [(x, e) | FieldAnn e <- ann_fact l] ++ 
                                    [(x, e) | StatAnn  e <- ann_fact l]
    go (MemberMethDef l _ x _ _)  = [(x, e) | MethAnn  e <- ann_fact l]
    go _                          = []

-- debugTyBinds p@(Nano {code = Src ss}) = trace msg p
--   where
--     xts = [(x, t) | (x, (t, _)) <- visibleNames ss ]
--     msg = unlines $ "debugTyBinds:" : (ppshow <$> xts)


-- | Class Hierachy 
---------------------------------------------------------------------------
buildCHA           :: PPR r => NanoBareR r -> NanoBareR r
---------------------------------------------------------------------------
buildCHA pgm        = pgm { pCHA = ClassHierarchy graph namesToKeys } 
  where 
    graph           = mkGraph nodes edges
    nodes           = zip ([0..] :: [Int]) $ fst3 <$> data_
    keysToTypes     = I.fromList $ zip [0..] (fst3 <$> data_)
    namesToKeys     = HM.fromList $ mapFst t_name . swap <$> I.toList keysToTypes
    edges           = concatMap toEdge data_  
    toEdge (_,s,ts) = [ (σ,τ,()) | t <- ts
                                 , σ <- maybeToList $ HM.lookup s namesToKeys
                                 , τ <- maybeToList $ HM.lookup t namesToKeys ]
    data_           = concatMap foo $ snd <$> qenvToList (pModules pgm)
    foo m           = bar . snd <$>  envToList (m_types m)
    bar d           = (d, t_name d, parentNames pgm (t_name d))

parentNames :: NanoBareR r -> AbsName -> [AbsName]
parentNames p = concat . maybeToList . pparentNames p
  where
    pparentNames p a = do t <- resolveTypeInPgm p a 
                          let (es,is) = t_base t
                          return $ (fst <$> es) ++ (fst <$> is)
 
type PState = Integer

instance PP Integer where
  pp = pp . show


--------------------------------------------------------------------------------------
parseAnnots :: [Statement (SourceSpan, [RawSpec])] 
             -> Either (FixResult Error) [Statement (SourceSpan, [Spec])]
--------------------------------------------------------------------------------------
parseAnnots ss = 
  case mapAccumL (mapAccumL f) (0,[]) ss of
    ((_,[]),b) -> Right $ b
    ((_,es),_) -> Left  $ Unsafe es
  where 
    f st (ss,sp) = mapSnd ((ss),) $ L.mapAccumL (parse ss) st sp

--------------------------------------------------------------------------------------
parse :: SourceSpan -> (PState, [Error]) -> RawSpec -> ((PState, [Error]), Spec)
--------------------------------------------------------------------------------------
parse _ (st,errs) c = failLeft $ runParser (parser c) st f (getSpecString c)
  where
    parser s = do a     <- parseAnnot s
                  state <- getState
                  it    <- getInput
                  case it of 
                    ""  -> return $ (state, a) 
                    _   -> unexpected $ "trailing input: " ++ it

    failLeft (Left err)      = ((st, (fromError err): errs), ErrorSpec)
    failLeft (Right (s, r))  = ((s, errs), r)

    -- Slight change from this one:
    -- http://hackage.haskell.org/package/parsec-3.1.5/docs/src/Text-Parsec-Error.html#ParseError
    showErr = showErrorMessages "or" "unknown parse error" "expecting" 
                "unexpected" "end of input" . errorMessages
    fromError err = mkErr ss   $ showErr err 
                              ++ "\n\nWhile parsing: " 
                              ++ show (getSpecString c)
    ss = srcPos c
    f = sourceName $ sp_begin ss


instance PP (RawSpec) where
  pp = text . getSpecString



-------------------------------------------------------------------------------------

-- | @convertTvar@ converts @RCon@s corresponding to _bound_ type-variables to @TVar@
convertTvar    :: (PP r, Reftable r, Transformable t, Show q) => [TVar] -> t q r -> t q r
convertTvar as = trans tx as []  
  where
    tx αs _ (TRef c [] r) | Just α <- mkTvar αs c = TVar α r 
    tx _  _ t             = t 

mkTvar αs r = listToMaybe [ α { tv_loc = srcPos r }  | α <- αs, symbol α == symbol r]

convertTvarVisitor :: (PP r, Reftable r) => Visitor () [TVar] (AnnRel r) 
convertTvarVisitor = defaultVisitor {
    ctxStmt = ctxStmtTvar
  , ctxCElt = ctxCEltTvar
  , txStmt  = transFmap (const . convertTvar) 
  , txExpr  = transFmap (const . convertTvar) 
  , txCElt  = transFmap (const . convertTvar)
  }

ctxStmtTvar as s = go s ++ as
  where
    go :: Statement (AnnRel r)  -> [TVar]
    go s@(FunctionStmt {}) = grab s 
    go s@(FuncAmbDecl {})  = grab s 
    go s@(FuncOverload {}) = grab s 
    go s@(IfaceStmt {})    = grab s
    go s@(ClassStmt {})    = grab s
    go s@(ModuleStmt {})   = grab s
    go _                   = []

    grab :: Statement (AnnQ q r) -> [TVar]
    grab = concatMap factTvars . ann_fact . getAnnotation 

ctxCEltTvar as s = go s ++ as
  where
    go :: ClassElt (AnnRel r)  -> [TVar]
    go s@Constructor{}     = grab s
    go s@MemberMethDef{}   = grab s
    go _                   = []

    grab :: ClassElt (AnnQ q r) -> [TVar]
    grab = concatMap factTvars . ann_fact . getAnnotation 


factTvars :: FactQ q r -> [TVar]
factTvars = go
  where
    tvars t                 | Just ts <- bkFuns t
                            = HS.toList $ foldUnions $ HS.fromList . fst4 <$> ts
                            | otherwise
                            = []
    
    foldUnions (α:αs)       = foldl HS.intersection α αs
    foldUnions _            = HS.empty

    go (VarAnn t)           = tvars t
    go (FuncAnn t)          = tvars t
    go (FieldAnn m)         = tvars $ f_type m
    go (MethAnn m)          = tvars $ f_type m
    go (StatAnn m)          = tvars $ f_type m
    go (ConsAnn m)          = tvars $ f_type m
    go (ClassAnn (as,_,_))  = as
    go (IfaceAnn i)         = t_args i
    go _                    = []
    
