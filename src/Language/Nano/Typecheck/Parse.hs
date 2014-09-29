{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances         #-} 
{-# LANGUAGE DeriveDataTypeable        #-} 
{-# LANGUAGE UndecidableInstances      #-} 
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
import           Data.Maybe                              (listToMaybe, catMaybes)
import           Data.Generics                    hiding (Generic)
import           Data.Aeson                              (eitherDecode)
import           Data.Aeson.Types                 hiding (Parser, Error, parse)
import qualified Data.Aeson.Types                 as     AI
import qualified Data.ByteString.Lazy.Char8       as     B
import           Data.Char                               (isLower)
import qualified Data.List                        as     L
-- import qualified Data.HashSet                     as     S
import           Text.PrettyPrint.HughesPJ               (text)
import qualified Data.Foldable                    as     FO
import           Data.Vector                             ((!))

import           Control.Monad
import           Control.Monad.Trans                     (MonadIO,liftIO)
import           Control.Applicative                     ((<$>), (<*>) , (<*) , (*>))

import           Language.Fixpoint.Types          hiding (quals, Loc, Expression)
import           Language.Fixpoint.Parse
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc                  (mapEither, mapSnd)

import           Language.Nano.Annots
import           Language.Nano.Errors
import           Language.Nano.Env
import           Language.Nano.Locations
import           Language.Nano.Names
import           Language.Nano.Program
import           Language.Nano.Types              hiding (Exported)
import           Language.Nano.Visitor     
import           Language.Nano.Typecheck.Types
import           Language.Nano.Liquid.Types
import           Language.Nano.Liquid.Alias
import           Language.Nano.Liquid.Qualifiers

import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Syntax.Annotations

import           Text.Parsec                      hiding (parse, State)
import           Text.Parsec.Pos                         (newPos)
import           Text.Parsec.Error                       (errorMessages, showErrorMessages)
import qualified Text.Parsec.Token                as     T
import           Text.Parsec.Prim                        (stateUser)

import           GHC.Generics

import           Debug.Trace                             ( trace, traceShow)

dot        = T.dot        lexer
plus       = T.symbol     lexer "+"
question   = T.symbol     lexer "?"

----------------------------------------------------------------------------------
-- | Type Binders 
----------------------------------------------------------------------------------

idBindP :: Parser (Id SourceSpan, RefType)
idBindP = withinSpacesP $ xyP identifierP dcolon bareTypeP

anonFuncP :: Parser RefType
anonFuncP = funcSigP

identifierP :: Parser (Id SourceSpan)
identifierP =  try (withSpan Id uIdP)
           <|>     (withSpan Id lIdP)
  where
    uIdP    = symbolString <$> upperIdP
    lIdP    = symbolString <$> lowerIdP
  
pAliasP :: Parser (Id SourceSpan, PAlias) 
pAliasP = do name <- identifierP
             πs   <- pAliasVarsP 
             reservedOp "="
             body <- predP 
             return  (name, Alias name [] πs body) 

pAliasVarsP = try (parens $ sepBy symbolP comma)
           <|> many symbolP

tAliasP :: Parser (Id SourceSpan, TAlias RefType) 
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
    
iFaceP   :: Parser (Id SourceSpan, IfaceDef Reft)
iFaceP   = do name   <- identifierP 
              as     <- option [] tParP
              h      <- optionMaybe extendsP
              -- FIXME
              es     <- braces $ propBindP def
              return (name, convertTvar as $ ID InterfaceKind name as h es)

extendsP = do reserved "extends"
              qn     <- RN <$> qnameP
              ts     <- option [] $ brackets $ sepBy bareTypeP comma
              return (qn, ts)

qnameP   = withSpan qname $ optionMaybe (char '#') >> sepBy1 qSymbolP (char '.')
  where
    qname s x = QName s (init x) (last x)

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
bareTypeP :: Parser RefType 
----------------------------------------------------------------------------------
bareTypeP = bareAllP $ bodyP 
  where
    bodyP =  try bUnP
         <|> try (refP rUnP)
         <|>     (xrefP rUnP)

rUnP        = mkU <$> parenNullP (bareTypeNoUnionP `sepBy1` plus) toN
  where
    mkU [x] = strengthen x
    mkU xs  = flattenUnions . TApp TUn (L.sort xs)
    toN     = (tNull:)

bUnP        = parenNullP (bareTypeNoUnionP `sepBy1` plus) toN >>= mkU
  where
    mkU [x] = return x
    mkU xs  = flattenUnions . TApp TUn xs <$> topP
    toN     = (tNull:)


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

boundTypeP = do s <- symbolP 
                withinSpacesP colon
                B s <$> bareTypeP

argBind t = B (rTypeValueVar t) t

bareAtomP p
  =  try (xrefP  p)
 <|> try (refP p)
 <|>     (dummyP p)


----------------------------------------------------------------------------------
bbaseP :: Parser (Reft -> RefType)
----------------------------------------------------------------------------------
-- ORIG bbaseP 
-- ORIG   =  try (TVar <$> tvarP)                  -- A
-- ORIG  <|> try objLitP                           -- {f1: T1; ... ; fn: Tn} 
-- ORIG  -- FIXME
-- ORIG  -- Disabling array in this form cause there is no room for mutability ...
-- ORIG  -- <|> try (rtArr <$> arrayP)                -- Array<T>
-- ORIG  <|> try (TApp <$> tConP <*> bareTyArgsP)  -- List[A], Tree[A,B] etc...
 
bbaseP 
  =  try objLitP                       -- {f1: T1; ... ; fn: Tn} 
 <|> (TApp <$> tConP <*> bareTyArgsP)  -- List[A], Tree[A,B] etc...
 

----------------------------------------------------------------------------------
objLitP :: Parser (Reft -> RefType)
----------------------------------------------------------------------------------
objLitP 
  = do m     <- option def (toType <$> mutP)
       bs    <- braces (propBindP m)
       return $ TCons bs m
 
mutP
  =  try (TVar <$> brackets tvarP <*> return ()) 
 <|> try (TApp <$> brackets tConP <*> return [] <*> return ())


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
tConP =  try (reserved "number"    >> return TInt)
     <|> try (reserved "boolean"   >> return TBool)
     <|> try (reserved "undefined" >> return TUndef)
     <|> try (reserved "void"      >> return TVoid)
     <|> try (reserved "top"       >> return TTop)
     <|> try (reserved "string"    >> return TString)
     <|> try (reserved "null"      >> return TNull)
     <|> try (reserved "bool"      >> return TFPBool)
     <|>     (TRef <$> RN <$> qnameP)

bareAllP p
  = do tvs   <- optionMaybe (reserved "forall" *> many1 tvarP <* dot) 
       t     <- p
       return $ maybe t (`tAll` t) tvs
    where
       tAll αs t = foldr TAll (convertTvar αs t) αs

propBindP defM =  sepEndBy propEltP semi
  where
    propEltP   =  try indexEltP 
              <|> try (fieldEltP defM)
              <|> try (statEltP defM)
              <|> try (methEltP defM)
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


-- | <[mut]> f: t
fieldEltP defM  = do 
    x          <- symbolP 
    _          <- colon
    m          <- option defM (toType <$> mutP)
    t          <- bareTypeP
    return      $ FieldSig x m t

-- | static <[mut]> f :: t
statEltP defM   = do 
    _          <- reserved "static"
    x          <- symbolP 
    _          <- colon
    m          <- option defM (toType <$> mutP)
    t          <- bareTypeP
    return      $ StatSig x m t

-- | <[mut]> m :: (ts): t
methEltP defM   = do
    x          <- symbolP 
    _          <- colon
    m          <- option defM (toType <$> mutP)
    t          <- methSigP
    return      $ MethSig x m t
  where
--     outT t      =  
--       case bkFun t of 
--         Just (_, (B x _):_,_ ) | x == symbol "this" -> t
--         Just (vs, bs      ,ot)                     -> mkFun (v:vs, B (symbol "this") tv : bs, ot)
--         _                                          -> t
--     -- XXX: using _THIS_ as a reserved sting here.
--     v  = TV (symbol "_THIS_") (srcPos (dummyPos "RSC.Parse.methEltP"))
--     tv = TVar v fTop


-- | <forall A .> (t...) => t

callEltP = CallSig <$> withinSpacesP (bareAllP bareFunP)


-- | new <forall A .> (t...) => t

consEltP = do
    try      $  reserved "new"
    ConsSig <$> withinSpacesP (bareAllP bareFunP)

 
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
classDeclP :: Parser (Id SourceSpan, ([TVar], Maybe (RelName, [RefType])))
----------------------------------------------------------------------------------
classDeclP = do
    reserved "class"
    id <- identifierP 
    vs <- option [] $ angles $ sepBy tvarP comma
    pr <- optionMaybe extendsP
    return (id, (vs, pr))



---------------------------------------------------------------------------------
-- | Specifications
---------------------------------------------------------------------------------

data RawSpec
  = RawMeas     (SourceSpan, String)   -- Measure
  | RawBind     (SourceSpan, String)   -- Named function or var bindings
  | RawFunc     (SourceSpan, String)   -- Anonymouns function type
  | RawIface    (SourceSpan, String)   -- Interface annots
  | RawClass    (SourceSpan, String)   -- Class annots
  | RawField    (SourceSpan, String)   -- Field annots
  | RawMethod   (SourceSpan, String)   -- Method annots
  | RawStatic   (SourceSpan, String)   -- Static annots
  | RawConstr   (SourceSpan, String)   -- Constructor annots
  | RawTAlias   (SourceSpan, String)   -- Type aliases
  | RawPAlias   (SourceSpan, String)   -- Predicate aliases
  | RawQual     (SourceSpan, String)   -- Qualifiers
  | RawInvt     (SourceSpan, String)   -- Invariants
  | RawCast     (SourceSpan, String)   -- Casts
  | RawExported (SourceSpan, String)   -- Exported
  deriving (Show,Eq,Ord,Data,Typeable,Generic)

data PSpec l r
  = Meas   (Id l, RType r)
  | Bind   (Id l, RType r) 
  | AnFunc (RType r) 
  | Field  (TypeMember r)
  | Constr (TypeMember r)
  | Method (TypeMember r)
  | Static (TypeMember r)
  | Iface  (Id l, IfaceDef r)
  | Class  (Id l, ([TVar], Maybe (RelName, [RType r])))
  | TAlias (Id l, TAlias (RType r))
  | PAlias (Id l, PAlias) 
  | Qual   Qualifier
  | Invt   l (RType r) 
  | CastSp l (RType r)
  | Exported l

  -- Used only for parsing specs
  | ErrorSpec
  deriving (Eq, Ord, Show, Data, Typeable)

type Spec = PSpec SourceSpan Reft

parseAnnot :: RawSpec -> Parser Spec
parseAnnot = go
  where
    go (RawMeas     (ss, _)) = Meas   <$> patch2 ss <$> idBindP
    go (RawBind     (ss, _)) = Bind   <$> patch2 ss <$> idBindP
    go (RawFunc     (_ , _)) = AnFunc <$>               anonFuncP
    go (RawField    (_ , _)) = Field  <$>               fieldEltP def -- FIXME: these need to be patched aferwards  
    go (RawMethod   (_ , _)) = Method <$>               methEltP def 
    go (RawStatic   (_ , _)) = Static <$>               statEltP def
    go (RawConstr   (_ , _)) = Constr <$>               consEltP
    go (RawIface    (ss, _)) = Iface  <$> patch2 ss <$> iFaceP
    go (RawClass    (ss, _)) = Class  <$> patch2 ss <$> classDeclP 
    go (RawTAlias   (ss, _)) = TAlias <$> patch2 ss <$> tAliasP
    go (RawPAlias   (ss, _)) = PAlias <$> patch2 ss <$> pAliasP
    go (RawQual     (_ , _)) = Qual   <$>               qualifierP
    go (RawInvt     (ss, _)) = Invt              ss <$> bareTypeP
    go (RawCast     (ss, _)) = CastSp            ss <$> bareTypeP
    go (RawExported (ss, _)) = return $ Exported ss


patch2 ss (id, t)    = (fmap (const ss) id , t)

getSpecString :: RawSpec -> String 
getSpecString = go
  where
    go (RawMeas     (_, s)) = s 
    go (RawBind     (_, s)) = s 
    go (RawFunc     (_, s)) = s 
    go (RawIface    (_, s)) = s  
    go (RawField    (_, s)) = s  
    go (RawMethod   (_, s)) = s  
    go (RawStatic   (_, s)) = s  
    go (RawConstr   (_, s)) = s  
    go (RawClass    (_, s)) = s  
    go (RawTAlias   (_, s)) = s  
    go (RawPAlias   (_, s)) = s  
    go (RawQual     (_, s)) = s  
    go (RawInvt     (_, s)) = s  
    go (RawCast     (_, s)) = s  
    go (RawExported (_, s)) = s  

instance IsLocated RawSpec where
  srcPos (RawMeas     (s,_)) = s 
  srcPos (RawBind     (s,_)) = s 
  srcPos (RawFunc     (s,_)) = s 
  srcPos (RawIface    (s,_)) = s  
  srcPos (RawField    (s,_)) = s  
  srcPos (RawMethod   (s,_)) = s  
  srcPos (RawStatic   (s,_)) = s  
  srcPos (RawConstr   (s,_)) = s  
  srcPos (RawClass    (s,_)) = s  
  srcPos (RawTAlias   (s,_)) = s  
  srcPos (RawPAlias   (s,_)) = s  
  srcPos (RawQual     (s,_)) = s  
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

--------------------------------------------------------------------------------------
parseNanoFromFiles :: [FilePath] -> IO (Either (FixResult Error) (NanoBareR Reft))
--------------------------------------------------------------------------------------
parseNanoFromFiles fs = 
  do  sa <- partitionEithers <$> mapM parseScriptFromJSON fs
      case sa of
        ([],ps) -> case expandAnnots $ concat ps of
                     Right ps -> return $ either (Left . Unsafe) Right $ mkCode ps
                     Left e   -> return $ Left e
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
mkCode :: [Statement (SourceSpan, [Spec])] -> Either [Error] (NanoBareR Reft)
---------------------------------------------------------------------------------
mkCode = --debugTyBinds .
         conflateTypeMembers
       . scrapeQuals 
       . expandAliases
       . visitNano convertTvarVisitor []
       . mkCode' 
    
mkCode' ss = Nano { 
        code          = Src (checkTopStmt <$> ss')
      , consts        = envFromList   [ t | Meas   t <- anns ] 
      , tAlias        = envFromList   [ t | TAlias t <- anns ] 
      , pAlias        = envFromList   [ t | PAlias t <- anns ] 
      , pQuals        =               [ t | Qual   t <- anns ] 
      , invts         = [Loc (srcPos l) t | Invt l t <- anns ]
    } 
  where
    toBare           :: (SourceSpan, [Spec]) -> AnnBare Reft 
    toBare (l,αs)     = Ann l $ catMaybes $ bb <$> αs 
    
    bb (Bind   (_,t)) = Just $ VarAnn   t   
    bb (Constr c    ) = Just $ ConsAnn  c   
    bb (Field  f    ) = Just $ FieldAnn f   
    bb (Method m    ) = Just $ MethAnn  m   
    bb (Static m    ) = Just $ StatAnn  m   
    bb (Class  (_,t)) = Just $ ClassAnn t   
    bb (Iface  (_,t)) = Just $ IfaceAnn t   
    bb (CastSp _ t  ) = Just $ UserCast t   
    bb (Exported _  ) = Just $ ExporedModElt
    bb (AnFunc t    ) = Just $ FuncAnn  t   
    bb _              = Nothing

    ss'               = (toBare <$>) <$> ss
    anns              = concatMap (FO.foldMap snd) ss

scrapeQuals     :: NanoBareR Reft -> NanoBareR Reft
scrapeQuals p = p { pQuals = qs ++ pQuals p}
  where
    qs        = qualifiers $ mkUq $ foldNano tbv [] [] p
    tbv       = defaultVisitor { accStmt = stmtTypeBindings
                               , accCElt = celtTypeBindings }

mkUq                  = zipWith tx [0..]
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
    go (MemberVarDecl _ _ (VarDecl l x _))     
                                  = [(x, e) | FieldAnn e <- ann_fact l] ++ 
                                    [(x, e) | StatAnn  e <- ann_fact l]
    go (MemberMethDecl l _ x _ _) = [(x, e) | MethAnn  e <- ann_fact l]
    go _                          = []

debugTyBinds p@(Nano {code = Src ss}) = trace msg p
  where
    xts = [(x, t) | (x, (t, _)) <- visibleNames ss ]
    msg = unlines $ "debugTyBinds:" : (ppshow <$> xts)
 


-------------------------------------------------------------------------------
getQualifPool :: [Statement (SourceSpan, [Spec])] -> [(Id SourceSpan, RefType)]
-------------------------------------------------------------------------------
getQualifPool a = concatMap ext $ everything (++) ([] `mkQ` f) a
  where
    ext (_, ss) = [ b | Bind b <- ss ]

    f :: Statement (SourceSpan, [Spec]) -> [(SourceSpan, [Spec])]
    f (FunctionStmt l _ _ _) = [l]
    f (ClassStmt _ _ _ _ es) = [l | MemberMethDecl l _ _ _ _ <- es ]  
    f (VarDeclStmt _ vds)    = [l | VarDecl l _ _ <- vds ]  
    f _                      = []


type PState = Integer

instance PP Integer where
  pp = pp . show


--------------------------------------------------------------------------------------
expandAnnots :: [Statement (SourceSpan, [RawSpec])] 
             -> Either (FixResult Error) [Statement (SourceSpan, [Spec])]
--------------------------------------------------------------------------------------
expandAnnots ss = 
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
convertTvar    :: (Transformable t) => [TVar] -> t r -> t r
convertTvar as = trans tx as []  
  where
    tx αs _ (TApp c [] r)
      | Just α <- mkTvar αs c = TVar α r 
    tx _ _ t                  = t 

mkTvar αs (TRef r) = listToMaybe [ α { tv_loc = srcPos r }  | α <- αs, symbol α == symbol r]
mkTvar _  _        = Nothing

convertTvarVisitor :: Visitor () [TVar] (AnnR r) 
convertTvarVisitor = defaultVisitor {
    ctxStmt = ctxStmtTvar
  , txStmt  = transFmap (\as _ -> convertTvar as) 
  , txExpr  = transFmap (\as _ -> convertTvar as) 
  }

ctxStmtTvar as s = go s ++ as
  where
    go :: Statement (AnnR r)  -> [TVar]
    go s@(FunctionStmt {}) = grab s 
    go s@(FunctionDecl {}) = grab s 
    go s@(IfaceStmt {})    = grab s
    go s@(ClassStmt {})    = grab s
    go s@(ModuleStmt {})   = grab s
    go _                   = []

    grab :: Statement (AnnR r) -> [TVar]
    grab = concatMap factTvars . ann_fact . getAnnotation 

factTvars :: Fact r -> [TVar]
factTvars = go
  where
    tvars                = fst . bkAll
    go (VarAnn t)        = tvars t
    go (FuncAnn t)       = tvars t
    go (FieldAnn m)      = tvars $ f_type m
    go (MethAnn m)       = tvars $ f_type m
    go (StatAnn m)       = tvars $ f_type m
    go (ConsAnn m)       = tvars $ f_type m
    go (ClassAnn (as,_)) = as
    go (IfaceAnn i)      = t_args i
    go _                 = []
    
