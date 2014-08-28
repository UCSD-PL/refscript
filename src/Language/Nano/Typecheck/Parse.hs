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

import           Data.Aeson                              (eitherDecode)
import           Data.Aeson.Types                 hiding (Parser, Error, parse)
import qualified Data.Aeson.Types                 as     AI
import qualified Data.ByteString.Lazy.Char8       as     B
import           Data.Char                               (isLower)
import qualified Data.List                        as     L
import           Data.Generics.Aliases                   ( mkQ)
import           Data.Generics.Schemes
import           Data.Traversable                        (mapAccumL)
import           Text.PrettyPrint.HughesPJ               (text)
import           Data.Data
import qualified Data.Foldable                    as     FO
import           Data.Vector                             ((!))

import           Control.Monad
import           Control.Monad.Trans                     (MonadIO,liftIO)
import           Control.Applicative                     ((<$>), ( <*>))

import           Language.Fixpoint.Types          hiding (quals, Loc, Expression)
import           Language.Fixpoint.Parse
import           Language.Fixpoint.Misc                  (mapEither, mapSnd)
import           Language.Nano.Types
import           Language.Nano.Errors
import           Language.Nano.Typecheck.Types    hiding (argBind)
import           Language.Nano.Liquid.Types
import           Language.Nano.Env
import           Language.Nano.Files

import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.PrettyPrint

import           Text.Printf 
import           Text.Parsec                      hiding (parse)
import           Text.Parsec.Pos                         (newPos)
import qualified Text.Parsec.Token                as     T

import           GHC.Generics

-- import           Debug.Trace                             ( trace, traceShow)

dot        = T.dot        lexer
plus       = T.symbol     lexer "+"
question   = T.symbol     lexer "?"

----------------------------------------------------------------------------------
-- | Type Binders 
----------------------------------------------------------------------------------

idBindP :: Parser (Id SourceSpan, RefType)
idBindP = xyP identifierP dcolon bareTypeP

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

aliasVarT :: (SourceSpan, Symbol) -> Either TVar Symbol
aliasVarT (l, x)      
  | isTvar x' = Left  $ tvar l x
  | otherwise = Right $ x 
  where
    x'        = symbolString x
    
iFaceP   :: Parser (Id SourceSpan, TDef Reft)
iFaceP   = do id     <- identifierP 
              vs     <- option [] tParP
              ext    <- optionMaybe extendsP
              es     <- braces propBindP
              return (id, TD False id vs ext es)

extendsP = do reserved "extends"
              pId <- (char '#' >> identifierP)
              ts  <- option [] $ brackets $ sepBy bareTypeP comma
              return (pId, ts)
               

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
-- then "top" refinement is used.
----------------------------------------------------------------------------------
bareTypeP :: Parser RefType 
----------------------------------------------------------------------------------
bareTypeP = bareAllP $ 
      try bUnP
  <|> try (refP rUnP)
  <|>     (xrefP rUnP)

rUnP        = mkU <$> parenNullP (bareTypeNoUnionP `sepBy1` plus) toN
  where
    mkU [x] = strengthen x
    mkU xs  = TApp TUn (L.sort xs)
    toN     = (tNull:)

bUnP        = parenNullP (bareTypeNoUnionP `sepBy1` plus) toN >>= mkU
  where
    mkU [x] = return x
    mkU xs  = TApp TUn xs <$> topP
    toN     = (tNull:)


-- | `bareTypeNoUnionP` parses a type that does not contain a union at the top-level.
bareTypeNoUnionP = try funcSigP <|> (bareAllP $ bareAtomP bbaseP)

-- | `optNullP` optionally parses "( `a` )?", where `a` is parsed by the input parser @pr@.
parenNullP p f =  try (f <$> postP p question) <|> p

-- | `funcSigP` parses a function type that is possibly generic and/or an intersection.
funcSigP =  try (bareAllP bareFunP)
        <|> try (intersectP $ bareAllP bareFunP) 
  where
    intersectP p = tAnd <$> many1 (reserved "/\\" >> withinSpacesP p)

methSigP =  try (bareAllP bareMethP)
        <|> try (intersectP $ bareAllP bareMethP) 
  where
    intersectP p = tAnd <$> many1 (reserved "/\\" >> withinSpacesP p)


-- | `bareFunP` parses a single function type
--
--  (x:t, ...) => t
--
bareFunP
  = do args   <- parens $ sepBy bareArgP comma
       reserved "=>" 
       ret    <- bareTypeP 
       r      <- topP
       return $ TFun args ret r


--  (x:t, ...): t
bareMethP
  = do args   <- parens $ sepBy bareArgP comma
       _      <- colon
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

----------------------------------------------------------------------------------
bbaseP :: Parser (Reft -> RefType)
----------------------------------------------------------------------------------
bbaseP 
  =  try (TVar <$> tvarP)                  -- A
 <|> try objLitP                           -- {f1: T1; ... ; fn: Tn} 
 -- FIXME
 -- Disabling array in this form cause there is no room for mutability ...
 -- <|> try (rtArr <$> arrayP)                -- Array<T>
 <|> try (TApp <$> tConP <*> bareTyArgsP)  -- #List[A], #Tree[A,B] etc...
 
----------------------------------------------------------------------------------
objLitP :: Parser (Reft -> RefType)
----------------------------------------------------------------------------------
objLitP 
  = do m     <- option anyMutability (toType <$> mutP)
       bs    <- braces propBindP
       return $ TCons bs m
 
mutP =  try (TVar <$> brackets tvarP <*> return ()) 
    <|> try (TApp <$> brackets tConP <*> return [] <*> return ())


bareTyArgsP = try (brackets $ sepBy bareTyArgP comma) <|> return []

bareTyArgP  = try bareTypeP 
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
     <|>     (withinSpacesP $ char '#' >> identifierP >>= idToTRefP)

----------------------------------------------------------------------------------
idToTRefP :: Id SourceSpan -> Parser TCon
----------------------------------------------------------------------------------
idToTRefP (Id _ s) = return $ TRef (symbol s)

bareAll1P p
  = do reserved "forall"
       αs <- many1 tvarP
       dot
       t  <- p
       return $ foldr TAll t αs

bareAllP p =  try p 
          <|> bareAll1P p

-- arrayP = brackets bareTypeP

propBindP =  sepEndBy (
              try indexEltP 
          <|> try fieldEltP
          <|> try statEltP
          <|> try methEltP
          <|> try callEltP
          <|> try consEltP
              ) semi


-- | [f: string]: t
-- | [f: number]: t

indexEltP = do ((x,it),t) <- xyP (brackets indexP) colon bareTypeP
               case it of 
                 "number" -> return $ IndexSig x False t
                 "string" -> return $ IndexSig x True t
                 _        -> error $ "Index signature can only have " ++
                                     "string or number as index." 

indexP = xyP id colon sn
  where
    id = symbol <$> (try lowerIdP <|> upperIdP)
    sn = withinSpacesP (string "string" <|> string "number")


-- | <[mut]> f: t
fieldEltP = do 
    x          <- symbolP 
    _          <- colon
    m          <- option inheritedMut (toType <$> mutP)
    t          <- bareTypeP
    return      $ FieldSig x m t

-- | static <[mut]> f :: t
statEltP = do 
    _          <- reserved "static"
    x          <- symbolP 
    _          <- colon
    m          <- option anyMutability (toType <$> mutP)
    t          <- bareTypeP
    return      $ StatSig x m t

-- | <[mut]> m :: (ts): t
methEltP = do
    x          <- symbolP 
    _          <- colon
    m          <- option anyMutability (toType <$> mutP)
    t          <- methSigP
    return      $ MethSig x m $ outT t
  where
    outT t      =  
      case bkFun t of 
        Just (_, (B x _):_,_) | x == symbol "this" -> t
        Just (vs, bs      ,ot)                     -> mkFun (v:vs, B (symbol "this") tv : bs, ot)
        _                                          -> t
    -- XXX: using _THIS_ as a reserved sting here.
    v  = TV (symbol "_THIS_") (srcPos (dummyPos "RSC.Parse.methEltP"))
    tv = TVar v fTop


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
classDeclP :: Parser (Id SourceSpan, ([TVar], Maybe (Id SourceSpan, [RefType])))
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
  = RawMeas   (SourceSpan, String)   -- Measure
  | RawBind   (SourceSpan, String)   -- Function bindings
  | RawFunc   (SourceSpan, String)   -- Anonymouns function type
  | RawExtern (SourceSpan, String)   -- Extern declarations
  | RawType   (SourceSpan, String)   -- Variable declaration annotations
  | RawClass  (SourceSpan, String)   -- Class annots
  | RawField  (SourceSpan, String)   -- Field annots
  | RawMethod (SourceSpan, String)   -- Method annots
  | RawStatic (SourceSpan, String)   -- Static annots
  | RawConstr (SourceSpan, String)   -- Constructor annots
  | RawTAlias (SourceSpan, String)   -- Type aliases
  | RawPAlias (SourceSpan, String)   -- Predicate aliases
  | RawQual   (SourceSpan, String)   -- Qualifiers
  | RawInvt   (SourceSpan, String)   -- Invariants
  | RawCast   (SourceSpan, String)   -- Casts
  deriving (Show,Eq,Ord,Data,Typeable,Generic)

data PSpec l r
  = Meas   (Id l, RType r)
  | Bind   (Id l, RType r) 
  | AnFunc (RType r) 
  | Field  (TElt r)
  | Constr (TElt r)
  | Method (TElt r)
  | Static (TElt r)
  | Extern (Id l, RType r)
  | IFace  (Id l, TDef r)
  | Class  (Id l, ([TVar], Maybe (Id l,[RType r])))
  | TAlias (Id l, TAlias (RType r))
  | PAlias (Id l, PAlias) 
  | Qual   Qualifier
  | Invt   l (RType r) 
  | CastSp l (RType r)
  deriving (Eq, Ord, Show, Data, Typeable)

type Spec = PSpec SourceSpan Reft

parseAnnot :: RawSpec -> Parser Spec
parseAnnot (RawMeas   (ss, _)) = Meas   <$> patch2 ss <$> idBindP
parseAnnot (RawBind   (ss, _)) = Bind   <$> patch2 ss <$> idBindP
parseAnnot (RawFunc   (_ , _)) = AnFunc <$>               anonFuncP
parseAnnot (RawField  (_ , _)) = Field  <$>               fieldEltP
parseAnnot (RawMethod (_ , _)) = Method <$>               methEltP
parseAnnot (RawStatic (_ , _)) = Static <$>               statEltP
parseAnnot (RawConstr (_ , _)) = Constr <$>               consEltP
parseAnnot (RawExtern (ss, _)) = Extern <$> patch2 ss <$> idBindP
parseAnnot (RawType   (ss, _)) = IFace  <$> patch2 ss <$> iFaceP
parseAnnot (RawClass  (ss, _)) = Class  <$> patch2 ss <$> classDeclP 
parseAnnot (RawTAlias (ss, _)) = TAlias <$> patch2 ss <$> tAliasP
parseAnnot (RawPAlias (ss, _)) = PAlias <$> patch2 ss <$> pAliasP
parseAnnot (RawQual   (_ , _)) = Qual   <$>               qualifierP
parseAnnot (RawInvt   (ss, _)) = Invt              ss <$> bareTypeP
parseAnnot (RawCast   (ss, _)) = CastSp            ss <$> bareTypeP


patch2 ss (id, t)    = (fmap (const ss) id , t)

getSpecString :: RawSpec -> String 
getSpecString (RawMeas   (_, s)) = s 
getSpecString (RawBind   (_, s)) = s 
getSpecString (RawFunc   (_, s)) = s 
getSpecString (RawExtern (_, s)) = s  
getSpecString (RawType   (_, s)) = s  
getSpecString (RawField  (_, s)) = s  
getSpecString (RawMethod (_, s)) = s  
getSpecString (RawStatic (_, s)) = s  
getSpecString (RawConstr (_, s)) = s  
getSpecString (RawClass  (_, s)) = s  
getSpecString (RawTAlias (_, s)) = s  
getSpecString (RawPAlias (_, s)) = s  
getSpecString (RawQual   (_, s)) = s  
getSpecString (RawInvt   (_, s)) = s  
getSpecString (RawCast   (_, s)) = s  

getSpecSourceSpan :: RawSpec -> SourceSpan
getSpecSourceSpan (RawMeas   (s,_)) = s 
getSpecSourceSpan (RawBind   (s,_)) = s 
getSpecSourceSpan (RawFunc   (s,_)) = s 
getSpecSourceSpan (RawExtern (s,_)) = s  
getSpecSourceSpan (RawType   (s,_)) = s  
getSpecSourceSpan (RawField  (s,_)) = s  
getSpecSourceSpan (RawMethod (s,_)) = s  
getSpecSourceSpan (RawStatic (s,_)) = s  
getSpecSourceSpan (RawConstr (s,_)) = s  
getSpecSourceSpan (RawClass  (s,_)) = s  
getSpecSourceSpan (RawTAlias (s,_)) = s  
getSpecSourceSpan (RawPAlias (s,_)) = s  
getSpecSourceSpan (RawQual   (s,_)) = s  
getSpecSourceSpan (RawInvt   (s,_)) = s  
getSpecSourceSpan (RawCast   (s,_)) = s  


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
parseNanoFromFiles :: [FilePath] -> IO (NanoBareR Reft)
--------------------------------------------------------------------------------------
parseNanoFromFiles fs 
  = do  ssP   <- parseScriptFromJSON =<< getPreludePath
        ssF   <- concat <$> mapM parseScriptFromJSON fs
        return $ mkCode $ expandAnnots $ ssP ++ ssF

--------------------------------------------------------------------------------------
getJSON :: MonadIO m => FilePath -> m B.ByteString
--------------------------------------------------------------------------------------
getJSON = liftIO . B.readFile

--------------------------------------------------------------------------------------
parseScriptFromJSON :: FilePath -> IO [Statement (SourceSpan, [RawSpec])]
--------------------------------------------------------------------------------------
parseScriptFromJSON filename = decodeOrDie <$> getJSON filename
  where 
    decodeOrDie s =
      case eitherDecode s :: Either String [Statement (SourceSpan, [RawSpec])] of
        Left msg -> error $ "JSON decode error:\n" ++ msg
        Right p  -> p

--------------------------------------------------------------------------------------
mkCode :: [Statement (SourceSpan, [Spec])] -> NanoBareR Reft
--------------------------------------------------------------------------------------
mkCode ss       =  Nano {
        code      = Src (checkTopStmt <$> ss')
      , externs   = envFromList   [ t | Extern t <- anns ]       -- externs
      -- FIXME: same name methods in different classes.
      , specs     = catFunSpecDefs δ ss                          -- function sigs (no methods...)
      , defs      = δ
      , consts    = envFromList   [ t | Meas   t <- anns ] 
      , tAlias    = envFromList   [ t | TAlias t <- anns ] 
      , pAlias    = envFromList   [ t | PAlias t <- anns ] 
      , quals     =               [ t | Qual   t <- anns ] 
      , invts     = [Loc (srcPos l) t | Invt l t <- anns ]
    } 
  where
    δ             = tDefFromList [ processInterface t | IFace  t <- anns ] 
    toBare       :: (SourceSpan, [Spec]) -> AnnBare Reft 
    toBare (l,αs) = Ann l $ [VarAnn   t     | Bind   (_,t) <- αs ]
                         ++ [ConsAnn  c     | Constr c     <- αs ]
                         ++ [FieldAnn f     | Field  f     <- αs ]
                         ++ [MethAnn  m     | Method m     <- αs ]
                         ++ [StatAnn  m     | Static m     <- αs ]
                         ++ [ClassAnn t     | Class  (_,t) <- αs ]
                         ++ [UserCast t     | CastSp _ t   <- αs ]
                         ++ [FuncAnn  t     | AnFunc t     <- αs ]
    ss'           = (toBare <$>) <$> ss
    anns          = concatMap (FO.foldMap snd) ss

-- | At the moment we only support a single index signature with no other
--   elements, or (normally) bound types without index signature.
processInterface (t, TD n x vs p elts)
  | nTi == 0  || (nTi == 1 && nTe == 0 && nTn == 0) 
  -- Replace the 'this' binding in every method element with the type of the interface
  = (t, TD n x vs p (f <$> elts))

  | otherwise = error   $ "[UNIMPLEMENTED] Object types " ++ 
                          "can only have a single indexable " ++
                          "signature and no other elements."
  where 
    nTn                 = length [ () | IndexSig _ False _ <- elts ]
    nTi                 = length [ () | IndexSig _ _ _     <- elts ]
    nTe                 = length [ () | FieldSig _ _ _     <- elts ]

    tvs v               = TVar v fTop
    tClass              = TApp (TRef $ symbol x) (tvs <$> vs) fTop
    f m@(MethSig _ _ _) = setThisBinding m tClass
    f m                 = m


type PState = Integer

instance PP Integer where
  pp = pp . show


--------------------------------------------------------------------------------------
expandAnnots :: [Statement (SourceSpan, [RawSpec])] -> [Statement (SourceSpan, [Spec])]
--------------------------------------------------------------------------------------
expandAnnots = snd <$> mapAccumL (mapAccumL f) 0
  where f st (ss,sp) = mapSnd ((ss),) $ L.mapAccumL (parse ss) st sp

--------------------------------------------------------------------------------------
parse :: SourceSpan -> PState -> RawSpec -> (PState, Spec)
--------------------------------------------------------------------------------------
parse ss st c = foo c
  where foo s    = failLeft $ runParser (parser s) st f (getSpecString s)
        parser s = do a <- parseAnnot s
                      st <- getState
                      it <- getInput
                      if it /= "" 
                        then unexpected $ "trailing input: " ++ it
                        else return $ (st, a) 
        failLeft (Left _) = error $ "Error while parsing: " 
                                  ++ show (getSpecString c) 
                                  ++ "\nAt position: " 
                                  ++ ppshow (getSpecSourceSpan c)
        failLeft (Right r) = r
        f = sourceName $ sp_begin ss

instance PP (RawSpec) where
  pp = text . getSpecString


-- FIXME: Disabling the check here
--------------------------------------------------------------------------------------
catFunSpecDefs :: TDefEnv Reft -> [Statement (SourceSpan, [Spec])] -> Env RefType
--------------------------------------------------------------------------------------
-- catFunSpecDefs δ ss = envFromList [ (i, checkType δ t) | l <- ds , Bind (i,t) <- snd l ]
catFunSpecDefs _ ss = envFromList [ (i, t) | (_,l) <- definedFuns ss, Bind (i,t) <- l ]

-- --------------------------------------------------------------------------------------
-- catVarSpecDefs :: [Statement (SourceSpan, [Spec])] -> Env RefType
-- --------------------------------------------------------------------------------------
-- catVarSpecDefs ss = envFromList [ a | l <- ds , Bind a <- snd l ]
--   where ds        = varDeclStmts ss


-- SYB examples at: http://web.archive.org/web/20080622204226/http://www.cs.vu.nl/boilerplate/#suite
--------------------------------------------------------------------------------------
definedFuns       :: (Data a, Typeable a) => [Statement a] -> [a]
--------------------------------------------------------------------------------------
definedFuns stmts = everything (++) ([] `mkQ` fromFunction) stmts
  where 
    fromFunction (FunctionStmt l _ _ _) = [l] 
    fromFunction _                      = []


--------------------------------------------------------------------------------
-- | Sanity checks on types
--------------------------------------------------------------------------------
--
-- Perhaps move these to typechecking
--

data TypeError = NameNotFound Symbol
               | InvalidMutability Symbol
  deriving (Data, Typeable)

instance Show TypeError where
  show (NameNotFound s)      = printf "Type '%s' is unbound" (ppshow s)
  show (InvalidMutability s) = "Invalid mutability symbol '" 
                            ++ ppshow s 
                            ++ "'. "
                            ++ "Possible fix: "
                            ++ "add a mutability modifier as the first type argument"


-- -- FIXME: This won't work here cause classes have not been included in δ 
-- --------------------------------------------------------------------------------
-- checkType :: TDefEnv Reft -> RefType -> RefType
-- --------------------------------------------------------------------------------
-- checkType δ typ = 
--     case everything (++) ([] `mkQ` fromType) typ of
--       [] -> typ
--       es -> error $ show es 
--   where 
--     fromType :: RefType -> [TypeError]
--     fromType (TApp (TRef x) (m:_) _) | isNothing (findSym x δ) = [NameNotFound x] 
--                                      | not (validMutability m) = [InvalidMutability x]
--     fromType _                       = []
-- 
--     validMutability (TVar _ _)       = True
--     validMutability t                = isMutabilityType t
-- 
