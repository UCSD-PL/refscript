{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances         #-} 
{-# LANGUAGE DeriveDataTypeable        #-} 
{-# LANGUAGE UndecidableInstances      #-} 
{-# LANGUAGE TypeSynonymInstances      #-} 
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE DeriveGeneric             #-}

module Language.Nano.Typecheck.Parse (
    parseNanoFromFile, printFile
  ) where

import           Prelude                          hiding ( mapM)

import           Data.Aeson                              (eitherDecode)
import           Data.Aeson.Types                 hiding (Parser, Error, parse)
import qualified Data.Aeson.Types                 as     AI
import qualified Data.ByteString.Lazy.Char8       as     B
import           Data.Char                               (isLower)
import           Data.Default
import           Data.Maybe                              (isJust, isNothing)
import qualified Data.List                        as     L
import           Data.Generics.Aliases                   ( mkQ)
import           Data.Generics.Schemes
import           Data.Traversable                        (mapAccumL)
import           Text.PrettyPrint.HughesPJ               (text)
import           Data.Data
import qualified Data.Foldable                    as     FO
import           Data.Vector                             ((!))

import           Control.Monad                    hiding (mapM)
import           Control.Monad.Trans                     (MonadIO,liftIO)
import           Control.Applicative                     ((<$>), ( <*>))

import           Language.Fixpoint.Types          hiding (quals, Loc, Expression)
import           Language.Fixpoint.Parse
import           Language.Fixpoint.Misc                  (mapEither, mapSnd)
import           Language.Nano.Types
import           Language.Nano.Errors
import           Language.Nano.Typecheck.Types
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
star       = T.symbol     lexer "*"
question   = T.symbol     lexer "?"

----------------------------------------------------------------------------------
-- | Type Binders 
----------------------------------------------------------------------------------

idBindP :: Parser (Id SourceSpan, RefType)
idBindP = xyP identifierP dcolon bareTypeP

idFieldP = xyzP identifierP dcolon bareTypeP

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

xyzP lP sepP rP
  = (\x _ y z -> (x, y, z)) <$> lP 
                            <*> (spaces >> sepP) 
                            <*> (isJust <$> optionMaybe star) 
                            <*> rP

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
       colon
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
  = do m     <- option mutable (toType <$> mutP)
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

tvar l x = TV (stringSymbol x) l

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
              try indexSigP 
          <|> try fieldSigP
          <|> try statSigP
          <|> try methSigP
          <|> try callSigP
          <|> try consSigP
              ) semi


-- | [f: string]: t
-- | [f: number]: t

indexSigP = do ((x,it),t) <- xyP (brackets indexP) colon bareTypeP
               case it of 
                 "number" -> return $ IndexSig x False t
                 "string" -> return $ IndexSig x True t
                 _        -> error $ "Index signature can only have " ++
                                     "string or number as index." 

indexP = xyP id colon sn
  where
    id = symbol <$> (try lowerIdP <|> upperIdP)
    sn = withinSpacesP (string "string" <|> string "number")


-- | <[mut]> f<[τ]>: t
fieldSigP = do 
   --  s          <- option False $ try $ reserved "static" >> return True
    m          <- option def (toType <$> mutP)
    x          <- symbolP 
    _          <- colon
    τ          <- optionMaybe $ withinSpacesP $ brackets bareTypeP
    t          <- bareTypeP
    
    return      $ FieldSig x m τ t

-- | static <[mut]> f: t
statSigP = do 
    _          <- reserved "static"
    m          <- option def (toType <$> mutP)
    x          <- symbolP 
    _          <- colon
    t          <- bareTypeP
    
    return      $ StatSig x m t

-- | <[mut]> m<[τ]>(ts): t
methSigP = do
    m          <- option def (toType <$> mutP)
    x          <- symbolP 
    _          <- colon
    τ          <- optionMaybe $ withinSpacesP $ brackets bareTypeP
    t          <- bareAllP bareMethP
    return      $ MethSig x m τ t


-- | <forall A .> (t...) => t

callSigP = CallSig <$> withinSpacesP (bareAllP bareFunP)


-- | new <forall A .> (t...) => t

consSigP = do
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
      return $ t (Reft (stringSymbol "v", ras))

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
  = RawMeas   String   -- Measure
  | RawBind   String   -- Function bindings
  | RawExtern String   -- Extern declarations
  | RawType   String   -- Variable declaration annotations
  | RawClass  String   -- Class annots
  | RawField  String   -- Field annots
  | RawMethod String   -- Method annots
  | RawConstr String   -- Constructor annots
  | RawTAlias String   -- Type aliases
  | RawPAlias String   -- Predicate aliases
  | RawQual   String   -- Qualifiers
  | RawInvt   String   -- Invariants
  deriving (Show,Eq,Ord,Data,Typeable,Generic)

data PSpec l r
  = Meas   (Id l, RType r)
  | Bind   (Id l, RType r) 
  | Field  (Id l, Bool, RType r) 
  | Constr (Id l, RType r)
  | Method (Id l, RType r)
  | Extern (Id l, RType r)
  | IFace  (Id l, TDef r)
  | Class  (Id l, ([TVar], Maybe (Id l,[RType r])))
  | TAlias (Id l, TAlias (RType r))
  | PAlias (Id l, PAlias) 
  | Qual   Qualifier
  | Invt   l (RType r) 
  deriving (Eq, Ord, Show, Data, Typeable)

type Spec = PSpec SourceSpan Reft

parseAnnot :: SourceSpan -> RawSpec -> Parser Spec
parseAnnot ss (RawMeas   _) = Meas   <$> patch2 ss <$> idBindP
parseAnnot ss (RawBind   _) = Bind   <$> patch2 ss <$> idBindP
parseAnnot ss (RawField  _) = Field  <$> patch3 ss <$> idFieldP
parseAnnot ss (RawMethod _) = Method <$> patch2 ss <$> idBindP
parseAnnot ss (RawConstr _) = Constr <$> patch2 ss <$> idBindP
parseAnnot ss (RawExtern _) = Extern <$> patch2 ss <$> idBindP
parseAnnot ss (RawType   _) = IFace  <$> patch2 ss <$> iFaceP
parseAnnot ss (RawClass  _) = Class  <$> patch2 ss <$> classDeclP 
parseAnnot ss (RawTAlias _) = TAlias <$> patch2 ss <$> tAliasP
parseAnnot ss (RawPAlias _) = PAlias <$> patch2 ss <$> pAliasP
parseAnnot _  (RawQual   _) = Qual   <$>               qualifierP
parseAnnot ss (RawInvt   _) = Invt              ss <$> bareTypeP


patch2 ss (id, t)    = (fmap (const ss) id , t)
patch3 ss (id, m, t) = (fmap (const ss) id , m, t)

getSpecString :: RawSpec -> String 
getSpecString (RawMeas   s) = s 
getSpecString (RawBind   s) = s 
getSpecString (RawExtern s) = s  
getSpecString (RawType   s) = s  
getSpecString (RawField  s) = s  
getSpecString (RawMethod s) = s  
getSpecString (RawConstr s) = s  
getSpecString (RawClass  s) = s  
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
-- | Parse File and Type Signatures 
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
parseNanoFromFile :: FilePath-> IO (NanoBareR Reft)
-------------------------------------------------------------------------------
parseNanoFromFile f 
  = do  ssP   <- parseScriptFromJSON =<< getPreludePath
        ssF   <- parseScriptFromJSON f 
        -- Concat the programs at the JSON level
        return $ mkCode f $ expandAnnots $ ssP ++ ssF

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
mkCode :: FilePath -> [Statement (SourceSpan, [Spec])] -> NanoBareR Reft
--------------------------------------------------------------------------------------
mkCode f ss =  Nano {
        fp      = f
      , code    = Src (checkTopStmt <$> ss')
      , externs = envFromList   [ t | Extern t <- anns ]          -- externs
      -- FIXME: same name methods in different classes.
      , specs   = catFunSpecDefs δ ss                               -- function sigs (no methods...)
      -- , glVars  = catVarSpecDefs ss                            -- variables
      , consts  = envFromList   [ t | Meas   t <- anns ] 
      , defs    = δ
      , tAlias  = envFromList   [ t | TAlias t <- anns ] 
      , pAlias  = envFromList   [ t | PAlias t <- anns ] 
      , quals   =               [ t | Qual   t <- anns ] 
      , invts   = [Loc (srcPos l) t | Invt l t <- anns ]
    } 
  where
    δ             = tDefFromList [ checkIF t | IFace  t <- anns ] 
    toBare     :: (SourceSpan, [Spec]) -> AnnBare Reft 
    toBare (l,αs) = Ann l $ [VarAnn t       | Bind   (_,t) <- αs ]
                         ++ [ConsAnn t      | Constr (_,t) <- αs ]
                         ++ [FieldAnn (m,t) | Field  (_,m,t) <- αs ]
                         ++ [MethAnn t      | Method (_,t) <- αs ]
                         ++ [ClassAnn t     | Class  (_,t) <- αs ]
    ss'           = (toBare <$>) <$> ss
    anns          = concatMap (FO.foldMap snd) ss

-- | At the moment we only support a single index signature with no other
-- elements, or (normally) bound types without index signature.
checkIF t@(_,TD _ _ _ _ elts) 
  | nTi == 0  = t
  | nTi == 1 && nTe == 0 && nTn == 0 = t
  | otherwise = error $ "[UNIMPLEMENTED] Object types " ++ 
                        "can only have a single indexable " ++
                        "signature and no other elements."
  where 
    nTn = length [ () | IndexSig _ False _ <- elts ]
    nTi = length [ () | IndexSig _ _ _     <- elts ]
    nTe = length [ () | FieldSig _ _ _ _   <- elts ]


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
        parser s = do a <- parseAnnot ss s
                      st <- getState
                      it <- getInput
                      if it /= "" 
                        then unexpected $ "trailing input: " ++ it
                        else return $ (st, a) 

        failLeft (Left s) = error $ "Error parsing: " ++ show c ++ "\n" ++ show s
        failLeft (Right r) = r
        f = sourceName $ sp_begin ss

instance PP (RawSpec) where
  pp = text . getSpecString


--------------------------------------------------------------------------------------
catFunSpecDefs :: TDefEnv Reft -> [Statement (SourceSpan, [Spec])] -> Env RefType
--------------------------------------------------------------------------------------
catFunSpecDefs δ ss = envFromList [ (i, checkType δ t) | l <- ds , Bind (i,t) <- snd l ]
  where ds     = definedFuns ss

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

-- --------------------------------------------------------------------------------------
-- varDeclStmts         :: (Data a, Typeable a) => [Statement a] -> [a]
-- --------------------------------------------------------------------------------------
-- varDeclStmts stmts    = everything (++) ([] `mkQ` fromVarDecl) stmts
--   where 
--     fromVarDecl (VarDecl l _ _) = [l]

--------------------------------------------------------------------------------
printFile :: FilePath -> IO () -- Either Error (NanoBareR Reft))
--------------------------------------------------------------------------------
printFile f = parseNanoFromFile f >>= putStr . ppshow



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


checkType :: TDefEnv Reft -> RefType -> RefType
checkType δ typ = 
    case everything (++) ([] `mkQ` fromType) typ of
      [] -> typ
      es -> error $ show es 
  where 
    fromType :: RefType -> [TypeError]
    fromType (TApp (TRef x) (m:_) _) | isNothing (findSym x δ) = [NameNotFound x] 
                                     | not (validMutability m) = [InvalidMutability x]
    fromType _                       = []

    validMutability (TVar _ _)       = True
    validMutability t                = isMutabilityType t

