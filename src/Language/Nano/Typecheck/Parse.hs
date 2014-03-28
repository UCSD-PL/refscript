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
import           Data.Char                               (isLower, isSpace)
import           Data.Maybe                              (isJust)
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

import           Text.Parsec                      hiding (parse)
import           Text.Parsec.Pos                         (newPos)
import qualified Text.Parsec.Token                as     Token

import           GHC.Generics

-- import           Debug.Trace                             ( trace, traceShow)

dot        = Token.dot        lexer
plus       = Token.symbol     lexer "+"
star       = Token.symbol     lexer "*"


type ParserS a = Parser (TDefEnv RefType, Integer) a 

----------------------------------------------------------------------------------
-- | Type Binders ----------------------------------------------------------------
----------------------------------------------------------------------------------

idBindP :: ParserS (Id SourceSpan, RefType)
idBindP = xyP identifierP dcolon bareTypeP

idFieldP = xyzP identifierP dcolon bareTypeP

identifierP :: ParserS (Id SourceSpan)
identifierP =   try (withSpan Id upperIdP)
           <|>      (withSpan Id lowerIdP)

pAliasP :: ParserS (Id SourceSpan, PAlias) 
pAliasP = do name <- identifierP
             πs   <- pAliasVarsP -- many symbolP 
             reservedOp "="
             body <- predP 
             return  (name, Alias name [] πs body) 

pAliasVarsP = try (parens $ sepBy symbolP comma)
           <|> many symbolP

tAliasP :: ParserS (Id SourceSpan, TAlias RefType) 
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

tBodyP :: ParserS (Id SourceSpan, TDef RefType)
tBodyP = do  id     <- identifierP 
             vs     <- option [] tParP'
             -- FIXME: add bindings for new(Ts) and call(Ts):T
             ext    <- optionMaybe extendsP
             es     <- braces $ eltsP
             return (id, TD (Just id) vs ext es)


extendsP = do reserved "extends"
              pId <- identifierP
              ts  <- option [] $ angles $ sepBy bareTypeP comma
              return (pId, ts)
               


-- [A,B,C...]
tParP = brackets $ sepBy tvarP comma

tParP' = angles $ sepBy tvarP comma

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

----------------------------------------------------------------------------------
-- | RefTypes --------------------------------------------------------------------
----------------------------------------------------------------------------------

-- | `bareTypeP` parses top-level "bare" types. If no refinements are supplied, 
-- then "top" refinement is used.

bareTypeP :: ParserS RefType 
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
mkUn ts  = TApp TUn (L.sort ts)

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
                withinSpacesP colon
                B s <$> bareTypeP

argBind t = B (rTypeValueVar t) t

bareAtomP p
  =  try (xrefP  p)
 <|> try (refP p)
 <|>     (dummyP p)

bbaseP :: ParserS (Reft -> RefType)
bbaseP 
  =  try (TVar <$> tvarP)                  -- A
 <|> try objLitP                           -- {f1: T1, ... , fn: Tn} 
 <|> try (TArr <$> arrayP)                 -- [T]
 <|> try (TApp <$> tConP <*> bareTyArgsP)  -- list[A], tree[A,B] etc...

objLitP :: ParserS (Reft -> RefType)
objLitP = do 
  bs          <- braces $ bindsP
  -- all fields are public 
  let d        = TD Nothing [] Nothing [ TE s True t | B s t <- bs ]
  (u, n)      <- getState
  let (u', id) = addObjLitTy d u 
  putState     $ (u', n)                -- update the type definitions environment
  return       $ TApp (TRef id) []      -- no type vars 


bareTyArgsP = try (brackets $ sepBy bareTyArgP comma) <|> return []

bareTyArgP  = try bareTypeP 
           <|> (TExp <$> exprP)

tvarP    :: ParserS TVar
tvarP    = withSpan tvar $ wordP isTvar                  -- = withSpan {- (\l x -> TV x l) -} (flip TV) (stringSymbol <$> upperWordP)

tvar l x = TV (stringSymbol x) l

isTvar   = not . isLower . head

wordP p  = condIdP ok p
  where 
    ok   = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0'..'9']

tConP :: ParserS TCon
tConP =  try (reserved "number"    >> return TInt)
     <|> try (reserved "boolean"   >> return TBool)
     <|> try (reserved "undefined" >> return TUndef)
     <|> try (reserved "void"      >> return TVoid)
     <|> try (reserved "top"       >> return TTop)
     <|> try (reserved "string"    >> return TString)
     <|> try (reserved "null"      >> return TNull)
     <|>     (withinSpacesP (char '#') >> identifierP >>= idToTRefP)

idToTRefP :: Id SourceSpan -> ParserS TCon
idToTRefP (Id _ s) = do
  (u, _)      <- getState
  let (u', id) = addSym (symbol s) u 
  modifyState  $ \(_,n) -> (u', n)
  return       $ TRef id

bareAll1P 
  = do reserved "forall"
       αs <- many1 tvarP
       dot
       t  <- bareTypeP
       return $ foldr TAll t αs

arrayP = brackets bareTypeP

-- | f1: t1, f2: t2, ..., fn: tn
bindsP 
  =  try (sepBy1 bareBindP comma)
 <|> (spaces >> return [])

-- | f : type 
bareBindP = do s <- binderP
               withinSpacesP colon
               t <- bareTypeP
               return $ B s t 

-- | f1:[*] t1, f2:[*] t2, ..., fn:[*] tn
eltsP 
  =  try (sepBy1 bareEltP comma)
 <|> (spaces >> return [])

-- | f :[*] type 
bareEltP = do s <- binderP
              withinSpacesP colon
              mut <- isJust <$> optionMaybe star
              t <- bareTypeP
              return $ TE s mut t 

 
dummyP ::  ParserS (Reft -> b) -> ParserS b
dummyP fm = fm `ap` topP 

topP   :: ParserS Reft
topP   = (Reft . (, []) . vv . Just) <$> freshIntP'

-- Using a slightly changed version of `freshIntP`
freshIntP' :: ParserS Integer
freshIntP' = do (u,n) <- stateUser <$> getParserState
                putState $ (u, n+1)
                return n



-- | Parses refined types of the form: `{ kind | refinement }`
xrefP :: ParserS (Reft -> a) -> ParserS a
xrefP kindP
  = braces $ do
      t   <- kindP
      reserved "|"
      ras <- refasP 
      return $ t (Reft (stringSymbol "v", ras))

refasP :: ParserS [Refa]
refasP  =  (try (brackets $ sepBy (RConc <$> predP) semi)) 
       <|> liftM ((:[]) . RConc) predP
 
binderP :: ParserS Symbol
binderP = try (stringSymbol <$> idP badc)
      <|> try (star >> return (stringSymbol "*"))
      <|> liftM pwr (parens (idP bad))
      where idP p  = many1 (satisfy (not . p))
            badc c = (c == ':') ||  bad c
            bad c  = isSpace c || c `elem` "()"
            pwr s  = stringSymbol $ "(" ++ s ++ ")" 

withinSpacesP :: ParserS a -> ParserS a
withinSpacesP p = do { spaces; a <- p; spaces; return a } 
             
classDeclP :: ParserS (Id SourceSpan, ([TVar], Maybe (Id SourceSpan, [RefType])))
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

data PSpec l t 
  = Meas   (Id l, t)
  | Bind   (Id l, t) 
  | Field  (Id l, Bool, t) 
  | Constr (Id l, t)
  | Method (Id l, t)
  | Extern (Id l, t)
  | IFace  (Id l, TDef t)
  | Class  (Id l, ([TVar], Maybe (Id l,[t])))
  | TAlias (Id l, TAlias t)
  | PAlias (Id l, PAlias) 
  | Qual   Qualifier
  | Invt   l t 
  deriving (Eq, Ord, Show, Data, Typeable)

type Spec = PSpec SourceSpan RefType

parseAnnot :: SourceSpan -> RawSpec -> ParserS Spec
parseAnnot ss (RawMeas   _) = Meas   <$> patch2 ss <$> idBindP
parseAnnot ss (RawBind   _) = Bind   <$> patch2 ss <$> idBindP
parseAnnot ss (RawField  _) = Field  <$> patch3 ss <$> idFieldP
parseAnnot ss (RawMethod _) = Method <$> patch2 ss <$> idBindP
parseAnnot ss (RawConstr _) = Constr <$> patch2 ss <$> idBindP
parseAnnot ss (RawExtern _) = Extern <$> patch2 ss <$> idBindP
parseAnnot ss (RawType   _) = IFace  <$> patch2 ss <$> tBodyP >>= registerIface
parseAnnot ss (RawClass  _) = Class  <$> patch2 ss <$> classDeclP 
parseAnnot ss (RawTAlias _) = TAlias <$> patch2 ss <$> tAliasP
parseAnnot ss (RawPAlias _) = PAlias <$> patch2 ss <$> pAliasP
parseAnnot _  (RawQual   _) = Qual   <$>              qualifierP
parseAnnot ss (RawInvt   _) = Invt             ss <$> bareTypeP

registerIface :: Spec -> ParserS Spec
registerIface (IFace (s, t)) = do
  (u, n)  <- getState
  -- XXX: losing the SourceSpan on s here.
  putState $ (fst $ addTySym (symbol s) t u, n)
  return $ IFace (s,t) 
registerIface _ = error "BUG:registerIface"


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
-- | Parse File and Type Signatures -------------------------------------------
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
mkCode :: FilePath -> (PState, [Statement (SourceSpan, [Spec])]) -> NanoBareR Reft
--------------------------------------------------------------------------------------
mkCode f (u, ss) =  Nano {
        fp      = f
      , code    = Src (checkTopStmt <$> ss')
      , externs = envFromList   [ t | Extern t <- anns ] -- externs
      -- FIXME: same name methods in different classes.
      , specs   = catFunSpecDefs ss                      -- function sigs (no methods...)
      , glVars  = catVarSpecDefs ss                      -- variables
      , consts  = envFromList   [ t | Meas   t <- anns ] 
      , defs    = fst u
      , tAlias  = envFromList   [ t | TAlias t <- anns ] 
      , pAlias  = envFromList   [ t | PAlias t <- anns ] 
      , quals   =               [ t | Qual   t <- anns ] 
      , invts   = [Loc (srcPos l) t | Invt l t <- anns ]
    } 
  where
    toBare     :: (SourceSpan, [Spec]) -> AnnBare Reft 
    toBare (l,αs) = Ann l $ [VarAnn t       | Bind   (_,t) <- αs ]
                         ++ [ConsAnn t      | Constr (_,t) <- αs ]
                         ++ [FieldAnn (m,t) | Field  (_,m,t) <- αs ]
                         ++ [MethAnn t      | Method (_,t) <- αs ]
                         ++ [ClassAnn t     | Class  (_,t) <- αs ]
    ss'           = (toBare <$>) <$> ss
    anns          = concatMap (FO.foldMap snd) ss


type PState = (TDefEnv RefType, Integer)


instance PP Integer where
  pp = pp . show

--------------------------------------------------------------------------------------
expandAnnots :: [Statement (SourceSpan, [RawSpec])] -> (PState, [Statement (SourceSpan, [Spec])])
--------------------------------------------------------------------------------------
expandAnnots = mapAccumL (mapAccumL f) (tDefEmpty, 0)
  where f st (ss,sp) = mapSnd ((ss),) $ L.mapAccumL (parse ss) st sp

--------------------------------------------------------------------------------------
parse :: SourceSpan -> PState -> RawSpec -> (PState, Spec)
--------------------------------------------------------------------------------------
parse ss st c = foo c
  where foo s = failLeft $ runParser 
                  (do a <- parseAnnot ss s
                      st <- getState
                      return (st, a)) 
                  st f (getSpecString s)
        failLeft (Left s) = error $ show s
        failLeft (Right r) = r
        f = sourceName $ sp_begin ss

instance PP (RawSpec) where
  pp = text . getSpecString


--------------------------------------------------------------------------------------
catFunSpecDefs :: [Statement (SourceSpan, [Spec])] -> Env RefType
--------------------------------------------------------------------------------------
catFunSpecDefs ss = envFromList [ a | l <- ds , Bind a <- snd l ]
  where ds     = definedFuns ss

--------------------------------------------------------------------------------------
catVarSpecDefs :: [Statement (SourceSpan, [Spec])] -> Env RefType
--------------------------------------------------------------------------------------
catVarSpecDefs ss = envFromList [ a | l <- ds , Bind a <- snd l ]
  where ds     = varDeclStmts ss


-- SYB examples at: http://web.archive.org/web/20080622204226/http://www.cs.vu.nl/boilerplate/#suite
--------------------------------------------------------------------------------------
definedFuns       :: (Data a, Typeable a) => [Statement a] -> [a]
--------------------------------------------------------------------------------------
definedFuns stmts = everything (++) ([] `mkQ` fromFunction) stmts
  where 
    fromFunction (FunctionStmt l _ _ _) = [l] 
    fromFunction _                      = []

--------------------------------------------------------------------------------------
varDeclStmts         :: (Data a, Typeable a) => [Statement a] -> [a]
--------------------------------------------------------------------------------------
varDeclStmts stmts    = everything (++) ([] `mkQ` fromVarDecl) stmts
  where 
    fromVarDecl (VarDecl l _ _) = [l]


--------------------------------------------------------------------------------
printFile :: FilePath -> IO () -- Either Error (NanoBareR Reft))
--------------------------------------------------------------------------------
printFile f = parseNanoFromFile f >>= putStr . ppshow

