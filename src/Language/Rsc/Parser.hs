{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}


module Language.Rsc.Parser
    ( FError
    , parseRscFromFiles
    , parseScriptFromJSON
    , parseIdFromJSON
    ) where

import           Control.Applicative              ((<$>))
import           Control.Monad
import           Control.Monad.Trans              (MonadIO, liftIO)
import           Data.Aeson                       (eitherDecode)
import qualified Data.ByteString.Lazy.Char8       as B
import           Data.Either                      (partitionEithers)
import qualified Data.Foldable                    as FO
import qualified Data.List                        as L
import           Data.Maybe                       (catMaybes)
import           Data.Monoid
import           Data.Traversable                 (mapAccumL)
import           Data.Tuple
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Types          as F
import           Language.Rsc.Annotations         hiding (err)
import           Language.Rsc.AST
import           Language.Rsc.Core.Env
import           Language.Rsc.Errors
import           Language.Rsc.Liquid.Alias
import           Language.Rsc.Liquid.Qualifiers
import           Language.Rsc.Locations           hiding (val)
import           Language.Rsc.Misc                ((&))
import           Language.Rsc.Names
import           Language.Rsc.Parser.Annotations
import           Language.Rsc.Parser.Declarations ()
import           Language.Rsc.Parser.Types        (pCtxFromList)
import           Language.Rsc.Program
import           Language.Rsc.Transformations
import           Language.Rsc.Traversals
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
import           Language.Rsc.Visitor
import           Prelude                          hiding (mapM)
import           Text.Parsec                      hiding (State, parse)
import           Text.Parsec.Error                (errorMessages, showErrorMessages)

-- import           Debug.Trace                             ( trace, traceShow)
import           Language.Rsc.Pretty


type FError = F.FixResult Error

--------------------------------------------------------------------------------
-- | Parse File and Type Signatures
--------------------------------------------------------------------------------

-- | Parse the contents of a FilePath list into a program structure with relative
-- qualified names.
--------------------------------------------------------------------------------
parseRscFromFiles :: [FilePath] -> IO (Either FError RefScript)
--------------------------------------------------------------------------------
parseRscFromFiles fs =
  partitionEithers <$> mapM parseScriptFromJSON fs >>= \case
    ([],ps) -> return (parseAnnotations (concat ps) >>= mkRsc)
    (es,_ ) -> return (Left $ mconcat es)

--------------------------------------------------------------------------------
getJSON :: MonadIO m => FilePath -> m B.ByteString
--------------------------------------------------------------------------------
getJSON = liftIO . B.readFile

--------------------------------------------------------------------------------
parseScriptFromJSON :: FilePath -> IO (Either (F.FixResult a) [Statement (SrcSpan, [RawSpec])])
--------------------------------------------------------------------------------
parseScriptFromJSON filename = decodeOrDie <$> getJSON filename
  where
    decodeOrDie s =
      case eitherDecode s :: Either String [Statement (SrcSpan, [RawSpec])] of
        Left msg -> Left  $ F.Crash [] $ "JSON decode error:\n" ++ msg
        Right p  -> Right p

--------------------------------------------------------------------------------
parseIdFromJSON :: FilePath -> IO (Either (F.FixResult a) [Id (SrcSpan, [RawSpec])])
--------------------------------------------------------------------------------
parseIdFromJSON filename = decodeOrDie <$> getJSON filename
  where
    decodeOrDie s =
      case eitherDecode s :: Either String [Id (SrcSpan, [RawSpec])] of
        Left msg -> Left  $ F.Crash [] $ "JSON decode error:\n" ++ msg
        Right p  -> Right p

--------------------------------------------------------------------------------
mkRsc :: [Statement (SrcSpan, [Spec])] -> Either FError RefScript
--------------------------------------------------------------------------------
mkRsc ss = let p1          = mkRelRsc        ss
               p2          = expandAliases   p1
               (p3, errs)  = replaceAbsolute p2
               p4          = replaceDotRef   p3
               p5          = fixFunBinders   p4 in
            case errs of
              [] -> Right p5
              _  -> Left $ F.Unsafe errs

--------------------------------------------------------------------------------
mkRelRsc :: [Statement (SrcSpan, [Spec])] -> RelRefScript
--------------------------------------------------------------------------------
mkRelRsc ss = Rsc {
        code          = Src (checkTopStmt <$> ss')
      , consts        = envFromList [ mapSnd (ntransPure f g) t | MeasureSpec t <- anns ]
      , tAlias        = envFromList [ t | TypeAliasSpec      t <- anns ]
      , pAlias        = envFromList [ t | PredicateAliasSpec t <- anns ]
      , pQuals        = scrapeQuals [ t | QualifierSpec      t <- anns ] ss'
      , pOptions      =             [ t | OptionSpec         t <- anns ]
      , invts         = [Loc (srcPos l) (ntransPure f g t) | InvariantSpec l t <- anns ]
      , maxId         = endingId
    }
  where
    toBare           :: Int -> (SrcSpan, [Spec]) -> AnnRel F.Reft
    toBare n (l,αs)   = FA n l $ catMaybes $ extractFact αs
    f (QN p s)        = QN (g p) s
    g (QP RK_ l s)    = QP AK_ l s
    starting_id       = 0
    (endingId, ss')   = mapAccumL (mapAccumL (\n -> (n+1,) . toBare n)) starting_id ss
    anns              = concatMap (FO.foldMap snd) ss

--------------------------------------------------------------------------------
extractFact :: [PSpec t r] -> [Maybe (FactQ RK r)]
--------------------------------------------------------------------------------
extractFact fs = map go fs
  where
    exprt ExportedSpec = True
    exprt _  = False
    loc | any exprt fs = Exported
        | otherwise = Local
    go (FunctionDeclarationSpec (_,t))     = Just $ SigAnn loc t
    go (VariableDeclarationSpec (_, a, t)) = Just $ VarAnn loc a t
    go (FunctionExpressionSpec t)          = Just $ SigAnn loc t
    go (InterfaceSpec t)                   = Just $ InterfaceAnn t
    go (ClassSpec t)                       = Just $ ClassAnn loc t
    go (ConstructorSpec t)                 = Just $ CtorAnn t
    go (CastSpec _ t)                      = Just $ UserCast t
    go (FieldSpec f)                       = Just $ FieldAnn f
    go (MethodSpec m)                      = Just $ MethAnn m
    go _                                   = Nothing


--------------------------------------------------------------------------------
parseAnnotations :: [Statement (SrcSpan, [RawSpec])] -> Either FError [Statement (SrcSpan, [Spec])]
--------------------------------------------------------------------------------
parseAnnotations ss
  | [] <- errs = Right ss'
  | otherwise  = Left (F.Unsafe errs)
  where
    ses     = strans f g mempty <$> ss
    errs    = concatMap (FO.concatMap fst) ses
    ss'     = fmap snd <$> ses

    f :: PContext -> (SrcSpan, [RawSpec]) -> ([Error], (SrcSpan, [Spec]))
    f ctx (ss, specs) = mapSnd (ss,) $ L.mapAccumL (\errs spec -> parse ctx errs spec) [] specs

    g :: PContext -> ([Error], (SrcSpan, [Spec])) -> PContext
    g ctx = (ctx `mappend`) . mconcat . map h . snd . snd

    h :: Spec -> PContext
    h (FunctionDeclarationSpec t) = pCtxFromList $ fst $ bkAll $ snd t
    h (FunctionExpressionSpec t)  = pCtxFromList $ fst $ bkAll t
    h (InterfaceSpec t)           = pCtxFromList $ b_args $ sigTRef $ typeSig t
    h (ClassSpec t)               = pCtxFromList $ b_args $ sigTRef t
    h (MethodSpec t)              = mconcat $ pCtxFromList . fst . bkAll . snd <$> m_ty t
    h _                           = mempty


--------------------------------------------------------------------------------
parse :: PContext -> [Error] -> RawSpec -> ([Error], Spec)
--------------------------------------------------------------------------------
parse ctx errs rawspec = failLeft $ runParser (parseRawSpec ctx rawspec) 0 f (getSpecString rawspec)
  where
    failLeft (Left err) = (fromError err : errs, ErrorSpec)
    failLeft (Right r)  = (errs, r)
    f = sourceName $ sp_start ss
    fromError err = mkErr ss   $ showErr err
                              ++ "\n\nWhile parsing: "
                              ++ show (getSpecString rawspec)
    ss = srcPos rawspec
    --
    -- Slight change from this one:
    -- http://hackage.haskell.org/package/parsec-3.1.5/docs/src/Text-Parsec-Error.html#ParseError
    --
    showErr = showErrorMessages "or" "unknown parse error" "expecting"
                "unexpected" "end of input" . errorMessages

parseRawSpec ctx s
  = do  a     <- parseSpec ctx s
        it    <- getInput
        case it of
          ""  -> return a
          _   -> unexpected $ "trailing input: " ++ it

