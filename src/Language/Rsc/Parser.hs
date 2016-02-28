{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}


module Language.Rsc.Parser (parseRscFromFiles, parseScriptFromJSON, parseIdFromJSON) where

import           Control.Arrow                    (second)
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
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Parse          (Parser)
import qualified Language.Fixpoint.Types          as F
import           Language.Fixpoint.Types.Errors
import           Language.Rsc.Annotations
import           Language.Rsc.AST
import qualified Language.Rsc.Core.EitherIO       as EIO
import           Language.Rsc.Core.Env
import           Language.Rsc.Errors
import           Language.Rsc.Liquid.Alias
import           Language.Rsc.Locations
import           Language.Rsc.Names
import           Language.Rsc.Parser.Annotations
import           Language.Rsc.Parser.Declarations ()
import           Language.Rsc.Parser.Types
import           Language.Rsc.Pretty.Common
import           Language.Rsc.Program
import           Language.Rsc.Transformations
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
import           Prelude                          hiding (mapM)
import           Text.Parsec                      hiding (State, parse)
import           Text.Parsec.Error                (errorMessages, showErrorMessages)
import           Text.PrettyPrint.HughesPJ        (nest, ($+$))

--------------------------------------------------------------------------------
-- | Parse File and Type Signatures
--------------------------------------------------------------------------------

-- | Parse the contents of a FilePath list into a program structure with relative
-- qualified names.
--------------------------------------------------------------------------------
parseRscFromFiles :: [FilePath] -> EIO.EitherIO FError RefScript
--------------------------------------------------------------------------------
parseRscFromFiles fs = EIO.EitherIO $ do
    startPhase Loud "Parse"
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

-- TODO: allow more error to lift from other parts of program creation
--------------------------------------------------------------------------------
mkRsc :: [Statement (SrcSpan, [Spec])] -> Either FError RefScript
--------------------------------------------------------------------------------
mkRsc ss = either (Left . F.Unsafe) Right $ do
    p1  <- pure (mkRelRsc ss)
    p2  <- pure (expandAliases p1)     -- Expand type and predicate aliases
    p3  <- replaceAbsolute p2
    p4  <- pure (replaceDotRef p3)
    p5  <- pure (fixFunBinders p4)
    return p5


-- XXX: Type aliases might appear twice as part of a TS annotation and an RSC
--      annotation. But that's fine, since they're gonna be the same.
--
--------------------------------------------------------------------------------
mkRelRsc :: [Statement (SrcSpan, [Spec])] -> RelRefScript
--------------------------------------------------------------------------------
mkRelRsc ss = Rsc {
        code          = Src (checkTopStmt <$> ss')
      , consts        = envFromList [ second (ntransPure f g) t | MeasureSpec t <- anns ]
      , tAlias        = envFromList [ t | TypeAliasSpec      t <- anns ]
      , pAlias        = envFromList [ t | PredicateAliasSpec t <- anns ]
      , pQuals        =             [ t | QualifierSpec      t <- anns ]
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
        | otherwise    = Local
    go (FunctionDeclarationSpec (x,t))     = Just $ SigAnn (F.symbol x) loc t
    go (VariableDeclarationSpec (x, a, t)) = Just $ VarAnn (F.symbol x) loc a t
    go (FunctionExpressionSpec t)          = Just $ SigAnn (F.symbol "anonymous") loc t
    go (InterfaceSpec t)                   = Just $ InterfaceAnn t
    go (ClassSpec t)                       = Just $ ClassAnn loc t
    go (ClassInvSpec p)                    = Just $ ClassInvAnn p
    go (ModuleSpec t)                      = Just $ ModuleAnn loc t
    go (ConstructorSpec t)                 = Just $ CtorAnn t
    go (CastSpec _ t)                      = Just $ UserCast t
    go (FieldSpec f)                       = Just $ MemberAnn f
    go (MethodSpec m)                      = Just $ MemberAnn m
    go _                                   = Nothing


--------------------------------------------------------------------------------
parseAnnotations
  :: [Statement (SrcSpan, [RawSpec])] -> Either FError [Statement (SrcSpan, [Spec])]
--------------------------------------------------------------------------------
parseAnnotations ss
  | [] <- errs = Right ss'
  | otherwise  = Left (F.Unsafe errs)
  where
    ses     = strans f g mempty <$> ss
    errs    = concatMap (FO.concatMap fst) ses
    ss'     = fmap snd <$> ses

    f :: PContext -> (SrcSpan, [RawSpec]) -> ([Error], (SrcSpan, [Spec]))
    f ctx (ss, specs) = second (ss,) $ L.mapAccumL (parseSpec ctx) [] specs

    g :: PContext -> ([Error], (SrcSpan, [Spec])) -> PContext
    g ctx = (ctx `mappend`) . mconcat . map h . snd . snd

    -- Update the tvar context
    h :: Spec -> PContext
    h (FunctionDeclarationSpec t) = pCtxFromList (fst (bkAll (snd t)))
    h (FunctionExpressionSpec t)  = pCtxFromList (fst (bkAll t))
    h (InterfaceSpec t)           = pCtxFromList (b_args (sigTRef (typeSig t)))
    h (ClassSpec t)               = classCtx (sigTRef t)
    h (MethodSpec t)              = mconcat (pCtxFromList . fst . bkAll . snd <$> m_ty t)
    h _                           = mempty

    classCtx (BGen _ (b:bs)) = PContext (map btvToTV (b:bs)) (Just (btVar b))
    classCtx (BGen _ _     ) = PContext [] Nothing


--------------------------------------------------------------------------------
parseSpec :: PContext -> [Error] -> RawSpec -> ([Error], Spec)
--------------------------------------------------------------------------------
parseSpec ctx es rawspec =
    failLeft $ runParser (parseRawSpecWithError ctx rawspec) 0 f (getSpecString rawspec)

  where

    failLeft (Left err) = (fromError err : es, ErrorSpec)
    failLeft (Right r)  = (es, r)

    f = sourceName $ sp_start ss

    fromError err = mkErr ss $ pp (showErr err)    $+$
                               pp "While parsing:" $+$
                               nest 2 (pp (getSpecString rawspec))
    ss = srcPos rawspec
    -- Slight change from this one:
    -- http://hackage.haskell.org/package/parsec-3.1.5/docs/src/Text-Parsec-Error.html#ParseError
    showErr :: ParseError -> String
    showErr = showErrorMessages "or" "unknown parse error" "expecting"
                "unexpected" "end of input" . errorMessages

--------------------------------------------------------------------------------
parseRawSpecWithError :: PContext -> RawSpec -> Parser Spec
--------------------------------------------------------------------------------
parseRawSpecWithError ctx s
  = do  a     <- parseRawSpec ctx s
        it    <- getInput
        case it of
          ""  -> return a
          _   -> unexpected $ "trailing input: " ++ it

