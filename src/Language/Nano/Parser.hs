{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}


module Language.Nano.Parser
    (
      parseNanoFromFiles
    , parseScriptFromJSON
    , parseIdFromJSON
    ) where

import           Control.Applicative               ((*>), (<$>), (<*), (<*>))
import           Control.Exception                 (throw)
import           Control.Monad
import           Control.Monad.Trans               (MonadIO, liftIO)
import           Data.Aeson                        (eitherDecode)
import           Data.Aeson.Types                  hiding (Error, Parser, parse)
import qualified Data.Aeson.Types                  as AI
import qualified Data.ByteString.Lazy.Char8        as B
import           Data.Char                         (isLower)
import           Data.Default
import           Data.Either                       (partitionEithers)
import qualified Data.Foldable                     as FO
import           Data.Generics                     hiding (Generic)
import qualified Data.HashMap.Strict               as HM
import qualified Data.HashSet                      as HS
import           Data.Interned.Internal.Text
import qualified Data.IntMap.Strict                as I
import qualified Data.List                         as L
import           Data.Maybe                        (catMaybes, fromMaybe, listToMaybe, maybeToList)
import           Data.Monoid                       (mappend, mconcat, mempty)
import           Data.Text                         (Text)
import qualified Data.Text                         as DT
import           Data.Traversable                  (mapAccumL)
import           Data.Tuple
import           Data.Vector                       ((!))
import           GHC.Generics
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Names
import           Language.Fixpoint.Parse
import qualified Language.Fixpoint.Types           as F
import qualified Language.Fixpoint.Visitor         as FV
import           Language.Nano.Annots              hiding (err)
import           Language.Nano.AST
import qualified Language.Nano.ClassHierarchy      as CHA
import           Language.Nano.Core.Env
import           Language.Nano.Errors
import           Language.Nano.Liquid.Alias
import           Language.Nano.Liquid.Qualifiers
import           Language.Nano.Liquid.Types
import           Language.Nano.Locations           hiding (val)
import           Language.Nano.Misc                (fst4, (&), (<###>), (<##>))
import           Language.Nano.Names
import           Language.Nano.Parser.Annotations
import           Language.Nano.Parser.Common
import           Language.Nano.Parser.Declarations
import           Language.Nano.Parser.Types
import           Language.Nano.Pretty
import           Language.Nano.Program
import           Language.Nano.Transformations
import           Language.Nano.Traversals
import           Language.Nano.Typecheck.Types
import           Language.Nano.Types               hiding (Exported)
import           Language.Nano.Visitor
import           Prelude                           hiding (mapM)
import           Text.Parsec                       hiding (State, parse)
import           Text.Parsec.Error                 (errorMessages, showErrorMessages)
import           Text.Parsec.Language              (emptyDef)
import           Text.Parsec.Pos                   (SourcePos, newPos)
import           Text.Parsec.Token                 (identLetter, identStart)
import qualified Text.Parsec.Token                 as T
import           Text.PrettyPrint.HughesPJ         (text, ($$), (<+>))
import qualified Text.PrettyPrint.HughesPJ         as P

-- import           Debug.Trace                             ( trace, traceShow)


type FError = F.FixResult Error

--------------------------------------------------------------------------------------
-- | Parse File and Type Signatures
--------------------------------------------------------------------------------------

-- | Parse the contents of a FilePath list into a program structure with relative
-- qualified names.
--------------------------------------------------------------------------------------
parseNanoFromFiles :: [FilePath] -> IO (Either FError RefScript)
--------------------------------------------------------------------------------------
parseNanoFromFiles fs =
  partitionEithers <$> mapM parseScriptFromJSON fs >>= \case
  -- TODO
    -- ([],ps) -> return $ either Left mkRsc $ parseAnnots $ concat ps
    (es,_ ) -> return $ Left $ mconcat es

--------------------------------------------------------------------------------------
getJSON :: MonadIO m => FilePath -> m B.ByteString
--------------------------------------------------------------------------------------
getJSON = liftIO . B.readFile

--------------------------------------------------------------------------------------
parseScriptFromJSON :: FilePath -> IO (Either (F.FixResult a) [Statement (SrcSpan, [RawSpec])])
--------------------------------------------------------------------------------------
parseScriptFromJSON filename = decodeOrDie <$> getJSON filename
  where
    decodeOrDie s =
      case eitherDecode s :: Either String [Statement (SrcSpan, [RawSpec])] of
        Left msg -> Left  $ F.Crash [] $ "JSON decode error:\n" ++ msg
        Right p  -> Right $ p


--------------------------------------------------------------------------------------
parseIdFromJSON :: FilePath -> IO (Either (F.FixResult a) [Id (SrcSpan, [RawSpec])])
--------------------------------------------------------------------------------------
parseIdFromJSON filename = decodeOrDie <$> getJSON filename
  where
    decodeOrDie s =
      case eitherDecode s :: Either String [Id (SrcSpan, [RawSpec])] of
        Left msg -> Left  $ F.Crash [] $ "JSON decode error:\n" ++ msg
        Right p  -> Right $ p


---------------------------------------------------------------------------------
mkRsc :: [Statement (SrcSpan, [Spec])] -> Either FError (RefScript, CHA.ClassHierarchy F.Reft)
---------------------------------------------------------------------------------
mkRsc ss = do
  let rsc               = ss
                        & mkRelRsc
                        & convertTVars
                        & expandAliases
                        & replaceAbsolute
                        & replaceDotRef
  let quals             = scrapeQuals rsc
  modules              <- accumModules rsc
  let (enums, rsc')     = fixEnums modules rsc
  let (modules', rsc'') = fixFunBinders modules rsc'
  let cha               = CHA.fromModuleDef modules'
  return                $ (rsc'', cha)

---------------------------------------------------------------------------------
mkRelRsc :: [Statement (SrcSpan, [Spec])] -> RelRefScript
---------------------------------------------------------------------------------
mkRelRsc ss = Rsc {
        code          = Src (checkTopStmt <$> ss')
      , consts        = envFromList [ mapSnd (ntrans f g) t | Meas t <- anns ]
      , tAlias        = envFromList [ t | TAlias t <- anns ]
      , pAlias        = envFromList [ t | PAlias t <- anns ]
      , pQuals        =             [ t | Qual   t <- anns ]
      , pOptions      =             [ t | Option t <- anns ]
      , invts         = [Loc (srcPos l) (ntrans f g t) | Invt l t <- anns ]
      , maxId         = endingId
    }
  where
    toBare           :: Int -> (SrcSpan, [Spec]) -> AnnRel F.Reft
    toBare n (l,αs)   = FA n l $ catMaybes $ extractFact <$> αs
    f (QN p s)        = QN (g p) s
    g (QP RK_ l ss)   = QP AK_ l ss
    starting_id       = 0
    (endingId, ss')   = mapAccumL (mapAccumL (\n -> (n+1,) . toBare n)) starting_id ss
    anns              = concatMap (FO.foldMap snd) ss

---------------------------------------------------------------------------------
extractFact :: PSpec t r -> Maybe (FactQ RK r)
---------------------------------------------------------------------------------
extractFact = go
  where
    go (Bind    (_,a,t))  = Just $ VarAnn a t
    go (AmbBind (_,t)  )  = Just $ AmbVarAnn t

    go (Constr  t)        = Just $ ConsAnn t
    go (Field (s, f))     = Just $ FieldAnn s f
    go (Method  (s, m))   = Just $ MethAnn s m

    go (Class t)          = Just $ ClassAnn t
    go (Iface t)          = Just $ InterfaceAnn t
    go (CastSp _ t)       = Just $ UserCast t
    go (Exported  _)      = Just $ ExportedElt
    go (RdOnly _)         = Just $ ReadOnlyVar
    go (AnFunc t)         = Just $ FuncAnn t
    go _                  = Nothing


type PState = Integer

--------------------------------------------------------------------------------------
parseAnnots :: [Statement (SrcSpan, [RawSpec])] -> Either FError [Statement (SrcSpan, [Spec])]
--------------------------------------------------------------------------------------
parseAnnots ss =
  case mapAccumL (mapAccumL f) (0,[]) ss of
    ((_,[]),b) -> Right $ b
    ((_,es),_) -> Left  $ F.Unsafe es
  where
    f st (ss,sp) = mapSnd ((ss),) $ L.mapAccumL (parse ss) st sp

--------------------------------------------------------------------------------------
parse :: SrcSpan -> (PState, [Error]) -> RawSpec -> ((PState, [Error]), Spec)
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
    --
    -- http://hackage.haskell.org/package/parsec-3.1.5/docs/src/Text-Parsec-Error.html#ParseError
    --
    showErr = showErrorMessages "or" "unknown parse error" "expecting"
                "unexpected" "end of input" . errorMessages
    fromError err = mkErr ss   $ showErr err
                              ++ "\n\nWhile parsing: "
                              ++ show (getSpecString c)
    ss = srcPos c
    f = sourceName $ sp_start ss

