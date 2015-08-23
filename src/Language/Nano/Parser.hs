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

import           Control.Applicative               ((<$>))
import           Control.Monad
import           Control.Monad.Trans               (MonadIO, liftIO)
import           Data.Aeson                        (eitherDecode)
import qualified Data.ByteString.Lazy.Char8        as B
import           Data.Either                       (partitionEithers)
import qualified Data.Foldable                     as FO
import qualified Data.List                         as L
import           Data.Maybe                        (catMaybes)
import           Data.Monoid                       (mconcat)
import           Data.Traversable                  (mapAccumL)
import           Data.Tuple
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Types           as F
import           Language.Nano.Annots              hiding (err)
import           Language.Nano.AST
import           Language.Nano.Core.Env
import           Language.Nano.Errors
import           Language.Nano.Liquid.Alias
import           Language.Nano.Locations           hiding (val)
import           Language.Nano.Misc                ((&))
import           Language.Nano.Names
import           Language.Nano.Parser.Annotations
import           Language.Nano.Parser.Declarations ()
import           Language.Nano.Program
import           Language.Nano.Transformations
import           Language.Nano.Traversals
import           Language.Nano.Visitor
import           Prelude                           hiding (mapM)
import           Text.Parsec                       hiding (State, parse)
import           Text.Parsec.Error                 (errorMessages, showErrorMessages)

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
    ([],ps) -> return $ mkRsc <$> parseAnnots (concat ps)
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
mkRsc :: [Statement (SrcSpan, [Spec])] -> RefScript
---------------------------------------------------------------------------------
mkRsc ss = ss
         & mkRelRsc
         & convertTVars
         & expandAliases
         & replaceAbsolute
         & replaceDotRef
         & fixFunBinders

---------------------------------------------------------------------------------
mkRelRsc :: [Statement (SrcSpan, [Spec])] -> RelRefScript
---------------------------------------------------------------------------------
mkRelRsc ss = Rsc {
        code          = Src (checkTopStmt <$> ss')
      , consts        = envFromList [ mapSnd (ntrans f g) t | Meas t <- anns ]
      , tAlias        = envFromList [ t | TAlias t <- anns ]
      , pAlias        = envFromList [ t | PAlias t <- anns ]
      , pQuals        = scrapeQuals [ t | Qual   t <- anns ] ss'
      , pOptions      =             [ t | Option t <- anns ]
      , invts         = [Loc (srcPos l) (ntrans f g t) | Invt l t <- anns ]
      , maxId         = endingId
    }
  where
    toBare           :: Int -> (SrcSpan, [Spec]) -> AnnRel F.Reft
    toBare n (l,αs)   = FA n l $ catMaybes $ extractFact <$> αs
    f (QN p s)        = QN (g p) s
    g (QP RK_ l s)    = QP AK_ l s
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

