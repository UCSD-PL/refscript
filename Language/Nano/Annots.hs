-- | Code for Saving and Rendering Annotations

{-# LANGUAGE OverloadedStrings          #-}

module Language.Nano.Annots (
   
    -- * Type for Annotation Map
    AnnInfo 
    
    -- * Adding new Annotations
  , addAnnot

    -- * Rendering Annotations
  , annotate 
  ) where

import qualified Data.List              as L
import qualified Data.ByteString.Lazy   as B
import qualified Data.Vector            as V
import qualified Data.Text              as T
import qualified Data.HashMap.Strict    as M
import           Data.Monoid
import           Data.Aeson               
import           GHC.Exts                           (groupWith, sortWith)

import           Language.Fixpoint.Files
import           Language.Fixpoint.Misc             (inserts)
import qualified Language.Fixpoint.Types    as F
-- import           Language.ECMAScript3.PrettyPrint   ()
import           Language.ECMAScript3.Parser        (SourceSpan (..))
import           Text.Parsec.Pos                   
import           Language.Nano.Types

{-@ type NonNull a = {v: [a] | 0 < (len v)} @-}
type NonEmpty a    = [a] 
newtype AnnInfo a  = AI (M.HashMap SourceSpan (NonEmpty a))

instance Functor AnnInfo where 
  fmap f (AI m) = AI (fmap (fmap f) m)

instance Monoid (AnnInfo a) where 
  mempty                  = AI M.empty
  mappend (AI m1) (AI m2) = AI (M.unionWith mappend m1 m2)

------------------------------------------------------------------------------
-- | Adding New Annotations --------------------------------------------------
------------------------------------------------------------------------------

addAnnot            :: SourceSpan -> a -> AnnInfo a -> AnnInfo a
addAnnot l x (AI m) = AI (inserts l x m)

------------------------------------------------------------------------------
-- | Dumping Annotations To Disk ---------------------------------------------
------------------------------------------------------------------------------

-- annotate :: ( ) => FilePath -> FixResult SourceSpan -> AnnInfo a -> IO ()
annotate src res a = B.writeFile jsonFile annJson 
  where 
    jsonFile       = extFileName Json src
    annJson        = encode $ mkAnnMap res a

------------------------------------------------------------------------------
-- | Type Representing Inferred Annotations ----------------------------------
------------------------------------------------------------------------------

data AnnMap  = Ann { 
    types  :: M.HashMap SourceSpan (String, String)  -- ^ SourceSpan -> (Var, Type)
  , errors :: [SourceSpan]                           -- ^ List of errors
  } 

mkAnnMap ::  F.FixResult SourceSpan -> AnnInfo (String, String) -> AnnMap 
mkAnnMap res ann = Ann (mkAnnMapTyp ann) (mkAnnMapErr res)

mkAnnMapErr (F.Unsafe ls) = ls
mkAnnMapErr _             = []

mkAnnMapTyp (AI m) 
  = M.fromList
  $ map (head . sortWith (srcSpanEndCol . fst)) 
  $ groupWith (lineCol . fst) 
  $ M.toList
  $ M.map head
  $ M.filterWithKey validAnnot
  $ m
   
validAnnot sp _ = sp /= dummySpan && oneLine sp
oneLine l       = srcSpanStartLine l == srcSpanEndLine l
lineCol sp      = (srcSpanStartLine sp, srcSpanStartCol sp) 

------------------------------------------------------------------------------
-- | JSON: Annotation Data Types ---------------------------------------------
------------------------------------------------------------------------------

data Assoc k a = Asc (M.HashMap k a)
type AnnTypes  = Assoc Int (Assoc Int Annot1)
type AnnErrors = [SourceSpan]
data Annot1    = A1  { ident :: String
                     , ann   :: String
                     , row   :: Int
                     , col   :: Int  
                     }

------------------------------------------------------------------------
-- | JSON Instances ----------------------------------------------------
------------------------------------------------------------------------

instance ToJSON Annot1 where 
  toJSON (A1 i a r c) = object [ "ident" .= i
                               , "ann"   .= a
                               , "row"   .= r
                               , "col"   .= c
                               ]

instance ToJSON SourcePos where
  toJSON z           = object [("line" .= toJSON l), ("column" .= toJSON c)]
    where 
      l              = sourceLine   z
      c              = sourceColumn z 
 
instance ToJSON SourceSpan where
  toJSON (Span l l') = object [ ("start" .= toJSON l), ("stop"  .= toJSON l') ]

instance ToJSON AnnErrors where 
  toJSON             = Array . V.fromList . fmap toJSON 

instance (Show k, ToJSON a) => ToJSON (Assoc k a) where
  toJSON (Asc kas) = object [ (tshow k) .= (toJSON a) | (k, a) <- M.toList kas ]
    where
      tshow        = T.pack . show 

instance ToJSON AnnMap where 
  toJSON a = object [ ("types"  .= (toJSON $ annTypes a))
                    , ("errors" .= (toJSON $ errors   a))
                    ]

annTypes            :: AnnMap -> AnnTypes 
annTypes a          = grp [(l, c, ann1 l c x s) | (l, c, x, s) <- binders]
  where 
    ann1 l c x s    = A1 x s l c 
    grp             = L.foldl' (\m (r,c,x) -> ins r c x m) (Asc M.empty)
    binders         = map binder $ M.toList $ types a

binder (sp, (x, s)) = (srcSpanStartLine sp, srcSpanStartCol sp, x, s)

ins r c x (Asc m)   = Asc (M.insert r (Asc (M.insert c x rm)) m)
  where 
    Asc rm          = M.lookupDefault (Asc M.empty) r m

-----------------------------------------------------------------------------

