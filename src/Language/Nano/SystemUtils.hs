-- | Code for Saving and Rendering Annotations

{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Language.Nano.SystemUtils (

    -- * Type for Annotation Map
    UAnnInfo
  , UAnnSol (..)

    -- * Adding new Annotations
  , addAnnot

    -- * Rendering Annotations
  , annotByteString
  , annotVimString
  ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy             as B
import qualified Data.HashMap.Strict              as M
import qualified Data.List                        as L
import           Data.List.Split
import           Data.Monoid
import qualified Data.Text                        as T
import qualified Data.Vector                      as V
import           GHC.Exts                         (groupWith, sortWith)

import           Control.Applicative              ((<$>))
import           Language.Fixpoint.Types.Errors
import           Language.Fixpoint.Utils.Files          ()
import           Language.Fixpoint.Misc           (inserts)
import           Language.Fixpoint.Types.Names          (symbolString)
import qualified Language.Fixpoint.Types          as F
import           Language.Nano.Locations
import           Language.Nano.Syntax.PrettyPrint
import           Language.Nano.Typecheck.Parse
import           Language.Nano.Types              ()
import           Text.Parsec.Pos
import           Text.PrettyPrint.HughesPJ        (nest, punctuate, render, text, vcat, ($+$), (<+>))

------------------------------------------------------------------------------
-- | Type Definitions For Annotations ----------------------------------------
------------------------------------------------------------------------------

data AnnBind t        = AnnBind { ann_bind :: F.Symbol,
                                ann_type   :: t }

{-@ type NonNull a = {v: [a] | 0 < (len v)} @-}
type    NonEmpty a  = [a]
newtype UAnnInfo a  = AI (M.HashMap SrcSpan (NonEmpty (AnnBind a)))
data    UAnnSol  a  = NoAnn
                    | SomeAnn (UAnnInfo a) (UAnnInfo a -> UAnnInfo a)


instance Functor AnnBind where
  fmap f (AnnBind x t) = AnnBind x (f t)

instance Functor UAnnInfo where
  fmap f (AI m) = AI (fmap (fmap (fmap f)) m)

instance Monoid (UAnnInfo a) where
  mempty                  = AI M.empty
  mappend (AI m1) (AI m2) = AI (M.unionWith mappend m1 m2)

------------------------------------------------------------------------
-- | PP Instance -------------------------------------------------------
------------------------------------------------------------------------

instance PP a => PP (UAnnInfo a) where
  pp (AI m)  = vcatLn [pp sp $+$ nest 4 (vcatLn $ map ppB bs) | (sp, bs) <- M.toList m]
    where
      ppB a  = pp (ann_bind a) <+> text "::" <+> pp (ann_type a)
      vcatLn = vcat . punctuate nl
      nl     = text "\n"

------------------------------------------------------------------------------
-- | Adding New Annotations --------------------------------------------------
------------------------------------------------------------------------------

addAnnot :: (F.Symbolic x) => SrcSpan -> x -> a -> UAnnInfo a -> UAnnInfo a
addAnnot l x t (AI m) = AI (inserts l (AnnBind (F.symbol x) t) m)

------------------------------------------------------------------------------
-- | Dumping Annotations To Disk ---------------------------------------------
------------------------------------------------------------------------------

-- writeAnnotations :: (PP t) => FilePath -> F.FixResult SrcSpan -> UAnnInfo t -> IO ()
-- writeAnnotations f res a = B.writeFile f annJson
--   where
--     annJson              = encode $ mkAnnMap res a

annotByteString       :: PP t => F.FixResult Error -> UAnnInfo t -> B.ByteString
annotByteString res a = encode $ mkAnnMap res a

annotVimString        :: PP a => F.FixResult Error -> UAnnInfo a -> String
annotVimString res a  = toVim $ mkAnnMap res a

------------------------------------------------------------------------------
-- | Type Representing Inferred Annotations ----------------------------------
------------------------------------------------------------------------------

data AnnMap  = AnnMap {
    status :: String
  , types  :: M.HashMap SrcSpan (String, String)     -- ^ SrcSpan -> (Var, Type)
  , errors :: AnnErrors                              -- ^ List of errors
  }

mkAnnMap res ann = AnnMap (mkAnnMapStatus res) (mkAnnMapTyp ann) (mkAnnMapErr res)

mkAnnMapStatus (F.Crash _ _)      = "error"
mkAnnMapStatus (F.Safe)           = "safe"
mkAnnMapStatus (F.Unsafe _)       = "unsafe"
--mkAnnMapStatus (F.UnknownError _) = "crash"

mkAnnMapErr (F.Unsafe ls)         = eInfo "Liquid Error: "   <$> ls
mkAnnMapErr (F.Crash ls msg)      = eInfo ("Crash: " ++ msg) <$> ls
mkAnnMapErr _                     = []

eInfo msg err                     = (srcPos $ errLoc err', errMsg err')
  where
    err'                          = catMessage err msg

mkAnnMapTyp (AI m)
  = M.map (\a -> (symbolString $ ann_bind a, render $ pp (ann_type a)))
  $ M.fromList
  $ map (head . sortWith (srcSpanEndCol . fst))
  $ groupWith (lineCol . fst)
  $ M.toList
  $ M.map head
  $ M.filterWithKey validAnnot
  $ m

validAnnot sp _ = sourceSpanSrcSpan sp /= dummySpan && oneLine sp
oneLine l       = srcSpanStartLine l == srcSpanEndLine l
lineCol sp      = (srcSpanStartLine sp, srcSpanStartCol sp)

------------------------------------------------------------------------------
-- | JSON: Annotation Data Types ---------------------------------------------
------------------------------------------------------------------------------

data Assoc k a = Asc (M.HashMap k a)
type AnnTypes  = Assoc Int (Assoc Int Annot1)
type AnnErrors = [(SrcSpan, String)]
data Annot1    = A1 String String Int Int
                    --  { ident :: String
                    --  , ann   :: String
                    --  , row   :: Int
                    --  , col   :: Int
                    --  }

------------------------------------------------------------------------
-- | JSON Instances
------------------------------------------------------------------------

instance ToJSON Annot1 where
  toJSON (A1 i a r c) = object [ "ident" .= i
                               , "ann"   .= a
                               , "row"   .= r
                               , "col"   .= c
                               ]

-- TEMPORARILY

-- instance ToJSON SourcePos where
--   toJSON z           = object [("line" .= toJSON l), ("column" .= toJSON c)]
--     where
--       l              = sourceLine   z
--       c              = sourceColumn z
--
-- instance ToJSON SrcSpan where
--   toJSON = object . sourceSpanBinds

instance ToJSON AnnErrors where
  toJSON = Array . V.fromList . fmap (\(sp, str) -> object $ ("message" .= str) : sourceSpanBinds sp)


sourceSpanBinds (SS l l') = [ ("start" .= toJSON l), ("stop"  .= toJSON l') ]

instance (Show k, ToJSON a) => ToJSON (Assoc k a) where
  toJSON (Asc kas) = object [ (tshow k) .= (toJSON a) | (k, a) <- M.toList kas ]
    where
      tshow        = T.pack . show

instance ToJSON AnnMap where
  toJSON a = object [ ("status" .= (toJSON $ status   a))
                    , ("types"  .= (toJSON $ annTypes a))
                    , ("errors" .= (toJSON $ errors   a))
                    ]

annTypes            :: AnnMap -> AnnTypes
annTypes a          = grp [(l, c, ann1 l c x s) | (l, c, x, s) <- binders]
  where
    ann1 l c x s    = A1 x s l c
    grp             = L.foldl' (\m (r,c,x) -> ins r c x m) (Asc M.empty)
    binders         = map binder $ M.toList $ types a

binder (sp, (x, s)) = (srcSpanStartLine sp, srcSpanStartCol sp, killSSA x, s)
  where
    killSSA         = head . splitOn "_SSA_"

binder1 (sp, (x,s)) = (srcSpanStartLine sp, srcSpanStartCol sp,
                        srcSpanEndLine sp, srcSpanEndCol sp, killSSA x, s)
  where
    killSSA         = head . splitOn "_SSA_"

ins r c x (Asc m)   = Asc (M.insert r (Asc (M.insert c x rm)) m)
  where
    Asc rm          = M.lookupDefault (Asc M.empty) r m



------------------------------------------------------------------------
-- | VIM interface
------------------------------------------------------------------------


toVim :: AnnMap -> String
toVim (AnnMap _ ty _) = mconcat $ L.intersperse "\n" $ ss <$> lines
  where
    lines = map binder1 $ M.toList ty
    ss (l1,c1, l2, c2, _,s) = show l1 ++ ":"
                           ++ show c1 ++ "-"
                           ++ show l2 ++ ":"
                           ++ show c2 ++ "::"
                           ++ show s
