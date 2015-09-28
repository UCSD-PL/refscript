{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}

-- | Pretty-printing JavaScript.
module Language.Rsc.Pretty.Common (
    ppArgs
  , ppshow
  , pprint
  , PP (..)
  , PPR
  , diePP
  , inComments
  ) where

import           Control.Applicative           ((<$>))
import           Control.Exception.Base
import           Data.Data
import           Data.Function                 (on)
import qualified Data.IntMap                   as I
import           Data.List                     (isPrefixOf, sortBy)
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Types       as F
-- import           Language.Rsc.Pretty
import           Language.Fixpoint.PrettyPrint
import qualified Language.Fixpoint.Types       as F
import           Language.Rsc.Core.Env
import           Language.Rsc.Locations
import           Language.Rsc.Names
import           Prelude                       hiding (maybe)
import           Text.PrettyPrint.HughesPJ


class PP a where
  pp :: a -> Doc

type PPR r = (PP r, F.Reftable r)

instance PP F.Symbol where
  pp = pprint

instance PP a => PP (Either String a) where
  pp (Left s)  = text $ "ERROR!" ++ s
  pp (Right x) = pp x

instance PP String where
  pp = text

instance PP Int where
  pp = int

instance PP Integer where
  pp = pp . show

---------------------------------------------------------------------
ppArgs :: PP a => (Doc -> t) -> Doc -> [a] -> t
---------------------------------------------------------------------
ppArgs p sp l = p $ intersperse sp $ map pp l

instance PP a => PP [a] where
  pp = ppArgs brackets comma

instance (PP a, PP b) => PP (a,b) where
  pp (a,b) = pp a <+> text ":" <+>  pp b

instance (PP a, PP b, PP c) => PP (a,b,c) where
  pp (a,b,c) = pp a <+> text ":" <+>  pp b <+> text ":" <+> pp c

instance (PP a, PP b, PP c, PP d) => PP (a,b,c,d) where
  pp (a,b,c,d) = pp a <+> text ":" <+>  pp b <+> text ":" <+> pp c <+> text ":" <+> pp d

instance (PP a, PP b, PP c, PP d, PP e) => PP (a,b,c,d,e) where
  pp (a,b,c,d,e) = pp a <+> text ":" <+>  pp b <+> text ":" <+> pp c <+> text ":" <+> pp d <+> text ":" <+> pp e

instance PP t => PP (I.IntMap t) where
  pp m = vcat (pp <$> I.toList m)

instance (PP t) => PP (F.SEnv t) where
  pp m = -- pp (take 80 (repeat '-')) $+$
         vcat (
           map (\(x,t) -> pp x $$ nest 20 (dcolon <> text " " <> pp t)) $
           sortBy (on compare (show . fst)) $
           filter (not . isPrefixOf "builtin_" . ppshow . fst) $   -- TOGGLE
           F.toListSEnv m
         )
      -- $+$ pp (take 80 (repeat '-'))


---------------------------------------------------------------------
ppshow :: (PP a) => a -> String
---------------------------------------------------------------------
ppshow = render . pp


---------------------------------------------------------------------
-- | Names
---------------------------------------------------------------------

instance PP (QN l) where
  pp (QN (QP _ _ []) s) = pp s
  pp (QN p s) = pp p <> dot <> pp s

instance PP (QP l) where
  pp (QP _ _ []) = pp "[Top-Level]"
  pp (QP _ _ ms) = hcat $ punctuate dot $ map pp ms

instance (Ord a, F.Fixpoint a) => PP (F.FixResult a) where
  pp = F.resultDoc

instance PP F.Pred where
  pp = pprint

instance PP a => PP (Located a) where
  pp x = pp (val x) -- <+> text "at:" <+> pp (loc x)


---------------------------------------------------------------------
-- | SrcSpans
---------------------------------------------------------------------

instance PP SrcSpan where
  pp    = pprint

---------------------------------------------------------------------
-- | SrcSpans
---------------------------------------------------------------------
data EString a = EString a deriving (Typeable)

instance (PP a, Typeable a) => Exception (EString a)

instance PP a => Show (EString a) where
  show (EString e) = ppshow e

diePP :: (PP e, Typeable e) => e -> a
diePP = throw . EString


inComments p
  =   text "/*"
  $+$ p
  $+$ text "*/"

