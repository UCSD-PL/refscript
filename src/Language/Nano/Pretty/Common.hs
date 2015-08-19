{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}

-- | Pretty-printing JavaScript.
module Language.Nano.Pretty.Common ( ppArgs, ppshow, pprint, PP (..), PPR ) where

import           Control.Applicative           ((<$>))
import qualified Data.IntMap                   as I
import           Language.Fixpoint.Misc
import           Language.Fixpoint.PrettyPrint
import qualified Language.Fixpoint.Types       as F
import           Language.Nano.Core.Env
import           Language.Nano.Locations
import           Language.Nano.Names
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

instance PP t => PP (F.SEnv t) where
  pp m = vcat $ pp <$> F.toListSEnv m


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
  pp (QP _ _ []) = pp "<global>"
  pp (QP _ _ ms) = hcat $ punctuate dot $ map pp ms

instance (Ord a, F.Fixpoint a) => PP (F.FixResult a) where
  pp = F.resultDoc

instance PP F.Pred where
  pp = pprint

instance PP a => PP (Located a) where
  pp x = pp (val x) <+> text "at:" <+> pp (loc x)


---------------------------------------------------------------------
-- | Env
---------------------------------------------------------------------

instance PP t => PP (Env t) where
  pp = vcat . (ppBind <$>) . F.toListSEnv . fmap val

instance PP t => PP (QEnv t) where
  pp m = vcat $ (ppBind <$>) $ qenvToList m

ppBind (x, t) = pp x <+> dcolon <+> pp t


---------------------------------------------------------------------
-- | SrcSpans
---------------------------------------------------------------------

instance PP SrcSpan where
  pp    = pprint

