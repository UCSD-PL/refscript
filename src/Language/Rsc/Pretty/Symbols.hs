{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}

module Language.Rsc.Pretty.Symbols where

import           Language.Rsc.Pretty.Common
import           Language.Rsc.Pretty.Types  ()
import           Language.Rsc.Symbols
import           Text.PrettyPrint.HughesPJ


instance PPR r => PP (SymInfo r) where
  -- pp (SI _ _ a _ t) = brackets (pp a) <+> pp t
  pp (SI x _ a _ t) = pp x <+> brackets (pp a) <> colon <+> pp t

instance PPR r => PP (SymList r) where
  pp = vcat . map pp . s_list
