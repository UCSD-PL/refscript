{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE IncoherentInstances       #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}

module Language.Rsc.Pretty.Symbols where

import           Control.Applicative               ((<$>))
import           Data.Graph.Inductive.Graph        hiding (empty)
import           Data.Graph.Inductive.PatriciaTree
import qualified Data.HashMap.Strict               as HM
import qualified Data.Map.Strict                   as M
import           Language.Fixpoint.Misc            (intersperse)
import qualified Language.Fixpoint.Types           as F
import           Language.Rsc.AST
import           Language.Rsc.Pretty.Common
import           Language.Rsc.Pretty.Syntax
import           Language.Rsc.Pretty.Types
import           Language.Rsc.Program
import           Language.Rsc.Symbols
import           Language.Rsc.Typecheck.Subst
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
import           Text.PrettyPrint.HughesPJ


instance PPR r => PP (SymInfo r) where
  pp (SI n _ a _ t) = parens (pp a) <+> pp n <> colon <+> pp t

instance PPR r => PP (SymList r) where
  pp = vcat . map pp . s_list
