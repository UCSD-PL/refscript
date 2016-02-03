{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE IncoherentInstances       #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}

module Language.Rsc.Pretty.ClassHierarchy where

import           Data.Graph.Inductive.Graph  hiding (empty)
import qualified Language.Fixpoint.Types     as F
import           Language.Rsc.ClassHierarchy
import           Language.Rsc.Core.Env
import           Language.Rsc.Pretty.Common
import           Language.Rsc.Pretty.Module  ()
import           Text.PrettyPrint.HughesPJ

instance (F.Reftable r, PP r) => PP (ClassHierarchy r) where
  pp (CHA g _ ms)  =  text ""
                  $+$ pp (take 80 (repeat '='))
                  $+$ text "Class Hierarchy"
                  $+$ pp (replicate 80 '-')
                  $+$ vcat (ppEdge <$> edges g)
                  $+$ pp (replicate 80 '=')
                  $+$ vcat (pp . snd <$> qenvToList ms)
    where
      ppEdge (a,b) = ppNode a <+> text "->" <+> ppNode b
      ppNode       = pp . lab' . context g

