{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveFoldable            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE IncoherentInstances       #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}

module Language.Nano.Pretty.ClassHierarchy (

) where

import           Control.Applicative               ((<$>))
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree
import qualified Data.Map.Strict                   as M
import           Language.Fixpoint.Misc            (intersperse)
import qualified Language.Fixpoint.Types           as F
import           Language.Nano.AST
import           Language.Nano.ClassHierarchy
import           Language.Nano.Pretty.Common
import           Language.Nano.Pretty.Syntax
import           Language.Nano.Program
import           Language.Nano.Typecheck.Types
import           Language.Nano.Types
import           Text.PrettyPrint.HughesPJ

instance (F.Reftable r, PP r) => PP (ClassHierarchy r) where
  pp (ClassHierarchy g _)   =  text "***********************************************"
                           $+$ text "Class Hierarchy"
                           $+$ text "***********************************************"
                           $+$ vcat (ppEdge <$> edges g)
    where
      ppEdge (a,b)          =  ppNode a <+> text "->" <+> ppNode b
      ppNode                =  pp . lab' . context g

