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

module Language.Rsc.Pretty.ClassHierarchy (

) where
--
-- import           Control.Applicative               ((<$>))
-- import           Data.Graph.Inductive.Graph
-- import           Data.Graph.Inductive.PatriciaTree
-- import qualified Data.Map.Strict                   as M
-- import           Language.Fixpoint.Misc            (intersperse)
-- import qualified Language.Fixpoint.Types           as F
-- import           Language.Rsc.AST
-- import           Language.Rsc.ClassHierarchy
-- import           Language.Rsc.Pretty.Common
-- import           Language.Rsc.Pretty.Syntax
-- import           Language.Rsc.Program
-- import           Language.Rsc.Typecheck.Types
-- import           Language.Rsc.Types
-- import           Text.PrettyPrint.HughesPJ
--
-- instance (F.Reftable r, PP r) => PP (ClassHierarchy r) where
--   pp (CHA g _ _)   =  text ""
--                   $+$ pp (take 80 (repeat '='))
--                   $+$ text "Class Hierarchy"
--                   $+$ pp (take 80 (repeat '-'))
--                   $+$ vcat (ppEdge <$> edges g)
--                   $+$ pp (take 80 (repeat '='))
--     where
--       ppEdge (a,b) =  ppNode a <+> text "->" <+> ppNode b
--       ppNode       =  pp . lab' . context g
--
