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
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}

module Language.Rsc.Pretty.Module where

import           Control.Applicative                ((<$>))
import           Data.Graph.Inductive.Graph         hiding (empty)
import           Data.Graph.Inductive.PatriciaTree
import qualified Data.HashMap.Strict                as HM
import qualified Data.Map.Strict                    as M
import           Language.Fixpoint.Misc             (intersperse)
import qualified Language.Fixpoint.Types            as F
import           Language.Rsc.AST
import           Language.Rsc.ClassHierarchy
import           Language.Rsc.Module
import           Language.Rsc.Pretty.Common
import           Language.Rsc.Pretty.Symbols
import           Language.Rsc.Pretty.Syntax
import           Language.Rsc.Pretty.Types
import           Language.Rsc.Program
import           Language.Rsc.Symbols
import           Language.Rsc.Typecheck.Environment
import           Language.Rsc.Typecheck.Subst
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
import           Text.PrettyPrint.HughesPJ

instance (PP r, F.Reftable r) => PP (ModuleDef r) where
  pp (ModuleDef vars tys enums path) =
          pp (take 80 (repeat '-'))
      $+$ text "module" <+> pp path
      $+$ pp (take 80 (repeat '-'))
      $+$ text "Variables"    $+$ nest 4 (pp vars)
      $+$ text "Types"        $+$ nest 4 (pp tys)
      $+$ text "Enums"        $+$ nest 4 (pp enums)

