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

module Language.Rsc.Pretty.Symbols where

import           Control.Applicative                ((<$>))
import           Data.Graph.Inductive.Graph         hiding (empty)
import           Data.Graph.Inductive.PatriciaTree
import qualified Data.HashMap.Strict                as HM
import qualified Data.Map.Strict                    as M
import           Language.Fixpoint.Misc             (intersperse)
import qualified Language.Fixpoint.Types            as F
import           Language.Rsc.AST
import           Language.Rsc.ClassHierarchy
import           Language.Rsc.Pretty.Common
import           Language.Rsc.Pretty.Syntax
import           Language.Rsc.Pretty.Types
import           Language.Rsc.Program
import           Language.Rsc.Symbols
import           Language.Rsc.Typecheck.Environment
import           Language.Rsc.Typecheck.Subst
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
import           Text.PrettyPrint.HughesPJ


instance PPR r => PP (SymInfo r) where
  pp (SI l a i t) = pp l <+> pp a <+> pp i <+> pp t


