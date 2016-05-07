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

import qualified Language.Fixpoint.Types     as F
import           Language.Rsc.Module
import           Language.Rsc.Pretty.Common
import           Language.Rsc.Pretty.Symbols ()
import           Text.PrettyPrint.HughesPJ

instance (PP r, F.Reftable r) => PP (ModuleDef r) where
  pp (ModuleDef vars tys enums path) =
          pp (take 80 (repeat '-'))
      $+$ text "module" <+> pp path
      $+$ pp (take 80 (repeat '-'))
      $+$ text "Variables"    $+$ nest 4 (pp vars)
      $+$ text "Types"        $+$ nest 4 (pp tys)
      $+$ text "Enums"        $+$ nest 4 (pp enums)

