{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}

module Language.Nano.Liquid.Constraint (

  -- * Refinement Types
    RefType

  -- * Constraint Environments
  , CGEnvR(..), CGEnv

  -- * Constraint Information
  , Cinfo (..), ci

  -- * Constraints
  , SubC (..) , WfC (..), FixSubC   , FixWfC


  ) where

import           Language.Fixpoint.Errors
import qualified Language.Fixpoint.Types          as F
import           Language.Nano.Liquid.Environment
import           Language.Nano.Liquid.Types
import           Language.Nano.Locations
import           Language.Nano.Pretty
import           Text.PrettyPrint.HughesPJ


----------------------------------------------------------------------------
-- | Constraint Information
----------------------------------------------------------------------------

data Cinfo = Ci { ci_info    :: !Error
                , ci_srcspan :: !SrcSpan
                } deriving (Eq, Ord, Show)

ci   :: (IsLocated a) => Error -> a -> Cinfo
ci e = Ci e . srcPos

instance PP Cinfo where
  pp (Ci e l)   = text "CInfo:" <+> pp l <+> (parens $ pp e)

instance IsLocated Cinfo where
  srcPos = ci_srcspan

instance F.Fixpoint Cinfo where
  toFix = pp . ci_srcspan

----------------------------------------------------------------------------
-- | Constraints
----------------------------------------------------------------------------

-- | Subtyping Constraints

data SubC
  = Sub { senv  :: !CGEnv      -- ^ Environment
        , sinfo :: !Cinfo      -- ^ Source Information
        , slhs  :: !RefType    -- ^ Subtyping LHS
        , srhs  :: !RefType    -- ^ Subtyping RHS   ... senv |- slhs <: srhs
        }

-- | Wellformedness Constraints

data WfC
  = W { wenv  :: !CGEnv      -- ^ Scope/Environment
      , winfo :: !Cinfo      -- ^ Source Information
      , wtyp  :: !RefType    -- ^ Type to be Well-formed ... wenv |- wtyp
      }

instance PP SubC where
  pp (Sub γ i t t') = pp (cge_names γ) $+$ pp (cge_guards γ)
                        $+$ (text "|-" <+> (pp t $+$ text "<:" $+$ pp t'))
                        $+$ (text "from:" <+> pp i)

instance PP WfC where
  pp (W γ t i)      = pp (cge_names γ)
                        $+$ (text "|-" <+> pp t)
                        $+$ (text "from:" <+> pp i)

instance IsLocated SubC where
  srcPos = srcPos . sinfo

instance IsLocated WfC where
  srcPos = srcPos . winfo

-- | Aliases for Fixpoint Constraints

type FixSubC = F.SubC Cinfo
type FixWfC  = F.WfC  Cinfo

