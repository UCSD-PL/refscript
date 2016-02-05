{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}

module Language.Rsc.Constraints (

  -- * Refinement Types
    RefType

  -- * Constraint Environments
  , CGEnvR(..), CGEnv

  -- * Constraint Information
  , Cinfo (..), ci, ciToError

  -- * Constraints
  , SubC (..) , WfC (..), FixSubC   , FixWfC


  ) where

import           Control.DeepSeq
import qualified Language.Fixpoint.Types        as F
import           Language.Fixpoint.Types.Errors
import           Language.Rsc.Environment
import           Language.Rsc.Liquid.Types
import           Language.Rsc.Locations
import           Language.Rsc.Pretty
import           Text.PrettyPrint.HughesPJ


----------------------------------------------------------------------------
-- | Constraint Information
----------------------------------------------------------------------------

data Cinfo = Ci { ci_info    :: !Error
                , ci_srcspan :: !SrcSpan
                , ci_history :: [SubC]
                }

ciToError (Ci e _ h) = err (errLoc e) (errMsg e ++ "\n" ++ hs)
  where
    hs = show (ppAll h)
    ppAll [] = empty
    ppAll (x:xs) = ppFull x $+$ ppTail xs

    ppTail xs = vcat (map ppShort xs)

    ppFull s = -- @(Sub g _ l r) =
      -- text "In the environment" $+$ nest 2 (pp (cge_names g)) $+$
      ppShort s
    ppShort (Sub _ _ l r) =
      text "Which was produced from the subtyping between:" $+$
      nest 2 (nest 2 (pp l) $+$ text "and" $+$ nest 2 (pp r))

ci   :: (IsLocated a) => Error -> a -> Cinfo
ci e l = Ci e (srcPos l) []

instance NFData Cinfo where
  rnf (Ci _ s _) = seq s ()

instance PP Cinfo where
  pp (Ci e l _) = text "CInfo:" <+> pp l <+> (parens $ pp e)

instance IsLocated Cinfo where
  srcPos = ci_srcspan

instance F.Fixpoint Cinfo where
  toFix = pp . ci_srcspan

----------------------------------------------------------------------------
-- | Constraints
----------------------------------------------------------------------------

-- | Subtyping Constraints

data SubC
  = Sub { senv  :: !CGEnv         -- ^ Environment
        , sinfo :: !Cinfo         -- ^ Source Information
        , slhs  :: !RefType       -- ^ Subtyping LHS
        , srhs  :: !RefType       -- ^ Subtyping RHS   ... senv |- slhs <: srhs
        }

-- | Wellformedness Constraints

data WfC
  = W { wenv  :: !CGEnv      -- ^ Scope/Environment
      , winfo :: !Cinfo      -- ^ Source Information
      , wtyp  :: !RefType    -- ^ Type to be Well-formed ... wenv |- wtyp
      }

instance PP SubC where
  pp (Sub γ i t t') = pp (cge_names γ)
                    $+$ pp (cge_guards γ)
                    $+$ text "|-"    <+> (pp t $+$ text "<:" $+$ pp t')
                    $+$ text "from:" <+> pp i

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

