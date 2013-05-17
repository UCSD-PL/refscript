-- | Module pertaining to Refinement Type descriptions and conversions
--   Likely mergeable with @Language.Nano.Typecheck.Types@

module Language.Nano.Liquid.Types ( 
  
  -- * Refinement Types and Environments
    RefType 
  , REnv

  -- * Constraint Environments
  , CGEnv (..)
  , emptyCGEnv

  -- * Constraint Information
  , Cinfo (..)

  -- * Constraints
  , SubC (..) , WfC (..)
  , FixSubC   , FixWfC

  -- * Conversions
  , RefTypable (..)
  -- , ofType                      -- ofType :: Type    -> RefType
  -- , toType                      -- toType :: RType a -> Type

  -- * Manipulating RefType
  , rTypeReft
  , rTypeSort
  , rTypeSortedReft
  , strengthen 

  -- * Predicates On RefType 
  , isBaseRType

  -- * Monadic map (TODO: Applicative/Traversable)
  , mapReftM
  ) 
 
  , where

import qualified Language.Fixpoint.Types as F
import           Language.Nano.Typecheck.Types
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.PrettyPrint

  
-------------------------------------------------------------------------------------
----- | Refinement Types and Environments -------------------------------------------
-------------------------------------------------------------------------------------

type RefType = RType F.SortedReft
type REnv    = Env RefType


-------------------------------------------------------------------------------------
-- | Constraint Generation Environment  ---------------------------------------------
-------------------------------------------------------------------------------------

data CGEnv   
  = CGE { renv   :: !REnv         -- ^ bindings in scope 
        , fenv   :: F.IBindEnv    -- ^ fixpoint bindings
        , guards :: ![F.Pred]     -- ^ branch target conditions  
        , retTy  :: Maybe RefType -- ^ return type of function 
        }

emptyCGEnv = CGE emptyREnv F.emptyIBindEnv [] Nothing

instance PP CGEnv where
  pp (CGE re _ gs rt) = vcat [pp re, pp gs, pp rt] 

----------------------------------------------------------------------------
-- | Constraint Information ------------------------------------------------
----------------------------------------------------------------------------

newtype Cinfo = Ci SourcePos deriving (Eq, Ord, Show) 

emptyCinfo    = Ci ()

instance PP Cinfo where
  pp (Ci l)   = text "CInfo:" <+> pp l 

-------------------------------------------------------------------------------------
-- | Constraints --------------------------------------------------------------------
-------------------------------------------------------------------------------------

-- | Subtyping Constraints

data SubC     
  = Sub { senv :: !CGEnv      -- ^ Environment
        , slhs :: !RefType    -- ^ Subtyping LHS
        , srhs :: !RefType    -- ^ Subtyping RHS   ... senv |- slhs <: srhs
        }

-- | Wellformedness Constraints

data WfC 
  = W { wenv :: !CGEnv      -- ^ Scope/Environment
      , wtyp :: !RefType    -- ^ Type to be Well-formed ... wenv |- wtyp
      }

instance PP SubC where
  pp (Sub γ t t') = pp (renv γ)   $+$ pp (guards γ) 
                       $+$ ((text "|-") <+> (pp t $+$ text "<:" $+$ pp t'))

instance PP WfC where
  pp (W γ t)      = F.toFix (renv γ) 
                    $+$ (text "|-" <+> pp t) 

-- | Aliases for Fixpoint Constraints

type FixSubC = F.SubC Cinfo
type FixWfC  = F.WfC  Cinfo

------------------------------------------------------------------------
-- | Embedding Values as RefTypes --------------------------------------
------------------------------------------------------------------------

class RefTypable a where
  rType :: a -> RefType 

instance RefTypable Type where
  rType = ofType

instance RefTypable RefType where
  rType = ofType . toType           -- removes all refinements


------------------------------------------------------------------------------
-- | Converting RType to Fixpoint --------------------------------------------
------------------------------------------------------------------------------

rTypeSortedReft   ::  (Reftable r) => RType r -> SortedReft
rTypeSortedReft t = RR (rTypeSort t) (rTypeReft t)

rTypeReft :: (Reftable r) => RType r -> Reft
rTypeReft = fromMaybe top . fmap toReft . stripRTypeBase 


rTypeValueVar :: (Reftable r) => RType r -> Symbol
rTypeValueVar t = vv where Reft (vv,_) =  rTypeReft t 

------------------------------------------------------------------------------------------
rTypeSort :: (Reftable r) => RType r -> Sort
------------------------------------------------------------------------------------------

rTypeSort = error "TOBD"
-- rTypeSort (TypeInteger _)   = FInt
-- rTypeSort (TypeInteger _)   = FInt
-- rTypeSort (TypePointer t _) = FApp ptrFTycon [typeSort t]
-- ptrFTycon = stringFTycon "ptr"

------------------------------------------------------------------------------------------
stripRTypeBase :: RType r -> r 
------------------------------------------------------------------------------------------

stripRTypeBase = error "TOBD"
-- stripRTypeBase (RTypeInteger _ r) = Just r
-- stripRTypeBase t                  = errorstar msg
--   where msg                       = "stripRTypeBase: cannot handle" ++ showFix t

 

