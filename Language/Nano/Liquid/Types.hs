-- | Global type definitions for Refinement Type Checker

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module Language.Nano.Liquid.Types (

  -- * Annotated Program
    Nano (..)

  -- * Refinement Types
  , RType (..)
  , RefType

  -- * Regular Types
  , Type (..)
  , TVar (..)
  , TCon (..)

  ) where 

import           Language.Nano.Types
import qualified Language.Fixpoint.Types as F
import           Text.Parsec

-- | Type Variables
newtype TVar = TV (Located F.Symbol)

-- | Constructed Type Bodies
data TBody r 
  = TD { td_con  :: !TCon
       , td_args :: ![TVar]
       , td_body :: !(RType r)
       , td_pos  :: !SourcePos
       } deriving (Functor)

-- | Type Constructors
data TCon 
  = TInt                   
  | TBool                
  | TVoid              
  | TDef  F.Symbol
    deriving (Eq, Ord)

-- | (Raw) Refined Types 
data RType r  
  = TApp TCon [RType r] r
  | TVar TVar r 
  | TFun [RType r] (RType r)
  | TAll TVar (RType r)
    deriving (Functor)

-- | Standard Types
type Type    = RType ()

-- | (Real) Refined Types
type RefType = RType F.Reft 

---------------------------------------------------------------------------------

type Nano        = [Fun SourcePos] 

data Fun a       = Fun { floc  :: a             -- ^ sourceloc 
                       , fname :: Id a          -- ^ name
                       , fargs :: [Id a]        -- ^ parameters
                       , fbody :: [Statement a] -- ^ body
                       , ftyp  :: !Type         -- ^ precondition
                       }

-- functions fns = fns

mkNano :: [Statement SourcePos] -> Maybe Nano
mkNano =  sequence . map mkFun 


