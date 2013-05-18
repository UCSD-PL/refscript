-- | Module pertaining to Refinement Type descriptions and conversions
--   Likely mergeable with @Language.Nano.Typecheck.Types@

module Language.Nano.Liquid.Types ( 
  
  -- * Refinement Types and Environments
    RefType 
  , REnv
  , NanoRefType

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

  -- * Manipulating RefType
  , rTypeReft
  , rTypeSort
  , rTypeSortedReft
  , strengthen 

  -- * Predicates On RefType 
  , isBaseRType

  -- * Monadic map (TODO: Applicative/Traversable)
  , mapReftM
  ) where

import           Control.Applicative ((<$>), (<*>))
import           Data.Maybe             (fromMaybe, isJust)
import           Data.Monoid            hiding ((<>))            
import           Data.Ord               (comparing) 
import qualified Data.List               as L
import qualified Data.HashMap.Strict     as M
import           Data.Generics.Aliases
import           Data.Generics.Schemes

import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations
import           Language.ECMAScript3.PrettyPrint

import           Language.Nano.Errors
import           Language.Nano.Types
import           Language.Nano.Typecheck.Types
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Misc
import           Language.Fixpoint.PrettyPrint
import           Text.PrettyPrint.HughesPJ
import           Text.Parsec.Pos    (initialPos)
import           Control.Applicative 

  
-------------------------------------------------------------------------------------
----- | Refinement Types and Environments -------------------------------------------
-------------------------------------------------------------------------------------

type RefType     = RType F.Reft
type REnv        = Env RefType
type NanoRefType = Nano AnnType RefType 

-------------------------------------------------------------------------------------
-- | Constraint Generation Environment  ---------------------------------------------
-------------------------------------------------------------------------------------

data CGEnv   
  = CGE { renv   :: !REnv         -- ^ bindings in scope 
        , fenv   :: F.IBindEnv    -- ^ fixpoint bindings
        , guards :: ![F.Pred]     -- ^ branch target conditions  
        , retTy  :: Maybe RefType -- ^ return type of function 
        }

emptyCGEnv = CGE envEmpty F.emptyIBindEnv [] Nothing

instance PP CGEnv where
  pp (CGE re _ gs rt) = vcat [pp re, pp gs, pp rt] 

----------------------------------------------------------------------------
-- | Constraint Information ------------------------------------------------
----------------------------------------------------------------------------

newtype Cinfo = Ci SourcePos deriving (Eq, Ord, Show) 

emptyCinfo    = Ci $ initialPos ""

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
  pp (W γ t)      = pp (renv γ) 
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

rTypeSortedReft   ::  (F.Reftable r) => RType r -> F.SortedReft
rTypeSortedReft t = F.RR (rTypeSort t) (rTypeReft t)

rTypeReft         :: (F.Reftable r) => RType r -> F.Reft
rTypeReft         = fromMaybe F.top . fmap F.toReft . stripRTypeBase 

rTypeValueVar     :: (F.Reftable r) => RType r -> F.Symbol
rTypeValueVar t   = vv where F.Reft (vv,_) =  rTypeReft t 

------------------------------------------------------------------------------------------
rTypeSort :: (F.Reftable r) => RType r -> F.Sort
------------------------------------------------------------------------------------------

rTypeSort (TApp TInt [] _) = F.FInt
rTypeSort (TVar α _)       = F.FObj $ F.symbol α 
rTypeSort t@(TAll _ _)     = rTypeSortForAll t 
rTypeSort (TFun ts t)      = F.FFunc 0 $ rTypeSort <$> ts ++ [t]
rTypeSort (TApp c ts _)    = rTypeSortApp c ts 


rTypeSortApp TInt [] = F.FInt
rTypeSortApp c ts    = F.FApp (tconFTycon c) (rTypeSort <$> ts) 

tconFTycon TInt      = F.intFTyCon
tconFTycon TBool     = F.stringFTycon "boolean"
tconFTycon TVoid     = F.stringFTycon "void"
tconFTycon (TDef s)  = F.stringFTycon $ F.symbolString s


rTypeSortForAll t    = genSort n θ $ rTypeSort tbody
  where 
    (αs, tbody)      = bkAll t
    n                = length αs
    θ                = M.fromList $ zip (F.symbol <$> αs) (F.FVar <$> [0..])
    
genSort n θ (F.FFunc _ t)  = F.FFunc n (F.sortSubst θ <$> t)
genSort n θ t              = F.FFunc n [F.sortSubst θ t]

------------------------------------------------------------------------------------------
stripRTypeBase :: RType r -> Maybe r 
------------------------------------------------------------------------------------------
stripRTypeBase (TApp _ _ r) = Just r
stripRTypeBase (TVar _ r)   = Just r
stripRTypeBase _            = Nothing
 
------------------------------------------------------------------------------------------
strengthen                   :: F.Reftable r => RType r -> r -> RType r
------------------------------------------------------------------------------------------
strengthen (TApp c ts r) r'  = TApp c ts $ r `F.meet` r' 
strengthen (TVar α r)    r'  = TVar α    $ r `F.meet` r'
strengthen t _               = t 

------------------------------------------------------------------------------------------
-- | Substitutions -----------------------------------------------------------------------
------------------------------------------------------------------------------------------

instance (F.Reftable r, F.Subable r) => F.Subable (RType r) where
  syms        = foldReft (\r acc -> F.syms r ++ acc) [] 
  substa      = fmap . F.substa 
  substf f    = emapReft (F.substf . F.substfExcept f) [] 
  subst su    = emapReft (F.subst  . F.substExcept su) []
  subst1 t su = emapReft (\xs r -> F.subst1Except xs r su) [] t

------------------------------------------------------------------------------------------
-- | Traversals over @RType@ -------------------------------------------------------------
------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------
emapReft  :: (F.Reftable a) => ([F.Symbol] -> a -> b) -> [F.Symbol] -> RType a -> RType b
------------------------------------------------------------------------------------------
emapReft f γ (TVar α r)    = TVar α (f γ r)
emapReft f γ (TApp c ts r) = TApp c (emapReft f γ <$> ts) (f γ r)
emapReft f γ (TAll α t)    = TAll α (emapReft f γ t)
emapReft f γ (TFun ts t)   = TFun (emapReft f γ' <$> ts) (emapReft f γ' t) where γ' = (rTypeValueVar <$> ts) ++ γ 

------------------------------------------------------------------------------------------
mapReftM :: (Monad m, Applicative m) => (a -> m b) -> RType a -> m (RType b)
------------------------------------------------------------------------------------------
mapReftM f (TVar α r)      = TVar α <$> f r
mapReftM f (TApp c ts r)   = TApp c <$> mapM (mapReftM f) ts <*> f r
mapReftM f (TFun ts t)     = TFun   <$> mapM (mapReftM f) ts <*> mapReftM f t
mapReftM f (TAll α t)      = TAll α <$> mapReftM f t

------------------------------------------------------------------------------------------
-- | fold over @RType@ -------------------------------------------------------------------
------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------
foldReft  :: (F.Reftable r) => (r -> a -> a) -> a -> RType r -> a
------------------------------------------------------------------------------------------
foldReft  f = efoldReft (\_ -> ()) (\_ -> f) F.emptySEnv 

------------------------------------------------------------------------------------------
efoldReft :: (F.Reftable r) => (RType r -> b) -> (F.SEnv b -> r -> a -> a) -> F.SEnv b -> a -> RType r -> a
------------------------------------------------------------------------------------------
efoldReft _ f γ z (TVar _ r)       = f γ r z
efoldReft g f γ z t@(TApp _ ts r)  = f γ r $ efoldRefts g f (efoldExt g t γ) z ts
efoldReft g f γ z (TFun ts t)      = efoldReft g f γ' (efoldRefts g f γ' z ts) t  where γ' = foldr (efoldExt g) γ ts
efoldReft g f γ z (TAll α t)       = efoldReft g f γ z t
efoldRefts g f γ z ts              = L.foldl' (efoldReft g f γ) z ts
efoldExt g t γ                     = F.insertSEnv (rTypeValueVar t) (g t) γ

------------------------------------------------------------------------------------------
isBaseRType :: RType r -> Bool
------------------------------------------------------------------------------------------
isBaseRType (TApp c [] _) = True
isBaseRType (TVar _ _)    = True
isBaseRType _             = False




