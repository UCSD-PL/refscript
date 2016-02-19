{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}

-- | Module pertaining to Refinement Type descriptions and conversions
--   Likely mergeable with @Language.Rsc.Typecheck.Types@

module Language.Rsc.Liquid.Refinements (

  -- * Manipulating RefType
    rTypeReft
  , rTypeSort
  , rTypeSortedReft
  , rTypeValueVar

  ) where

import qualified Data.HashMap.Strict             as HM
import           Data.Maybe                      (mapMaybe)
import qualified Data.Text                       as T
import qualified Language.Fixpoint.Smt.Bitvector as BV
import qualified Language.Fixpoint.Types         as F
import           Language.Fixpoint.Types.Names   (symbolText)
import           Language.Rsc.AST
import           Language.Rsc.Names
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types


--------------------------------------------------------------------------------
rTypeSortedReft   :: F.Reftable r => RTypeQ q r -> F.SortedReft
rTypeReft         :: F.Reftable r => RTypeQ q r -> F.Reft
rTypeValueVar     :: F.Reftable r => RTypeQ q r -> F.Symbol
--------------------------------------------------------------------------------
rTypeSortedReft t = F.RR (rTypeSort t) (rTypeReft t)
rTypeReft         = maybe fTop F.toReft . stripRTypeBase
rTypeValueVar t   = vv where F.Reft (vv,_) = rTypeReft t

--------------------------------------------------------------------------------
stripRTypeBase :: RTypeQ q r -> Maybe r
--------------------------------------------------------------------------------
stripRTypeBase (TPrim _ r)  = Just r
stripRTypeBase (TRef _ r)   = Just r
stripRTypeBase (TVar _ r)   = Just r
stripRTypeBase (TFun _ _ r) = Just r
stripRTypeBase (TObj _ _ r) = Just r
stripRTypeBase (TOr _ r)    = Just r
stripRTypeBase _            = Nothing



-- Avoid incluging mutability of `TRef` - it crashes FP
--------------------------------------------------------------------------------
rTypeSort :: F.Reftable r => RTypeQ q r -> F.Sort
--------------------------------------------------------------------------------
rTypeSort (TVar α _)              = F.FObj $ F.symbol α
rTypeSort (TAll v t)              = rTypeSortForAll $ TAll v t
rTypeSort (TFun xts t _)          = F.mkFFunc 0 $ rTypeSort <$> (b_type <$> xts) ++ [t]
rTypeSort (TPrim c _)             = rTypeSortPrim c
rTypeSort (TOr _ _)               = F.fAppTC (rawStringFTycon unionSym ) []
rTypeSort (TAnd _)                = F.fAppTC (rawStringFTycon intersSym) []
rTypeSort (TRef (Gen n (_:ts)) _) = F.fAppTC (rawSymbolFTycon (F.symbol n)) (rTypeSort <$> ts)
rTypeSort (TRef (Gen n []    ) _) = F.fAppTC (rawSymbolFTycon (F.symbol n)) []
rTypeSort  TObj{}                 = F.fAppTC (rawStringFTycon objectSym) []
rTypeSort  TClass{}               = F.fAppTC (rawStringFTycon classSym ) []
rTypeSort  TMod{}                 = F.fAppTC (rawStringFTycon moduleSym) []
rTypeSort _                       = error "BUG: Unsupported in rTypeSort"


rTypeSortForAll t        = genSort n θ $ rTypeSort tbody
  where
    (αs, tbody)          = bkAll t
    n                    = length αs
    θ                    = HM.fromList $ zip (F.symbol <$> αs) (F.FVar <$> [0..])



rTypeSortPrim TBV32      = BV.mkSort BV.S32
rTypeSortPrim TNumber    = F.intSort
rTypeSortPrim TReal      = F.realSort
rTypeSortPrim TString    = F.strSort
rTypeSortPrim TBoolean   = F.fAppTC (rawStringFTycon boolSym ) []
rTypeSortPrim TVoid      = F.fAppTC (rawStringFTycon voidSym ) []
rTypeSortPrim TTop       = F.fAppTC (rawStringFTycon topSym  ) []
rTypeSortPrim TBot       = F.fAppTC (rawStringFTycon botSym  ) []
rTypeSortPrim TAny       = F.fAppTC (rawStringFTycon anySym  ) []
rTypeSortPrim TNull      = F.fAppTC (rawStringFTycon nullSym ) []
rTypeSortPrim TUndefined = F.fAppTC (rawStringFTycon undefSym) []
rTypeSortPrim TFPBool    = F.boolSort
rTypeSortPrim c          = error $ "impossible: rTypeSortPrim " ++ show c

genSort n θ t = case F.bkFFunc t of
                 Just (_, ts) -> F.mkFFunc n (F.sortSubst θ <$> ts)
                 Nothing      -> F.mkFFunc n [F.sortSubst θ t]

rawStringFTycon = F.symbolFTycon
                . F.locAt "RSC.Types.rawStringFTycon"
                . F.symbol


rawSymbolFTycon :: F.Symbol -> F.FTycon
rawSymbolFTycon = F.symbolFTycon
                . F.locAt "RSC.Types.rawStringFTycon"

