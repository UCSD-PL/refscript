{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}

-- | Utilities share by both Typecheck and Liquid modules

module Language.Rsc.TypeUtilities (

    mkDotRefFunTy
  , setPropTy

  ) where

import           Control.Applicative
import           Data.Default
import qualified Data.HashMap.Strict          as HM
import qualified Data.List                    as L
import           Data.Maybe                   (catMaybes, fromMaybe, maybeToList)
import           Data.Monoid                  (mconcat)
import qualified Data.Traversable             as T
import qualified Language.Fixpoint.Bitvector  as BV
import           Language.Fixpoint.Errors
import qualified Language.Fixpoint.Types      as F
import           Language.Rsc.AST
import           Language.Rsc.Environment
import           Language.Rsc.Errors
import           Language.Rsc.Liquid.Types
import           Language.Rsc.Locations
import           Language.Rsc.Lookup
import           Language.Rsc.Names
import           Language.Rsc.Pretty
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
import           Text.PrettyPrint.HughesPJ



-- | Dot ref
--
---------------------------------------------------------------------------------
mkDotRefFunTy :: (PP r, EnvLike r t, PP f, IsLocated l, F.Symbolic f, Monad m
                 , ExprReftable F.Expr r, ExprReftable Int r, F.Reftable r)
              => l -> t r -> f -> RType r -> RType r -> m (RType r)
---------------------------------------------------------------------------------
mkDotRefFunTy l g f tObj tField
  -- | Case array.length:
  | isArrayType tObj, F.symbol "length" == F.symbol f
  = globalLengthType g
  -- | Case immutable field: (x: tObj) => { bField | v = x_f }
  | Just m <- getFieldMutability (envCHA g) tObj (F.symbol f)
  , isImm m
  = return $ mkFun ([], [B x tObj], (fmap F.top tField) `eSingleton` mkOffset x f)
  -- | TODO: Case: TEnum
  -- | Rest (x: tTobj) => tField[this/x]
  | otherwise
  = return $ mkFun ([], [B x tObj], substThis x tField)
  where
    x = F.symbol "x"
mkDotRefFunTy l _ f tObj _
  = die $ errorMissingFld (srcPos l) f tObj


-- | setProp<A, M extends Mutable>(o: { f[M]: A }, x: A) => A
--
--------------------------------------------------------------------------------------------
setPropTy :: (F.Reftable r, F.Symbolic f) => f -> RType r
--------------------------------------------------------------------------------------------
setPropTy f = mkAll [bvt, bvm] ft
  where
    ft      = TFun [b1, b2] t fTop
 -- b1      = B (F.symbol "o") $ TObj tImm (tmFromFieldList [(f, FI Opt m t)]) fTop
    b1      = B (F.symbol "o") $ TObj tImm (tmFromFieldList [(f, FI Req m t)]) fTop
    b2      = B (F.symbol "x") $ t
    m       = toTTV bvm :: F.Reftable r => RType r
    t       = toTTV bvt
    bvt     = BTV (F.symbol "A") def Nothing
    bvm     = BTV (F.symbol "M") def (Just tMut) :: F.Reftable r => BTVar r
    toTTV   :: F.Reftable r => BTVar r -> RType r
    toTTV   = (`TVar` fTop) . btvToTV

