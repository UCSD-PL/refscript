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
  , idTy, idTys
  , castTy

  ) where

import qualified Language.Fixpoint.Types      as F
import           Language.Rsc.Environment
import           Language.Rsc.Liquid.Types
import           Language.Rsc.Pretty
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types


-- | Dot ref
--
---------------------------------------------------------------------------------
mkDotRefFunTy ::
  (CheckingEnvironment r t, PP r, ExprReftable F.Expr r, F.Symbolic f, F.Reftable r, Monad m) =>
  t r -> f -> RType r -> Mutability r -> FieldAsgn -> RType r -> m (RType r)
---------------------------------------------------------------------------------
mkDotRefFunTy g f tRcvr mRcvr a tf
  -- Array
  | isArrayType tRcvr
  , F.symbol "length" == F.symbol f
  = globalLengthType g

  | isIM mRcvr
  , a /= Assignable
  = return $ mkFun ([], [B x tRcvr], fmap F.top tf `eSingleton` mkOffset x f)

  | a == Final
  = return $ mkFun ([], [B x tRcvr], fmap F.top tf `eSingleton` mkOffset x f)

  -- TODO: Case: TEnum

  | otherwise
  = return $ mkFun ([], [B x tRcvr], substThis x tf)
  where
    x = F.symbol "x"


-- | setProp<A, M extends Mutable>(o: { f[M]: A }, x: A) => A
--
-- --------------------------------------------------------------------------------------------
-- setPropTy :: (F.Reftable r, F.Symbolic f) => f -> RType r
-- --------------------------------------------------------------------------------------------
-- setPropTy f = mkAll [bvt, bvm] ft
--   where
--     ft      = TFun [b1, b2] t fTop
--     b1      = B (F.symbol "o") $ tRcvr tIM (tmFromFieldList [(f, FI Req m t)]) fTop
--     b2      = B (F.symbol "x") $ t
--     m       = toTTV bvm :: F.Reftable r => RType r
--     t       = toTTV bvt
--     bvt     = BTV (F.symbol "A") def Nothing
--     bvm     = BTV (F.symbol "M") def (Just tMU) :: F.Reftable r => BTVar r
--     toTTV   :: F.Reftable r => BTVar r -> RType r
--     toTTV   = (`TVar` fTop) . btvToTV


--------------------------------------------------------------------------------------------
idTy  :: (ExprReftable F.Symbol r, F.Reftable r) => RTypeQ q r -> RTypeQ q r
idTys :: (ExprReftable F.Symbol r, F.Reftable r) => [RTypeQ q r] -> RTypeQ q r
--------------------------------------------------------------------------------------------
idTy t = mkFun ([], [B sx t], t `strengthen` uexprReft sx)
  where
    sx    = F.symbol "x_"

idTys = mkAnd . map idTy


--------------------------------------------------------------------------------------------
castTy :: (ExprReftable F.Symbol r, F.Reftable r) => RType r -> RType r
--------------------------------------------------------------------------------------------
castTy t = TFun [B sx t] (t `eSingleton` sx) fTop
  where
    sx    = F.symbol "x"
