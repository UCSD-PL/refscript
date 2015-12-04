{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

-- | Utilities share by both Typecheck and Liquid modules

module Language.Rsc.TypeUtilities (

    mkDotRefFunTy
  , idTy, idTys
  , castTy
  , arrayLitTy
  -- , objLitTy

  ) where

import           Language.Fixpoint.Names      (symbolString)
import qualified Language.Fixpoint.Types      as F
import           Language.Rsc.AST
import           Language.Rsc.ClassHierarchy
import           Language.Rsc.Environment
import           Language.Rsc.Errors
import           Language.Rsc.Liquid.Types
import           Language.Rsc.Names
import           Language.Rsc.Pretty
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types


-- | Dot ref
--
---------------------------------------------------------------------------------
mkDotRefFunTy ::
  (CheckingEnvironment r t, PP r, ExprReftable F.Expr r, F.Symbolic f, F.Reftable r, Monad m) =>
  t r -> f -> RType r -> MutabilityR r -> FieldAsgn -> RType r -> m (RType r)
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



---------------------------------------------------------------------------------
-- | Array literal types
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- arrayLitTy :: F.Subable (RType r) =>RType r -> Int -> RType r -> RType r
---------------------------------------------------------------------------------
arrayLitTy l g@(envCHA -> c) e (Just t0) n
  | TRef nm _          <- t0
  , Just (Gen _ [m,t]) <- weaken c nm (mkAbsName [] arrayName)
  = if isIM m then mkImmArrTy l g t n
              else mkArrTy    l g t n
  | otherwise
  = return $ Left $ errorArrayLitType l e t0

arrayLitTy l _ e _ _
  = return $ Left $ errorArrayLitCtxType l e


mkImmArrTy l g t n
  = do  opTy <- safeEnvFindTy l g (builtinOpId BIImmArrayLit)
        case opTy of
          TAll (BTV s l _) (TFun [B x_ t_] tOut r) ->
            return $ Right $ mkAll [BTV s l (Just t)] (TFun (bs x_ t_) (rt tOut) r)
          _ -> return undefined
  where
    bs x_ t_ = [ B (tox x_ i) t_ | i <- [1..n] ]
    rt t_    = F.subst1 t_ (F.symbol $ builtinOpId BINumArgs, F.expr (n::Int))
    tox x    = F.symbol . ((symbolString x) ++) . show

mkArrTy l g t n
  = do  opTy <- safeEnvFindTy l g (builtinOpId BIArrayLit)
        case opTy of
          TAll μ (TAll α (TFun [B x_ t_] rt r)) ->
            return $ Right $ mkAll [μ,α] (TFun (bs x_ t_) rt r)
  where
    bs x_ t_ = [ B (tox x_ i) t_ | i <- [1..n] ]
    rt       = F.subst1 t (F.symbol $ builtinOpId BINumArgs, F.expr (n::Int))
    tox x    = F.symbol . ((symbolString x) ++) . show


-- --------------------------------------------------------------------------------------------
-- objLitTy         :: (F.Reftable r, IsLocated a) => a -> [Prop a] -> RType r
-- --------------------------------------------------------------------------------------------
-- objLitTy l ps     = mkFun (avs, bs, rt)
--   where
--     bs            = [B s (ofType a) | (s,a) <- zip ss ats ]
--     rt            = TObj tIM tms fTop
--     tms           = typeMembersFromList [ (s, FI Req Inherited a) | (s, a) <- zip ss ats ]
--     (avs, ats)    = unzip $ map (freshBTV l aSym Nothing) [1..length ps]  -- field type vars
--     ss            = [F.symbol p | p <- ps]
--     mSym          = F.symbol "M"
--     aSym          = F.symbol "A"

