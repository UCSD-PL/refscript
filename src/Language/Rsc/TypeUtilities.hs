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

    idTy, idTys
  , castTy
  , arrayLitTy
  , objLitTy
  , mkCondExprTy

  ) where

import           Data.List                     (unzip4)
import qualified Language.Fixpoint.Types       as F
import           Language.Fixpoint.Types.Names (symbolString)
import           Language.Rsc.AST
import           Language.Rsc.ClassHierarchy
import           Language.Rsc.Environment
import           Language.Rsc.Errors
import           Language.Rsc.Liquid.Types
import           Language.Rsc.Locations
import           Language.Rsc.Names
import           Language.Rsc.Pretty
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types

type PPRE r = (ExprReftable F.Expr r, ExprReftable Int r, PPR r)

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


---------------------------------------------------------------------------------
idTy  :: (ExprReftable F.Symbol r, F.Reftable r) => RTypeQ q r -> RTypeQ q r
idTys :: (ExprReftable F.Symbol r, F.Reftable r) => [RTypeQ q r] -> RTypeQ q r
---------------------------------------------------------------------------------
idTy t = mkFun ([], [B sx t], t `strengthen` uexprReft sx)
  where
    sx    = F.symbol "x_"

idTys = mkAnd . map idTy

---------------------------------------------------------------------------------
castTy :: (ExprReftable F.Symbol r, F.Reftable r) => RType r -> RType r
---------------------------------------------------------------------------------
castTy t = TFun [B sx t] (t `eSingleton` sx) fTop
  where
    sx    = F.symbol "x"

---------------------------------------------------------------------------------
-- | Array literal types
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
arrayLitTy :: (Monad m, F.Reftable r, IsLocated l, PP r, PP a1, CheckingEnvironment r t)
           => l -> t r -> a1 -> Maybe (RType r) -> Int -> m (Either F.Error (RType r))
---------------------------------------------------------------------------------
arrayLitTy l g@(envCHA -> c) e (Just t0) n
  | TRef nm _          <- t0
  , Just (Gen _ [m,t]) <- weaken c nm (mkAbsName [] arrayName)
  = if isIM m then mkImmArrTy l g t n
              else mkArrTy    l g n
  | otherwise
  = return $ Left $ errorArrayLitType l e t0
arrayLitTy l g e Nothing n = mkUniqueArrTy l g n

-- | builtin_BIImmArrayLit :: <A>(x: A) => {v: IArray<A> | len v = builtin_BINumArgs }
--
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

-- | declare function builtin_BIUniqueArrayLit<A>(a: A): Array<Unique, A>
--
mkUniqueArrTy l g n
  = do  opTy <- safeEnvFindTy l g (builtinOpId BIUniqueArrayLit)
        case opTy of
          TAll α (TFun [B x_ t_] rt r) ->
            return $ Right $ mkAll [α] (TFun (bs x_ t_) rt r)
  where
    bs x_ t_ = [ B (tox x_ i) t_ | i <- [1..n] ]
    tox x    = F.symbol . ((symbolString x) ++) . show

-- | declare function builtin_BIArrayLit<M extends ReadOnly, A>(a: A): Array<M, A>
--
mkArrTy l g n
  = do  opTy <- safeEnvFindTy l g (builtinOpId BIArrayLit)
        case opTy of
          TAll μ (TAll α (TFun [B x_ t_] rt r)) ->
            return $ Right $ mkAll [μ,α] (TFun (bs x_ t_) rt r)
  where
    bs x_ t_ = [ B (tox x_ i) t_ | i <- [1..n] ]
    tox x    = F.symbol . ((symbolString x) ++) . show

--------------------------------------------------------------------------------
objLitTy :: (PPRE r, IsLocated l, CheckingEnvironment r t)
         => l -> t r -> [Prop l] -> Maybe (RType r) -> ([Maybe (RType r)], RType r)
--------------------------------------------------------------------------------
objLitTy l g ps to = (ct, mkFun (concat bbs, bs, TObj (typeMembersFromList et) fTop))
  where
    (ct, bbs, bs, et) = unzip4 (map propToBind ps)
    propToBind p      = propParts l (F.symbol p) (F.lookupSEnv (F.symbol p) ctxTys)
    ctxTys            = maybe mempty (i_mems . typeMembersOfType (envCHA g)) to

--------------------------------------------------------------------------------
propParts
  :: (PPRE r, IsLocated l)
  => l -> F.Symbol -> Maybe (TypeMember r)
  -> (Maybe (RType r), [BTVar r], Bind r, TypeMember r)
  --  ^^^^^^^^^^^^^^^
  --  Contextual type
  --
--------------------------------------------------------------------------------
propParts l p (Just (FI _ o m t)) = (Just t, [abtv], b, ot)
  where
    loc   = srcPos l
    aSym  = F.symbol "A" `F.suffixSymbol` p     -- TVar symbol
    at    = TV  aSym loc
    abtv  = BTV aSym loc Nothing
    b     = B p ty
    ot    = FI p o m ty
    ty    = TVar at fTop

propParts l p Nothing = (Nothing, [abtv, pbtv], b, ot)
  where
    loc   = srcPos l
    aSym  = F.symbol "A" `F.suffixSymbol` p
    pSym  = F.symbol "M" `F.suffixSymbol` p
    at    = TV  aSym loc
    abtv  = BTV aSym loc Nothing
    pt    = TV  pSym loc
    pbtv  = BTV pSym loc Nothing
    pty   = TVar pt fTop
    b     = B p ty
    ot    = FI p Req pty ty
    ty    = TVar at fTop


mkCondExprTy l g t
  = do  opTy <- safeEnvFindTy l g (builtinOpId BICondExpr)
        case bkAll opTy of
          ([c, BTV α la _, BTV β lb _], TFun [B c_ tc_, B a_ ta_, B b_ tb_] rt r') ->
            return $ mkAll [c, BTV α la (Just t), BTV β lb (Just t)]
                           (TFun [B c_ tc_, B a_ ta_, B b_ tb_] (t `strengthen` rTypeR rt) r')
          _ -> error "[BUG] mkCondExprTy"

