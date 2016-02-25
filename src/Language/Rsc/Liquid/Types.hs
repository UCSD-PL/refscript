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
{-# LANGUAGE ViewPatterns              #-}

-- | Module pertaining to Refinement Type descriptions and conversions
--   Likely mergeable with @Language.Rsc.Typecheck.Types@

module Language.Rsc.Liquid.Types (

  -- * Refinement Types
    RefType, RRType

  -- * Conversions
  , RefTypable (..), eSingleton, uSingleton, pSingleton

  -- * Manipulating RefType
  , rTypeValueVar

  -- * Manipulating Reft
  , noKVars

  -- * Predicates On RefType
  , isTrivialRefType

  -- * Accessing Spec Annotations
  , getSpec, getRequires, getEnsures, getAssume, getAssert
  , getInvariant, getFunctionIds

  -- * Raw low-level Location-less constructors
  , rawStringSymbol

  -- * 'this' related substitutions
  , substThis, substThisCtor

  , mkQualSym
  , mkOffsetSym

  ) where

import           Data.Maybe                      (mapMaybe)
import qualified Data.Text                       as T
import qualified Language.Fixpoint.Types         as F
import           Language.Fixpoint.Types.Names   (symbolText)
import           Language.Rsc.AST
import           Language.Rsc.Liquid.Refinements
import           Language.Rsc.Names
import           Language.Rsc.Pretty
import           Language.Rsc.Transformations
import           Language.Rsc.Traversals
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types

--------------------------------------------------------------------------------
-- | Refinement Types and Annotations
--------------------------------------------------------------------------------

type RefType     = RType F.Reft
type RRType      = RTypeQ RK F.Reft


--------------------------------------------------------------------------------
-- | Converting `Rsc` values into `Fixpoint` values,
--   i.e. *language* level entities into *logic* level entities.
--------------------------------------------------------------------------------

instance F.Expression (Id a) where
  expr = F.eVar

instance F.Expression (LValue a) where
  expr = F.eVar

instance F.Expression (Expression a) where
  expr (IntLit _ i)                 = F.expr i
  expr (VarRef _ x)                 = F.expr x
  expr (InfixExpr _ o e1 e2)        = F.EBin (bop o) (F.expr e1) (F.expr e2)
  expr (PrefixExpr _ PrefixMinus e) = F.EBin F.Minus (F.expr (0 :: Int)) (F.expr e)
  expr (Cast_ _ e)                  = F.expr e
  expr (Cast  _ e)                  = F.expr e
  expr e                            = convertError "F.Expr" e

instance F.Predicate  (Expression a) where
  prop (BoolLit _ True)            = F.PTrue
  prop (BoolLit _ False)           = F.PFalse
  prop (PrefixExpr _ PrefixLNot e) = F.PNot (F.prop e)
  prop e@(InfixExpr {})            = eProp e
  prop e                           = convertError "F.prop" e

--------------------------------------------------------------------------------
eProp :: Expression a -> F.Expr
--------------------------------------------------------------------------------
eProp (InfixExpr _ OpLT       e1 e2) = F.PAtom F.Lt (F.expr e1) (F.expr e2)
eProp (InfixExpr _ OpLEq      e1 e2) = F.PAtom F.Le (F.expr e1) (F.expr e2)
eProp (InfixExpr _ OpGT       e1 e2) = F.PAtom F.Gt (F.expr e1) (F.expr e2)
eProp (InfixExpr _ OpGEq      e1 e2) = F.PAtom F.Ge (F.expr e1) (F.expr e2)
eProp (InfixExpr _ OpStrictEq e1 e2) = F.PAtom F.Eq (F.expr e1) (F.expr e2)
eProp (InfixExpr _ OpNEq      e1 e2) = F.PAtom F.Ne (F.expr e1) (F.expr e2)
eProp (InfixExpr _ OpLAnd     e1 e2) = F.pAnd [F.prop e1, F.prop e2]
eProp (InfixExpr _ OpLOr      e1 e2) = F.pOr  [F.prop e1, F.prop e2]
eProp e                              = convertError "InfixExpr -> F.Prop" e

--------------------------------------------------------------------------------
bop       :: InfixOp -> F.Bop
--------------------------------------------------------------------------------
bop OpSub = F.Minus
bop OpAdd = F.Plus
bop OpMul = F.Times
bop OpDiv = F.Div
bop OpMod = F.Mod
bop o     = convertError "F.Bop" o


--------------------------------------------------------------------------------
-- | Embedding Values as RefTypes
--------------------------------------------------------------------------------

class RefTypable a where
  rType :: a -> RefType

instance F.Reftable r => RefTypable (RType r) where
  rType = ofType . toType           -- removes all refinements

eSingleton      :: (F.Reftable r, F.Expression e, ExprReftable e r) => RType r -> e -> RType r
eSingleton t e  = t `strengthen` (exprReft e)

uSingleton      :: (F.Reftable r, F.Expression e, ExprReftable e r) => RType r -> e -> RType r
uSingleton t e  = t `strengthen` (uexprReft e)

pSingleton      :: (F.Predicate p) => RefType -> p -> RefType
pSingleton t p  = t `strengthen` (F.propReft p)

--------------------------------------------------------------------------------
-- | Converting RType to Fixpoint
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
noKVars :: F.Reft -> F.Reft
--------------------------------------------------------------------------------
noKVars (F.Reft (x, p)) = F.Reft (x, dropKs p)
  where
    dropKs              = F.pAnd . filter (not . isK) . F.conjuncts
    isK (F.PKVar {})    = True
    isK _               = False


--------------------------------------------------------------------------------
-- | Substitutions
--------------------------------------------------------------------------------

instance (PPR r, F.Subable r) => F.Subable (RTypeQ q r) where
  syms        = foldReft (\r acc -> F.syms r ++ acc) []
  substa      = fmap . F.substa
  substf f    = emapReft (F.substf . F.substfExcept f) []
  subst su    = emapReft (F.subst  . F.substExcept su) []
  subst1 t su = emapReft (\xs r -> F.subst1Except xs r su) [] t

instance (PPR r, F.Subable r) => F.Subable (TypeMemberQ q r) where
  syms     (FI _ _ m t)    = F.syms m ++ F.syms t
  syms     (MI _ _ mts)    = F.syms mts
  substa f (FI s o m t)    = FI s o (F.substa f m  ) (F.substa f t)
  substa f (MI s o mts)    = MI s o (F.substa f mts)
  substf f (FI s o m t)    = FI s o (F.substf f m  ) (F.substf f t)
  substf f (MI s o mts)    = MI s o (F.substf f mts)
  subst su (FI s o m t)    = FI s o (F.subst su m) (F.subst su t)
  subst su (MI s o mts)    = MI s o (F.subst su mts)
  subst1   (FI s o m t) su = FI s o (F.subst1 m su) (F.subst1 t su)
  subst1   (MI s o mts) su = MI s o (F.subst1 mts su)


--------------------------------------------------------------------------------
isTrivialRefType :: RefType -> Bool
--------------------------------------------------------------------------------
-- | The only allowed top-level refinement of a function type is the
--   ('function') tag, So ignore this for this check.
isTrivialRefType (TFun a b _) = isTrivialRefType' (TFun a b fTop)
isTrivialRefType t            = isTrivialRefType' t

isTrivialRefType' :: RefType -> Bool
isTrivialRefType' = foldReft (\r -> (f r &&)) True
  where
    f :: F.Reft -> Bool
    f = F.isTauto -- (F.Reft (_,ras)) = null ras

rawStringSymbol = F.locAt "RSC.Types.rawStringSymbol"
                . F.symbol


--------------------------------------------------------------------------------
-- | Helpers for extracting specifications from @Rsc@ @Statement@
--------------------------------------------------------------------------------

getInvariant :: Statement a -> F.Expr
getInvariant = getSpec getInv . flattenStmt

getAssume    :: Statement a -> Maybe F.Expr
getAssume    = getStatementPred "assume"

getAssert    :: Statement a -> Maybe F.Expr
getAssert    = getStatementPred "assert"

getRequires  = getStatementPred "requires"
getEnsures   = getStatementPred "ensures"
getInv       = getStatementPred "invariant"

getStatementPred :: String -> Statement a -> Maybe F.Expr
getStatementPred name (ExprStmt _ (CallExpr _ (VarRef _ (Id _ f)) [p]))
  | name == f
  = Just $ F.prop p
getStatementPred _ _
  = Nothing

getSpec   :: (Statement a -> Maybe F.Expr) -> [Statement a] -> F.Expr
getSpec g = mconcat . mapMaybe g

getFunctionIds :: Statement a -> [Id a]
getFunctionIds s = [f | (FunctionStmt _ f _ _) <- flattenStmt s]


-------------------------------------------------------------------------------
substThis :: (F.Expression x, F.Subable t) => x -> t -> t
-------------------------------------------------------------------------------
substThis x = F.subst (F.mkSubst [(thisSym, F.expr x)])

-------------------------------------------------------------------------------
substThisCtor :: RefType -> RefType
-------------------------------------------------------------------------------
substThisCtor ft =
  case bkFun ft of
    Just (vs, bs, rt) -> mkFun (vs, bs, substThis (rTypeValueVar rt) rt)
    Nothing           -> ft

-------------------------------------------------------------------------------
qualifySymbol :: F.Symbol -> F.Symbol -> F.Symbol
-------------------------------------------------------------------------------
qualifySymbol (symbolText -> m) x'@(symbolText -> x)
  | isQualified x  = x'
  | otherwise      = F.symbol (m `mappend` "." `mappend` x)

isQualified y = "." `T.isInfixOf` y

-------------------------------------------------------------------------------
mkQualSym :: (F.Symbolic x, F.Symbolic f) => x -> f -> F.Symbol
-------------------------------------------------------------------------------
mkQualSym x f = F.symbol x `qualifySymbol` F.symbol f

-------------------------------------------------------------------------------
mkOffsetSym :: (F.Symbolic f, F.Expression x) => x -> f -> F.Expr
-------------------------------------------------------------------------------
mkOffsetSym x f = F.mkEApp offsetLocSym [F.expr x, F.expr $ symbolText $ F.symbol f]

