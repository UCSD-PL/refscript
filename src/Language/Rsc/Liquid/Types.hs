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
  , singleton

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
  , substThis, substThisWithSelf, substThisCtor

  , qualifySymbol
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
singleton :: F.Expression x => RefType -> x -> RefType
--------------------------------------------------------------------------------
singleton t x = toplevel (const (F.uexprReft x)) t

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



-- NEW -- --------------------------------------------------------------------------------
-- NEW -- unionCheck :: IsLocated l => l -> CGEnv -> RefType -> [RefType] -> Either [Error] [RefType]
-- NEW -- --------------------------------------------------------------------------------
-- NEW -- unionCheck l γ t ts
-- NEW --   | not $ null samePrims = Left $ uncurry (errorUnionMergePrims l t) <$> samePrims
-- NEW --   | not $ null sameVars  = Left $ uncurry (errorUnionMergeVars l t) <$> sameVars
-- NEW --   | not $ null sameAnds  = Left $ uncurry (errorUnionMergeAnds l t) <$> sameAnds
-- NEW --   | not $ null sameObjs  = Left $ uncurry (errorUnionMergeObjs l t) <$> sameObjs
-- NEW --   | not $ null sameTys   = Left $ uncurry (errorUnionMergeTys l t) <$> sameTys
-- NEW --   | not $ null sameMods  = Left $ uncurry (errorUnionMergeMods l t) <$> sameMods
-- NEW --   | length alls > 0      = Left [ errorUnionMergeAlls l t ]
-- NEW --   | length funs > 0      = Left [ errorUnionMergeFuns l t ]
-- NEW --   | length exps > 0      = Left [ bugUnionMergeExps l t ]
-- NEW --   | otherwise            = Right $ prims ++ vars ++ ands ++ refs ++ objs ++ tys ++ mods ++ funs
-- NEW --
-- NEW --   where
-- NEW --     sub   = isSubtype γ
-- NEW --     -- no unions here
-- NEW --     prims = [ t | t@(TPrim _ _ ) <- ts ]
-- NEW --     vars  = [ t | t@(TVar _ _  ) <- ts ]
-- NEW --     ands  = [ t | t@(TAnd _    ) <- ts ]
-- NEW --     refs  = [ t | t@(TRef _ _  ) <- ts ]
-- NEW --     objs  = [ t | t@(TObj _ _  ) <- ts ]
-- NEW --     tys   = [ t | t@(TClass _  ) <- ts ]
-- NEW --     mods  = [ t | t@(TMod _    ) <- ts ]
-- NEW --     alls  = [ t | t@(TAll _ _  ) <- ts ]
-- NEW --     funs  = [ t | t@(TFun _ _ _) <- ts ]
-- NEW --     exps  = [ t | t@(TExp _    ) <- ts ]
-- NEW --
-- NEW --     iprims = zip [0..] prims
-- NEW --     samePrims = [ (t1, t2) | (i1, t1@(TPrim p1 _)) <- iprims, (i2, t2@(TPrim p2 _)) <- iprims, p1 == p2, i1 /= i2 ]
-- NEW --
-- NEW --     ivars = zip [0..] vars
-- NEW --     sameVars = [ (t1, t2) | (i1, t1@(TVar v1 _)) <- ivars, (i2, t2@(TVar v2 _)) <- ivars, v1 == v2, i1 /= i2 ]
-- NEW --
-- NEW --     iands = zip [0..] ands
-- NEW --     sameAnds = [ (t1, t2) | (i1, t1@(TAnd _)) <- iands, (i2, t2@(TAnd _)) <- iands, i1 /= i2 ]
-- NEW --
-- NEW --     iobjs = zip [0..] $ refs ++ objs
-- NEW --     sameObjs = [ (t1, t2) | (i1, t1) <- iobjs, (i2, t2) <- iobjs, i1 /= i2, t1 `sub` t2 || t2 `sub` t1 ]
-- NEW --
-- NEW --     itys = zip [0..] tys
-- NEW --     sameTys = [ (t1, t2) | (i1, t1@(TClass n1)) <- itys, (i2, t2@(TClass n2)) <- itys
-- NEW --                           , i1 /= i2, t1 `sub` t2 || t2 `sub` t1 ]
-- NEW --
-- NEW --     imods = zip [0..] mods
-- NEW --     sameMods = [ (t1, t2) | (i1, t1@(TMod m1)) <- imods, (i2, t2@(TMod m2)) <- imods, i1 /= i2, m1 == m2 ]



-------------------------------------------------------------------------------
substThis :: (F.Expression x, F.Subable t) => x -> t -> t
-------------------------------------------------------------------------------
substThis x = F.subst (F.mkSubst [(thisSym, F.expr x)])

-------------------------------------------------------------------------------
substThisWithSelf :: RefType -> RefType
-------------------------------------------------------------------------------
substThisWithSelf t = substThis (rTypeValueVar t) t


substThisCtor ft | Just (vs, bs, rt) <- bkFun ft
                 = mkFun (vs, bs, substThisWithSelf rt)
                 | otherwise
                 = ft


-- substOffsetThis = emapReft (\_ -> V.trans vs () ()) []
--   where
--     vs     = V.defaultVisitor { V.txExpr = tx }
--     tx _ (F.EApp o [ F.EVar x, F.ESym (F.SL f) ])
--            | F.symbol o == offsetSym, F.symbol x == thisSym
--            = F.eVar f
--     tx _ e = e
--
--
-- -- | Substitute occurences of 'this' in type @t'@, given that the receiver
-- --   object is bound to symbol @x@ and it has a type @t@ under @g@.
-- -------------------------------------------------------------------------------
-- substThis' :: (IsLocated a, F.Symbolic a)
--            => CGEnv -> (a, RefType) -> RefType -> RefType
-- -------------------------------------------------------------------------------
-- substThis' g (x,t) = F.subst su
--   where
--     su            = F.mkSubst $ (this, F.expr $ F.symbol x) : fieldSu
--     this          = F.symbol $ builtinOpId BIThis
--
--     fieldSu       | Just (TCons _ fs _) <- expandType Coercive g t
--                   = [ subPair f | ((f,InstanceMember), FieldSig _ _ m _) <- M.toList fs
--                                 , isImmutable m ]
--                   | otherwise
--                   = []
--     qFld x f      = F.qualifySymbol (F.symbol x) f
--     subPair f     = (qFld this f, F.expr $ qFld x f)
--
--

qualifySymbol :: F.Symbol -> F.Symbol -> F.Symbol
qualifySymbol (symbolText -> m) x'@(symbolText -> x)
  | isQualified x  = x'
  -- | isParened x    = symbol (wrapParens (m `mappend` "." `mappend` stripParens x))
  | otherwise      = F.symbol (m `mappend` "." `mappend` x)

isQualified y = "." `T.isInfixOf` y
-- wrapParens x  = "(" `mappend` x `mappend` ")"


-------------------------------------------------------------------------------
mkQualSym :: (F.Symbolic x, F.Symbolic f) => x -> f -> F.Symbol
-------------------------------------------------------------------------------
mkQualSym x f = F.symbol x `qualifySymbol` F.symbol f

-------------------------------------------------------------------------------
mkOffsetSym :: (F.Symbolic f, F.Expression x) => x -> f -> F.Expr
-------------------------------------------------------------------------------
mkOffsetSym x f = F.mkEApp offsetLocSym [F.expr x, F.expr $ symbolText $ F.symbol f]



