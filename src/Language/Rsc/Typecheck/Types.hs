-- | Global type definitions for Refinement Type Checker

{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE IncoherentInstances       #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}

module Language.Rsc.Typecheck.Types (
  -- Export instances of Monoid
    Monoid(..)

  -- * Type operations
  , toType, ofType, rTop, strengthen, toplevel

  , ExprReftable(..)

  -- * Constructing Types
  , tOr, tOrR, mkFun, mkAll, mkAnd, mkAndOpt, mkInitFldTy

  -- * Deconstructing Types
  , bkFun, bkFunNoBinds, bkFuns, bkAll, bkAnd, bkUnion

  , stripNull, stripUndefined
  , orUndef, padUndefineds

  , rTypeR

  -- Bounded type variables
  , btvToTV, tvToBTV

  -- * Mutability primitives
  , tMU , tIM , tAF , tRO , tUQ
  , trMU, trIM, trAF, trRO, trUQ
  , isMU, isIM, isAF, isRO, isUQ

  , mutRelated, mutRelatedBVar

  -- * Primitive Types

  --   # Constructors
  , tNum, tBV32, tBool, tString, tTop, tVoid, tErr, tVar
  , btVar, tUndef, tNull, tBot, tAny

  --   # Tests
  , isTPrim, isTAny, isTTop, isTUndef, isTUnion, isTStr, isTBool
  , isBvEnum, isTVar, isTNull, isTVoid, isTFun
  , isArrayType, isTBot, isTNum, isBV32
  , maybeTObj, notNullObj

  --   # Predicates
  , isReqMember

  --   # Operations
  , orNull

  -- * Refinements
  , fTop

  -- * Type Definitions
  , mkTypeMembers, typeMembers, tmsFromList, typesOfTM

  -- * Operator Types
  , Identifier
  , infixOpId, prefixOpId, builtinOpId, finalizeTy

  -- * Builtin: Binders
  , mkId, argId
  -- , returnTy

  -- * BitVector
  , bitVectorValue

  , eqV

  ) where

import           Control.Arrow                   (second)
import           Data.Default
import qualified Data.List                       as L
import           Data.Maybe                      (maybeToList)
import           Data.Typeable                   ()
import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Smt.Bitvector as BV
import qualified Language.Fixpoint.Types         as F
import           Language.Rsc.AST.Syntax
import qualified Language.Rsc.Core.Env           as E
import           Language.Rsc.Misc               (mapSnd3)
import           Language.Rsc.Names
import           Language.Rsc.Types

-- import           Debug.Trace                     (trace)

---------------------------------------------------------------------
-- | Mutability
---------------------------------------------------------------------

mkMut s    = TRef (Gen (mkAbsName [] s) []) fTop
mkRelMut s = TRef (Gen (mkRelName [] s) []) fTop

tMU, tUQ, tIM, tRO, tAF :: F.Reftable r => RType r
tMU   = mkMut "Mutable"
tIM   = mkMut "Immutable"
tRO   = mkMut "ReadOnly"
tAF   = mkMut "AssignsFields"
tUQ   = mkMut "Unique"

trMU, trIM, trRO, trAF, trUQ :: F.Reftable r => RTypeQ RK r
trMU   = mkRelMut "Mutable"
trIM   = mkRelMut "Immutable"
trRO   = mkRelMut "ReadOnly"
trAF   = mkRelMut "AssignsFields"
trUQ   = mkRelMut "Unique"

typeName (Gen (QN _ s) _)  = s

isNamed s t | TRef n _ <- t, typeName n == F.symbol s = True | otherwise = False

isRO   = isNamed "ReadOnly"
isMU   = isNamed "Mutable"
isIM   = isNamed "Immutable"
isUQ   = isNamed "Unique"
isAF   = isNamed "AssignsFields"

mutRelated t = isMU t || isIM t || isUQ t || isRO t

mutRelatedBVar (BTV _ _ (Just m)) = mutRelated m
mutRelatedBVar _                  = False


---------------------------------------------------------------------
-- | Refinement manipulation
---------------------------------------------------------------------

-- | "pure" top-refinement
fTop :: (F.Reftable r) => r
fTop = mempty

-- | Stripping out Refinements
toType :: RTypeQ q a -> RTypeQ q ()
toType = fmap (const ())

-- | Adding in Refinements
ofType :: (F.Reftable r) => RTypeQ q () -> RTypeQ q r
ofType = fmap (const fTop)

-- | Top-up refinemnt
rTop = ofType . toType

class ExprReftable a r where
  exprReft  :: a -> r
  uexprReft :: a -> r

instance ExprReftable a () where
  exprReft  _ = ()
  uexprReft _ = ()

instance F.Expression a => ExprReftable a F.Reft where
  exprReft   = F.exprReft
  uexprReft  = F.uexprReft

instance F.Reftable r => ExprReftable BV.Bv r where
  exprReft  = F.ofReft . F.exprReft
  uexprReft = F.ofReft . F.uexprReft

padUndefineds xs yts
  | nyts <= nxs = Just $ yts ++ xundefs
  | otherwise   = Nothing
  where
    nyts        = length yts
    nxs         = length xs
    xundefs     = [ B (F.symbol x') Req tUndef | x' <- snd $ splitAt nyts xs ]

bkFuns :: RTypeQ q r -> Maybe [([BTVarQ q r], [BindQ q r], RTypeQ q r)]
bkFuns = sequence . fmap bkFun . bkAnd

bkFun :: RTypeQ q r -> Maybe ([BTVarQ q r], [BindQ q r], RTypeQ q r)
bkFun t | (αs, t')   <- bkAll t =
  do  (xts, t'')     <- bkArr t'
      return          $ (αs, xts, t'')

bkFunNoBinds :: RTypeQ q r -> Maybe ([BTVarQ q r], [RTypeQ q r], RTypeQ q r)
bkFunNoBinds = fmap (mapSnd3 $ map b_type) . bkFun

mkFun :: F.Reftable r  => ([BTVarQ q r], [BindQ q r], RTypeQ q r) -> RTypeQ q r
mkFun (αs, bs, rt)    = mkAll αs (TFun bs rt fTop)

bkArr :: RTypeQ q r -> Maybe ([BindQ q r], RTypeQ q r)
bkArr (TFun xts t _)  = Just (xts, t)
bkArr _               = Nothing

mkAll :: [BTVarQ q r] -> RTypeQ q r -> RTypeQ q r
mkAll αs t            = mkAnd $ go (reverse αs) <$> bkAnd t
  where
    go (x:xs)         = go xs . TAll x
    go []             = id

bkAll :: RTypeQ q r -> ([BTVarQ q r], RTypeQ q r)
bkAll                 = go []
  where
    go αs (TAll α t)  = go (α : αs) t
    go αs t           = (reverse αs, t)

bkAnd :: RTypeQ q r -> [RTypeQ q r]
bkAnd (TAnd ts) = snd <$> ts
bkAnd t         = [t]

mkAnd [t]       = t
mkAnd ts        = TAnd $ zip [0..] ts

mkAndOpt []     = Nothing
mkAndOpt ts     = Just $ mkAnd ts

instance F.Reftable r => Monoid (RTypeQ q r) where
  mempty        = tBot
  mappend t t'  = mkAnd $ bkAnd t ++ bkAnd t'

----------------------------------------------------------------------------------------
tOrR :: F.Reftable r => [RTypeQ q r] -> r -> RTypeQ q r
tOr  :: F.Reftable r => [RTypeQ q r] -> RTypeQ q r
----------------------------------------------------------------------------------------
tOrR [ ] = TPrim TBot
tOrR [t] = (t `strengthen`)
tOrR ts  = TOr (concatMap bkUnion ts)

tOr  ts  = tOrR ts fTop

----------------------------------------------------------------------------------------
bkUnion :: F.Reftable r => RTypeQ q r -> [RTypeQ q r]
----------------------------------------------------------------------------------------
bkUnion (TOr ts r) = map (`strengthen` r) ts
bkUnion t          = [t]

----------------------------------------------------------------------------------------
stripNull      :: F.Reftable r => RType r -> RType r
stripUndefined :: F.Reftable r => RType r -> RType r
----------------------------------------------------------------------------------------
stripNull      = tOr . filter (not . isTNull)  . bkUnion
stripUndefined = tOr . filter (not . isTUndef) . bkUnion

----------------------------------------------------------------------------------------
orUndef :: F.Reftable r => RTypeQ q r -> RTypeQ q r
----------------------------------------------------------------------------------------
orUndef t  | any isTUndef ts = t
           | otherwise = tOr (tUndef:ts)
  where ts = bkUnion t


----------------------------------------------------------------------------------------
toplevel :: (r -> r) -> RTypeQ q r -> RTypeQ q r
----------------------------------------------------------------------------------------
toplevel f (TPrim c r  ) = TPrim c (f r)
toplevel f (TVar v r   ) = TVar v (f r)
toplevel f (TOr ts r   ) = TOr (map (toplevel f) ts) (f r)
toplevel f (TAnd ts    ) = TAnd (map (second (toplevel f)) ts)
toplevel f (TRef n r   ) = TRef n (f r)
toplevel f (TObj m ms r) = TObj m ms (f r)
toplevel _ (TClass n   ) = TClass n
toplevel _ (TMod n     ) = TMod n
toplevel f (TAll b t   ) = TAll b (toplevel f t)
toplevel f (TFun b t r ) = TFun b t (f r)
toplevel _ (TExp e     ) = TExp e


-- | Strengthen the top-level refinement
----------------------------------------------------------------------------------------
strengthen :: F.Reftable r  => RTypeQ q r -> r -> RTypeQ q r
----------------------------------------------------------------------------------------
strengthen t r' = toplevel (r' `F.meet`) t

-- NOTE: r' is the OLD refinement.
--       We want to preserve its VV binder as it "escapes",
--       e.g. function types. Sigh. Should have used a separate function binder.


----------------------------------------------------------------------------------------
-- | Predicates on Types
----------------------------------------------------------------------------------------

isPrim c t | TPrim c' _ <- t, c == c' = True | otherwise = False

isTPrim t  | TPrim _ _ <- t = True | otherwise = False

isTTop    = isPrim TTop
isTUndef  = isPrim TUndefined
isTBot    = isPrim TBot
isTNull   = isPrim TNull
isTAny    = isPrim TAny
isTVoid   = isPrim TVoid
isTStr    = isPrim TString
isTNum    = isPrim TNumber
isTBool   = isPrim TBoolean
isBV32    = isPrim TBV32

isTVar   t | TVar _ _ <- t = True | otherwise = False
isTUnion t | TOr  _ _ <- t = True | otherwise = False

-- Type invariants
--
maybeTObj TRef{}     = True
maybeTObj TObj{}     = True
maybeTObj TClass{}   = True
maybeTObj TMod{}     = True
maybeTObj (TAll _ t) = maybeTObj t
maybeTObj (TOr ts _) = any maybeTObj ts
maybeTObj _          = False

notNullObj TRef{}     = True
notNullObj TObj{}     = True
notNullObj TClass{}   = True
notNullObj TMod{}     = True
notNullObj (TAll _ t) = notNullObj t
notNullObj (TOr ts _) = all notNullObj ts
notNullObj _          = False



isTFun  TFun{}       = True
isTFun (TAnd ts)     = all isTFun $ snd <$> ts
isTFun (TAll _ t)    = isTFun t
isTFun _             = False

isArrayType t | TRef (Gen x _) _ <- t
              = F.symbol x == F.symbol "Array"
              | otherwise
              = False

orNull t@(TOr ts r) | any isTNull ts = t
                    | otherwise      = TOr (tNull:ts) r
orNull t            | isTNull t      = t
                    | otherwise      = TOr [tNull,t] fTop

---------------------------------------------------------------------------------
eqV :: RType r -> RType r -> Bool
---------------------------------------------------------------------------------
TVar (TV s1 _) _ `eqV` TVar (TV s2 _) _ = s1 == s2
_                `eqV` _                = False



----------------------------------------------------------------------------------
rTypeR' :: F.Reftable r => RType r -> Maybe r
----------------------------------------------------------------------------------
rTypeR' (TPrim _ r)  = Just r
rTypeR' (TVar _ r)   = Just r
rTypeR' (TOr _ r)    = Just r
rTypeR' (TFun _ _ r) = Just r
rTypeR' (TRef _ r)   = Just r
rTypeR' (TObj _ _ r) = Just r
rTypeR' (TClass _)   = Just fTop
rTypeR' (TMod _)     = Just fTop
rTypeR' (TAnd _ )    = Nothing
rTypeR' (TAll _ _)   = Nothing
rTypeR' (TExp _)     = Nothing

----------------------------------------------------------------------------------
rTypeR :: F.Reftable r => RType r -> r
----------------------------------------------------------------------------------
rTypeR t | Just r <- rTypeR' t = r
         | otherwise = errorstar "Unimplemented: rTypeR"

isBvEnum = all hex . map snd . E.envToList . e_mapping
  where
    hex (HexLit _ _ ) = True
    hex _             = False


-----------------------------------------------------------------------
-- | Primitive / Base Types Constructors
-----------------------------------------------------------------------

-----------------------------------------------------------------------
btvToTV :: BTVarQ q r -> TVar
-----------------------------------------------------------------------
btvToTV  (BTV s l _ ) = TV s l

tvToBTV  (TV s l)     = BTV s l Nothing

tVar :: (F.Reftable r) => TVar -> RTypeQ q r
tVar = (`TVar` fTop)

btVar :: (F.Reftable r) => BTVarQ q r -> RTypeQ q r
btVar = tVar . btvToTV

tNum, tBV32, tBool, tString, tTop, tVoid, tBot, tUndef, tNull, tAny, tErr :: F.Reftable r => RTypeQ q r
tPrim   = (`TPrim` fTop)
tNum    = tPrim TNumber
tBV32   = tPrim TBV32
tBool   = tPrim TBoolean
tString = tPrim TString
tTop    = tPrim TTop
tVoid   = tPrim TVoid
tBot    = tPrim TBot
tUndef  = tPrim TUndefined
tAny    = tPrim TAny
tNull   = tPrim TNull
tErr    = tVoid





---------------------------------------------------------------------------------
-- | Object literal types
---------------------------------------------------------------------------------

-- -- TODO: Avoid capture
-- ---------------------------------------------------------------------------------
-- freshBTV :: (F.Reftable r, IsLocated l, Show a)
--          => l -> F.Symbol -> Maybe (RTypeQ q r) -> a -> (BTVarQ q r, RTypeQ q r)
-- ---------------------------------------------------------------------------------
-- freshBTV l s b n  = (bv,t)
--   where
--     i             = F.intSymbol s n
--     bv            = BTV i (srcPos l) b
--     v             = TV i (srcPos l)
--     t             = TVar v fTop

instance F.Symbolic (LValue a) where
  symbol (LVar _ x) = F.symbol x
  symbol _          = F.symbol "DummyLValue"

instance F.Symbolic (Prop a) where
  symbol (PropId _ (Id _ x)) = F.symbol x -- TODO $ "propId_"     ++ x
  symbol (PropString _ s)    = F.symbol $ "propString_" ++ s
  symbol (PropNum _ n)       = F.symbol $ "propNum_"    ++ show n



---------------------------------------------------------------------------------
mkTypeMembers :: [(F.Symbol, TypeMemberQ q r)] -> [(F.Symbol, TypeMemberQ q r)]
              -> [RTypeQ q r] -> [RTypeQ q r]
              -> [(MutabilityQ q r, RTypeQ q r)]
              -> [(MutabilityQ q r, RTypeQ q r)]
              -> TypeMembersQ q r
---------------------------------------------------------------------------------
mkTypeMembers lms lsms lcs lct lsi lni = TM ms sms call ctor sidx nidx
  where
    ms   = L.foldl' step mempty lms
    sms  = L.foldl' step mempty lsms
    call | [] <- lcs = Nothing | otherwise = Just (mkAnd lcs)
    ctor | [] <- lct = Nothing | otherwise = Just (mkAnd lct)

    -- XXX: Dropping excess index binders
    sidx | (m,t):_ <- lsi = Just (m,t)
         | otherwise      = Nothing
    nidx | (m,t):_ <- lni = Just (m,t)
         | otherwise      = Nothing

    step g (x, MI n o mts) | Just (MI _ o' mts') <- F.lookupSEnv x g
                           = F.insertSEnv x (MI n (o `mappend` o') (mts' ++ mts)) g
    step g (x, f)          = F.insertSEnv x f g

--------------------------------------------------------------------------------------------
typeMembers :: F.SEnv (TypeMemberQ q r) -> TypeMembersQ q r
--------------------------------------------------------------------------------------------
typeMembers f = TM f mempty Nothing Nothing Nothing Nothing

--------------------------------------------------------------------------------------------
tmsFromList :: [TypeMember r] -> TypeMembers r
--------------------------------------------------------------------------------------------
tmsFromList f = TM (F.fromListSEnv (map (\f_ -> (F.symbol f_, f_)) f))
                   mempty Nothing Nothing Nothing Nothing

-- XXX: Not including mutabilities
--------------------------------------------------------------------------------------------
typesOfTM :: TypeMembers r -> [RType r]
--------------------------------------------------------------------------------------------
typesOfTM (TM m sm c k s n) =
  concatMap typesOfMem (map snd $ F.toListSEnv m ) ++
  concatMap typesOfMem (map snd $ F.toListSEnv sm) ++
  concatMap maybeToList [c, k] ++
  map snd (concatMap maybeToList [s, n])

typesOfMem (FI _ _ _ t) = [t]
typesOfMem (MI _ _ mts) = map snd mts

isReqMember (FI _ Req _ _) = True
isReqMember (MI _ Req _  ) = True
isReqMember _              = False

-- --------------------------------------------------------------------------------
-- returnTy :: F.Reftable r => RType r -> Bool -> RType r
-- --------------------------------------------------------------------------------
-- returnTy t True  = mkFun ([], [B (F.symbol "r") t], tVoid)
-- returnTy _ False = mkFun ([], [], tVoid)

--------------------------------------------------------------------------------
finalizeTy :: (F.Reftable r, ExprReftable F.Symbol r) => RType r -> RType r
--------------------------------------------------------------------------------
finalizeTy t  | TRef (Gen x (m:ts)) _ <- t, isUQ m
              = mkFun ([mOut], [B sx Req t], TRef (Gen x (tOut:ts)) (uexprReft sx))
              | otherwise
              = mkFun ([mV], [B (F.symbol "x") Req tV], tV)
  where
    sx        = F.symbol "x_"
    tOut      = tVar $ btvToTV mOut
    mOut      = BTV (F.symbol "N") def Nothing
    tV        = tVar $ btvToTV mV
    mV        = BTV (F.symbol "V") def Nothing

mkInitFldTy t = mkFun ([], [B (F.symbol "f") Req t], tVoid)

type Identifier = Id F.SrcSpan

builtinOpId :: BuiltinOp -> Identifier
builtinOpId BIUndefined      = builtinId "BIUndefined"
builtinOpId BIBracketRef     = builtinId "BIBracketRef"
builtinOpId BIBracketAssign  = builtinId "BIBracketAssign"
builtinOpId BIArrayLit       = builtinId "BIArrayLit"
builtinOpId BICallExpr       = builtinId "BICallExpr"
builtinOpId BIDotRefCallExpr = builtinId "BIDotRefCallExpr"
builtinOpId BIExprT          = builtinId "BIExprT"
builtinOpId BIAnonymousFun   = builtinId "BIAnonymousFun"
builtinOpId BIFieldInit      = builtinId "BIFieldInit"
builtinOpId BIUniqueArrayLit = builtinId "BIUniqueArrayLit"
builtinOpId BIImmArrayLit    = builtinId "BIImmArrayLit"
builtinOpId BIObjectLit      = builtinId "BIObjectLit"
builtinOpId BISetProp        = builtinId "BISetProp"
builtinOpId BIForInKeys      = builtinId "BIForInKeys"
builtinOpId BICtorExit       = builtinId "BICtorExit"
builtinOpId BINumArgs        = builtinId "BINumArgs"
builtinOpId BITruthy         = builtinId "BITruthy"
builtinOpId BICondExpr       = builtinId "BICondExpr"
builtinOpId BICastExpr       = builtinId "BICastExpr"
builtinOpId BISuper          = builtinId "BISuper"
builtinOpId BISuperVar       = builtinId "BISuperVar"
builtinOpId BICtor           = builtinId "BICtor"
builtinOpId BIThis           = mkId "this"

infixOpId OpLT              = builtinId "OpLT"
infixOpId OpLEq             = builtinId "OpLEq"
infixOpId OpGT              = builtinId "OpGT"
infixOpId OpGEq             = builtinId "OpGEq"
infixOpId OpEq              = builtinId "OpEq"
infixOpId OpStrictEq        = builtinId "OpSEq"
infixOpId OpNEq             = builtinId "OpNEq"
infixOpId OpStrictNEq       = builtinId "OpSNEq"
infixOpId OpLAnd            = builtinId "OpLAnd"
infixOpId OpLOr             = builtinId "OpLOr"
infixOpId OpSub             = builtinId "OpSub"
infixOpId OpAdd             = builtinId "OpAdd"
infixOpId OpMul             = builtinId "OpMul"
infixOpId OpDiv             = builtinId "OpDiv"
infixOpId OpMod             = builtinId "OpMod"
infixOpId OpInstanceof      = builtinId "OpInstanceof"
infixOpId OpBOr             = builtinId "OpBOr"
infixOpId OpBXor            = builtinId "OpBXor"
infixOpId OpBAnd            = builtinId "OpBAnd"
infixOpId OpIn              = builtinId "OpIn"
infixOpId OpLShift          = builtinId "OpLShift"
infixOpId OpSpRShift        = builtinId "OpSpRShift"
infixOpId OpZfRShift        = builtinId "OpZfRShift"

prefixOpId PrefixMinus      = builtinId "PrefixMinus"
prefixOpId PrefixPlus       = builtinId "PrefixPlus"
prefixOpId PrefixLNot       = builtinId "PrefixLNot"
prefixOpId PrefixTypeof     = builtinId "PrefixTypeof"
prefixOpId PrefixBNot       = builtinId "PrefixBNot"
prefixOpId o                = errorstar $ "prefixOpId: Cannot handle: " ++ show o

mkId      = Id F.dummySpan
builtinId = mkId . ("builtin_" ++)


-- | BitVectors

bitVectorValue ('0':x) = Just $ exprReft $ BV.Bv BV.S32  $ "#" ++ x
bitVectorValue _       = Nothing

