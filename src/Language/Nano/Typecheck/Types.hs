-- | Global type definitions for Refinement Type Checker

{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE DeriveFoldable            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE IncoherentInstances       #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.Nano.Typecheck.Types (

  -- * Type operations
    toType, ofType, rTop, strengthen 

  , PPR, ExprReftable(..) 

  -- * Constructing Types
  , mkUnion, mkFun, mkAll, mkAnd, mkInitFldTy, mkTCons

  -- * Deconstructing Types
  , bkFun, bkFuns, bkAll, bkAnd, bkUnion
  
  , orUndef, funTys, padUndefineds
  
  , rTypeR

  , btvToTV

  , renameBinds

  -- * Mutability primitives
  , tMut, tUqMut, tImm, tIM, tRO, trMut, trImm, trIM, trRO
  , isMut, isImm, isUM

  -- * Primitive Types

  --   # Constructors
  , tNum, tBV32, tBool, tString, tTop, tVoid, tErr, tVar, tUndef, tNull, tObject

  --   # Tests
  , isTUndef, isTUnion, isTStr, isTBool, isBvEnum, isTVar, maybeTObj
  , isExpandable, isTNull, isTVoid, isTFun, isArr
  
  --   # Operations
  , orNull
  
  -- * Refinements
  , fTop

  -- * Element ops 
  , requiredMember, optionalMember

  -- * Operator Types
  , infixOpId, prefixOpId, builtinOpId, arrayLitTy, objLitTy, setPropTy, localTy, finalizeTy

  -- * Builtin: Binders
  , mkId, argId, mkArgTy, returnTy

  -- * BitVector
  , bitVectorValue

  ) where 

import           Data.Data
import           Data.Default
import           Data.Hashable
import           Data.Either                    (partitionEithers)
import qualified Data.List                      as L
import           Data.Maybe                     (fromMaybe, isJust, fromJust)
import           Data.Monoid                    hiding ((<>))            
import qualified Data.Map.Strict                as M
import           Data.Typeable                  ()
import           Language.Nano.Syntax 
import           Language.Nano.Syntax.PrettyPrint
import qualified Language.Nano.Env              as E
import           Language.Nano.Misc
import           Language.Nano.Types
import           Language.Nano.Errors
import           Language.Nano.Locations
import           Language.Nano.Names

import qualified Language.Fixpoint.Types        as F
import qualified Language.Fixpoint.Bitvector    as BV
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Errors
import           Language.Fixpoint.PrettyPrint
import           Text.PrettyPrint.HughesPJ 
import           Text.Parsec.Pos                    (initialPos)

import           Control.Applicative            hiding (empty)
import           Control.Exception              (throw)

-- import           Debug.Trace (trace)


type PPR  r = (ExprReftable F.Symbol r, ExprReftable Int r, PP r, F.Reftable r, Data r)

---------------------------------------------------------------------
-- | Primitive Types
---------------------------------------------------------------------

mkAName p s = QN AK_ (srcPos dummySpan) p (F.symbol s)
mkRName p s = QN RK_ (srcPos dummySpan) p (F.symbol s)

mkPrimTy s m = TRef (Gen (mkAName [] s) [m]) fTop

tObject = mkPrimTy "Object" tImm


---------------------------------------------------------------------
-- | Mutability
---------------------------------------------------------------------

instance Default Mutability where
  def = mkMut "Immutable"

mkMut s    = TRef (Gen (mkAName [] s) []) fTop
mkRelMut s = TRef (Gen (mkRName [] s) []) fTop

tMut    = mkMut "Mutable"
tUqMut  = mkMut "UniqueMutable"
tImm    = mkMut "Immutable"
tRO     = mkMut "ReadOnly"
tIM     = mkMut "InheritedMut"

trMut   = mkRelMut "Mutable"
trImm   = mkRelMut "Immutable"
trRO    = mkRelMut "ReadOnly"
trIM    = mkRelMut "InheritedMut"

typeName (Gen (QN _ _ _ s) _)  = s

isNamed s t | TRef n _ <- t, typeName n == F.symbol s = True | otherwise = False

isMut = isNamed "Mutable"
isImm = isNamed "Immutable"
isUM  = isNamed "UniqueMutable"


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


funTys l f xs ft | Just ts <- bkFuns ft
                 = case partitionEithers [funTy l xs t | t <- ts] of 
                     ([], fts) -> Right $ zip ([0..] :: [Int]) fts
                     (_ , _  ) -> Left  $ errorArgMismatch (srcPos l)
                 | otherwise
                 = Left $ errorNonFunction (srcPos l) f ft 


funTy l xs (αs, yts, t) =
  case padUndefineds xs yts of
    Nothing   -> Left  $ errorArgMismatch (srcPos l)
    Just yts' -> Right $ (αs, ts', F.subst su t)
                   where
                    (su, ts') = renameBinds yts' xs 

padUndefineds xs yts
  | nyts <= nxs = Just $ yts ++ xundefs
  | otherwise   = Nothing
  where
    nyts        = length yts
    nxs         = length xs
    xundefs     = [B (F.symbol x') tUndef | x' <- snd $ splitAt nyts xs ]

renameBinds yts xs    = (su, [F.subst su ty | B _ ty <- yts])
  where 
    su                = F.mkSubst suL 
    suL               = safeZipWith "renameBinds" fSub yts xs 
    fSub yt x         = (b_sym yt, F.eVar x)

bkFuns :: RTypeQ q r -> Maybe [([BTVarQ q r], [BindQ q r], RTypeQ q r)]
bkFuns = sequence . fmap bkFun . bkAnd

bkFun :: RTypeQ q r -> Maybe ([BTVarQ q r], [BindQ q r], RTypeQ q r)
bkFun t = 
  do  (xts, t'')     <- bkArr t'
      return          $ (αs, xts, t'')
  where
    (αs, t')          = bkAll t
 
mkFun :: F.Reftable r  => ([BTVarQ q r], [BindQ q r], RTypeQ q r) -> RTypeQ q r
mkFun (αs, bs, rt)    = mkAll αs (TFun bs rt fTop)
         
bkArr (TFun xts t _)  = Just (xts, t)
bkArr _               = Nothing

mkAll αs t            = mkAnd $ go (reverse αs) <$> bkAnd t
  where
    go (α:αs) t       = go αs (TAll α t)
    go []     t       = t

bkAll :: RTypeQ q r -> ([BTVarQ q r], RTypeQ q r)
bkAll t               = go [] t
  where 
    go αs (TAll α t)  = go (α : αs) t
    go αs t           = (reverse αs, t)

bkAnd :: RTypeQ q r -> [RTypeQ q r]
bkAnd (TAnd ts) = ts
bkAnd t         = [t]

mkAnd [t]       = t
mkAnd ts        = TAnd ts

mapAnd f t      = mkAnd $ f <$> bkAnd t

mkTCons         = (`TObj` fTop)

----------------------------------------------------------------------------------------
mkUnion :: (F.Reftable r) => [RType r] -> RType r
----------------------------------------------------------------------------------------
mkUnion [ ] = TPrim TBot fTop
mkUnion [t] = t
mkUnion ts  = flattenUnions $ TOr ts

----------------------------------------------------------------------------------------
bkUnion :: RType r -> [RType r]
----------------------------------------------------------------------------------------
bkUnion (TOr ts) = concatMap bkUnion ts
bkUnion t        = [t]

----------------------------------------------------------------------------------------
flattenUnions :: RType r -> RType r
----------------------------------------------------------------------------------------
flattenUnions t@(TOr _) = TOr $ bkUnion t
flattenUnions t         = t

----------------------------------------------------------------------------------------
orUndef :: F.Reftable r => RType r -> RType r
----------------------------------------------------------------------------------------
orUndef t   | any isTUndef ts = t 
            | otherwise = mkUnion $ tUndef:ts
  where ts  = bkUnion t 


-- | Strengthen the top-level refinement
----------------------------------------------------------------------------------------
strengthen :: F.Reftable r  => RTypeQ q r -> r -> RTypeQ q r
----------------------------------------------------------------------------------------
strengthen (TPrim c r)   r' = TPrim c   $ r' `F.meet` r
strengthen (TVar α r)    r' = TVar α    $ r' `F.meet` r 
strengthen (TOr ts)      r' = TOr       $ (`strengthen` r') <$> ts
strengthen (TRef n r)    r' = TRef n    $ r' `F.meet` r 
strengthen (TObj m r)    r' = TObj m    $ r' `F.meet` r 
strengthen (TFun xs t r) r' = TFun xs t $ r' `F.meet` r
strengthen t _              = t    -- TAnd, TType, TAll, TExp fall through

-- NOTE: r' is the OLD refinement. 
--       We want to preserve its VV binder as it "escapes", 
--       e.g. function types. Sigh. Should have used a separate function binder.


-- -- XXX : add to env
-- ----------------------------------------------------------------------------------------
-- bound :: F.Reftable r       => RType r -> RType r 
-- ----------------------------------------------------------------------------------------
-- bound (TVar  (TV _ c _ ) _) = c
-- bound (TOr ts)              = TOr $ bound <$> ts
-- boudn t                     = t


----------------------------------------------------------------------------------------
-- | Predicates on Types 
----------------------------------------------------------------------------------------

isPrim c t | TPrim c _ <- t = True | otherwise = False

isTUndef  = isPrim TUndefined
isTNull   = isPrim TNull
isTVoid   = isPrim TVoid
isTStr    = isPrim TString
isTNum    = isPrim TNumber
isTBool   = isPrim TBoolean
isBV32    = isPrim TBV32

isTVar   t | TVar _ _ <- t = True | otherwise = False
isTUnion t | TOr  _   <- t = True | otherwise = False

maybeTObj (TRef _ _) = True
maybeTObj (TObj _ _) = True
maybeTObj (TType _ ) = True
maybeTObj (TAll _ t) = maybeTObj t
maybeTObj (TOr ts)   = any maybeTObj ts
maybeTObj _          = False

isExpandable v       = maybeTObj v || isTNum v || isTBool v || isTStr v

isTFun (TFun _ _ _)  = True
isTFun (TAnd ts)     = all isTFun ts
isTFun (TAll _ t)    = isTFun t
isTFun _             = False

isArr t | TRef x _ <- t = F.symbol x == F.symbol "Array" | otherwise = False

orNull t@(TOr ts) | any isTNull ts = t
                  | otherwise      = TOr $ tNull:ts
orNull t          | isTNull t      = t
                  | otherwise      = TOr [tNull,t]

----------------------------------------------------------------------------------
rTypeR' :: F.Reftable r => RType r -> Maybe r
----------------------------------------------------------------------------------
rTypeR' (TPrim _ r)  = Just r
rTypeR' (TVar _ r)   = Just r
rTypeR' (TOr _ )     = Just fTop
rTypeR' (TFun _ _ r) = Just r
rTypeR' (TRef _ r)   = Just r
rTypeR' (TObj _ r)   = Just r
rTypeR' (TType _)    = Just fTop
rTypeR' (TAnd _ )    = Nothing 
rTypeR' (TAll _ _)   = Nothing 
rTypeR' (TExp _)     = Nothing 

----------------------------------------------------------------------------------
rTypeR :: (PP r, F.Reftable r) => RType r -> r
----------------------------------------------------------------------------------
rTypeR t | Just r <- rTypeR' t = r
         | otherwise           = errorstar $ "Unimplemented: rTypeR" ++ ppshow t
 

-- | Modifier predicates
--
optionalMember ms = Optional `elem` ms
requiredMember    = not . optionalMember

-- subtypeable e = not (isConstr e)

isBvEnum              = all hex . map snd . E.envToList . e_mapping
  where
    hex (HexLit _ _ ) = True 
    hex _             = False


----------------------------------------------------------------------------------
-- | Pretty Printer Instances
----------------------------------------------------------------------------------

angles p = char '<' <> p <> char '>'
ppHMap p = map (p . snd) . M.toList 

instance PP Bool where
  pp True   = text "True"
  pp False  = text "False"

instance PP () where 
  pp _ = text ""

instance PP a => PP (Maybe a) where 
  pp = maybe (text "Nothing") pp 

instance PP Char where
  pp = char

instance (F.Reftable r, PP r) => PP (RTypeQ q r) where
  pp (TPrim c r)     = F.ppTy r $ pp c
  pp (TVar α r)      = F.ppTy r $ (text "#" <> pp α)
  pp (TOr ts)        = ppArgs id (text " +") ts
  pp (TAnd ts)       = vcat [text "/\\" <+> pp t | t <- ts]
  pp (TRef t r)      = F.ppTy r $ pp t
  pp (TObj ms r)     = F.ppTy r $ braces $ pp ms
  pp (TType t)       = text "typeof" <+> pp t
  pp t@(TAll _ _)    = ppArgs angles comma αs <> text "." <+> pp t' where (αs, t') = bkAll t
  pp (TFun xts t _)  = ppArgs parens comma xts <+> text "=>" <+> pp t  
  pp (TExp e)        = pprint e 

instance (F.Reftable r, PP r) => PP (TypeMembersQ q r) where
  pp (TM ps ms cs ctors sidx nidx) =  ppProp ps <+> ppMeth ms <+> ppCall cs 
                                  <+> ppCtor ctors <+> ppIndexer sidx <+> ppIndexer nidx

ppProp    = undefined
ppMeth    = undefined
ppCall    = undefined
ppCtor    = undefined
ppIndexer = undefined

instance (F.Reftable r, PP r) => PP (TGenQ q r) where
  pp (Gen x ts) = pp x <> ppArgs angles comma ts

instance (F.Reftable r, PP r) => PP (BTGenQ q r) where
  pp (BGen x ts) = pp x <> ppArgs angles comma ts

instance PP TVar where 
  pp = pprint . F.symbol

instance (F.Reftable r, PP r) => PP (BTVarQ q r) where 
  pp (BTV v t _) = pprint v <+> text "<:" <+> pp t

instance PP TPrim where
  pp TString     = text "string"
  pp (TStrLit s) = text "\"" <> text s <> text "\"" 
  pp TNumber     = text "number" 
  pp TBoolean    = text "boolean" 
  pp TBV32       = text "bitvector32" 
  pp TVoid       = text "void" 
  pp TUndefined  = text "undefined"
  pp TNull       = text "null"
  pp TBot        = text "_|_"
  pp TTop        = text " T "

instance (PP r, F.Reftable r) => PP (BindQ q r) where 
  pp (B x t)          = pp x <> colon <> pp t 

instance (PP s, PP t) => PP (M.Map s t) where
  pp m = vcat $ pp <$> M.toList m

instance PP Assignability where
  pp Ambient      = text "Ambient"
  pp WriteLocal   = text "WriteLocal"
  pp ForeignLocal = text "ForeignLocal"
  pp WriteGlobal  = text "WriteGlobal"
  pp ReturnVar    = text "ReturnVar"

instance (PP r, F.Reftable r) => PP (TypeDeclQ q r) where
  pp (TD k n h ts) = pp k <+> pp n <+> ppHeritage h <+> lbrace $+$ nest 2 (pp ts) $+$ rbrace

instance PP TypeDeclKind where
  pp InterfaceKind  = text "interface"
  pp ClassKind      = text "class"

ppHeritage (es,is) = ppExtends es <+> ppImplements is

ppExtends Nothing  = text ""
ppExtends (Just n) = text "extends" <+> pp n

ppImplements [] = text ""
ppImplements ts = text "implements" <+> intersperse comma (pp <$> ts)

mutSym (TRef n _) | s == F.symbol "Mutable"       = Just "_MU_"
                  | s == F.symbol "UniqueMutable" = Just "_UM_"
                  | s == F.symbol "Immutable"     = Just "_IM_"
                  | s == F.symbol "ReadOnly"      = Just "_RO_"
                  | s == F.symbol "AssignsFields" = Just "_AF"
  where s = F.symbol n
mutSym _ = Nothing

ppMut t@TVar{} = pp t
ppMut t        | Just s <- mutSym t = pp s
               | otherwise          = pp "_??_"

instance PP EnumDef where
  pp (EnumDef n m) = pp n <+> braces (pp m)

instance (F.Reftable r, PP r) => PP (VarInfo q r) where 
  pp (VI _ _ t) = pp t
 
instance (PP r, F.Reftable r) => PP (ModuleDef r) where
  pp (ModuleDef vars tys enums path) =  
          text "==================="
      $+$ text "module" <+> pp path 
      $+$ text "==================="
      $+$ text "Variables" 
      $+$ text "----------"
      $+$ braces (pp vars)
      $+$ text "-----"
      $+$ text "Types" 
      $+$ text "-----"
      $+$ pp tys
      $+$ text "-----"
      $+$ text "Enums" 
      $+$ text "-----"
      $+$ pp enums


-----------------------------------------------------------------------
-- | Primitive / Base Types Constructors
-----------------------------------------------------------------------

btvToTV  (BTV s _ l) = TV s l

tVar :: (F.Reftable r) => TVar -> RType r
tVar = (`TVar` fTop) 

tNum, tBV32, tBool, tString, tTop, tVoid, tBot, tUndef, tNull, tErr :: F.Reftable r => RTypeQ q r
tPrim   = (`TPrim` fTop)
tNum    = tPrim TNumber
tBV32   = tPrim TBV32 
tBool   = tPrim TBoolean
tString = tPrim TString
tTop    = tPrim TTop  
tVoid   = tPrim TVoid
tBot    = tPrim TBot
tUndef  = tPrim TUndefined
tNull   = tPrim TNull
tErr    = tVoid


---------------------------------------------------------------------------------
-- | Array literal types
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
arrayLitTy :: (F.Subable (RType r), IsLocated a) => a -> Int -> RType r -> RType r
---------------------------------------------------------------------------------
arrayLitTy l n (TAll μ (TAll α (TFun [xt] t r))) = mkAll [μ,α] $ TFun αs rt r
  where αs       = arrayLitBinds n xt
        rt       = F.subst1 t (F.symbol $ builtinOpId BINumArgs, F.expr (n::Int))
arrayLitTy l n _ = die $ bug (srcPos l) $ "Bad Type for ArrayLit Constructor"
      
arrayLitBinds n (B x t) = [B (x_ i) t | i <- [1..n]] 
  where xs       = F.symbolString x
        x_       = F.symbol . (xs ++) . show 


---------------------------------------------------------------------------------
-- | Object literal types
---------------------------------------------------------------------------------

-- FIXME: Avoid capture
freshBTV l s b n  = (bv,t)
  where 
    i             = F.intSymbol s n
    bv            = BTV i b (srcPos l)
    v             = TV i (srcPos l)
    t             = TVar v ()

--------------------------------------------------------------------------------------------
objLitTy         :: (F.Reftable r, IsLocated a) => a -> [Prop a] -> RType r
--------------------------------------------------------------------------------------------
objLitTy l ps     = mkFun (vs, bs, undefined) -- rt)
  where
    vs            = mvs ++ avs
    bs            = [B s (ofType a) | (s,a) <- zip ss ats ]
    rt            = TObj tms fTop

    tms           = TM (F.fromListSEnv [ (s, FI [] m a) | (s,m,a) <- zip3 ss mts ats ])  -- Props
                       mempty -- meths
                       mempty -- calls
                       mempty -- ctors
                       mempty -- indexer
                       mempty -- indexer

    (mvs, mts)    = unzip $ map (freshBTV l mSym Nothing) [1..length ps]  -- field mutability
    (avs, ats)    = unzip $ map (freshBTV l aSym Nothing) [1..length ps]  -- field type vars

    ss            = [F.symbol p | p <- ps]
    mSym          = F.symbol "M"
    aSym          = F.symbol "A"

lenId l           = Id l "length" 
argId l           = Id l "arguments"

instance F.Symbolic (LValue a) where
  symbol (LVar _ x) = F.symbol x
  symbol lv         = convertError "F.Symbol" lv


instance F.Symbolic (Prop a) where 
  symbol (PropId _ (Id _ x)) = F.symbol x -- TODO $ "propId_"     ++ x
  symbol (PropString _ s)    = F.symbol $ "propString_" ++ s
  symbol (PropNum _ n)       = F.symbol $ "propNum_"    ++ show n


-- | @argBind@ returns a dummy type binding `arguments :: T `
--   where T is an object literal containing the non-undefined `ts`. 
--
--------------------------------------------------------------------------------------------
mkArgTy :: (F.Reftable r, IsLocated l) => l -> [RType r] -> RType r
--------------------------------------------------------------------------------------------
mkArgTy l ts   = immObjectLitTy l [pLen] [tLen]
  where
    ts'        = take k ts
    ps'        = PropNum l . toInteger <$> [0 .. k-1]
    pLen       = PropId l $ lenId l
    tLen       = tNum `strengthen` rLen
    rLen       = F.ofReft $ F.uexprReft k
    k          = fromMaybe (length ts) $ L.findIndex isTUndef ts
   
--------------------------------------------------------------------------------------------
immObjectLitTy :: (F.Reftable r, IsLocated l) => l -> [Prop l] -> [RType r] -> RType r
--------------------------------------------------------------------------------------------
immObjectLitTy l ps ts  | length ps == length ts 
                        = TObj elts fTop
                        | otherwise
                        = die $ bug (srcPos l) $ "Mismatched args for immObjectLit"
  where
    elts                = fromFieldList [ (F.symbol p, FI [] tImm t) | (p,t) <- safeZip "immObjectLitTy" ps ts ]

-- | setProp<A, M extends Mutable>(o: { f[M]: A }, x: A) => A
--
--------------------------------------------------------------------------------------------
setPropTy :: (F.Reftable r, IsLocated l) => l -> F.Symbol -> RType r -> RType r
--------------------------------------------------------------------------------------------
setPropTy l f ty = mkAll [bvt, bvm] t
  where
    ft           = TFun [b1, b2] t fTop
    b1           = B (F.symbol "o") $ TObj (fromFieldList [(f, FI [Optional] m t)]) fTop
    b2           = B (F.symbol "x") $ t
    m            = toTTV bvm :: F.Reftable r => RType r
    t            = toTTV bvt
    bvt          = BTV (F.symbol "A") Nothing     def
    bvm          = BTV (F.symbol "M") (Just tMut) def :: F.Reftable r => BTVar r 
    toTTV        :: F.Reftable r => BTVar r -> RType r
    toTTV        = (`TVar` fTop) . btvToTV

--------------------------------------------------------------------------------------------
fromFieldList :: (F.Symbolic s) => [(s, FieldInfo r)] -> TypeMembers r
--------------------------------------------------------------------------------------------
fromFieldList f = TM (F.fromListSEnv $ mapFst F.symbol <$> f) mempty Nothing Nothing Nothing Nothing

---------------------------------------------------------------------------------
returnTy :: (PP r, F.Reftable r) => RType r -> Bool -> RType r
---------------------------------------------------------------------------------
returnTy t True  = mkFun ([], [B (F.symbol "r") t], tVoid)
returnTy _ False = mkFun ([], [], tVoid)

---------------------------------------------------------------------------------
finalizeTy :: (PP r, F.Reftable r, ExprReftable F.Symbol r) => RType r -> RType r
---------------------------------------------------------------------------------
finalizeTy t  | TRef (Gen x (m:ts)) _ <- t, isUM m 
              = mkFun ([mOut], [B sx t], TRef (Gen x (tOut:ts)) (uexprReft sx))
              | otherwise
              = mkFun ([mV], [B (F.symbol "x") tV], tV)
  where 
    sx        = F.symbol "x_"
    tOut      = tVar $ btvToTV mOut
    mOut      = BTV (F.symbol "N") Nothing def
    tV        = tVar $ btvToTV mV
    mV        = BTV (F.symbol "V") Nothing def

localTy t = mkFun ([], [B sx t], t `strengthen` uexprReft sx)
  where 
    sx    = F.symbol "x_"

mkInitFldTy t = mkFun ([], [B (F.symbol "f") t], tVoid)

 
builtinOpId BIUndefined     = builtinId "BIUndefined"
builtinOpId BIBracketRef    = builtinId "BIBracketRef"
builtinOpId BIBracketAssign = builtinId "BIBracketAssign"
builtinOpId BIArrayLit      = builtinId "BIArrayLit"
builtinOpId BIObjectLit     = builtinId "BIObjectLit"
builtinOpId BISetProp       = builtinId "BISetProp"
builtinOpId BIForInKeys     = builtinId "BIForInKeys"
builtinOpId BICtorExit      = builtinId "BICtorExit"
builtinOpId BINumArgs       = builtinId "BINumArgs"
builtinOpId BITruthy        = builtinId "BITruthy"
builtinOpId BICondExpr      = builtinId "BICondExpr"
builtinOpId BICastExpr      = builtinId "BICastExpr"
builtinOpId BISuper         = builtinId "BISuper"
builtinOpId BISuperVar      = builtinId "BISuperVar"
builtinOpId BICtor          = builtinId "BICtor"
builtinOpId BIThis          = mkId "this"

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
prefixOpId o                = errorstar $ "prefixOpId: Cannot handle: " ++ ppshow o

mkId      = Id (initialPos "") 
builtinId = mkId . ("builtin_" ++)


-- | BitVectors

bitVectorValue ('0':x) = Just $ exprReft $ BV.Bv BV.S32  $ "#" ++ x
bitVectorValue _       = Nothing


