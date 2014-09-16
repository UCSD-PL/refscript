-- | Global type definitions for Refinement Type Checker

{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE DeriveFoldable            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE ConstraintKinds           #-}
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

  -- * Predicates on Types 
  , isTop, isNull, isVoid, isTNum, isUndef, isUnion

  -- * Constructing Types
  , mkUnion, mkFun, mkAll, mkAnd, mkEltFunTy, mkInitFldTy

  -- * Deconstructing Types
  , bkFun, bkFuns, bkAll, bkAnd, bkUnion, funTys -- , methTys
  
  , rUnion, rTypeR, setRTypeR

  , renameBinds

  -- * Mutability primitives
  , t_mutable, t_immutable, t_anyMutability, t_inheritedMut, t_readOnly, combMut 
  , isMutable, isImmutable -- , isAnyMut, isMutabilityType, variance, varianceTDef

  -- * Primitive Types
  , tInt, tBool, tString, tTop, tVoid, tErr, tFunErr, tVar, tUndef, tNull
  , tAnd, isTVar, isTObj, isConstr, isTFun, fTop, orNull
  -- , isArr , tArr, rtArr

  -- * Element ops 
  , sameBinder, eltType, isStaticSig, nonStaticSig, nonConstrElt, mutability, baseType
  , isMethodSig, isFieldSig, setThisBinding, remThisBinding, mapEltM

  -- * Operator Types
  , infixOpId 
  , prefixOpId 
  , builtinOpId 
  , arrayLitTy
  , objLitTy
  , setPropTy

    
  -- * Builtin: Binders
  , argId
  , argTy
  , returnTy


  ) where 

import           Text.Printf
import           Data.Default
import           Data.Hashable
import           Data.Either                    (partitionEithers)
import qualified Data.List                      as L
import           Data.Maybe                     (fromMaybe)
import           Data.Monoid                    hiding ((<>))            
import qualified Data.HashMap.Strict            as M
import           Data.Typeable                  ()
import           Language.ECMAScript3.Syntax 
import           Language.ECMAScript3.PrettyPrint
import           Language.Nano.Misc
import           Language.Nano.Types
import           Language.Nano.Errors
import           Language.Nano.Env
import           Language.Nano.Locations
import           Language.Nano.Names

import qualified Language.Fixpoint.Types        as F
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Errors
import           Language.Fixpoint.PrettyPrint
import           Text.PrettyPrint.HughesPJ 
import           Text.Parsec.Pos                    (initialPos)

import           Control.Applicative            hiding (empty)

-- import           Debug.Trace (trace)


type PPR  r = (PP r, F.Reftable r)


---------------------------------------------------------------------
-- | Mutability
---------------------------------------------------------------------

-- validMutNames = F.symbol <$> ["ReadOnly", "Mutable", "Immutable", "AnyMutability"]

mkMut s = TApp (TRef $ RN $ QName (srcPos dummySpan) [] (F.symbol s)) [] ()

instance Default Mutability where
  def = mkMut "Immutable"

t_mutable       = mkMut "Mutable"
t_immutable     = mkMut "Immutable"
t_anyMutability = mkMut "AnyMutability"
t_readOnly      = mkMut "ReadOnly"
t_inheritedMut  = mkMut "InheritedMut"


-- isMutabilityType (TApp (TRef (QN [] s)) _ _) = s `elem` validMutNames
-- isMutabilityType _                           = False
-- 
isMutable        (TApp (TRef (RN (QName _ [] s))) _ _) = s == F.symbol "Mutable"
isMutable _                                            = False
 
isImmutable      (TApp (TRef (RN (QName _ [] s))) _ _) = s == F.symbol "Immutable"
isImmutable _                                          = False
-- 
-- isAnyMut         (TApp (TRef (QN [] s)) _ _) = s == F.symbol "AnyMutability"
-- isAnyMut _                                   = False
-- 
-- isReadOnly       (TApp (TRef (QN [] s)) _ _) = s == F.symbol "ReadOnly"
-- isReadOnly _                                 = False
-- 
-- isInheritedMut   (TApp (TRef (QN [] s)) _ _) = s == F.symbol "InheritedMut"
-- isInheritedMut _                             = False

-- 
-- Is this not the common ancestor ?
--
combMut _ μf | isMutable μf                 = μf
combMut μ _  | otherwise                    = μ


-- | Variance: true if v is in a positive position in t
--
--   FIXME: implement these
--
--    variance      :: TVar -> RType r -> Bool
--
--    varianceTDef  :: IfaceDef r -> [Bool]




---------------------------------------------------------------------
-- | Refinement manipulation
---------------------------------------------------------------------

-- | "pure" top-refinement
fTop :: (F.Reftable r) => r
fTop = mempty

-- | Stripping out Refinements 
toType :: RType a -> Type
toType = fmap (const ())
  
-- | Adding in Refinements
ofType :: (F.Reftable r) => Type -> RType r
ofType = fmap (const fTop)

-- | Top-up refinemnt
rTop = ofType . toType

funTys l f xs ft 
  = case bkFuns ft of
      Nothing -> Left $ errorNonFunction (srcPos l) f ft 
      Just ts -> 
        case partitionEithers [funTy l xs t | t <- ts] of 
          ([], fts) -> Right $ zip ([0..] :: [Int]) fts
          (_ , _  ) -> Left  $ errorArgMismatch (srcPos l)

-- methTys l f xs i (m,ft0)
--   = case remThisBinding ft0 of
--       Nothing        -> Left  $ errorNonFunction (srcPos l) f ft0 
--       Just (αs,bs,t) -> Right $ (i,m,αs,bs,t)

--   = case bkFuns ft0 of
--       Nothing -> Left $ errorNonFunction (srcPos l) f ft0 
--       Just ts -> 
--         case partitionEithers [funTy l xs t | t <- ts] of 
--           ([], fts) -> Right $ zip3 ([0..] :: [Int]) (repeat m) fts
--           (_ , _  ) -> Left  $ errorArgMismatch (srcPos l)


-- NEW
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
    su                = F.mkSubst $ safeZipWith "renameBinds" fSub yts xs 
    fSub yt x         = (b_sym yt, F.eVar x)


bkFuns :: RType r -> Maybe [([TVar], [Bind r], RType r)]
bkFuns = sequence . fmap bkFun . bkAnd 

bkFun :: RType r -> Maybe ([TVar], [Bind r], RType r)
bkFun t = do let (αs, t') = bkAll t
             (xts, t'')  <- bkArr t'
             return        (αs, xts, t'')

mkFun :: (F.Reftable r) => ([TVar], [Bind r], RType r) -> RType r
mkFun ([], bs, rt) = TFun bs rt fTop 
mkFun (αs, bs, rt) = mkAll αs (TFun bs rt fTop)
         
bkArr (TFun xts t _) = Just (xts, t)
bkArr _              = Nothing

mkAll αs t           = go (reverse αs) t
  where
    go (α:αs) t      = go αs (TAll α t)
    go []     t      = t

bkAll                :: RType a -> ([TVar], RType a)
bkAll t              = go [] t
  where 
    go αs (TAll α t) = go (α : αs) t
    go αs t          = (reverse αs, t)

bkAnd                :: RType r -> [RType r]
bkAnd (TAnd ts)      = ts
bkAnd t              = [t]

mkAnd [t]            = t
mkAnd ts             = TAnd ts

mapAnd f t           = mkAnd $ f <$> bkAnd t


----------------------------------------------------------------------------------------
mkUnion :: (F.Reftable r) => [RType r] -> RType r
----------------------------------------------------------------------------------------
mkUnion [ ] = tErr
mkUnion [t] = t             
mkUnion ts  = TApp TUn ts fTop

----------------------------------------------------------------------------------------
bkUnion :: RType r -> [RType r]
----------------------------------------------------------------------------------------
bkUnion (TApp TUn xs _) = xs
bkUnion t               = [t]


-- | Strengthen the top-level refinement
----------------------------------------------------------------------------------------
strengthen                   :: F.Reftable r => RType r -> r -> RType r
----------------------------------------------------------------------------------------
strengthen (TApp c ts r) r'  = TApp c ts  $ r' `F.meet` r 
strengthen (TCons ts m r) r' = TCons ts m $ r' `F.meet` r 
strengthen (TVar α r)    r'  = TVar α     $ r' `F.meet` r 
strengthen t _               = t                         

-- NOTE: r' is the OLD refinement. 
--       We want to preserve its VV binder as it "escapes", 
--       e.g. function types. Sigh. Should have used a separate function binder.


-- | Strengthen the refinement of a type @t2@ deeply, using the 
-- refinements of an equivalent (having the same raw version) 
-- type @t1@.


----------------------------------------------------------------------------------------
-- | Predicates on Types 
----------------------------------------------------------------------------------------

-- | Top-level Top (any) check
isTop :: RType r -> Bool
isTop (TApp TTop _ _)   = True 
isTop (TApp TUn  ts _ ) = any isTop ts
isTop _                 = False

isUndef :: RType r -> Bool
isUndef (TApp TUndef _ _)   = True 
isUndef _                   = False

isNull :: RType r -> Bool
isNull (TApp TNull _ _)   = True 
isNull _                  = False

isVoid :: RType r -> Bool
isVoid (TApp TVoid _ _)   = True 
isVoid _                  = False

isTObj (TApp (TRef _) _ _) = True
isTObj (TCons _ _ _)       = True
isTObj _                   = False

isTNum (TApp TInt _ _ )    = True
isTNum _                   = False

isConstr (ConsSig _)  = True
isConstr _            = False

isUnion :: RType r -> Bool
isUnion (TApp TUn _ _) = True           -- top-level union
isUnion _              = False

-- Get the top-level refinement for unions - use Top (True) otherwise
rUnion               :: F.Reftable r => RType r -> r
rUnion (TApp TUn _ r) = r
rUnion _              = fTop

-- Get the top-level refinement 
rTypeR               :: F.Reftable r => RType r -> r
rTypeR (TApp _ _ r ) = r
rTypeR (TVar _ r   ) = r
rTypeR (TFun _ _ r ) = r
rTypeR (TCons _ _ r) = r
rTypeR (TModule _  ) = fTop
rTypeR (TClass _   ) = fTop
rTypeR (TAll _ _   ) = errorstar "Unimplemented: rTypeR - TAll"
rTypeR (TAnd _ )     = errorstar "Unimplemented: rTypeR - TAnd"
rTypeR (TExp _)      = errorstar "Unimplemented: rTypeR - TExp"

-- Set the top-level refinement (wherever applies)
setRTypeR :: RType r -> r -> RType r
setRTypeR (TApp c ts _   ) r = TApp c ts r
setRTypeR (TVar v _      ) r = TVar v r
setRTypeR (TFun xts ot _ ) r = TFun xts ot r
setRTypeR (TCons x y _)    r = TCons x y r  
setRTypeR t                _ = t


-- mapElt f (CallSig t)      = CallSig (f t)
-- mapElt f (ConsSig t)      = ConsSig (f t)
-- mapElt f (IndexSig i b t) = IndexSig i b ( f t)
-- mapElt f (FieldSig x m t) = FieldSig x m (f t)
-- mapElt f (MethSig  x m t) = MethSig x m (f t)
-- mapElt f (StatSig x m t)  = StatSig x m (f t)

mapEltM f (CallSig t)        = CallSig <$> f t
mapEltM f (ConsSig t)        = ConsSig <$> f t
mapEltM f (IndexSig i b t)   = IndexSig i b <$> f t
mapEltM f (FieldSig x m t)   = FieldSig x m <$> f t
mapEltM f (MethSig  x m t)   = MethSig x m <$> f t
mapEltM f (StatSig x m t)    = StatSig x m <$> f t
 

isStaticSig (StatSig _ _ _)   = True
isStaticSig _                 = False

nonStaticSig = not . isStaticSig

nonConstrElt = not . isConstr
  
isMethodSig (MethSig _ _ _ ) = True
isMethodSig _                = False

isFieldSig (FieldSig _ _ _ ) = True
isFieldSig _                 = False


setThisBinding m@(MethSig _ _ t) t' = m { f_type = mapAnd bkTy t }
  where
    bkTy ty = 
      case bkFun ty of
        Just (vs, bs@(B x _:_),ot) | x == F.symbol "this" -> mkFun (vs,bs,ot)
        Just (vs, bs,ot) -> mkFun (vs, B (F.symbol "this") t':bs, ot)
        _ -> ty

setThisBinding m _        = m

remThisBinding t =
  case bkFun t of
    Just (vs, (B x _):bs,ot) | x == F.symbol "this" -> Just (vs, bs, ot)
    Just ft                                         -> Just ft
    _                                               -> Nothing


instance F.Symbolic (TypeMember t) where
  symbol (FieldSig s _ _)     = s
  symbol (MethSig  s _ _)     = s
  symbol (ConsSig       _)    = F.symbol "__constructor__"
  symbol (CallSig       _)    = F.symbol "__call__"
  symbol (IndexSig _ True _)  = F.symbol "__string__index__"
  symbol (IndexSig _ False _) = F.symbol "__numeric__index__"
  symbol (StatSig s _ _ )     = s

    
sameBinder (CallSig _)       (CallSig _)        = True
sameBinder (ConsSig _)       (ConsSig _)        = True
sameBinder (IndexSig _ b1 _) (IndexSig _ b2 _)  = b1 == b2
sameBinder (FieldSig x1 _ _) (FieldSig x2 _ _)  = x1 == x2
sameBinder (MethSig x1 _ _)  (MethSig x2 _ _)   = x1 == x2
sameBinder (StatSig x1 _ _)  (StatSig x2 _ _)   = x1 == x2
sameBinder _                 _                  = False

mutability (CallSig _)      = Nothing
mutability (ConsSig _)      = Nothing  
mutability (IndexSig _ _ _) = Nothing  
mutability (FieldSig _ m _) = Just m
mutability (MethSig _ m  _) = Just m
mutability (StatSig _ m _)  = Just m

baseType (FieldSig _ _ t) = 
  case bkFun t of
    Just (_, (B x t):_,_) | x == F.symbol "this" -> Just t
    _                                            -> Nothing
baseType (MethSig _ _ t) = 
  case bkFun t of
    Just (_, (B x _):_,_) | x == F.symbol "this" -> Just t
    _                                            -> Nothing
baseType _               = Nothing


eltType (FieldSig _ _ t)  = t
eltType (MethSig _ _  t)  = t
eltType (ConsSig       t) = t
eltType (CallSig       t) = t
eltType (IndexSig _ _  t) = t
eltType (StatSig _ _ t)   = t


----------------------------------------------------------------------------------
-- | Pretty Printer Instances
----------------------------------------------------------------------------------

angles p = char '<' <> p <> char '>'

instance PP Bool where
  pp True   = text "True"
  pp False  = text "False"

instance PP () where 
  pp _ = text ""

instance PP a => PP (Maybe a) where 
  pp = maybe (text "Nothing") pp 

instance PP Char where
  pp = char


instance (PP r, F.Reftable r) => PP (RType r) where
  pp (TVar α r)               = F.ppTy r $ pp α 
  pp (TFun xts t _)           = ppArgs parens comma xts <+> text "=>" <+> pp t 
  pp t@(TAll _ _)             = text "∀" <+> ppArgs id space αs <> text "." <+> pp t' where (αs, t') = bkAll t
  pp (TAnd ts)                = vcat [text "/\\" <+> pp t | t <- ts]
  pp (TExp e)                 = pprint e 
  pp (TApp TUn ts r)          = F.ppTy r $ ppArgs id (text " +") ts 
  pp (TApp (TRef x) (m:ts) r) = F.ppTy r $ pp x <> ppMut m <> ppArgs brackets comma ts 
  pp (TApp c [] r)            = F.ppTy r $ pp c 
  pp (TApp c ts r)            = F.ppTy r $ parens (pp c <+> ppArgs id space ts)  
  pp (TCons bs m r)           | length bs < 5 
                              = F.ppTy r $ ppMut m <> braces (intersperse semi $ map pp bs)
                              | otherwise
                              = F.ppTy r $ lbrace $+$ nest 2 (vcat $ map pp bs) $+$ rbrace
  pp (TModule s  )            = text "module" <+> pp s
  pp (TClass c   )            = text "typeof" <+> pp c
  pp _                        = text "ERROR TYPE"

instance PP TVar where 
  pp     = pprint . F.symbol

instance PP TCon where
  pp TInt      = text "number"
  pp TBool     = text "boolean"
  pp TString   = text "string"
  pp TVoid     = text "void"
  pp TTop      = text "top"
  pp TUn       = text "union:"
  pp (TRef x)  = pp x
  pp TNull     = text "null"
  pp TUndef    = text "undefined"
  pp TFPBool   = text "bool"

instance Hashable TCon where
  hashWithSalt s TInt         = hashWithSalt s (0 :: Int)
  hashWithSalt s TBool        = hashWithSalt s (1 :: Int)
  hashWithSalt s TString      = hashWithSalt s (2 :: Int)
  hashWithSalt s TVoid        = hashWithSalt s (3 :: Int)
  hashWithSalt s TTop         = hashWithSalt s (4 :: Int)
  hashWithSalt s TUn          = hashWithSalt s (5 :: Int)
  hashWithSalt s TNull        = hashWithSalt s (6 :: Int)
  hashWithSalt s TUndef       = hashWithSalt s (7 :: Int)
  hashWithSalt s TFPBool      = hashWithSalt s (8 :: Int)
  hashWithSalt s (TRef z)     = hashWithSalt s (10:: Int) + hashWithSalt s z

instance (PP r, F.Reftable r) => PP (Bind r) where 
  pp (B x t)          = pp x <> colon <> pp t 

instance (PP s, PP t) => PP (M.HashMap s t) where
  pp m = vcat $ pp <$> M.toList m


instance PP Visibility where
  pp Local = text ""
  pp Exported = text "exported "


instance PP Assignability where
  pp ReadOnly    = text "ReadOnly"
  pp WriteLocal  = text "WriteLocal"
  pp WriteGlobal = text "WriteGlobal"


instance (PP r, F.Reftable r) => PP (IfaceDef r) where
  pp (ID c nm vs Nothing ts) =  
        pp (if c then "class" else "interface")
    <+> pp nm <> ppArgs angles comma vs 
    <+> braces (intersperse semi $ map pp ts)
  pp (ID c nm vs (Just (p,ps)) ts) = 
        pp (if c then "class" else "interface")
    <+> pp nm <> ppArgs angles comma vs
    <+> text "extends" 
    <+> pp p  <> ppArgs angles comma ps
    <+> braces (intersperse semi $ map pp ts)


instance (PP r, F.Reftable r) => PP (TypeMember r) where
  pp (CallSig t)          =  text "call" <+> pp t 
  pp (ConsSig t)          =  text "new" <+> pp t
  pp (IndexSig x True t)  =  brackets (pp x <> text ": string") <> text ":" <+> pp t
  pp (IndexSig x False t) =  brackets (pp x <> text ": number") <> text ":" <+> pp t
  pp (FieldSig x m t)     =  ppMut m <+> pp x <> text ":" <+> pp t 
  pp (MethSig x m t)      =  ppMut m <+> pp x <> ppMeth t
  pp (StatSig x m t)      =  text "static" <+> ppMut m <+> pp x <> text ":" <+> pp t


ppMeth t = 
  case bkFun t of
    Just ([],ts,t) -> ppfun ts t
    Just (αs,ts,t) -> angles (ppArgs id comma αs) <> ppfun ts t
    Nothing        -> text "ERROR TYPE"
  where
    ppfun ts t = ppArgs parens comma ts <> text ":" <+> pp t


ppMut t@(TApp (TRef (RN (QName _ _ s))) _ _)
  | s == F.symbol "Mutable"       = pp "◁"
  | s == F.symbol "Immutable"     = pp "◀"
  | s == F.symbol "AnyMutability" = pp "₌"
  | s == F.symbol "ReadOnly"      = pp "◆"
  | s == F.symbol "InheritedMut"  = pp "◇"
  | otherwise                     = pp "?"
ppMut t@(TVar{})                  = pp "[" <> pp t <> pp "]" 
ppMut t                           = pp "?"

 
instance (PP r, F.Reftable r) => PP (ModuleDef r) where
  pp (ModuleDef vars tys path) =  
    text "module" <+> pp path 
      $$ text "Variables" $$ braces (pp vars) 
      $$ text "Types" $$ (pp tys)


-----------------------------------------------------------------------
-- | Primitive / Base Types
-----------------------------------------------------------------------

tVar                       :: (F.Reftable r) => TVar -> RType r
tVar                        = (`TVar` fTop) 

isTVar (TVar _ _)           = True
isTVar _                    = False

tInt, tBool, tUndef, tNull, tString, tVoid, tErr :: (F.Reftable r) => RType r
tInt                        = TApp TInt     [] fTop 
tBool                       = TApp TBool    [] fTop
tString                     = TApp TString  [] fTop
tTop                        = TApp TTop     [] fTop
tVoid                       = TApp TVoid    [] fTop
tUndef                      = TApp TUndef   [] fTop
tNull                       = TApp TNull    [] fTop
tErr                        = tVoid
tFunErr                     = ([],[],tErr)
tAnd ts                     = case ts of 
                                [ ] -> errorstar "BUG: empty intersection"
                                [t] -> t
                                _   -> TAnd ts

-- FIXME !!!!
--    Get this throug get_common_ts
--
-- tArr _                      = rtArr fTop
-- rtArr t                     = TApp (TRef $ QN [] (F.symbol "Array")) [t] 

-- isArr (TApp (TRef (QN [] s)) _ _) | s == F.symbol "Array" = True
-- isArr _                           = False

isTFun (TFun _ _ _)         = True
isTFun (TAnd ts)            = all isTFun ts
isTFun (TAll _ t)           = isTFun t
isTFun _                    = False


orNull t@(TApp TUn ts _)    | any isNull ts = t
orNull   (TApp TUn ts r)    | otherwise     = TApp TUn (tNull:ts) r
orNull t                    | isNull t      = t
orNull t                    | otherwise     = TApp TUn [tNull,t] fTop



-----------------------------------------------------------------------
builtinOpTy       :: (IsLocated l) => l -> BuiltinOp -> Env t -> t
-----------------------------------------------------------------------
builtinOpTy l o g = fromMaybe err $ envFindTy ox g
  where 
    err           = die $ bugUnknown (srcPos l) "builtinOp" o
    ox            = builtinOpId o
 
builtinOpId BIUndefined     = builtinId "BIUndefined"
builtinOpId BIBracketRef    = builtinId "BIBracketRef"
builtinOpId BIBracketAssign = builtinId "BIBracketAssign"
builtinOpId BIArrayLit      = builtinId "BIArrayLit"
builtinOpId BIObjectLit     = builtinId "BIObjectLit"
builtinOpId BISetProp       = builtinId "BISetProp"
builtinOpId BINumArgs       = builtinId "BINumArgs"
builtinOpId BITruthy        = builtinId "BITruthy"
builtinOpId BICondExpr      = builtinId "BICondExpr"


---------------------------------------------------------------------------------
-- | Array literal types
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
arrayLitTy :: (F.Subable (RType r), IsLocated a) => a -> Int -> RType r -> RType r
---------------------------------------------------------------------------------
arrayLitTy l n ty
  = case ty of
      TAll μ (TAll α (TFun [xt] t r)) 
                  -> mkAll [μ,α] $ TFun (arrayLitBinds n xt) (substBINumArgs n t) r
      _           -> err 
    where
      -- ty          = fst $ builtinOpTy l BIArrayLit g
      err         = die $ bug (srcPos l) $ "Bad Type for ArrayLit Constructor"
      
arrayLitBinds n (B x t) = [B (x_ i) t | i <- [1..n]] 
  where
    xs            = F.symbolString x
    x_            = F.symbol . (xs ++) . show 

substBINumArgs n t = F.subst1 t (F.symbol $ builtinOpId BINumArgs, F.expr (n::Int))


---------------------------------------------------------------------------------
-- | Object literal types
---------------------------------------------------------------------------------

-- FIXME: Avoid capture
freshTV l s n     = (v,t)
  where 
    i             = F.intSymbol s n
    v             = TV i (srcPos l)
    t             = TVar v ()

--------------------------------------------------------------------------------------------
objLitTy         :: (F.Reftable r, IsLocated a) => a -> [Prop a] -> RType r
--------------------------------------------------------------------------------------------
objLitTy l ps     = mkFun (vs, bs, rt)
  where
    vs            = [mv] ++ mvs ++ avs
    bs            = [B s (ofType a) | (s,a) <- zip ss ats ]
    rt            = TCons elts mt fTop -- $ objLitR l nps g fTop
    elts          = [FieldSig s m (ofType a) | (s,m,a) <- zip3 ss mts ats ] 
    nps           = length ps
    (mv, mt)      = freshTV l mSym 0                             -- obj mutability
    (mvs, mts)    = unzip $ map (freshTV l mSym) [1..length ps]  -- field mutability
    (avs, ats)    = unzip $ map (freshTV l aSym) [1..length ps]  -- field type vars
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
  -- symbol p                   = error $ printf "Symbol of property %s not supported yet" (ppshow p)


-- | @argBind@ returns a dummy type binding `arguments :: T `
--   where T is an object literal containing the non-undefined `ts`.
    
argTy l ts g   = immObjectLitTy l g (pLen : ps') (tLen : ts')
  where
    ts'        = take k ts
    ps'        = PropNum l . toInteger <$> [0 .. k-1]
    pLen       = PropId l $ lenId l
    tLen       = tInt `strengthen` rLen
    rLen       = F.ofReft $ F.uexprReft k
    k          = fromMaybe 0 $ L.findIndex isUndef ts


immObjectLitTy l _ ps ts 
  | nps == length ts = TCons elts t_immutable fTop -- objLitR l nps g 
  | otherwise        = die $ bug (srcPos l) $ "Mismatched args for immObjectLit"
  where
    elts             = safeZipWith "immObjectLitTy" (\p t -> FieldSig (F.symbol p) t_immutable t) ps ts
    nps              = length ps

-- objLitR l n g  = fromMaybe fTop $ substBINumArgs n . rTypeR . thd3 <$> bkFun t 
--   where
--     t          = builtinOpTy l BIObjectLit g

   

    
---------------------------------------------------------------------------------
setPropTy :: (PPR r, IsLocated l) => l -> F.Symbol -> RType r -> RType r
---------------------------------------------------------------------------------
setPropTy l f ty =
    case ty of 
      TAll α2 (TAll μ2 (TFun [xt2,a2] rt2 r2)) -> TAll α2 (TAll μ2 (TFun [tr xt2,a2] rt2 r2))
      _                                        -> errorstar $ "setPropTy " ++ ppshow ty
  where
    -- replace the field name
    tr (B n (TCons [FieldSig x μx t] μ r)) 
          | x == F.symbol "f"
          = B n (TCons [FieldSig f μx t] μ r)
    tr t  = error $ "setPropTy:tr " ++ ppshow t
    -- ty    = builtinOpTy l BISetProp (envMap fst g)



---------------------------------------------------------------------------------
returnTy :: (PP r, F.Reftable r) => RType r -> Bool -> RType r
---------------------------------------------------------------------------------
returnTy t True  = mkFun ([], [B (F.symbol "r") t], tVoid)
returnTy _ False = mkFun ([], [], tVoid)


-- | `mkEltFunTy`: Creates a function type that corresponds to an invocation 
--   to the input element. 
---------------------------------------------------------------------------------
mkEltFunTy :: F.Reftable r => TypeMember r -> Maybe (RType r)
---------------------------------------------------------------------------------
-- `τ` is the type for the lately bound object, to be used in the position of 
-- "this". It will only be used if `m` does not specify it.
mkEltFunTy (MethSig _ _  t) = mkEltFromType t
mkEltFunTy (FieldSig _ _ t) = mkEltFromType t
mkEltFunTy (StatSig _ _  t) = mkEltFromType t
mkEltFunTy _                = Nothing

mkEltFromType t = fmap mkAnd $ fmap (fmap mkFun) $ sequence $ bkFun <$> bkAnd t


mkInitFldTy (FieldSig _ _ t) = Just $ mkFun ([], [B (F.symbol "f") t], tVoid)
mkInitFldTy (StatSig  _ _ t) = Just $ mkFun ([], [B (F.symbol "f") t], tVoid)
mkInitFldTy _                = Nothing


-- ---------------------------------------------------------------------------------
-- infixOpTy :: PP t => InfixOp -> Env t -> t 
-- ---------------------------------------------------------------------------------
-- infixOpTy o g = fromMaybe err $ envFindTy ox g
--   where 
--     err       = errorstar $ printf "Cannot find infixOpTy %s in %s" (ppshow ox) (ppshow $ g)
--     ox        = infixOpId o

infixOpId OpLT         = builtinId "OpLT"
infixOpId OpLEq        = builtinId "OpLEq"
infixOpId OpGT         = builtinId "OpGT"
infixOpId OpGEq        = builtinId "OpGEq"
infixOpId OpEq         = builtinId "OpEq"
infixOpId OpStrictEq   = builtinId "OpSEq"
infixOpId OpNEq        = builtinId "OpNEq"
infixOpId OpStrictNEq  = builtinId "OpSNEq"
infixOpId OpLAnd       = builtinId "OpLAnd"
infixOpId OpLOr        = builtinId "OpLOr"
infixOpId OpSub        = builtinId "OpSub"
infixOpId OpAdd        = builtinId "OpAdd"
infixOpId OpMul        = builtinId "OpMul"
infixOpId OpDiv        = builtinId "OpDiv"
infixOpId OpMod        = builtinId "OpMod"
infixOpId OpInstanceof = builtinId "OpInstanceof"
infixOpId OpBOr        = builtinId "OpBOr"
infixOpId o            = errorstar $ "infixOpId: Cannot handle: " ++ ppshow o

-- -----------------------------------------------------------------------
-- prefixOpTy :: PrefixOp -> Env t -> t 
-- -----------------------------------------------------------------------
-- prefixOpTy o g = fromMaybe err $ envFindTy (prefixOpId o) g
--   where 
--     err       = convertError "prefixOpTy" o

prefixOpId PrefixMinus  = builtinId "PrefixMinus"
prefixOpId PrefixLNot   = builtinId "PrefixLNot"
prefixOpId PrefixTypeof = builtinId "PrefixTypeof"
prefixOpId PrefixBNot   = builtinId "PrefixBNot"
prefixOpId o            = errorstar $ "prefixOpId: Cannot handle: " ++ ppshow o


mkId            = Id (initialPos "") 
builtinId       = mkId . ("builtin_" ++)


