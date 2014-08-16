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

  -- * Mutability
  , t_mutable, t_immutable, t_anyMutability, t_inheritedMut, t_readOnly
  -- , combMut, isMutable, isImmutable, isAnyMut, isMutabilityType, variance, varianceTDef

  -- * Primitive Types
  , tInt, tBool, tString, tTop, tVoid, tErr, tFunErr, tVar, tUndef, tNull
  , tAnd, isTVar, isTObj, isConstr, isTFun, fTop, orNull
  -- , isArr , tArr, rtArr

  -- * Element ops 
  , sameBinder, eltType, isStaticSig, nonStaticSig, nonConstrElt, mutability, baseType
  , isMethodSig, isFieldSig, setThisBinding, remThisBinding, mapEltM

  -- * Operator Types
  , infixOpTy, prefixOpTy, builtinOpTy, arrayLitTy, objLitTy, setPropTy, returnTy


  ) where 

import           Text.Printf
import           Data.Default
import           Data.Hashable
import           Data.Either                    (partitionEithers)
import           Data.Maybe                     (fromMaybe)
import           Data.Traversable               hiding (sequence, mapM) 
import           Data.Foldable                  (Foldable()) 
import           Data.Monoid                    hiding ((<>))            
import qualified Data.List                      as L
import qualified Data.IntMap                    as I
import qualified Data.HashMap.Strict            as M
import           Data.Generics                   
import           Data.Typeable                  ()
import           Language.ECMAScript3.Syntax 
import           Language.ECMAScript3.Syntax.Annotations
import           Language.ECMAScript3.PrettyPrint
import           Language.Nano.Misc
import           Language.Nano.Types
import           Language.Nano.Errors
import           Language.Nano.Env
import           Language.Nano.Locations

import qualified Language.Fixpoint.Types        as F
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Errors
import           Language.Fixpoint.PrettyPrint
import           Text.PrettyPrint.HughesPJ 
import           Text.Parsec.Pos                    (initialPos)

import           Control.Applicative            hiding (empty)

-- import           Debug.Trace (trace)


---------------------------------------------------------------------
-- | Mutability
---------------------------------------------------------------------

-- validMutNames = F.symbol <$> ["ReadOnly", "Mutable", "Immutable", "AnyMutability"]

mkMut s = TApp (TRef $ QN (srcPos dummySpan) [] (F.symbol s)) [] ()

instance Default Mutability where
  def = mkMut "Mutable"

t_mutable       = mkMut "Mutable"
t_immutable     = mkMut "Immutable"
t_anyMutability = mkMut "AnyMutability"
t_readOnly      = mkMut "ReadOnly"
t_inheritedMut  = mkMut "InheritedMut"


-- isMutabilityType (TApp (TRef (QN [] s)) _ _) = s `elem` validMutNames
-- isMutabilityType _                           = False
-- 
-- isMutable        (TApp (TRef (QN [] s)) _ _) = s == F.symbol "Mutable"
-- isMutable _                                  = False
-- 
-- isImmutable      (TApp (TRef (QN [] s)) _ _) = s == F.symbol "Immutable"
-- isImmutable _                                = False
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
-- combMut _ μf | isMutable μf                 = μf
-- combMut μ _  | otherwise                    = μ

-- | Variance: true if v is in a positive position in t
--
-- FIXME: implement these
--
-- variance :: TVar -> RType r -> Bool
-- variance _ _  = True

-- varianceTDef :: IfaceDef r -> [Bool]
-- varianceTDef (ID _ _ vs _ _) = take (length vs) $ repeat True




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


---------------------------------------------------------------------------------
mkUnion :: (F.Reftable r) => [RType r] -> RType r
---------------------------------------------------------------------------------
mkUnion [ ] = tErr
mkUnion [t] = t             
mkUnion ts  = TApp TUn ts fTop

---------------------------------------------------------------------------------
bkUnion :: RType r -> [RType r]
---------------------------------------------------------------------------------
bkUnion (TApp TUn xs _) = xs
bkUnion t               = [t]


-- | Strengthen the top-level refinement
---------------------------------------------------------------------------------
strengthen                   :: F.Reftable r => RType r -> r -> RType r
---------------------------------------------------------------------------------
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


---------------------------------------------------------------------------------
-- | Predicates on Types 
---------------------------------------------------------------------------------

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


---------------------------------------------------------------------------
-- | Pretty Printer Instances
---------------------------------------------------------------------------

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
  pp (TApp d@(TRef _ ) ts r)  = F.ppTy r $ pp d <> ppArgs brackets comma ts 
  pp (TApp c [] r)            = F.ppTy r $ pp c 
  pp (TApp c ts r)            = F.ppTy r $ parens (pp c <+> ppArgs id space ts)  
  pp (TCons bs _ r)           | length bs < 5 
                              = F.ppTy r $ {- ppMut m <> -} braces (intersperse semi $ map pp bs)
                              | otherwise
                              = F.ppTy r $ lbrace $+$ nest 2 (vcat $ map pp bs) $+$ rbrace
  pp (TModule s  )            = text "module" <+> pp s
  pp (TClass c   )            = text "typeof" <+> pp c

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

ppArgs p sep l = p $ intersperse sep $ map pp l

instance (PP s, PP t) => PP (M.HashMap s t) where
  pp m = vcat $ pp <$> M.toList m

instance (F.Reftable r, PP r) => PP (ModuleExports r) where
  pp = braces . intersperse semi . map pp

instance (F.Reftable r, PP r) => PP (ModuleMember r) where
  pp (ModClass x t ) = pp x <> colon <+> pp t
  pp (ModVar x t   ) = pp x <> colon <+> pp t
  pp (ModModule x  ) = pp x


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
  pp (FieldSig x _ t)     =  text "field"  {- <+> ppMut m -} <+> pp x <> text ":" <+> pp t 
  pp (MethSig x _ t)      =  text "method" {- <+> ppMut m -} <+> pp x <> text ":" <+> pp t
  pp (StatSig x _ t)      =  text "static" {- <+> ppMut m -} <+> pp x <> text ":" <+> pp t


-- ppMut t | isMutable t      = brackets $ pp "mut"
--         | isAnyMut t       =            pp ""
--         | isInheritedMut t =            pp ""
--         | isReadOnly t     = brackets $ pp "ro"
--         | isImmutable t    = brackets $ pp "imm"
--         | isTVar t         = brackets $ pp t
--         | isTop t          =            pp "top"    -- FIXME: this should go ...
--         | otherwise        = error    $ "ppMut: case not covered: " ++ ppshow t
   


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
builtinOpId BISetProp       = builtinId "BISetProp"
builtinOpId BINumArgs       = builtinId "BINumArgs"
builtinOpId BITruthy        = builtinId "BITruthy"
builtinOpId BICondExpr      = builtinId "BICondExpr"


--------------------------------------------------------------------------
-- | Array literal types
--------------------------------------------------------------------------

-----------------------------------------------------------------------
arrayLitTy :: (F.Subable (RType r), IsLocated a) 
           => a -> Int -> Env (RType r) -> RType r
-----------------------------------------------------------------------
arrayLitTy l n g 
  = case ty of 
      TAll μ (TAll α (TFun [xt] t r)) 
                  -> mkAll [μ,α] $ TFun (arrayLitBinds n xt) (arrayLitOut n t) r
      _           -> err 
    where
      ty          = builtinOpTy l BIArrayLit g
      err         = die $ bug (srcPos l) $ "Bad Type for ArrayLit Constructor"
      
arrayLitBinds n (B x t) = [B (x_ i) t | i <- [1..n]] 
  where
    xs            = F.symbolString x
    x_            = F.symbol . (xs ++) . show 

arrayLitOut n t   = F.subst1 t (F.symbol $ builtinOpId BINumArgs, F.expr (n::Int))


--------------------------------------------------------------------------
-- | Object literal types
--------------------------------------------------------------------------

-- FIXME: Avoid capture
freshTV l s n     = (v,t)
  where 
    i             = F.intSymbol s n
    v             = TV i (srcPos l)
    t             = TVar v ()

--------------------------------------------------------------------------
objLitTy         :: (F.Reftable r, IsLocated a) 
                 => a -> [Prop a] -> RType r
--------------------------------------------------------------------------
objLitTy l ps     = mkFun (vs, bs, rt)
  where
    (mv,mt)       = freshTV l mSym 0                             -- obj mutability
    (mvs,mts)     = unzip $ map (freshTV l mSym) [1..length ps]  -- field mutability
    (avs,ats)     = unzip $ map (freshTV l aSym) [1..length ps]  -- field type vars
    ss            = [ F.symbol p | p <- ps]
    vs            = [mv] ++ mvs ++ avs
    bs            = [ B s (ofType a) | (s,a) <- zip ss ats ]
    elts          = [ FieldSig s m (ofType a) | (s,m,a) <- zip3 ss mts ats ] 
    rt            = TCons elts mt fTop
    mSym          = F.symbol "M"
    aSym          = F.symbol "A"
    
--------------------------------------------------------------------------
setPropTy :: (PP r, F.Reftable r, IsLocated l) 
          => F.Symbol -> l -> F.SEnv (Located (RType r)) -> RType r
--------------------------------------------------------------------------
setPropTy f l g =
    case ty of 
      TAnd [TAll α1 (TAll μ1 (TFun [xt1,a1] rt1 r1)) ,
            TAll α2 (TAll μ2 (TFun [xt2,a2] rt2 r2)) ] -> 
        TAnd [TAll α1 (TAll μ1 (TFun [tr xt1,a1] rt1 r1)) ,
              TAll α2 (TAll μ2 (TFun [tr xt2,a2] rt2 r2)) ]
      _ -> errorstar $ "setPropTy " ++ ppshow ty
  where
    tr (B n (TCons [FieldSig x μx t] μ r)) 
          | x == F.symbol "f"
          = B n (TCons [FieldSig f μx t] μ r)
    tr t  = error $ "setPropTy:tr " ++ ppshow t
    ty    = builtinOpTy l BISetProp g



--------------------------------------------------------------------------
returnTy :: (PP r, F.Reftable r) => RType r -> Bool -> RType r
--------------------------------------------------------------------------
returnTy t True  = mkFun ([], [B (F.symbol "r") t], tVoid)
returnTy _ False = mkFun ([], [], tVoid)


-- | `mkEltFunTy`: Creates a function type that corresponds to an invocation 
--   to the input element. 
--------------------------------------------------------------------------
mkEltFunTy :: F.Reftable r => TypeMember r -> Maybe (RType r)
--------------------------------------------------------------------------
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


-----------------------------------------------------------------------
infixOpTy :: InfixOp -> Env t -> t 
-----------------------------------------------------------------------
infixOpTy o g = fromMaybe err $ envFindTy ox g
  where 
    err       = errorstar $ printf "Cannot find infixOpTy %s" (ppshow ox) -- (ppshow g)
    ox        = infixOpId o

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
infixOpId o            = errorstar $ "infixOpId: Cannot handle: " ++ ppshow o

-----------------------------------------------------------------------
prefixOpTy :: PrefixOp -> Env t -> t 
-----------------------------------------------------------------------
prefixOpTy o g = fromMaybe err $ envFindTy (prefixOpId o) g
  where 
    err       = convertError "prefixOpTy" o

prefixOpId PrefixMinus  = builtinId "PrefixMinus"
prefixOpId PrefixLNot   = builtinId "PrefixLNot"
prefixOpId PrefixTypeof = builtinId "PrefixTypeof"
prefixOpId PrefixBNot   = builtinId "PrefixBNot"
prefixOpId o            = errorstar $ "prefixOpId: Cannot handle: " ++ ppshow o


mkId            = Id (initialPos "") 
builtinId       = mkId . ("builtin_" ++)


-----------------------------------------------------------------------
-- funTys
-----------------------------------------------------------------------
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


funTy l xs (αs, yts, t) 
  | length xs == length yts = let (su, ts') = renameBinds yts xs 
                              in  Right (αs, ts', F.subst su t)    
  | otherwise               = Left $ errorArgMismatch (srcPos l)

