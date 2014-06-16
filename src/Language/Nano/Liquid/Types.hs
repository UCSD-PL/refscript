{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

-- | Module pertaining to Refinement Type descriptions and conversions
--   Likely mergeable with @Language.Nano.Typecheck.Types@

module Language.Nano.Liquid.Types ( 
  
  -- * Refinement Types and Environments
    RefType 
  , REnv
  , NanoRefType

  -- * Constraint Environments
  , CGEnv (..)

  -- * Constraint Information
  , Cinfo (..)
  , ci

  -- * Constraints
  , SubC (..) , WfC (..)
  , FixSubC   , FixWfC

  -- * Conversions
  , RefTypable (..)
  , eSingleton
  , pSingleton
  -- , shiftVVs

  -- * Manipulating RefType
  , rTypeReft
  , rTypeSort
  , rTypeSortedReft
  , rTypeValueVar

  -- * Predicates On RefType 
  , isBaseRType
  , isTrivialRefType

  -- * Monadic map (TODO: Applicative/Traversable)
  , mapReftM

  -- * Primitive Types
  , prefixOpRTy
  , infixOpRTy 

  -- * Useful Operations
  , foldReft
  , efoldRType
  , AnnTypeR

  -- * Raw low-level Location-less constructors
  , rawStringSymbol 

  -- Zip types
  , zipType

  ) where

import           Data.Maybe             (fromMaybe, fromJust)
import qualified Data.List               as L
import           Data.Function                  (on)
import qualified Data.HashMap.Strict     as M
import           Text.PrettyPrint.HughesPJ
import           Text.Printf 
import           Control.Applicative 

import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.PrettyPrint
import           Language.Nano.Errors
import           Language.Nano.Types
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Env
import           Language.Nano.Misc
import           Language.Fixpoint.Misc
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Sub
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.PrettyPrint
  
-- import           Debug.Trace                        (trace)

type PPR r = (PP r, F.Reftable r)

-------------------------------------------------------------------------------------
-- | Refinement Types and Environments
-------------------------------------------------------------------------------------

type RefType     = RType F.Reft
type REnv        = Env RefType

type AnnTypeR    = AnnType F.Reft

-------------------------------------------------------------------------------------
-- | Constraint Generation Environment 
-------------------------------------------------------------------------------------

data CGEnv   
  = CGE { renv     :: !REnv               -- ^ bindings in scope 
        , fenv     :: F.IBindEnv          -- ^ fixpoint bindings
        , guards   :: ![F.Pred]           -- ^ branch target conditions  
        , cge_ctx  :: !IContext           -- ^ intersection-type context 
        , cge_spec :: !(Env RefType)      -- ^ specifications for defined functions
        , cge_defs :: !(TDefEnv RefType)  -- ^ type definitions
        }

----------------------------------------------------------------------------
-- | Constraint Information 
----------------------------------------------------------------------------

newtype Cinfo = Ci SourceSpan deriving (Eq, Ord, Show) 

-- emptyCinfo    = Ci $ initialPos ""

ci :: (IsLocated a) => a -> Cinfo
ci = Ci . srcPos 

instance PP Cinfo where
  pp (Ci l)   = text "CInfo:" <+> pp l 

instance IsLocated Cinfo where
  srcPos (Ci x) = x

instance F.Fixpoint Cinfo where 
  toFix = pp

----------------------------------------------------------------------------
-- | Constraints 
----------------------------------------------------------------------------

-- | Subtyping Constraints

data SubC     
  = Sub { senv  :: !CGEnv      -- ^ Environment
        , sinfo :: !Cinfo      -- ^ Source Information
        , slhs  :: !RefType    -- ^ Subtyping LHS
        , srhs  :: !RefType    -- ^ Subtyping RHS   ... senv |- slhs <: srhs
        }

-- | Wellformedness Constraints

data WfC 
  = W { wenv  :: !CGEnv      -- ^ Scope/Environment
      , winfo :: !Cinfo      -- ^ Source Information
      , wtyp  :: !RefType    -- ^ Type to be Well-formed ... wenv |- wtyp
      }

instance PP F.Reft where 
  pp = pprint

instance PP SubC where
  pp (Sub γ i t t') = pp (renv γ) $+$ pp (guards γ) 
                        $+$ ((text "|-") <+> (pp t $+$ text "<:" $+$ pp t'))
                        $+$ ((text "from:") <+> pp i) 

instance PP WfC where
  pp (W γ t i)      = pp (renv γ) 
                        $+$ (text "|-" <+> pp t) 
                        $+$ ((text "from:") <+> pp i) 

instance IsLocated SubC where
  srcPos = srcPos . sinfo

instance IsLocated WfC where
  srcPos = srcPos . winfo

-- | Aliases for Fixpoint Constraints

type FixSubC = F.SubC Cinfo
type FixWfC  = F.WfC  Cinfo

------------------------------------------------------------------------
-- | Embedding Values as RefTypes
------------------------------------------------------------------------

class RefTypable a where
  rType :: a -> RefType 

instance RefTypable Type where
  rType = ofType

instance RefTypable RefType where
  rType = ofType . toType           -- removes all refinements

eSingleton      :: (F.Expression e) => RefType -> e -> RefType 
eSingleton t e  = t `strengthen` (F.uexprReft e)

pSingleton      :: (F.Predicate p) => RefType -> p -> RefType 
pSingleton t p  = t `strengthen` (F.propReft p)

------------------------------------------------------------------------------
-- | Converting RType to Fixpoint
------------------------------------------------------------------------------

rTypeSortedReft   ::  PPR r => RType r -> F.SortedReft
rTypeSortedReft t = F.RR (rTypeSort t) (rTypeReft t)

rTypeReft         :: (F.Reftable r) => RType r -> F.Reft
rTypeReft         = fromMaybe fTop . fmap F.toReft . stripRTypeBase 

rTypeValueVar     :: (F.Reftable r) => RType r -> F.Symbol
rTypeValueVar t   = vv where F.Reft (vv,_) = rTypeReft t 

------------------------------------------------------------------------------------------
rTypeSort :: (PPR r) => RType r -> F.Sort
------------------------------------------------------------------------------------------
rTypeSort   (TVar α _)     = F.FObj $ F.symbol α 
rTypeSort t@(TAll _ _)     = rTypeSortForAll t 
rTypeSort   (TFun xts t _) = F.FFunc 0 $ rTypeSort <$> (b_type <$> xts) ++ [t]
rTypeSort   (TApp c ts _)  = rTypeSortApp c ts 
rTypeSort   (TAnd (t:_))   = rTypeSort t
rTypeSort   (TCons _ _ _ ) = F.FObj $ F.symbol "cons"
rTypeSort t                = error $ render $ text "BUG: rTypeSort does not support " <+> pp t

rTypeSortApp TInt _  = F.FInt
rTypeSortApp TUn  _  = F.FApp (tconFTycon TUn) [] -- simplifying union sorts, the checks will have been done earlier
rTypeSortApp c ts    = F.FApp (tconFTycon c) (rTypeSort <$> ts) 

tconFTycon :: TCon -> F.FTycon 
tconFTycon TInt         = F.intFTyCon
tconFTycon TBool        = rawStringFTycon "Boolean"
tconFTycon TFPBool      = F.boolFTyCon
tconFTycon TVoid        = rawStringFTycon "Void"
tconFTycon (TRef s _)   = rawStringFTycon $ F.symbolString s
tconFTycon TUn          = rawStringFTycon "Union"
tconFTycon TString      = F.strFTyCon
tconFTycon TTop         = rawStringFTycon "Top"
tconFTycon TNull        = rawStringFTycon "Null"
tconFTycon TUndef       = rawStringFTycon "Undefined"


rTypeSortForAll t    = genSort n θ $ rTypeSort tbody
  where 
    (αs, tbody)      = bkAll t
    n                = length αs
    θ                = M.fromList $ zip (F.symbol <$> αs) (F.FVar <$> [0..])
    
genSort n θ (F.FFunc _ t)  = F.FFunc n (F.sortSubst θ <$> t)
genSort n θ t              = F.FFunc n [F.sortSubst θ t]

------------------------------------------------------------------------------------------
stripRTypeBase :: RType r -> Maybe r 
------------------------------------------------------------------------------------------
stripRTypeBase (TApp _ _ r)  = Just r
stripRTypeBase (TVar _ r)    = Just r
stripRTypeBase (TFun _ _ r)  = Just r
stripRTypeBase (TCons _ _ r) = Just r
stripRTypeBase _             = Nothing

------------------------------------------------------------------------------------------
-- | Substitutions
------------------------------------------------------------------------------------------

instance (PPR r, F.Subable r) => F.Subable (RType r) where
  syms        = foldReft (\r acc -> F.syms r ++ acc) [] 
  substa      = fmap . F.substa 
  substf f    = emapReft (F.substf . F.substfExcept f) [] 
  subst su    = emapReft (F.subst  . F.substExcept su) []
  subst1 t su = emapReft (\xs r -> F.subst1Except xs r su) [] t

------------------------------------------------------------------------------------------
-- | Traversals over @RType@ 
------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------
emapReft  :: PPR a => ([F.Symbol] -> a -> b) -> [F.Symbol] -> RType a -> RType b
------------------------------------------------------------------------------------------
emapReft f γ (TVar α r)      = TVar α (f γ r)
emapReft f γ (TApp c ts r)   = TApp c (emapReft f γ <$> ts) (f γ r)
emapReft f γ (TAll α t)      = TAll α (emapReft f γ t)
emapReft f γ (TFun xts t r)  = TFun (emapReftBind f γ' <$> xts) (emapReft f γ' t) (f γ r)
  where    γ'                = (b_sym <$> xts) ++ γ
emapReft f γ (TCons xts m r) = TCons (emapReftElt f γ' <$> xts) m (f γ r)
  where    γ'                = (f_sym <$> xts) ++ γ
emapReft _ _ _               = error "Not supported in emapReft"

emapReftBind f γ (B x t)     = B x $ emapReft f γ t

emapReftElt  :: PPR a => ([F.Symbol] -> a -> b) -> [F.Symbol] -> TElt (RType a) -> TElt (RType b)
emapReftElt f γ e            = fmap (emapReft f γ) e

mapReftM f (TVar α r)        = TVar α <$> f r
mapReftM f (TApp c ts r)     = TApp c <$> mapM (mapReftM f) ts <*> f r
mapReftM f (TFun xts t r)    = TFun   <$> mapM (mapReftBindM f) xts <*> mapReftM f t <*> (return $ F.top r) --f r 
mapReftM f (TAll α t)        = TAll α <$> mapReftM f t
mapReftM f (TAnd ts)         = TAnd   <$> mapM (mapReftM f) ts
mapReftM f (TCons bs m r)    = TCons  <$> mapM (mapReftEltM f) bs <*> return m <*> f r
mapReftM _ t                 = error   $ render $ text "Not supported in mapReftM: " <+> pp t 

mapReftBindM f (B x t)       = B x     <$> mapReftM f t

mapReftEltM f (FieldSig x s m τ t) = FieldSig x s m <$> mapReftMaybeM f τ <*> mapReftM f t
mapReftEltM f (CallSig t)          = CallSig        <$> mapReftM f t
mapReftEltM f (ConsSig  t)         = ConsSig        <$> mapReftM f t
mapReftEltM f (IndexSig x b t)     = IndexSig x b   <$> mapReftM f t

mapReftMaybeM f (Just t) = Just <$> mapReftM f t
mapReftMaybeM _ _        = return Nothing


------------------------------------------------------------------------------------------
-- | fold over @RType@ 
------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------
foldReft  :: PPR r => (r -> a -> a) -> a -> RType r -> a
------------------------------------------------------------------------------------------
foldReft  f = efoldReft (\_ -> ()) (\_ -> f) F.emptySEnv 

------------------------------------------------------------------------------------------
efoldReft :: PPR r => (RType r -> b) -> (F.SEnv b -> r -> a -> a) -> F.SEnv b -> a -> RType r -> a
------------------------------------------------------------------------------------------
efoldReft g f = go 
  where 
    go γ z (TVar _ r)       = f γ r z
    go γ z t@(TApp _ ts r)  = f γ r $ gos (efoldExt g (B (rTypeValueVar t) t) γ) z ts
    go γ z (TAll _ t)       = go γ z t
    go γ z (TFun xts t r)   = f γ r $ go γ' (gos γ' z (b_type <$> xts)) t  where γ' = foldr (efoldExt g) γ xts
    go γ z (TAnd ts)        = gos γ z ts 
    go γ z (TCons bs _ r)   = f γ' r $ gos γ z (f_type <$> bs) where γ' = foldr (efoldExt' g) γ bs
    go _ _ t                = error $ "Not supported in efoldReft: " ++ ppshow t

    gos γ z ts              = L.foldl' (go γ) z ts

efoldExt g xt γ             = F.insertSEnv (b_sym xt) (g $ b_type xt) γ
efoldExt' g xt γ            = F.insertSEnv (f_sym xt) (g $ f_type xt) γ

------------------------------------------------------------------------------------------
efoldRType :: PPR r => (RType r -> b) -> (F.SEnv b -> RType r -> a -> a) -> F.SEnv b -> a -> RType r -> a
------------------------------------------------------------------------------------------
efoldRType g f               = go
  where 
    go γ z t@(TVar _ _)      = f γ t z
    go γ z t@(TApp _ ts _)   = f γ t $ gos (efoldExt g (B (rTypeValueVar t) t) γ) z ts
    go γ z t@(TAll _ t1)     = f γ t $ go γ z t1
    go γ z t@(TFun xts t1 _) = f γ t $ go γ' (gos γ' z (b_type <$> xts)) t1  where γ' = foldr (efoldExt g) γ xts
    go γ z   (TAnd ts)       = gos γ z ts 
    go γ z t@(TCons xts _ _) = f γ t $ gos γ' z (f_type <$> xts) where γ' = foldr (efoldExt' g) γ xts
    go _ _ t                 = error $ "Not supported in efoldRType: " ++ ppshow t
    gos γ z ts               = L.foldl' (go γ) z ts


------------------------------------------------------------------------------------------
isBaseRType :: RType r -> Bool
------------------------------------------------------------------------------------------
isBaseRType (TApp _ [] _) = True
isBaseRType (TVar _ _)    = True
isBaseRType _             = False

------------------------------------------------------------------------------------------
isTrivialRefType :: RefType -> Bool
------------------------------------------------------------------------------------------
isTrivialRefType t     = foldReft (\r -> (f r &&)) True t
  where 
    f (F.Reft (_,ras)) = null ras

------------------------------------------------------------------------------------------
prefixOpRTy :: PrefixOp -> CGEnv -> RefType
------------------------------------------------------------------------------------------
prefixOpRTy o g = prefixOpTy o $ renv g

------------------------------------------------------------------------------------------
infixOpRTy :: InfixOp -> CGEnv -> RefType
------------------------------------------------------------------------------------------
infixOpRTy o g = infixOpTy o $ renv g


rawStringSymbol            = F.Loc F.dummyPos . F.stringSymbol
rawStringFTycon            = F.stringFTycon . F.Loc F.dummyPos 


-- | Related types ( ~~ ) 

class Related a where
  related :: TDefEnv RefType -> a -> a -> Bool

instance Related RefType where
  related δ t t' = isSubtype δ t t' || isSubtype δ t' t
--   
--   related δ (TApp TUn t1s r1) (TApp TUn t2s _) = True
--   related δ _                 (TApp TUn _ _  ) = True
--   related δ (TApp TUn _ _)    _                = True
-- 
--   related δ t1@(TApp (TRef x1 s1) t1s r1) t2@(TApp (TRef x2 s2) t2s r2) 
--     | (x1,s1) == (x2,s2) = related δ t1s t2s
--     | otherwise = 
--         case weaken δ (findSymOrDie x1 δ, t1s) x2 of
--           Just (_, t1s') -> related δ (TApp (TRef x1 s1) t1s' r1) t2
--           Nothing -> 
--             case weaken δ (findSymOrDie x2 δ, t1s) x1 of
--               Just (_, t2s') -> related δ t1 (TApp (TRef x2 s2) t2s' r2)
--               Nothing -> False
-- 
--   related δ t1@(TApp (TRef _ _) _ _) t2 = related δ (flattenType δ t1) t2
--   related δ t1 t2@(TApp (TRef _ _) _ _) = related δ t1 (flattenType δ t2)
-- 
--   related _ (TApp c [] r) (TApp c' [] _) | c == c' = True
-- 
--   -- | Top ??
--   -- related _ _ t2@(TApp TTop _ _ ) = t2
-- 
--   related _ (TVar v r) (TVar v' r') | v == v' = True
-- 
--   related δ (TFun x1s t1 _) (TFun x2s t2 _) = 
--       related δ t1 t2 && related δ x1s x2s
-- 
-- | Object types
--
--  { F1,F2 } | { F1',F3' } = { F1|F1',top(F3) }, where disjoint F2 F3'
--
-- related δ (TCons f1s m1 r1) (TCons f2s _ _) = 
--     True


instance Related (RType ()) where
  related δ t t' = isSubtype δ τ τ' || isSubtype δ τ' τ
    where
      τ  = ofType t
      τ' = ofType t'

instance Related (TElt RefType) where
  related δ (CallSig t1)            (CallSig t2)            = related δ t1 t2
  related δ (ConsSig t1)            (ConsSig t2)            = related δ t1 t2
  related δ (IndexSig _ _ t1)       (IndexSig _ _ t2)       = related δ t1 t2
  related δ (FieldSig _ _ μ1 τ1 t1) (FieldSig _ _ μ2 τ2 t2) = and [related δ μ1 μ2, related δ τ1 τ2, related δ t1 t2]
  related δ (MethSig  _ _ μ1 τ1 t1) (MethSig  _ _ μ2 τ2 t2) = and [related δ μ1 μ2, related δ τ1 τ2, related δ t1 t2]
  related δ _                       _                       = False

instance Related a => Related (Maybe a) where
  related δ (Just t1) (Just t2) = related δ t1 t2
  related _ _         _         = True

instance Related a => Related [a] where
  related δ t1s t2s = and $ zipWith (related δ) t1s t2s

instance Related (RType a) => Related (Bind a) where
  related δ (B _ t1) (B _ t2) = related δ t1 t2



-- | `zipType` returns a type that is:
--
--  * structurally equivalent to @t2@
--  * semantically equivalent to @t1@
--  * applys @f@ whenever refinements are present on both sides
--  * applys @g@ whenever the respective part in type @t2@ is missing
--
--------------------------------------------------------------------------------
zipType :: TDefEnv RefType -> RefType -> RefType -> RefType
--------------------------------------------------------------------------------
--
--  s1 \/ .. sn | t1 \/ .. tm = s1'|t1' \/ .. tk|tk' \/ .. bot(tm')
--
zipType δ (TApp TUn t1s r1) (TApp TUn t2s _) =
    TApp TUn (pair <$> t2s) r1
  where
    pair t2 = 
      case L.find (related δ t2) t1s of
        Just t1 -> zipType δ t1 t2
        Nothing -> fmap F.bot t2

zipType δ t1 t2@(TApp TUn _ _) = zipType δ (TApp TUn [t1] fTop) t2
zipType δ t1@(TApp TUn _ _) t2 = zipType δ t1 (TApp TUn [t2] fTop)

-- | Class/interface types
--
-- 
--   C<Vi> extends C'<Wi>
--   --------------------------------
--   C<Si> || C'<Ti> = C'<Wi[Vi/Si]>
--   
--   
--   C </: C'
--   ------------------------------------------------------
--   C<Si> || C'<Ti> = toStruct(C<Si>) || toStruct(C<Ti>)
--   
--   
--   C<Si> || {F;M} = toStruct(C<Si>) || {F;M}
--   
zipType δ t1@(TApp (TRef x1 s1) t1s r1) t2@(TApp (TRef x2 s2) t2s _) 
  | (x1,s1) == (x2,s2)
  = TApp (TRef x1 s1) (zipWith (zipType δ) t1s t2s) r1
  | otherwise
  = case weaken δ (findSymOrDie x1 δ, t1s) x2 of
      -- Try to move along the class hierarchy
      Just (_, t1s') -> zipType δ (TApp (TRef x2 s1) t1s' r1) t2
      -- Unfold structures
      Nothing        -> zipType δ (flattenType δ t1) (flattenType δ t2)

zipType δ t1@(TApp (TRef _ _) _ _) t2 = zipType δ (flattenType δ t1) t2
zipType δ t1 t2@(TApp (TRef _ _) _ _) = zipType δ t1 (flattenType δ t2)
 

zipType _ (TApp c [] r) (TApp c' [] _) | c == c' = TApp c [] r

-- | Top ??
zipType _ _ t2@(TApp TTop _ _ ) = t2

zipType _ (TVar v r) (TVar v' r') | v == v' = TVar v r

-- | Function types
--
--  (Si)=>S || (Ti)=>T = (Si||Ti)=>S||T
--
zipType δ (TFun x1s t1 r1) (TFun x2s t2 _) = 
    TFun xs y r1
  where
    xs = zipWith (zipBind δ) x1s x2s
    y  = zipType δ t1 t2

-- | Object types
--
--  { F1,F2 } | { F1',F3' } = { F1|F1',top(F3) }, where disjoint F2 F3'
--
zipType δ (TCons f1s m1 r1) (TCons f2s _ _) = 
    TCons (common' ++ disjoint') m1 r1
  where 
    common' = (uncurry $ zipElts δ) <$> common
    disjoint' = (rType <$>) <$> disjoint  -- top
    (common, disjoint) = partition [] [] f2s

    partition g1 g2 [] = (g1, g2)
    partition g1 g2 (e2:e2s) =
      case pick e2 of 
        [  ] -> partition g1 (e2:g2) e2s
        [ee] -> partition (ee:g1) g2 e2s
        ees  -> error $ "zipType: " ++ ppshow e2 ++ " got matched with " 
                                    ++ ppshow ees

    pick f =  [ (f1, f) | f1 <- f1s, compatible f1 f ]
    compatible e e' = sameBinder e e' && related δ e e'

-- | Intersection types
--
--  s1 /\ s2 .. /\ sn | t1 /\ t2 .. tm = s1'|t1' /\ .. sk'|tk' /\ .. top(tm')
--
zipType δ (TAnd t1s) (TAnd t2s) =
    case [ (pick t2, t2) | t2 <- t2s ] of
      []        -> error $ "ziptype: impossible intersection types" 
      [(t1,t2)] -> zipType δ t1 t2
      ts        -> TAnd $ (uncurry $ zipType δ) <$> ts
  where
    pick t = case [ t1 | t1 <- t1s, related δ t1 t ] of
               [t1] -> t1
               _    -> error $ "zipType: cannot match " ++ ppshow t 
                            ++ " with any part of " ++ ppshow t1s

zipType δ t1 (TAnd t2s) = zipType δ (TAnd [t1]) (TAnd t2s)
zipType δ (TAnd t1s) t2 = zipType δ (TAnd t1s) (TAnd [t2])

zipType _ t1 t2 = 
  errorstar $ printf "BUG[zipType]: mis-aligned types in:\n\t%s\nand\n\t%s" (ppshow t1) (ppshow t2)


zipBind δ (B s1 t1) (B s2 t2) = B s2 $ zipType δ t1 t2 


zipElts δ (CallSig t1) (CallSig t2)
  = CallSig $ zipType δ t1 t2 

zipElts δ (ConsSig t1) (ConsSig t2)       
  = ConsSig $ zipType δ t1 t2 

zipElts δ (IndexSig x1 b1 t1) (IndexSig x2 b2 t2)  
  = IndexSig x2 b2 $ zipType δ t1 t2 

zipElts δ (FieldSig x1 s1 m1 (Just τ1) t1) (FieldSig x2 s2 m2 (Just τ2) t2)
  = FieldSig x2 s2 m2 (Just $ zipType δ τ1 τ2) $ zipType δ t1 t2

zipElts δ (FieldSig x1 s1 m1 Nothing t1) (FieldSig x2 s2 m2 Nothing t2)
  = FieldSig x2 s2 m2 Nothing $ zipType δ t1 t2

zipElts δ (MethSig x1 s1 m1 (Just τ1) t1) (MethSig x2 s2 m2 (Just τ2) t2)
  = MethSig x2 s2 m2 (Just $ zipType δ τ1 τ2) $ zipType δ t1 t2

zipElts δ (MethSig x1 s1 m1 Nothing t1) (MethSig x2 s2 m2 Nothing t2)
  = MethSig x2 s2 m2 Nothing $ zipType δ t1 t2

zipElts _ e1 e2
  = error $ "Cannot zip: " ++ ppshow e1 ++ " and " ++ ppshow e2

