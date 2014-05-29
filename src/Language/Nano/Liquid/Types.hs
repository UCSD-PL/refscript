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

  -- Tag 
  -- , tagR


  ) where

import           Data.Maybe             (fromMaybe, maybeToList, fromJust)
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
{-eSingleton t e  | isUnion t = t `strengthen` (F.uexprReft e)-}
{-                | otherwise = t `strengthen` (F.uexprReft e)-}

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
rTypeSort   (TCons _ _ )   = F.FObj $ F.symbol "cons"
rTypeSort t                = error $ render $ text "BUG: rTypeSort does not support " <+> pp t

rTypeSortApp TInt _  = F.FInt
rTypeSortApp TUn  _  = F.FApp (tconFTycon TUn) [] -- simplifying union sorts, the checks will have been done earlier
rTypeSortApp c ts    = F.FApp (tconFTycon c) (rTypeSort <$> ts) 

tconFTycon :: TCon -> F.FTycon 
tconFTycon TInt         = F.intFTyCon
tconFTycon TBool        = rawStringFTycon "Boolean"
tconFTycon TFPBool      = F.boolFTyCon
tconFTycon TVoid        = rawStringFTycon "Void"
tconFTycon (TRef (s,_)) = rawStringFTycon $ F.symbolString s
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
stripRTypeBase (TApp _ _ r) = Just r
stripRTypeBase (TVar _ r)   = Just r
stripRTypeBase (TFun _ _ r) = Just r
stripRTypeBase (TCons _ r)  = Just r
stripRTypeBase _            = Nothing
 
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
emapReft f γ (TVar α r)     = TVar α (f γ r)
emapReft f γ (TApp c ts r)  = TApp c (emapReft f γ <$> ts) (f γ r)
emapReft f γ (TAll α t)     = TAll α (emapReft f γ t)
emapReft f γ (TFun xts t r) = TFun (emapReftBind f γ' <$> xts) (emapReft f γ' t) (f γ r) 
  where    γ'               = (b_sym <$> xts) ++ γ 
emapReft f γ (TCons xts r)  = TCons (emapReftElt f γ' <$> xts) (f γ r)
  where    γ'               = (f_sym <$> xts) ++ γ 
emapReft _ _ _              = error "Not supported in emapReft"

emapReftBind f γ (B x t)    = B x $ emapReft f γ t

emapReftElt  :: PPR a => ([F.Symbol] -> a -> b) -> [F.Symbol] -> TElt (RType a) -> TElt (RType b)
emapReftElt f γ e            = fmap (emapReft f γ) e

mapReftM f (TVar α r)      = TVar α <$> f r
mapReftM f (TApp c ts r)   = TApp c <$> mapM (mapReftM f) ts <*> f r
mapReftM f (TFun xts t r)  = TFun   <$> mapM (mapReftBindM f) xts <*> mapReftM f t <*> (return $ F.top r) --f r 
mapReftM f (TAll α t)      = TAll α <$> mapReftM f t
mapReftM f (TAnd ts)       = TAnd   <$> mapM (mapReftM f) ts
mapReftM f (TCons bs r)    = TCons  <$> mapM (mapReftEltM f) bs <*> f r
mapReftM _ t               = error   $ render $ text "Not supported in mapReftM: " <+> pp t 

mapReftBindM f (B x t)    = B x     <$> mapReftM f t

mapReftEltM f (PropSig x s m τ t) = PropSig x s m <$> mapReftMaybeM f τ <*> mapReftM f t
mapReftEltM f (MethSig x s τ t)   = MethSig x s   <$> mapReftMaybeM f τ <*> mapReftM f t
mapReftEltM f (CallSig t)         = CallSig       <$> mapReftM f t
mapReftEltM f (ConsSig  t)        = ConsSig       <$> mapReftM f t
mapReftEltM f (IndexSig x b t)    = IndexSig x b  <$> mapReftM f t

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
    go γ z (TCons bs r)     = f γ' r $ gos γ z (f_type <$> bs) where γ' = foldr (efoldExt' g) γ bs
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
    go γ z t@(TCons xts _)   = f γ t $ gos γ' z (f_type <$> xts) where γ' = foldr (efoldExt' g) γ xts
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


-- | `zipType` pairs up equivalent parts of types @t1@ and @t2@, preserving the
-- type structure of @t2@, and applying @f@ whenever refinements are present on 
-- both sides and @g@ whenever the respective part in type @t2@ is missing.
--------------------------------------------------------------------------------
zipType :: TDefEnv RefType     -> 
  (F.Reft -> F.Reft -> F.Reft) ->   -- applied to refs that are present on both sides
  (F.Reft -> F.Reft)           ->   -- applied when pred is absent on the LHS
  RefType -> RefType -> RefType
--------------------------------------------------------------------------------
--
--                                | t1|_t1' \/ .. tm|_tm' \/ .. tn|g
--                                |         , if n > m, ti ~ ti'
-- t1 \/ .. tn |_ t1' \/ .. tm' = |
--                                | t1|_t1' \/ .. tm|_tm'              
--                                |         , if n <= m, ti ~ ti'
--
zipType δ f g (TApp TUn t1s r1) (TApp TUn t2s r2) 
  = TApp TUn (cmn ++ snd) $ f r1 r2
  where
    cmn        = [ zipType δ f g τ1 τ2 | τ2 <- t2s
                                       , τ1 <- maybeToList $ L.find (equiv τ2) t1s ]
    snd        = fmap g <$> [ t        | t <- t2s
                                       , not $ exists (equiv t) t1s ]
zipType δ f g t1 t2@(TApp TUn _ _ ) 
  = zipType δ f g (TApp TUn [t1] fTop) t2
  -- = zipType δ f g (TApp TUn [t1] $ rTypeReft t1) t2

zipType δ f g (TApp TUn t1s r1) t2 
  = zipType δ f g ((fromJust $ L.find (equiv t2) t1s) `strengthen` r1) t2
                                       

zipType δ f g t1@(TApp (TRef i1) t1s r1) t2@(TApp (TRef i2) t2s r2) 
  | i1 == i2  
  = TApp (TRef i1) (zipWith (zipType δ f g) t1s t2s) $ f r1 r2
  | otherwise 
  = on (zipType δ f g) (flattenType δ) t1 t2 
 
zipType _ f _ (TApp c [] r) (TApp c' [] r') 
  | c == c' = TApp c [] $ f r r'

zipType _ f _ (TVar v r) (TVar v' r') 
  | v == v' = TVar v $ f r r'

zipType δ f g (TFun x1s t1 r1) (TFun x2s t2 r2) 
  = TFun xs y $ f r1 r2
  where
    xs = zipWith (zipBind δ f g) x1s x2s
    y  = zipType δ f g t1 t2

zipType δ f g (TCons e1s r1) (TCons e2s r2) 
  = TCons (cmn ++ snd) (f r1 r2)
  where 
    -- FIXME: mutabilities? m1 `mconcat` m2
    cmn = [ zipElts (zipType δ f g) e1 e2 | e1 <- e1s, e2 <- e2s, e1 `sameBinder` e2 ] 
    snd = [ e        | e <- e2s , not (eltSym e `elem` ks1) ]
    ks1 = [ eltSym e | e <- e1s ]

zipType _ _ _ t1 t2 = 
  errorstar $ printf "BUG[zipType]: mis-aligned types in:\n\t%s\nand\n\t%s" (ppshow t1) (ppshow t2)


zipBind δ f g (B s1 t1) (B s2 t2) = B s2 $ zipType δ f g t1 t2 


-- | Tags 
-- --------------------------------------------------------------------------------
-- tagR                        :: RType F.Reft -> F.Reft
-- --------------------------------------------------------------------------------
-- tagR t                       = predReft (rTypeValueVar t) $ F.pOr ps -- por ps
--   where
--     por []                   = F.PTrue
--     por [p]                  = p
--     por ps                   = F.POr ps
--     predReft v p             = F.Reft (v, [F.RConc $ F.prop p])
--     ps                       = F.PAtom F.Eq tagCall <$> tagStrs
--     tagCall                  = F.EApp tagSym [v]
--     v                        = F.EVar $ rTypeValueVar t
--     tagStrs                  = F.expr . F.symbol . ("lit#" ++) <$> tof t
--     tagSym                   = F.Loc F.dummyPos (F.symbol "ttag")
--     tof                     :: RefType -> [String]
--     tof t | isTFun t         = ["function"]
--     tof (TApp TInt _ _)      = ["number"]
--     tof (TApp TBool _ _)     = ["boolean"]
--     tof (TApp TString _ _)   = ["string"]
--     tof (TApp TNull _ _)     = ["object"]
--     tof (TApp (TRef _ ) _ _) = ["object"]
--     tof (TCons _   _ )       = ["object"]
--     tof (TApp TUn ts _)      = [] -- concatMap tof ts
--     tof (TApp TUndef _ _ )   = ["undefined"]
--     tof _                    = []
-- 
