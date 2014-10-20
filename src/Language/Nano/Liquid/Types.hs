{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}

-- | Module pertaining to Refinement Type descriptions and conversions
--   Likely mergeable with @Language.Nano.Typecheck.Types@

module Language.Nano.Liquid.Types ( 
  
  -- * Refinement Types
    RefType 

  -- * Constraint Environments
  , CGEnvR(..), CGEnv

  -- * Constraint Information
  , Cinfo (..), ci

  -- * Constraints
  , SubC (..) , WfC (..), FixSubC   , FixWfC

  -- * Some Operators on Pred
  , pAnd, pOr

  -- * Conversions
  , RefTypable (..), eSingleton, pSingleton -- , shiftVVs

  -- * Manipulating RefType
  , rTypeReft, rTypeSort, rTypeSortedReft, rTypeValueVar

  -- * Predicates On RefType 
  , isTrivialRefType

  -- * Monadic map 
  , mapReftM

  -- * Useful Operations
  , foldReft, efoldRType, AnnTypeR

  -- * Accessing Spec Annotations
  , getSpec, getRequires, getEnsures, getAssume, getAssert
  , getInvariant, getFunctionIds
    -- ,  returnSymbol, returnId, symbolId, mkId

  -- * Raw low-level Location-less constructors
  , rawStringSymbol 

  -- Zip types
  , zipType

  ) where

import           Data.Maybe              (fromMaybe, catMaybes, maybeToList)
import qualified Data.List               as L
import qualified Data.HashMap.Strict     as HM
import qualified Data.Map.Strict         as M
-- import qualified Data.HashSet            as S
import           Data.Monoid                        (mconcat)
import qualified Data.Traversable        as T
import           Text.PrettyPrint.HughesPJ
import           Text.Printf 
import           Control.Applicative 
import           Control.Monad          (zipWithM)

import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.PrettyPrint

import           Language.Nano.Annots
import           Language.Nano.Errors
import           Language.Nano.Locations
import           Language.Nano.Misc
import           Language.Nano.Names
import           Language.Nano.Types
import           Language.Nano.Program
import           Language.Nano.Liquid.Environment
import           Language.Nano.Typecheck.Resolve
import           Language.Nano.Typecheck.Sub
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Typecheck.Types

import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.PrettyPrint
import           Language.Fixpoint.Errors
  
-- import           Debug.Trace                        (trace)
--

-- Got these from Resolve.hs
isAncestor        = error "Liquid.Type.isAncestor"
absolutePathInEnv = error "Liquid.Type.absolutePathInEnv"


-------------------------------------------------------------------------------------
-- | Refinement Types and Annotations
-------------------------------------------------------------------------------------

type RefType     = RType F.Reft
type AnnTypeR    = AnnType F.Reft

----------------------------------------------------------------------------
-- | Constraint Information 
----------------------------------------------------------------------------

data Cinfo = Ci { ci_info    :: !Error
                , ci_srcspan :: !SourceSpan
                } deriving (Eq, Ord, Show) 

ci   :: (IsLocated a) => Error -> a -> Cinfo
ci e = Ci e . srcPos 

instance PP Cinfo where
  pp (Ci e l)   = text "CInfo:" <+> pp l <+> (parens $ pp e)
  
instance IsLocated Cinfo where
  srcPos = ci_srcspan 

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
  pp (Sub γ i t t') = pp (cge_names γ) $+$ pp (cge_guards γ) 
                        $+$ ((text "|-") <+> (pp t $+$ text "<:" $+$ pp t'))
                        $+$ ((text "from:") <+> pp i) 

instance PP WfC where
  pp (W γ t i)      = pp (cge_names γ) 
                        $+$ (text "|-" <+> pp t) 
                        $+$ ((text "from:") <+> pp i) 

instance IsLocated SubC where
  srcPos = srcPos . sinfo

instance IsLocated WfC where
  srcPos = srcPos . winfo

-- | Aliases for Fixpoint Constraints

type FixSubC = F.SubC Cinfo
type FixWfC  = F.WfC  Cinfo


------------------------------------------------------------------
-- | Converting `ECMAScript3` values into `Fixpoint` values, 
--   i.e. *language* level entities into *logic* level entities.
------------------------------------------------------------------

instance F.Expression (Id a) where
  expr = F.eVar

instance F.Expression (LValue a) where
  expr = F.eVar

instance F.Expression (Expression a) where
  expr (IntLit _ i)                 = F.expr i
  expr (VarRef _ x)                 = F.expr x
  expr (InfixExpr _ o e1 e2)        = F.EBin (bop o) (F.expr e1) (F.expr e2)
  expr (PrefixExpr _ PrefixMinus e) = F.EBin F.Minus (F.expr (0 :: Int)) (F.expr e)  
  expr (Cast_ l e)                  = F.expr e
  expr (Cast  l e)                  = F.expr e
  expr e                            = convertError "F.Expr" e

instance F.Predicate  (Expression a) where 
  prop (BoolLit _ True)            = F.PTrue
  prop (BoolLit _ False)           = F.PFalse
  prop (PrefixExpr _ PrefixLNot e) = F.PNot (F.prop e)
  prop e@(InfixExpr _ _ _ _ )      = eProp e
  prop e                           = convertError "F.Pred" e  


------------------------------------------------------------------
eProp :: Expression a -> F.Pred
------------------------------------------------------------------

eProp (InfixExpr _ OpLT   e1 e2)       = F.PAtom F.Lt (F.expr e1) (F.expr e2) 
eProp (InfixExpr _ OpLEq  e1 e2)       = F.PAtom F.Le (F.expr e1) (F.expr e2) 
eProp (InfixExpr _ OpGT   e1 e2)       = F.PAtom F.Gt (F.expr e1) (F.expr e2)  
eProp (InfixExpr _ OpGEq  e1 e2)       = F.PAtom F.Ge (F.expr e1) (F.expr e2)  
eProp (InfixExpr _ OpEq   e1 e2)       = F.PAtom F.Eq (F.expr e1) (F.expr e2) 
-- XXX @==@ and @===@ are translated the same. This should not make a difference
-- as long as same type operands are used.
eProp (InfixExpr _ OpStrictEq   e1 e2) = F.PAtom F.Eq (F.expr e1) (F.expr e2) 
eProp (InfixExpr _ OpNEq  e1 e2)       = F.PAtom F.Ne (F.expr e1) (F.expr e2) 
eProp (InfixExpr _ OpLAnd e1 e2)       = pAnd (F.prop e1) (F.prop e2) 
eProp (InfixExpr _ OpLOr  e1 e2)       = pOr  (F.prop e1) (F.prop e2)
eProp e                                = convertError "InfixExpr -> F.Prop" e

------------------------------------------------------------------
bop       :: InfixOp -> F.Bop
------------------------------------------------------------------

bop OpSub = F.Minus 
bop OpAdd = F.Plus
bop OpMul = F.Times
bop OpDiv = F.Div
bop OpMod = F.Mod
bop o     = convertError "F.Bop" o

------------------------------------------------------------------
pAnd p q  = F.pAnd [p, q] 
pOr  p q  = F.pOr  [p, q]



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

rTypeSortedReft   ::  PPR r => RTypeQ q r -> F.SortedReft
rTypeSortedReft t = F.RR (rTypeSort t) (rTypeReft t)

rTypeReft         :: (F.Reftable r) => RTypeQ q r -> F.Reft
rTypeReft         = fromMaybe fTop . fmap F.toReft . stripRTypeBase 

rTypeValueVar     :: (F.Reftable r) => RTypeQ q r -> F.Symbol
rTypeValueVar t   = vv where F.Reft (vv,_) = rTypeReft t 

------------------------------------------------------------------------------------------
rTypeSort :: (PPR r) => RTypeQ q r -> F.Sort
------------------------------------------------------------------------------------------
rTypeSort (TVar α _)               = F.FObj $ F.symbol α 
rTypeSort (TAll v t)               = rTypeSortForAll $ TAll v t
rTypeSort (TFun (Just s) xts t _)  = F.FFunc 0 $ rTypeSort <$> [s] ++ (b_type <$> xts) ++ [t]
rTypeSort (TFun Nothing  xts t _)  = F.FFunc 0 $ rTypeSort <$> (b_type <$> xts) ++ [t]
rTypeSort (TApp c ts _)            = rTypeSortApp c ts 
rTypeSort (TAnd (t:_))             = rTypeSort t
rTypeSort (TRef (QN _ _ _ s) ts _) = F.FApp (rawStringFTycon $ F.symbolString s) (rTypeSort <$> ts)
rTypeSort (TCons _ _ _ )           = F.FApp (rawStringFTycon $ F.symbol "Object") []
rTypeSort (TClass _)               = F.FApp (rawStringFTycon $ F.symbol "class" ) []
rTypeSort (TModule _)              = F.FApp (rawStringFTycon $ F.symbol "module") []
rTypeSort (TEnum _)                = F.FApp (rawStringFTycon $ F.symbol "enum"  ) []
rTypeSort t                        = error $ render $ text "BUG: rTypeSort does not support " <+> pp t

rTypeSortApp TInt _                = F.FInt
rTypeSortApp TUn  _                = F.FApp (tconFTycon TUn) []
rTypeSortApp c ts                  = F.FApp (tconFTycon c) (rTypeSort <$> ts) 

tconFTycon :: TCon -> F.FTycon 
tconFTycon TInt                    = F.intFTyCon
tconFTycon TBool                   = rawStringFTycon "Boolean"
tconFTycon TFPBool                 = F.boolFTyCon
tconFTycon TVoid                   = rawStringFTycon "Void"
tconFTycon TUn                     = rawStringFTycon "Union"
tconFTycon TString                 = F.strFTyCon
tconFTycon TTop                    = rawStringFTycon "Top"
tconFTycon TNull                   = rawStringFTycon "Null"
tconFTycon TUndef                  = rawStringFTycon "Undefined"


rTypeSortForAll t    = genSort n θ $ rTypeSort tbody
  where 
    (αs, tbody)      = bkAll t
    n                = length αs
    θ                = HM.fromList $ zip (F.symbol <$> αs) (F.FVar <$> [0..])
    
genSort n θ (F.FFunc _ t)  = F.FFunc n (F.sortSubst θ <$> t)
genSort n θ t              = F.FFunc n [F.sortSubst θ t]

------------------------------------------------------------------------------------------
stripRTypeBase :: RTypeQ q r -> Maybe r 
------------------------------------------------------------------------------------------
stripRTypeBase (TApp _ _ r)   = Just r
stripRTypeBase (TRef _ _ r)   = Just r
stripRTypeBase (TVar _ r)     = Just r
stripRTypeBase (TFun _ _ _ r) = Just r
stripRTypeBase (TCons _ _ r)  = Just r
stripRTypeBase _              = Nothing

------------------------------------------------------------------------------------------
noKVars :: F.Reft -> F.Reft
------------------------------------------------------------------------------------------
noKVars (F.Reft (s,ras)) = F.Reft (s, [ c | c@(F.RConc _) <- ras ])

------------------------------------------------------------------------------------------
-- | Substitutions
------------------------------------------------------------------------------------------

instance (PPR r, F.Subable r) => F.Subable (RTypeQ q r) where
  syms        = foldReft (\r acc -> F.syms r ++ acc) [] 
  substa      = fmap . F.substa 
  substf f    = emapReft (F.substf . F.substfExcept f) [] 
  subst su    = emapReft (F.subst  . F.substExcept su) []
  subst1 t su = emapReft (\xs r -> F.subst1Except xs r su) [] t

------------------------------------------------------------------------------------------
-- | Traversals over @RType@ 
------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------
emapReft  :: PPR a => ([F.Symbol] -> a -> b) -> [F.Symbol] -> RTypeQ q a -> RTypeQ q b
------------------------------------------------------------------------------------------
emapReft f γ (TVar α r)        = TVar α (f γ r)
emapReft f γ (TApp c ts r)     = TApp c (emapReft f γ <$> ts) (f γ r)
emapReft f γ (TRef c ts r)     = TRef c (emapReft f γ <$> ts) (f γ r)
emapReft f γ (TAll α t)        = TAll α (emapReft f γ t)
emapReft f γ (TFun s xts t r)  = TFun (emapReft f γ' <$> s) (emapReftBind f γ' <$> xts) 
                                      (emapReft f γ' t) (f γ r) where γ' = (b_sym <$> xts) ++ γ
emapReft f γ (TCons m xts r)   = TCons m (M.map (emapReftElt f γ) xts) (f γ r)
emapReft _ _ (TClass c)        = TClass c
emapReft _ _ (TModule m)       = TModule m
emapReft _ _ (TEnum e)         = TEnum e
emapReft f γ (TAnd ts)         = TAnd (emapReft f γ <$> ts)
emapReft _ _ _                 = error "Not supported in emapReft"

emapReftBind f γ (B x t)       = B x $ emapReft f γ t

emapReftElt  :: PPR a => ([F.Symbol] -> a -> b) -> [F.Symbol] -> TypeMemberQ q a -> TypeMemberQ q b
emapReftElt f γ e              = fmap (f γ) e

mapReftM f (TVar α r)          = TVar α  <$> f r
mapReftM f (TApp c ts r)       = TApp c  <$> mapM (mapReftM f) ts <*> f r
mapReftM f (TRef c ts r)       = TRef c  <$> mapM (mapReftM f) ts <*> f r
mapReftM f (TFun s xts t r)    = TFun    <$> T.mapM (mapReftM f) s 
                                         <*> mapM (mapReftBindM f) xts 
                                         <*> mapReftM f t 
                                         <*> return (F.top r)
mapReftM f (TAll α t)          = TAll α  <$> mapReftM f t
mapReftM f (TAnd ts)           = TAnd    <$> mapM (mapReftM f) ts
mapReftM f (TCons m bs r)      = TCons m <$> T.mapM (mapReftEltM f) bs <*> f r
mapReftM _ (TClass a)          = return   $ TClass a
mapReftM _ (TModule a)         = return   $ TModule a
mapReftM _ (TEnum a)           = return   $ TEnum a
mapReftM _ t                   = error    $ render $ text "Not supported in mapReftM: " <+> pp t 

mapReftBindM f (B x t)         = B x     <$> mapReftM f t

mapReftEltM f (FieldSig x m t) = FieldSig x m <$> mapReftM f t
mapReftEltM f (MethSig x m t)  = MethSig x m  <$> mapReftM f t
mapReftEltM f (CallSig t)      = CallSig      <$> mapReftM f t
mapReftEltM f (ConsSig  t)     = ConsSig      <$> mapReftM f t
mapReftEltM f (IndexSig x b t) = IndexSig x b <$> mapReftM f t


------------------------------------------------------------------------------------------
-- | fold over @RType@ 
------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------
foldReft  :: PPR r => (r -> a -> a) -> a -> RTypeQ q  r -> a
------------------------------------------------------------------------------------------
foldReft  f = efoldReft (\_ -> ()) (\_ -> f) F.emptySEnv 

------------------------------------------------------------------------------------------
efoldReft :: PPR r => (RTypeQ q  r -> b) -> (F.SEnv b -> r -> a -> a) -> F.SEnv b -> a -> RTypeQ q  r -> a
------------------------------------------------------------------------------------------
efoldReft g f = go 
  where 
    go γ z (TVar _ r)       = f γ r z
    go γ z t@(TApp _ ts r)  = f γ r $ gos (efoldExt g (B (rTypeValueVar t) t) γ) z ts
    go γ z t@(TRef _ ts r)  = f γ r $ gos (efoldExt g (B (rTypeValueVar t) t) γ) z ts
    go γ z (TAll _ t)       = go γ z t
    go γ z (TFun s xts t r) = f γ r $ go γ' (gos γ' z (maybeToList s ++ map b_type xts)) t  where γ' = foldr (efoldExt g) γ xts
    go γ z (TAnd ts)        = gos γ z ts 
    go γ z (TCons _ bs r)   = f γ' r $ gos γ z (f_type <$> M.elems bs) where γ' = foldr (efoldExt' g) γ $ M.elems bs
    go _ z (TClass _)       = z
    go _ z (TModule _)      = z
    go _ z (TEnum _)        = z
    go _ _ t                = error $ "Not supported in efoldReft: " ++ ppshow t

    gos γ z ts              = L.foldl' (go γ) z ts

efoldExt g xt γ             = F.insertSEnv (b_sym xt) (g $ b_type xt) γ
efoldExt' g xt γ            = F.insertSEnv (F.symbol xt) (g $ f_type xt) γ

------------------------------------------------------------------------------------------
efoldRType :: PPR r => (RTypeQ q  r -> b) -> (F.SEnv b -> RTypeQ q  r -> a -> a) -> F.SEnv b -> a -> RTypeQ q  r -> a
------------------------------------------------------------------------------------------
efoldRType g f                 = go
  where
    go γ z t@(TVar _ _ )       = f γ t z
    go γ z t@(TApp _ ts _)     = f γ t $ gos (efoldExt g (B (rTypeValueVar t) t) γ) z ts
    go γ z t@(TRef _ ts _)     = f γ t $ gos (efoldExt g (B (rTypeValueVar t) t) γ) z ts
    go γ z t@(TAll _ t1)       = f γ t $ go γ z t1
    go γ z t@(TFun s xts t1 _) = f γ t $ go γ' (gos γ' z (maybeToList s ++ map b_type xts)) t1  where γ' = foldr (efoldExt g) γ xts
    go γ z   (TAnd ts)         = gos γ z ts 
    go γ z t@(TCons _ bs _ )   = f γ t $ gos γ' z (f_type <$> M.elems bs) where γ' = M.foldr (efoldExt' g) γ bs
    go _ _ t                   = error $ "Not supported in efoldRType: " ++ ppshow t
    gos γ z ts                 = L.foldl' (go γ) z ts


------------------------------------------------------------------------------------------
isTrivialRefType :: RefType -> Bool
------------------------------------------------------------------------------------------
-- | The only allowed top-level refinement of a function type is the
--   ('function') tag, So ignore this for this check.
isTrivialRefType (TFun a b c _) = isTrivialRefType' (TFun a b c fTop)
isTrivialRefType t              = isTrivialRefType' t

isTrivialRefType' t     = foldReft (\r -> (f r &&)) True t
  where 
    f (F.Reft (_,ras)) = null ras

rawStringSymbol = F.Loc (F.dummyPos "RSC.Types.rawStringSymbol") . F.symbol
rawStringFTycon = F.symbolFTycon . F.Loc (F.dummyPos "RSC.Types.rawStringFTycon") . F.symbol


-----------------------------------------------------------------------------------
-- | Helpers for extracting specifications from @ECMAScript3@ @Statement@ 
-----------------------------------------------------------------------------------

getInvariant :: Statement a -> F.Pred 

getInvariant = getSpec getInv . flattenStmt

getAssume    :: Statement a -> Maybe F.Pred 
getAssume    = getStatementPred "assume"

getAssert    :: Statement a -> Maybe F.Pred 
getAssert    = getStatementPred "assert"

getRequires  = getStatementPred "requires"
getEnsures   = getStatementPred "ensures"
getInv       = getStatementPred "invariant"

getStatementPred :: String -> Statement a -> Maybe F.Pred 
getStatementPred name (ExprStmt _ (CallExpr _ (VarRef _ (Id _ f)) [p]))
  | name == f 
  = Just $ F.prop p
getStatementPred _ _ 
  = Nothing 

getSpec   :: (Statement a -> Maybe F.Pred) -> [Statement a] -> F.Pred 
getSpec g = mconcat . catMaybes . map g

getFunctionIds :: Statement a -> [Id a]
getFunctionIds s = [f | (FunctionStmt _ f _ _) <- flattenStmt s]


-- | @zipType@ returns a type that is:
--
--  * structurally equivalent to @t2@
--
--  * semantically equivalent to @t1@
--
--------------------------------------------------------------------------------
zipType :: IsLocated l => l -> CGEnv -> RefType -> RefType -> Maybe RefType
--------------------------------------------------------------------------------
-- 
-- Unions
--
zipType l γ (TApp TUn t1s r1) (TApp TUn t2s _) =
    (`mkUnionR` r1)           <$> mapM rr t2s
  where
    rr               t2        =  L.find (related γ t2) t1s `relate` t2
    relate (Just t1) t2        =  return $ t1 `strengthen` noKVars r1
    relate Nothing   t2        =  return $ fmap F.bot t2

zipType l γ t1 t2@(TApp TUn _ _) =  zipType l γ (TApp TUn [t1] fTop) t2

zipType l γ (TApp TUn t1s r1) t2 =  L.find (related γ t2) t1s `relate` t2
  where
    relate (Just t1) t2        =  zipType l γ (t1 `strengthen` noKVars r1) t2
    relate Nothing   t2        =  return $ fmap F.bot t2
--
-- No unions below this point
--
-- Class/interface types
-- 
--   C<Vi> extends C'<Wi>
--   --------------------------------
--   C<Si> || C'<Ti> = C'<Wi[Vi/Si]>
--   
--   C </: C'
--   ------------------------------------------------------
--   C<Si> || C'<Ti> = toStruct(C<Si>) || toStruct(C<Ti>)
--   
--   
--   C<Si> || {F;M} = toStruct(C<Si>) || {F;M}
--   
zipType l γ t1@(TRef x1 (_:t1s) r1) t2@(TRef x2 (m2:t2s) _) 
  | x1 == x2
  = do  ts    <- zipWithM (zipType l γ) t1s t2s
        return $ TRef x2 (m2:ts) r1

  | otherwise
  = case weaken γ (x1,t1s) x2 of
      -- Try to move along the class hierarchy
      Just (_, t1s') -> zipType l γ (TRef x2 (m2:t1s') r1 `strengthen` reftIO t1 (F.symbol x1)) t2

      -- Unfold structures
      Nothing        -> do  t1' <- flattenType γ t1 
                            t2' <- flattenType γ t2
                            zipType l γ t1' t2'
  where
    reftIO t c = F.Reft (vv t, [refaIO t c])
    refaIO t c = F.RConc $ F.PBexp $ F.EApp sym [F.expr $ vv t, F.expr  $ F.symbolText c]
    vv         = rTypeValueVar
    sym        = F.dummyLoc $ F.symbol "instanceof"

zipType l _ t1@(TRef _ [] _) _ = error $ "zipType l on " ++ ppshow t1   -- Invalid type
zipType l _ _ t2@(TRef _ [] _) = error $ "zipType l on " ++ ppshow t2  -- Invalid type

zipType l γ t1@(TRef _ _ _) t2 = do t1' <- flattenType γ t1
                                    zipType l γ t1' t2

zipType l γ t1 t2@(TRef _ _ _) = do t2' <- flattenType γ t2 
                                    zipType l γ t1 t2'

zipType l γ t1@(TClass x1) t2@(TClass x2) 
  | x2 `elem` ancestors γ x1
  = return $ TClass x2
  | otherwise 
  = do  t1' <- flattenType γ t1 
        t2' <- flattenType γ t2
        zipType l γ t1' t2'

zipType l γ t1@(TClass _) t2 = do t1' <- flattenType γ t1
                                  zipType l γ t1' t2

zipType l γ t1 t2@(TClass _) = do t2' <- flattenType γ t2
                                  zipType l γ t1 t2'

zipType l _ (TApp c [] r) (TApp c' [] _) | c == c'  = return $ TApp c [] r

zipType l _ _ t2                         | isTop t2 = return t2

zipType l _ (TVar v r) (TVar v' _)       | v == v'  = return $ TVar v r
-- 
-- Function types
--
-- (Si)=>S || (Ti)=>T = (Si||Ti)=>S||T
--
zipType l γ (TFun (Just s1) x1s t1 r1) (TFun (Just s2) x2s t2 _) = 
  TFun <$> Just <$> zipType l γ s1 s2 
       <*> zipWithM (zipBind l γ) x1s x2s 
       <*> zipType l γ t1 t2 
       <*> return r1

zipType l γ (TFun Nothing x1s t1 r1) (TFun Nothing x2s t2 _) = 
  TFun Nothing <$> zipWithM (zipBind l γ) x1s x2s 
               <*> zipType l γ t1 t2 
               <*> return r1

zipType l _ (TFun _ _ _ _ ) (TFun _ _ _ _) = Nothing
-- 
-- Object types
--
-- { F1,F2 } | { F1',F3' } = { F1|F1',top(F3) }, where disjoint F2 F3'
--
zipType l γ (TCons _ e1s r1) (TCons m2 e2s _) = do 
    common'            <- T.mapM (uncurry $ zipElts l γ) common
    return              $ TCons m2 (common' `M.union` disjoint') r1
  where 
    common              = M.intersectionWith (,) e1s e2s
    disjoint'           = M.map (fmap $ const fTop) $ e2s `M.difference` e1s
-- 
-- Intersection types
--
-- s1 /\ s2 .. /\ sn | t1 /\ t2 .. tm = s1'|t1' /\ .. sk'|tk' /\ .. top(tm')
--
zipType l γ (TAnd t1s) (TAnd t2s) =
    case [ (pick t2, t2) | t2 <- t2s ] of
      []        -> error $ "zipType l: impossible intersection types" 
      [(t1,t2)] -> zipType l γ t1 t2
      ts        -> TAnd <$> mapM (uncurry $ zipType l γ) ts
  where
    pick t2            = fromCandidates [ t1 | t1 <- t1s, t1 `matches` t2 ]
    t `matches` t'     = isSubtype γ t t' || isSubtype γ t' t 
    fromCandidates [ ] = die $ bug (srcPos l) 
                       $ "zipType l: cannot match " ++ ppshow t2s ++ " with any of " ++ ppshow t1s
    fromCandidates [t] = t
    fromCandidates  _  = die $ bug (srcPos l) 
                       $ "zipType l: multiple matches of " ++ ppshow t2s ++ " with " ++ ppshow t1s

zipType l γ t1 (TAnd t2s) = zipType l γ (TAnd [t1]) (TAnd t2s)
zipType l γ (TAnd t1s) t2 = zipType l γ (TAnd t1s) (TAnd [t2])

-- FIXME: preserve t1's ref in all occurences of TVar v1 in the LHS
zipType l γ (TAll v1 t1) (TAll v2 t2) = 
    TAll v1 <$> zipType l γ t1 (apply θ t2) 
  where
    θ = fromList [(v2, tVar v1 :: RefType)]

zipType l _ t1 t2 = errorstar $ printf "BUG[zipType l] Unsupported:\n\t%s\nand\n\t%s" (ppshow t1) (ppshow t2)


zipBind l γ (B _ t1) (B s2 t2) = B s2 <$> zipType l γ t1 t2 

------------------------------------------------------------------------------------------
zipElts :: IsLocated l => l -> CGEnv -> TypeMember F.Reft -> TypeMember F.Reft -> Maybe (TypeMember F.Reft) 
------------------------------------------------------------------------------------------
zipElts l γ (CallSig t1)      (CallSig t2)        = CallSig        <$> zipType l γ t1 t2 
zipElts l γ (ConsSig t1)      (ConsSig t2)        = ConsSig        <$> zipType l γ t1 t2 
zipElts l γ (IndexSig _ _ t1) (IndexSig x2 b2 t2) = IndexSig x2 b2 <$> zipType l γ t1 t2 
zipElts l γ (FieldSig _ _ t1) (FieldSig x2 m2 t2) = FieldSig x2 m2 <$> zipType l γ t1 t2
zipElts l γ (MethSig _ _  t1) (MethSig x2 m2 t2)  = MethSig  x2 m2 <$> zipType l γ t1 t2
zipElts l _ _                 _                   = Nothing

