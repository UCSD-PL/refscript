-- | Global type definitions for Refinement Type Checker

{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE DeriveFoldable            #-}
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

  -- * Programs
    Nano (..)
  , NanoBare, NanoSSA, NanoBareR, NanoSSAR, NanoRefType, NanoTypeR, NanoType, ExprSSAR, StmtSSAR
  , Source (..)
  , FunctionStatement

  -- * (Refinement) Types
  , RType (..), Bind (..), toType, ofType, rTop, strengthen 

  -- * Predicates on Types 
  , isTop, isNull, isVoid, isTNum, isUndef, isUnion

  -- * Constructing Types
  , mkUnion, mkFun, mkAll

  -- * Deconstructing Types
  , bkFun, bkFuns, bkAll, bkAnd, bkUnion, unionParts, unionParts', funTys
  
  -- Union ops
  , rUnion, rTypeR, setRTypeR
  
  , renameBinds

  -- * Regular Types
  , Type, TDef (..), TVar (..), TCon (..), TElt (..)

  -- * Mutability
  , Mutability, defaultM, mutableM, combMut, isMutable, isImmutable, isMutabilityType

  -- * Primitive Types
  , tInt, tBool, tString, tTop, tVoid, tErr, tFunErr, tVar, tArr, rtArr, tUndef, tNull
  , tAnd, isTVar, isRigid, isArr, isTObj, isConstr, isTFun, fTop, orNull

  -- * Print Types
  , ppArgs

  -- * Type comparison
--   , Equivalent
--   , equiv
  , sameBinder, eltType, zipElts, isStaticElt, nonStaticElt, nonConstrElt, mutability

  -- * Type definition env
  , TDefEnv (..), tDefEmpty, tDefFromList, tDefToList
  , addSym, findSym, findSymOrDie

  -- * Operator Types
  , infixOpTy, prefixOpTy, builtinOpTy, arrayLitTy, objLitTy, setPropTy

  -- * Annotations
  , Annot (..), UFact, Fact (..), phiVarsAnnot, ClassInfo
  
  -- * Casts
  , Cast(..), noCast, upCast, dnCast, ddCast

  -- * Aliases for annotated Source 
  , AnnBare, UAnnBare, AnnSSA , UAnnSSA
  , AnnType, UAnnType, AnnInfo, UAnnInfo

  -- * Contexts
  , CallSite (..), IContext, emptyContext, pushContext

  -- * Assignability 
  , Assignability (..), definedGlobs,  writeGlobalVars, readOnlyVars  

  -- * Aliases
  , Alias (..), TAlias, PAlias, PAliasEnv, TAliasEnv

  ) where 

import           Text.Printf
import           Data.Hashable
import           Data.Either                    (partitionEithers)
import           Data.Function                  (on)
import           Data.Maybe                     (fromMaybe, listToMaybe)
import           Data.Traversable               hiding (sequence) 
import           Data.Foldable                  (Foldable()) 
import           Data.Monoid                    hiding ((<>))            
import qualified Data.List                      as L
import qualified Data.IntMap                    as I
import qualified Data.HashMap.Strict            as M
import           Data.Generics                   
import           Data.Typeable                  ()
import           Language.ECMAScript3.Syntax    hiding (Cast)
import           Language.ECMAScript3.Syntax.Annotations
import           Language.ECMAScript3.PrettyPrint
import           Language.Nano.Types
import           Language.Nano.Errors
import           Language.Nano.Env

import qualified Language.Fixpoint.Types        as F
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Errors
import           Language.Fixpoint.PrettyPrint
import           Text.PrettyPrint.HughesPJ 

import           Control.Applicative            hiding (empty)
import           Control.Monad.Error            ()

-- import           Debug.Trace (trace)

-- | Type Variables
data TVar = TV { tv_sym :: F.Symbol
               , tv_loc :: SourceSpan 
               }
            deriving (Show, Ord, Data, Typeable)

instance Eq TVar where 
  a == b = tv_sym a == tv_sym b

instance IsLocated TVar where 
  srcPos = tv_loc

instance Hashable TVar where 
  hashWithSalt i α = hashWithSalt i $ tv_sym α 

instance F.Symbolic TVar where
  symbol = tv_sym 

instance F.Symbolic a => F.Symbolic (Located a) where 
  symbol = F.symbol . val


---------------------------------------------------------------------------------
-- | Mutability
---------------------------------------------------------------------------------

type Mutability = Type 

validMutNames = F.symbol <$> ["ReadOnly", "Mutable", "Immutable", "DefaultMutable"]

mkMut :: String -> Mutability
mkMut s = TApp (TRef (F.symbol s) False) [] ()

defaultM  = mkMut "DefaultMutable"
mutableM  = mkMut "Mutable"


isMutabilityType (TApp (TRef s False) _ _) = s `elem` validMutNames
isMutabilityType _                         = False

isMutable        (TApp (TRef s False) _ _) = s == F.symbol "Mutable"
isMutable _                                = False

isImmutable      (TApp (TRef s False) _ _) = s == F.symbol "Immutable"
isImmutable _                              = False

-- isDefaultMut (TApp (TRef s False) _ _) 
--   | s == F.symbol "DefaultMutable" = True 
-- isDefaultMut _ = False

combMut _ μf | isMutable μf    = μf
combMut μ _  | otherwise       = μ




---------------------------------------------------------------------------------
-- | Data types 
---------------------------------------------------------------------------------

data TDef t    = TD { 
        t_class :: Bool                           -- ^ Is this a class or interface type
      , t_name  :: !(Id SourceSpan)               -- ^ Name (possibly no name)
      , t_args  :: ![TVar]                        -- ^ Type variables
      , t_proto :: !(Maybe (Id SourceSpan, [t]))  -- ^ Heritage
      , t_elts  :: ![TElt t]                      -- ^ List of data type elts 
      } deriving (Eq, Ord, Show, Functor, Data, Typeable, Traversable, Foldable)

-- | Assignability is ingored atm.
-- TODO Mutability for CallSig and ConsSig?
data TElt t    = CallSig  { f_type :: t }         -- Call Signature
               | ConsSig  { f_type :: t }         -- Constructor Signature               
               | IndexSig { f_sym :: F.Symbol
                          , f_key :: Bool         -- Index Signature (T/F=string/number)
                          , f_type :: t }         
              
               | FieldSig { f_sym  :: F.Symbol    -- Name  
                          , f_sta  :: Bool        -- Static or not
                          , f_mut  :: Mutability  -- Mutability
                          , f_this :: Maybe t     -- Required object type
                          , f_type :: t }         -- Property type
  deriving (Ord, Show, Functor, Data, Typeable, Traversable, Foldable)


-- | Type definition environment

data TDefEnv t = G  { g_size  :: Int, g_env   :: F.SEnv (TDef t) } 
  deriving (Show, Functor, Data, Typeable, Foldable, Traversable)

instance F.Fixpoint t => F.Fixpoint (TDef t) where
  toFix (TD _ n _ _ _) = F.toFix $ F.symbol n

tDefEmpty = G 0 F.emptySEnv

---------------------------------------------------------------------------------
tDefFromList :: F.Symbolic s => [(s, TDef t)] -> TDefEnv t
---------------------------------------------------------------------------------
tDefFromList = foldr (uncurry addSym) tDefEmpty 


---------------------------------------------------------------------------------
tDefToList :: TDefEnv t -> [TDef t]
---------------------------------------------------------------------------------
tDefToList (G _ m) = snd <$> F.toListSEnv m 


---------------------------------------------------------------------------------
addSym :: F.Symbolic s => s -> TDef t -> TDefEnv t -> TDefEnv t
---------------------------------------------------------------------------------
addSym c t (G sz γ) =
  case F.lookupSEnv s γ of 
    Just _  -> G sz γ
    Nothing -> G sz' (F.insertSEnv s t γ)
  where
    s    = F.symbol c
    sz'  = sz + 1

---------------------------------------------------------------------------------
findSym :: F.Symbolic s => s -> TDefEnv t -> Maybe (TDef t)
---------------------------------------------------------------------------------
findSym s (G _ γ) = F.lookupSEnv (F.symbol s) γ

---------------------------------------------------------------------------------
findSymOrDie:: F.Symbolic s => s -> TDefEnv t -> TDef t
---------------------------------------------------------------------------------
findSymOrDie s γ = fromMaybe (error msg) $ findSym s γ 
  where msg = "findSymOrDie failed on: " ++ F.symbolString (F.symbol s)


-- | Type Constructors
data TCon
  = TInt
  | TBool
  | TString
  | TVoid
  | TTop
  | TRef F.Symbol Bool   -- The boolean is for static (T/F)
  | TUn
  | TNull
  | TUndef

  | TFPBool
    deriving (Ord, Show, Data, Typeable)

-- | (Raw) Refined Types 
data RType r  
  = TApp TCon [RType r]     r           -- ^ C T1,...,Tn
  | TVar TVar               r           -- ^ A
  | TFun [Bind r] (RType r) r           -- ^ (xi:T1,..,xn:Tn) => T
  | TCons [TElt (RType r)] Mutability r -- ^ {f1:T1,..,fn:Tn} 
  | TAll TVar (RType r)                 -- ^ forall A. T
  | TAnd [RType r]                      -- ^ (T1..) => T1' /\ ... /\ (Tn..) => Tn' 

  | TExp F.Expr                         -- ^ "Expression" parameters for type-aliases: 
                                        --   never appear in real/expanded RType

    deriving (Ord, Show, Functor, Data, Typeable, Traversable, Foldable)

data Bind r
  = B { b_sym  :: F.Symbol      -- ^ Binding's symbol
      , b_type :: !(RType r)    -- ^ Field type
      } 
    deriving (Eq, Ord, Show, Functor, Data, Typeable, Traversable, Foldable)

-- | "pure" top-refinement
fTop :: (F.Reftable r) => r
fTop = mempty

-- | Standard Types
type Type    = RType ()

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

funTy l xs (αs, yts, t) 
  | length xs == length yts = let (su, ts') = renameBinds yts xs 
                              in  Right (αs, ts', F.subst su t)    
  | otherwise               = Left $ errorArgMismatch (srcPos l)

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



-- | `unionParts` is a special case of `unionParts'` that uses Equivalent as 
-- the type equivalence relation.
--------------------------------------------------------------------------------
unionParts ::  RType r -> RType r -> ([(RType r, RType r)], [RType r], [RType r])
--------------------------------------------------------------------------------
unionParts = unionParts' (==)


-- General purpose function that pairs up the components of the two union typed
-- inputs @t1@ and @t2@, based on the Equivalence relation @eq@.
-- Top-level refinements are lost here - use `compareUnion` to preserve them.
-- The output consists of 
-- * The paired-up types (common as per @eq@)
-- * The types appearing just in @t1@
-- * The types appearing just in @t2@
--------------------------------------------------------------------------------
unionParts' :: (RType r -> RType r -> Bool) -> RType r -> RType r 
          -> ([(RType r, RType r)], [RType r], [RType r])
--------------------------------------------------------------------------------
unionParts' eq t1 t2 = (common t1s t2s, d1s, d2s)
  where
    (t1s, t2s) = sanityCheck $ mapPair bkUnion (t1, t2)
    (d1s, d2s) = distinct t1s t2s
    -- Compare the types based on the Equivalence relation and pair them up into
    -- 1. type structures that are common in both sides, and
    common xs ys | any null [xs,ys] = []
    common xs ys | otherwise        = [(x,y) | x <- xs, y <- ys, x `eq` y ]
    -- 2. type structures that are distinct in the two sides
    distinct xs [] = ([], xs)
    distinct [] ys = (ys, [])
    distinct xs ys = ([x | x <- xs, not $ any (x `eq`) ys ],
                      [y | y <- ys, not $ any (y `eq`) xs ])
    sanityCheck ([ ],[ ]) = errorstar "unionParts', called on too small input"
    sanityCheck ([_],[ ]) = errorstar "unionParts', called on too small input"
    sanityCheck ([ ],[_]) = errorstar "unionParts', called on too small input"
    sanityCheck ([_],[_]) = errorstar "unionParts', called on too small input"
    sanityCheck p         = p


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

isTObj (TApp (TRef _ _) _ _) = True
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
rTypeR               :: RType r -> r
rTypeR (TApp _ _ r ) = r
rTypeR (TVar _ r   ) = r
rTypeR (TFun _ _ r ) = r
rTypeR (TCons _ _ r) = r
rTypeR (TAll _ _   ) = errorstar "Unimplemented: rTypeR - TAll"
rTypeR (TAnd _ )     = errorstar "Unimplemented: rTypeR - TAnd"
rTypeR (TExp _)      = errorstar "Unimplemented: rTypeR - TExp"

-- Set the top-level refinement (wherever applies)
setRTypeR :: RType r -> r -> RType r
setRTypeR (TApp c ts _   ) r = TApp c ts r
setRTypeR (TVar v _      ) r = TVar v r
setRTypeR (TFun xts ot _ ) r = TFun xts ot r
setRTypeR t                _ = t


instance Eq TCon where
  TInt    == TInt    = True   
  TBool   == TBool   = True           
  TString == TString = True
  TVoid   == TVoid   = True         
  TTop    == TTop    = True
  TRef x1 s1 == TRef x2 s2 = (x1,s1) == (x2,s2)
  TUn     == TUn     = True
  TNull   == TNull   = True
  TUndef  == TUndef  = True
  TFPBool == TFPBool = True
  _       == _       = False
 

-- Ignoring refinements in equality check
instance Eq (RType r) where
  TApp TUn t1 _  == TApp TUn t2 _  = (null $ t1 L.\\ t2) && (null $ t2 L.\\ t1)
  TApp c1 t1s _  == TApp c2 t2s _  = (c1, t1s) == (c2, t2s)
  TVar v1 _      == TVar v2 _      = v1        == v2
  TFun b1 t1 _   == TFun b2 t2 _   = (b_type <$> b1, t1)  == (b_type <$> b2, t2)
  TAll v1 t1     == TAll v2 t2     = (v1,t1)  == (v2,t2)   -- Very strict Eq here
  TAnd t1s       == TAnd t2s       = t1s == t2s
  TCons e1s m1 _ == TCons e2s m2 _ = (e1s,m1) == (e2s,m2)
    where
      (e1s', e2s') = mapPair (L.sortBy (compare `on` F.symbol)) (e1s, e2s)
  _             == _              = False


instance Eq t => Eq (TElt t) where 
  CallSig t1              == CallSig t2              = t1 == t2
  ConsSig t1              == ConsSig t2              = t1 == t2
  IndexSig _ b1 t1        == IndexSig _ b2 t2        = (b1,t1) == (b2,t2)
  FieldSig f1 s1 m1 τ1 t1 == FieldSig f2 s2 m2 τ2 t2 = (f1,s1,m1,τ1,t1) == (f2,s2,m2,τ2,t2)
  _                       == _                       = False
 


---------------------------------------------------------------------------------
-- | Nano Program = Code + Types for all function binders
---------------------------------------------------------------------------------

data Nano a t = Nano { fp     :: FilePath                  -- ^ FilePath
                     , code   :: !(Source a)               -- ^ Code to check
                     , externs:: !(Env t)                  -- ^ Imported (unchecked) specifications 
                     , specs  :: !(Env t)                  -- ^ Function specs and 
                     -- , glVars :: !(Env t)                  -- ^ Global (annotated) vars
                     , consts :: !(Env t)                  -- ^ Measure Signatures
                     , defs   :: !(TDefEnv t)              -- ^ Type definitions
                                                           -- ^ After TC will also include class types
							       , tAlias :: !(TAliasEnv t)            -- ^ Type aliases
                     , pAlias :: !(PAliasEnv)              -- ^ Predicate aliases
                     , quals  :: ![F.Qualifier]            -- ^ Qualifiers
                     , invts  :: ![Located t]              -- ^ Type Invariants
                     } deriving (Functor, Data, Typeable)

type NanoBareR r   = Nano (AnnBare r) (RType r)            -- ^ After Parse
type NanoSSAR r    = Nano (AnnSSA  r) (RType r)            -- ^ After SSA  
type NanoTypeR r   = Nano (AnnType r) (RType r)            -- ^ After TC: Contains an updated TDefEnv
type NanoRefType   = Nano (AnnType F.Reft) (RType F.Reft)  -- ^ After Liquid

type ExprSSAR r    = Expression (AnnSSA r)
type StmtSSAR r    = Statement  (AnnSSA r)

type NanoBare      = NanoBareR ()
type NanoSSA       = NanoSSAR ()
type NanoType      = NanoTypeR ()

{-@ measure isFunctionStatement :: (Statement SourceSpan) -> Prop 
    isFunctionStatement (FunctionStmt {}) = true
    isFunctionStatement (_)               = false
  @-}

{-@ type FunctionStatement = {v:(Statement SourceSpan) | (isFunctionStatement v)} @-}
type FunctionStatement a = Statement a 

{-@ newtype Source a = Src [FunctionStatement a] @-}
-- newtype Source a = Src [FunctionStatement a]
newtype Source a = Src [Statement a]
  deriving (Data, Typeable)

instance Monoid (Source a) where
  mempty                    = Src []
  mappend (Src s1) (Src s2) = Src $ s1 ++ s2

instance Functor Source where 
  fmap f (Src zs) = Src (map (fmap f) zs)

instance (PP r, F.Reftable r) => PP (Nano a (RType r)) where
  pp pgm@(Nano {code = (Src s) }) 
    =   text "\n******************* Code **********************"
    $+$ pp s
    {-$+$ text "\n******************* Imported specs ************"-}
    {-$+$ ppEnv (externs pgm)-}
    $+$ text "\n******************* Checked fun sigs **********"
    $+$ pp (specs pgm)
--     $+$ text "\n******************* Global vars ***************"
--     $+$ pp (glVars pgm)
    $+$ text "\n******************* Constants *****************"
    $+$ pp (consts pgm) 
    $+$ text "\n******************* Type Definitions **********"
    $+$ pp (defs  pgm)
    $+$ text "\n******************* Predicate Aliases *********"
    $+$ pp (pAlias pgm)
    $+$ text "\n******************* Type Aliases **************"
    $+$ pp (tAlias pgm)
    $+$ text "\n******************* Qualifiers ****************"
    $+$ vcat (F.toFix <$> (take 3 $ quals pgm))
    $+$ text "..."
--     $+$ text "\n******************* Invariants ****************"
--     $+$ vcat (pp <$> (invts pgm))
    $+$ text "\n***********************************************\n"

-- ppEnv env = vcat [ pp id <+> text "::" <+> pp t <+> text"\n" | (id, t) <- envToList env]

instance PP t => PP (TDefEnv t) where
  pp (G s γ) =  (text "Size:" <+> text (show s))  $$ text "" $$
                (text "Type definitions:"  $$ nest 2 (pp γ))

instance PP t => PP (I.IntMap t) where
  pp m = vcat (pp <$> I.toList m)

instance PP t => PP (F.SEnv t) where
  pp m = vcat $ pp <$> F.toListSEnv m

instance (PP t) => PP (TDef t) where
    pp (TD c nm vs Nothing ts) = 
          pp (if c then "class" else "interface")
      <+> pp nm 
      <+> ppArgs brackets comma vs 
      <+> braces (
            text " " 
        <+> (vcat $ (\t -> pp t <> text ";") <$> ts) 
        <+> text " ")
    pp (TD c nm vs (Just (p,ps)) ts) = 
          pp (if c then "class" else "interface")
      <+> pp nm 
      <+> ppArgs brackets comma vs 
      <+> text "extends" <+> pp p <+> pp ps
      <+> braces (
            text " " 
        <+> (vcat $ (\t -> pp t <> text ";") <$> ts) 
        <+> text " ")


instance (PP t) => PP (TElt t) where
  pp (CallSig t)          = text "call" <+> pp t 
  pp (ConsSig t)          = text "new" <+> pp t
  pp (IndexSig x True t)  = brackets (pp x <> text ": string") <> text ":" <> pp t
  pp (IndexSig x False t) = brackets (pp x <> text ": number") <> text ":" <> pp t

  pp (FieldSig x s m τ t) =  bStr s "static" 
                         <+> brackets (pp m)
                         <+> pp x 
                         <>  mStr ((text "" <+>) . brackets . pp <$> τ)
                         <>  text ":" 
                         <+> pp t 

bStr True  s = text s
bStr False _ = text ""

mStr (Just s) = s
mStr _        = text ""


instance F.Symbolic (TElt t) where
  symbol (FieldSig s _ _ _ _) = s
  symbol (ConsSig       _)    = F.stringSymbol "__constructor__"
  symbol (CallSig       _)    = F.stringSymbol "__call__"
  symbol (IndexSig _ True _)  = F.stringSymbol "__string__index__"
  symbol (IndexSig _ False _) = F.stringSymbol "__numeric__index__"

    
sameBinder (CallSig _)            (CallSig _)             = True
sameBinder (ConsSig _)            (ConsSig _)             = True
sameBinder (IndexSig _ b1 _)      (IndexSig _ b2 _)       = b1 == b2
sameBinder (FieldSig x1 s1 _ _ _) (FieldSig x2 s2 m2 _ _) = x1 == x2 && s1 == s2
sameBinder _                      _                       = False

mutability (CallSig _) = Nothing
mutability (ConsSig _) = Nothing  
mutability (IndexSig _ _ _) = Nothing  
mutability (FieldSig _ _ m  _ _) = Just m



{-zipElts :: (PP r, F.Reftable r) => -}
{-  (RType r -> RType r-> RType r) -> TElt (RType r) -> TElt (RType r) -> TElt (RType r)-}

zipElts f e1@(CallSig t1) e2@(CallSig t2)
  = CallSig $ f t1 t2 

zipElts f e1@(ConsSig t1) e2@(ConsSig t2)       
  = ConsSig $ f t1 t2 

zipElts f e1@(IndexSig x b1 t1) e2@(IndexSig _ _ t2)  
  | sameBinder e1 e2 
  = IndexSig x b1 $ f t1 t2

zipElts f e1@(FieldSig x1 s1 m1 (Just τ1) t1) e2@(FieldSig _ _ m2 (Just τ2) t2)
  | sameBinder e1 e2 
  = FieldSig x1 s1 m1 (Just $ f τ1 τ2) $ f t1 t2
  {-where ff t1 t2 = mapPair toType $ f (ofType t1) (ofType t2)-}
zipElts f e1@(FieldSig x1 s1 m1 Nothing t1) e2@(FieldSig _ _ m2 Nothing t2)
  | sameBinder e1 e2
  = FieldSig x1 s1 m1 Nothing $ f t1 t2
  {-where ff t1 t2 = mapPair toType $ f (ofType t1) (ofType t2)-}

zipElts _ e1 e2
  = error $ "Cannot zip: " ++ ppshow e1 ++ " and " ++ ppshow e2

-- FIXME: get rid of this
eltType (FieldSig _ _ _ _ t) = t
eltType (ConsSig       t  ) = t
eltType (CallSig       t  ) = t
eltType (IndexSig _ _  t  ) = t


isStaticElt (FieldSig _ True _ _ _  ) = True
isStaticElt _                         = False


nonStaticElt = not . isStaticElt
nonConstrElt = not . isConstr


------------------------------------------------------------------------------------------
-- | Assignability 
------------------------------------------------------------------------------------------

data Assignability 
  = ReadOnly    -- ^ import,  cannot be modified, can appear in refinements
  | WriteLocal  -- ^ written in local-scope, can be SSA-ed, can appear in refinements
  | WriteGlobal -- ^ written in non-local-scope, cannot do SSA, cannot appear in refinements


-- | `writeGlobalVars p` returns symbols that have `WriteMany` status, i.e. may be 
--    re-assigned multiply in non-local scope, and hence
--    * cannot be SSA-ed
--    * cannot appear in refinements
--    * can only use a single monolithic type (declared or inferred)
 
writeGlobalVars   :: PP t => Nano a t -> [Id SourceSpan] 
writeGlobalVars p = envIds mGnty 
  where
    mGnty         = undefined -- glVars p  -- guarantees


definedGlobs       :: (Data r, Typeable r) => [Statement (AnnType r)] -> [(AnnType r, Id (AnnType r), RType r)]
definedGlobs stmts = everything (++) ([] `mkQ` fromVarDecl) stmts
  where 
    fromVarDecl (VarDecl l x _) =
      case listToMaybe $ [ t | VarAnn t <- ann_fact l ] of
        Just t                 -> [(l,x,t)]
        Nothing                -> []


-- | `immutableVars p` returns symbols that must-not be re-assigned and hence
--    * can appear in refinements

readOnlyVars   :: (IsLocated a, Data a, Typeable a) => Nano a t -> [Id SourceSpan] 
readOnlyVars p = envIds $ mAssm `envUnion` mMeas `envUnion` mExtr
  where 
    mMeas      = consts p     -- measures
    mAssm      = specs  p     -- assumes                      
    mExtr      = externs p    -- externs

instance PP Assignability where
  pp ReadOnly    = text "ReadOnly"
  pp WriteLocal  = text "WriteLocal"
  pp WriteGlobal = text "WriteGlobal"



---------------------------------------------------------------------------
-- | Pretty Printer Instances
---------------------------------------------------------------------------


instance PP Bool where
  pp True   = text "True"
  pp False  = text "False"

instance PP () where 
  pp _ = text ""

instance PP a => PP [a] where 
  pp = ppArgs brackets comma 

instance PP a => PP (Maybe a) where 
  pp = maybe (text "Nothing") pp 

instance PP Char where
  pp = char


instance (PP r, F.Reftable r) => PP (RType r) where
  pp (TVar α r)               = F.ppTy r $ pp α 
  pp (TFun xts t _)           = ppArgs parens comma xts <+> text "=>" <+> pp t 
  pp t@(TAll _ _)             = text "forall" <+> ppArgs id space αs <> text "." <+> pp t' where (αs, t') = bkAll t
  pp (TAnd ts)                = vcat [text "/\\" <+> pp t | t <- ts]
  pp (TExp e)                 = pprint e 
  pp (TApp TUn ts r)          = F.ppTy r $ ppArgs id (text " +") ts 
  pp (TApp d@(TRef _ _) ts r) = F.ppTy r $ pp d <> ppArgs brackets comma ts 
  pp (TApp c [] r)            = F.ppTy r $ pp c 
  pp (TApp c ts r)            = F.ppTy r $ parens (pp c <+> ppArgs id space ts)  
  pp (TCons bs m r)           = F.ppTy r $ brackets (pp m) <+> braces (intersperse semi $ map pp bs)
  -- pp (TCons bs r)           = F.ppTy r $ lbrace $+$ nest 2 (cat $ map pp bs) $+$ rbrace


instance PP TVar where 
  pp     = pprint . F.symbol


instance PP TCon where
  pp TInt             = text "number"
  pp TBool            = text "boolean"
  pp TString          = text "string"
  pp TVoid            = text "void"
  pp TTop             = text "top"
  pp TUn              = text "union:"
  pp (TRef x True)    = text "(static)" <> text (F.symbolString $ x)
  pp (TRef x _   )    = text (F.symbolString $ x)
  pp TNull            = text "null"
  pp TUndef           = text "undefined"
  pp TFPBool          = text "bool"

instance Hashable TCon where
  hashWithSalt s TInt         = hashWithSalt s (0 :: Int)
  hashWithSalt s TBool        = hashWithSalt s (1 :: Int)
  hashWithSalt s TString      = hashWithSalt s (2 :: Int)
  hashWithSalt s TVoid        = hashWithSalt s (3 :: Int)
  hashWithSalt s TTop         = hashWithSalt s (4 :: Int)
  hashWithSalt s TUn          = hashWithSalt s (5 :: Int)
  hashWithSalt s TNull        = hashWithSalt s (6 :: Int)
  hashWithSalt s TUndef       = hashWithSalt s (7 :: Int)
  hashWithSalt s (TRef z _)   = hashWithSalt s (8 :: Int) + hashWithSalt s z

instance (PP r, F.Reftable r) => PP (Bind r) where 
  pp (B x t)          = pp x <> colon <> pp t 

ppArgs _  _ [] = text ""
ppArgs p sep l = p $ intersperse sep $ map pp l

instance (PP s, PP t) => PP (M.HashMap s t) where
  pp m = vcat $ pp <$> M.toList m


-----------------------------------------------------------------------------
-- | IContext keeps track of context of intersection-type cases
-----------------------------------------------------------------------------

-- | Keeps track of intersection-type context, to allow casts to be guarded by
--   context. Otherwise, the "dead-casts" for one case must be proven in another
--   case which is impossible. See tests/liquid/pos/misc/negate-05.js
--   A context IC [i_1,...,i_n] denotes the case where we use the conjunct i_k
--   from the kth function in lexical scope order (ignoring functions that have
--   a single conjunct.)

class CallSite a where
  siteIndex :: a -> Int

instance CallSite Int where
  siteIndex i = i

newtype IContext = IC [Int] 
                   deriving (Eq, Ord, Show, Data, Typeable)

instance PP Int where
  pp        = int

instance PP IContext where
  pp (IC x) = text "Context: " <+> pp x

emptyContext         :: IContext
emptyContext         = IC []

pushContext          :: (CallSite a) => a -> IContext -> IContext
pushContext s (IC c) = IC ((siteIndex s) : c) 

-----------------------------------------------------------------------------
-- | Casts 
-----------------------------------------------------------------------------
data Cast r  = CNo                                      -- .
             | CDead {                 tgt :: RType r } -- |dead code|
             | CUp   { org :: RType r, tgt :: RType r } -- <t1 UP t2>
             | CDn   { org :: RType r, tgt :: RType r } -- <t1 DN t2>
             deriving (Eq, Ord, Show, Data, Typeable, Functor)

instance (PP r, F.Reftable r) => PP (Cast r) where
  pp CNo         = text "No cast"
  pp (CDead _)   = text "Dead code"
  pp (CUp t1 t2) = text "<" <+> pp t1 <+> text "UP" <+> pp t2 <+> text ">"
  pp (CDn t1 t2) = text "<" <+> pp t1 <+> text "DN" <+> pp t2 <+> text ">"

noCast CNo = True
noCast _   = False

upCast (CDead _)  = False
upCast CNo        = True
upCast (CUp _ _)  = True
upCast (CDn _ _)  = False

dnCast (CDead _)  = False
dnCast CNo        = True
dnCast (CUp _ _)  = False
dnCast (CDn _ _)  = True

ddCast (CDead _)  = True
ddCast _          = False


-----------------------------------------------------------------------------
-- | Annotations
-----------------------------------------------------------------------------

data Fact r
  = PhiVar      ![(Id SourceSpan)]
  | TypInst     !IContext ![RType r]
  -- Overloading
  | EltOverload !IContext  !(TElt (RType r))
  | Overload    !IContext  !(RType r)
  | TCast       !IContext !(Cast r)
  -- Type annotations
  | VarAnn      !(RType r)
  | FieldAnn    !(Bool, RType r)      -- (Assignability, type)
  | MethAnn     !(RType r)            -- type
  | ConsAnn     !(RType r)            -- type
  -- Class annotation
  | ClassAnn      !([TVar], Maybe (Id SourceSpan,[RType r]))
    deriving (Eq, Show, Data, Typeable, Functor)

type UFact = Fact ()

data Annot b a = Ann { ann :: a, ann_fact :: [b] } deriving (Show, Data, Typeable)
type AnnBare r = Annot (Fact r) SourceSpan -- NO facts
type AnnSSA  r = Annot (Fact r) SourceSpan -- Phi facts
type AnnType r = Annot (Fact r) SourceSpan -- Phi + t. annot. + Cast facts
type AnnInfo r = M.HashMap SourceSpan [Fact r] 
type ClassInfo r = Env (RType r)

type UAnnBare = AnnBare () 
type UAnnSSA  = AnnSSA  ()
type UAnnType = AnnType ()
type UAnnInfo = AnnInfo ()


instance HasAnnotation (Annot b) where 
  getAnnotation = ann 

instance Ord (AnnSSA  r) where 
  compare (Ann s1 _) (Ann s2 _) = compare s1 s2

-- XXX: This shouldn't have to be that hard...
instance Ord (Fact r) where
  compare (PhiVar i1       ) (PhiVar i2       )   = compare i1 i2
  compare (TypInst c1 t1   ) (TypInst c2 t2   )   = compare (c1,toType <$> t1) (c2,toType <$> t2)
  compare (EltOverload c1 t1) (EltOverload c2 t2) = compare (c1, toType <$> t1) (c2, toType <$> t2)
  compare (Overload c1 t1  ) (Overload c2 t2  )   = compare (c1, toType t1) (c2, toType t2)
  compare (TCast c1 _      ) (TCast c2 _      )   = compare c1 c2
  compare (VarAnn t1       ) (VarAnn t2       )   = on compare toType t1 t2
  compare (FieldAnn (b1,t1)) (FieldAnn (b2,t2))   = compare (b1, toType t1) (b2, toType t2)
  compare (MethAnn t1      ) (MethAnn t2      )   = on compare toType t1 t2
  compare (ConsAnn t1      ) (ConsAnn t2      )   = on compare toType t1 t2
  compare (ClassAnn (_,m1) ) (ClassAnn (_,m2) )   = on compare (fst <$>) m1 m2
  compare f1 f2                                   = on compare factToNum f1 f2

factToNum (PhiVar _        ) = 0
factToNum (TypInst _ _     ) = 1
factToNum (EltOverload _ _ ) = 2
factToNum (Overload _  _   ) = 3
factToNum (TCast _ _       ) = 4
factToNum (VarAnn _        ) = 5
factToNum (FieldAnn _      ) = 6
factToNum (MethAnn _       ) = 7
factToNum (ConsAnn _       ) = 8
factToNum (ClassAnn _      ) = 9


instance Eq (Annot a SourceSpan) where 
  (Ann s1 _) == (Ann s2 _) = s1 == s2

instance IsLocated (Annot a SourceSpan) where 
  srcPos = ann

instance (F.Reftable r, PP r) => PP (Fact r) where
  pp (PhiVar x)       = text "phi"  <+> pp x
  pp (TypInst ξ ts)   = text "inst" <+> pp ξ <+> pp ts 
  pp (Overload ξ i)   = text "overload" <+> pp ξ <+> pp i
  pp (EltOverload ξ i)= text "elt_overload" <+> pp ξ <+> pp i
  pp (TCast  ξ c)     = text "cast" <+> pp ξ <+> pp c
  pp (VarAnn t)       = text "Var Annotation" <+> pp t
  pp (ConsAnn t)      = text "Constructor Annotation" <+> pp t
  pp (FieldAnn (_,t)) = text "Field Annotation" <+> pp t
  pp (MethAnn t)      = text "Method Annotation" <+> pp t
  pp (ClassAnn _)     = error "UNIMPLEMENTED:pp:ClassAnn"

instance (F.Reftable r, PP r) => PP (AnnInfo r) where
  pp             = vcat . (ppB <$>) . M.toList 
    where 
      ppB (x, t) = pp x <+> dcolon <+> pp t

instance (PP a, PP b) => PP (Annot b a) where
  pp (Ann x ys) = text "Annot: " <+> pp x <+> pp ys

phiVarsAnnot l = concat [xs | PhiVar xs <- ann_fact l]

-----------------------------------------------------------------------
-- | Primitive / Base Types -------------------------------------------
-----------------------------------------------------------------------

tVar   :: (F.Reftable r) => TVar -> RType r
tVar   = (`TVar` fTop) 

isTVar (TVar _ _) = True
isTVar _          = False

isRigid = not . isTVar

tInt, tBool, tUndef, tNull, tString, tVoid, tErr :: (F.Reftable r) => RType r
tInt     = TApp TInt     [] fTop 
tBool    = TApp TBool    [] fTop
tString  = TApp TString  [] fTop
tTop     = TApp TTop     [] fTop
tVoid    = TApp TVoid    [] fTop
tUndef   = TApp TUndef   [] fTop
tNull    = TApp TNull    [] fTop
tErr     = tVoid
tFunErr  = ([],[],tErr)
tAnd ts  = case ts of 
             [ ] -> errorstar "BUG: empty intersection"
             [t] -> t
             _   -> TAnd ts

-- FIXME
tArr t   = TApp (TRef (F.symbol "Array") False) [t] fTop

rtArr t   = TApp (TRef (F.symbol "Array") False) [t] 

isArr (TApp (TRef s _) _ _ ) | s == F.symbol "Array" = True
isArr _                          = False

isTFun (TFun _ _ _) = True
isTFun (TAnd ts)    = all isTFun ts
isTFun (TAll _ t)   = isTFun t
isTFun _            = False


orNull t@(TApp TUn ts _) | any isNull ts = t
orNull   (TApp TUn ts r) | otherwise     = TApp TUn (tNull:ts) r
orNull t                 | isNull t      = t
orNull t                 | otherwise     = TApp TUn [tNull,t] fTop



-----------------------------------------------------------------------
-- | Operator Types
-----------------------------------------------------------------------

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
    (mv,mt)       = freshTV l "M" 0                             -- obj mutability
    (mvs,mts)     = unzip $ map (freshTV l "M") [1..length ps]  -- field mutability
    (avs,ats)     = unzip $ map (freshTV l "A") [1..length ps]  -- field type vars
    ss            = [ F.symbol p | p <- ps]
    vs            = [mv] ++ mvs ++ avs
    bs            = [ B s (ofType a) | (s,a) <- zip ss ats ]
    elts          = [ FieldSig s False m Nothing (ofType a) | (s,m,a) <- zip3 ss mts ats ] 
    rt            = TCons elts mt fTop
 
 
--------------------------------------------------------------------------
setPropTy :: (PP r, F.Reftable r, IsLocated l) 
          => F.Symbol -> l -> F.SEnv (Located (RType r)) -> RType r
--------------------------------------------------------------------------
setPropTy f l g =
    case ty of 
      TAnd [TAll α1 (TAll β1 (TAll μ1 (TFun [xt1,a1] rt1 r1))) ,
            TAll α2 (TAll β2 (TAll μ2 (TFun [xt2,a2] rt2 r2))) ] -> 
        TAnd [TAll α1 (TAll β1 (TAll μ1 (TFun [tr xt1,a1] rt1 r1))) ,
              TAll α2 (TAll β2 (TAll μ2 (TFun [tr xt2,a2] rt2 r2))) ]
      _ -> error $ "setPropTy " ++ ppshow ty
  where
    tr (B n (TCons [FieldSig x s μx τ t] μ r)) 
       | x == F.symbol "f"
       = B n (TCons [FieldSig f s μx τ t] μ r)
    tr t = error $ "setPropTy:tr " ++ ppshow t
    ty = builtinOpTy l BISetProp g


-----------------------------------------------------------------------
infixOpTy :: InfixOp -> Env t -> t 
-----------------------------------------------------------------------
infixOpTy o g = fromMaybe err $ envFindTy ox g
  where 
    err       = errorstar $ printf "Cannot find infixOpTy %s" (ppshow ox) -- (ppshow g)
    ox        = infixOpId o

infixOpId OpLT        = builtinId "OpLT"
infixOpId OpLEq       = builtinId "OpLEq"
infixOpId OpGT        = builtinId "OpGT"
infixOpId OpGEq       = builtinId "OpGEq"
infixOpId OpEq        = builtinId "OpEq"
infixOpId OpStrictEq  = builtinId "OpSEq"
infixOpId OpNEq       = builtinId "OpNEq"
infixOpId OpStrictNEq = builtinId "OpSNEq"
infixOpId OpLAnd      = builtinId "OpLAnd"
infixOpId OpLOr       = builtinId "OpLOr"
infixOpId OpSub       = builtinId "OpSub"
infixOpId OpAdd       = builtinId "OpAdd"
infixOpId OpMul       = builtinId "OpMul"
infixOpId OpDiv       = builtinId "OpDiv"
infixOpId OpMod       = builtinId "OpMod"
infixOpId o           = errorstar $ "infixOpId: Cannot handle: " ++ ppshow o

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


builtinId       = mkId . ("builtin_" ++)

-----------------------------------------------------------------------
-- Type and Predicate Aliases
-----------------------------------------------------------------------

data Alias a s t = Alias {
    al_name   :: Id SourceSpan  -- ^ alias name
  , al_tyvars :: ![a]           -- ^ type  parameters  
  , al_syvars :: ![s]           -- ^ value parameters 
  , al_body   :: !t             -- ^ alias body
  } deriving (Eq, Ord, Show, Functor, Data, Typeable)

type TAlias t    = Alias TVar F.Symbol t
type PAlias      = Alias ()   F.Symbol F.Pred 
type TAliasEnv t = Env (TAlias t)
type PAliasEnv   = Env PAlias

instance IsLocated (Alias a s t) where
  srcPos = srcPos . al_name

instance (PP a, PP s, PP t) => PP (Alias a s t) where
  pp (Alias n _ _ body) = text "alias" <+> pp n <+> text "=" <+> pp body 

