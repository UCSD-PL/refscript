-- | Global type definitions for Refinement Type Checker

{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.Nano.Typecheck.Types (

  -- * Programs
    Nano (..)
  , NanoBare, NanoSSA, NanoBareR, NanoSSAR, NanoRefType, NanoTypeR, NanoType, ExprSSAR, StmtSSAR
  , Source (..)
  , FunctionStatement
  , mapCode

  -- * (Refinement) Types
  , RType (..), Bind (..), toType, ofType, rTop, strengthen 

  -- * Predicates on Types 
  , isTop, isNull, isVoid, isUndef, isUnion

  -- * Constructing Types
  , mkUnion, mkUnionR, mkFun, mkAll

  -- * Deconstructing Types
  , bkFun, bkFuns, bkAll, bkAnd, bkUnion, bkPaddedUnion, unionParts, unionParts', funTys
  
  -- Union ops
  , rUnion, rTypeR, setRTypeR, noUnion, unionCheck
  
  , renameBinds
  , calleeType

  -- * Regular Types
  , Type, TDef (..), TVar (..), TCon (..), TElt (..)

  -- * Primitive Types
  , tInt, tBool, tString, tTop, tVoid, tErr, tFunErr, tVar, tArr, tUndef, tNull
  , tAnd, isTVar, isArr, isTRef, isTFun, fTop

  -- * Type comparison/joining/subtyping
  , Equivalent
  , equiv
  , SubDirection (..), arrDir

  -- * Type definition env
  , TyID
  , TDefEnv (..), tDefEmpty
  , addTySym, addSym, addObjLitTy
  , findTySym, findTySymOrDie, findTySymWithId, findTySymWithIdOrDie, findTyId, findTyIdOrDie
  , findTyIdOrDie', findEltWithDefault, symTDefMem
  , getDefNames
  , sortTDef

  -- * Operator Types
  , infixOpTy, prefixOpTy, builtinOpTy, arrayLitTy

  -- * Annotations
  , Annot (..), UFact, Fact (..), Cast(..), phiVarsAnnot, ClassInfo

  -- * Aliases for annotated Source 
  , AnnBare, UAnnBare, AnnSSA , UAnnSSA
  , AnnType, UAnnType, AnnInfo, UAnnInfo

  -- * Contexts
  , CallSite (..), IContext, emptyContext, pushContext

  -- * Mutability 
  , Mutability (..), writeGlobalVars, readOnlyVars  

  -- * Aliases
  , Alias (..), TAlias, PAlias, PAliasEnv, TAliasEnv

  ) where 

import           Text.Printf
import           Data.Hashable
import           Data.Either                    (partitionEithers)
import           Data.Maybe                     (fromMaybe)
import           Data.Monoid                    hiding ((<>))            
import qualified Data.List                      as L
import qualified Data.IntMap                    as I
import qualified Data.Set                       as S
import qualified Data.HashMap.Strict            as M
import           Data.Generics                   
import           Data.Typeable                  ()
import           Language.ECMAScript3.Syntax    hiding (Cast)
import           Language.ECMAScript3.Syntax.Annotations
import           Language.ECMAScript3.PrettyPrint
import           Language.Nano.Types
import           Language.Nano.Errors
import           Language.Nano.Env

import           GHC.Exts

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

instance PP TVar where 
  pp     = pprint . F.symbol

instance F.Symbolic a => F.Symbolic (Located a) where 
  symbol = F.symbol . val


-- | Data types 

type TyID      = Int

data TDef t    = TD { 
        t_name  :: !(Maybe (Id SourceSpan))       -- ^ Name (possibly no name)
      , t_args  :: ![TVar]                        -- ^ Type variables
      , t_proto :: !(Maybe (Id SourceSpan, [t]))  -- ^ Parent type symbol
      , t_elts  :: ![TElt t]                      -- ^ List of data type elts 
      } deriving (Eq, Ord, Show, Functor, Data, Typeable)

data TElt t    = TE { 
        f_sym   :: F.Symbol                       -- ^ Symbol
      , f_acc   :: Bool                           -- ^ Access modifier (public: true, private: false)
      , f_type  :: t                              -- ^ Type
      } deriving (Eq, Ord, Show, Functor, Data, Typeable)


-- | Type definition environment

data TDefEnv t = G  { 
        g_size    :: Int                -- ^ Size of the `env`
      , g_env     :: I.IntMap (TDef t)  -- ^ Type def env (includes object types)
      , g_names   :: F.SEnv TyID        -- ^ Named types - mapping to env
                    } deriving (Show, Functor, Data, Typeable)

tDefEmpty = G 0 I.empty F.emptySEnv

getDefNames (G _ _ n) = fst <$> F.toListSEnv n

---------------------------------------------------------------------------------
addTySym :: F.Symbol -> TDef t -> TDefEnv t -> (TDefEnv t, TyID)
---------------------------------------------------------------------------------
addTySym s t (G sz γ c) =
  case F.lookupSEnv s c of 
    -- Bind existing in the names env - update the TDef env
    Just tid -> (G sz  (I.insert tid t γ) c , tid )
    -- Bind fresh in names env - update both envs
    Nothing  -> (G sz' (I.insert sz' t γ) c', tid')
  where
    sz'  = sz + 1
    tid' = sz'
    c'  = F.insertSEnv s tid' c  

---------------------------------------------------------------------------------
addSym :: F.Symbol -> TDefEnv t -> (TDefEnv t, TyID)
---------------------------------------------------------------------------------
addSym s g@(G sz γ c) = maybe f (g,) (F.lookupSEnv s c)
  where
    f   = (G sz' γ c', id)
    sz' = sz + 1    -- Update the size of the env even though the 
                    -- new sym is not bound to an actual TDef
    id  = sz + 1
    c'  = F.insertSEnv s id c 

---------------------------------------------------------------------------------
addObjLitTy :: TDef t -> TDefEnv t -> (TDefEnv t, TyID)
---------------------------------------------------------------------------------
addObjLitTy t (G sz γ c) = (G sz' γ' c, id)
  where
    sz' = sz + 1
    id  = sz + 1
    γ'  = I.insert id t γ

---------------------------------------------------------------------------------
findTySym :: F.Symbolic s => s -> TDefEnv t -> Maybe (TDef t)
---------------------------------------------------------------------------------
findTySym s (G _ γ c) = F.lookupSEnv (F.symbol s) c >>= (`I.lookup` γ)

---------------------------------------------------------------------------------
findTySymWithId :: F.Symbolic s => s -> TDefEnv t -> Maybe (TyID, TDef t)
---------------------------------------------------------------------------------
findTySymWithId s (G _ γ c) = F.lookupSEnv (F.symbol s) c 
                          >>= \i -> (I.lookup i γ) 
                          >>= return . (i,)

---------------------------------------------------------------------------------
findTySymWithIdOrDie:: F.Symbolic s => s -> TDefEnv t -> (TyID, TDef t)
---------------------------------------------------------------------------------
findTySymWithIdOrDie s γ = fromMaybe (error "findTySymWithIdOrDie") $ findTySymWithId s γ 

---------------------------------------------------------------------------------
findTySymOrDie :: F.Symbolic s => s -> TDefEnv t -> TDef t
---------------------------------------------------------------------------------
findTySymOrDie s γ = fromMaybe (error "findTySymOrDie") $ findTySym s γ 

---------------------------------------------------------------------------------
findTyId :: TyID -> TDefEnv t -> Maybe (TDef t)
---------------------------------------------------------------------------------
findTyId i (G _ γ _) = I.lookup i γ

---------------------------------------------------------------------------------
findTyIdOrDie :: TyID -> TDefEnv t -> TDef t
---------------------------------------------------------------------------------
findTyIdOrDie i γ = fromMaybe (error $ "findTyIdOrDie") $ findTyId i γ 

---------------------------------------------------------------------------------
findTyIdOrDie' :: String -> TyID -> TDefEnv t -> TDef t
---------------------------------------------------------------------------------
findTyIdOrDie' msg i γ = fromMaybe (error $ "findTyIdOrDie:" ++ msg) $ findTyId i γ 

---------------------------------------------------------------------------------
findEltWithDefault :: F.Symbol -> t -> [TElt t] -> t
---------------------------------------------------------------------------------
findEltWithDefault s t elts = 
  maybe t f_type $ L.find ((== s) . f_sym) elts

---------------------------------------------------------------------------------
symTDefMem :: F.Symbol -> TDefEnv t -> Bool
---------------------------------------------------------------------------------
symTDefMem s = F.memberSEnv s . g_names



-- | Sort the fields of a TDef 
---------------------------------------------------------------------------------
sortTDef:: TDef t -> TDef t
---------------------------------------------------------------------------------
sortTDef (TD nm vs p elts) = TD nm vs p $ sortWith f_sym elts



-- | Type Constructors
data TCon
  = TInt
  | TBool
  | TString
  | TVoid
  | TTop
  | TRef  TyID
  | TUn
  | TNull
  | TUndef
    deriving (Ord, Show, Data, Typeable)

-- | (Raw) Refined Types 
data RType r  
  = TApp TCon [RType r]     r   -- ^ C T1,...,Tn
  | TVar TVar               r   -- ^ A
  | TFun [Bind r] (RType r) r   -- ^ (x1:T1,...,xn:Tn) => T
  | TArr (RType r)          r   -- ^ [T] 
  | TAll TVar (RType r)         -- ^ forall A. T
  | TAnd [RType r]              -- ^ (T1..) => T1' /\ ... /\ (Tn..) => Tn' 

  | TExp F.Expr                 -- ^ "Expression" parameters for type-aliases: never appear in real/expanded RType

  | TCons [Bind r]          r   -- ^ Flattened version of an object type 
                                --   To be used right before getting the
                                --   constraints for containers, to avoid 
                                --   having type references when needing to
                                --   bottify or k-var
    deriving (Ord, Show, Functor, Data, Typeable)

data Bind r
  = B { b_sym  :: F.Symbol      -- ^ Binding's symbol
      , b_type :: !(RType r)    -- ^ Field type
      } 
    deriving (Eq, Ord, Show, Functor, Data, Typeable)

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

-- | `calleeType` uses the types at the callsite to extract the appropriate
--   conjunct from an intersection.

calleeType _ ts (TAnd fts) = L.find (argsMatch ts) fts
calleeType _ _ ft          = Just ft

-- | `argsMatch ts ft` holds iff the arg-types in `ft` are identical to `ts` ... 
argsMatch :: [RType a] -> RType b -> Bool
argsMatch ts ft = case bkFun ft of 
                    Nothing        -> False
                    Just (_,xts,_) -> (toType <$> ts) == ((toType . b_type) <$> xts)

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
mkUnion = mkUnionR fTop 


---------------------------------------------------------------------------------
mkUnionR :: (F.Reftable r) => r -> [RType r] -> RType r
---------------------------------------------------------------------------------
mkUnionR _ [ ] = tErr
mkUnionR r [t] = strengthen t r
mkUnionR r ts  = TApp TUn ts r 

-- mkUnionR r ts  
--   | length ts' > 1 = TApp TUn ts' r
--   | otherwise      = strengthen (head ts') r
--   where 
--     ts'            = L.sort $ L.nub ts


---------------------------------------------------------------------------------
bkUnion :: RType r -> [RType r]
---------------------------------------------------------------------------------
bkUnion (TApp TUn xs _) = xs
bkUnion t               = [t]



-- | Type equivalence: This is equality on the raw type level, 
--  excluding union types.

class Equivalent a where 
  equiv :: a -> a -> Bool

instance Equivalent a => Equivalent [a] where
  equiv a b = and $ zipWith equiv a b 

instance Equivalent (RType r) where 
  equiv t t'  | toType t == toType t' = True
  equiv t t'  | any isUnion [t,t'] = 
  -- FIXME: 
  --
  --  ∀j. ∃i. Si ~ Tj  
  --  ∀i. ∃j. Si ~ Tj  
  -- -----------------
  --  \/ Si ~ \/ Tj
  --
    errorstar (printf "equiv: no unions: %s\n\t\t%s" 
    (ppshow $ toType t) (ppshow $ toType t'))
  equiv (TApp c ts _) (TApp c' ts' _) = c `equiv` c' && ts `equiv` ts'
  equiv (TArr t _   ) (TArr t' _    ) = t `equiv` t'
  equiv (TVar v _   ) (TVar v' _    ) = v == v'
  equiv (TFun b o _ ) (TFun b' o' _ ) = 
    (b_type <$> b) `equiv` (b_type <$> b') && o `equiv` o' 
  equiv (TAll _ _   ) (TAll _ _     ) = error "equiv-tall"
  equiv (TExp _     ) (TExp   _     ) = error "equiv-texp"
  equiv _             _               = False

instance Equivalent TCon where
  equiv (TRef i) (TRef i')  = i == i'
  equiv c        c'         = c == c'

instance Equivalent (TElt (RType r)) where 
  equiv (TE _ b1 t1) (TE _ b2 t2) = b1 == b2 && equiv t1 t2

instance Equivalent (Bind r) where 
  equiv (B s t) (B s' t') = s == s' && equiv t t' 

instance Equivalent (Id a) where 
  equiv i i' = F.symbol i == F.symbol i'


---------------------------------------------------------------------------------------
-- Subtyping direction ----------------------------------------------------------------
---------------------------------------------------------------------------------------

-- | Subtyping directions
data SubDirection = SubT    -- Subtype
                  | SupT    -- Supertype
                  | EqT     -- Same type
                  | Nth     -- No relation
                  deriving (Eq, Show)

instance PP SubDirection where 
  pp SubT = text "<:"
  pp SupT = text ":>"
  pp EqT  = text "="
  pp Nth  = text "≠"

-- The Subtyping directions form a monoid both as a `sum` and as a `product`, 
-- cause they are combined in different ways when used in unions and object (the
-- two places where subtyping occurs).

-- Sum: Relaxed version (To be used in unions)
instance Monoid SubDirection where
  mempty = EqT
  d    `mappend` d'   | d == d' = d
  -- We know nothing about the types so far (Nth), but we can use the other part
  -- to make any assumptions, that's why @d@ is propagated.
  Nth  `mappend` _              = Nth
  _    `mappend` Nth            = Nth
  EqT  `mappend` d              = d
  d    `mappend` EqT            = d
  _    `mappend` _              = Nth

arrDir     :: SubDirection -> SubDirection
arrDir EqT = EqT
arrDir _   = Nth


--------------------------------------------------------------------------------
bkPaddedUnion :: String -> RType r -> RType r -> [(RType r, RType r)]
--------------------------------------------------------------------------------
bkPaddedUnion msg t1 t2 =
  zipWith check (bkUnion t1) (bkUnion t2)
  where check t t' | equiv t t' = (t,t')
                   | otherwise  = 
                   errorstar $ printf "bkPaddedUnion[%s]\n\t%s\nand\n\t%s" 
                     msg (ppshow $ toType t1) (ppshow $ toType t2) 


-- | `unionParts`

-- Special case of `unionParts'` that uses `Equivalent` as the type
-- equivalence relation.
--------------------------------------------------------------------------------
unionParts ::  RType r -> RType r -> ([(RType r, RType r)], [RType r], [RType r])
--------------------------------------------------------------------------------
unionParts = unionParts' equiv


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




-- | Helper
instance (PP a, PP b, PP c, PP d) => PP (a,b,c,d) where
  pp (a,b,c,d) = pp a <+>  pp b <+> pp c <+> pp d




-- | Strengthen the top-level refinement

---------------------------------------------------------------------------------
strengthen                   :: F.Reftable r => RType r -> r -> RType r
---------------------------------------------------------------------------------
strengthen (TApp c ts r) r'  = TApp c ts $ r' `F.meet` r 
strengthen (TVar α r)    r'  = TVar α    $ r' `F.meet` r 
strengthen (TArr t r)    r'  = TArr t    $ r' `F.meet` r
strengthen t _               = t                         

-- NOTE: r' is the OLD refinement. 
--       We want to preserve its VV binder as it "escapes", 
--       e.g. function types. Sigh. Should have used a separate function binder.


-- | Strengthen the refinement of a type @t2@ deeply, using the 
-- refinements of an equivalent (having the same raw version) 
-- type @t1@.
-- TODO: Add checks for equivalence in union and objects

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

isTRef (TApp (TRef _) _ _) = True
isTRef _                   = False

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
rTypeR (TArr _ r   ) = r
rTypeR (TCons _ r  ) = r
rTypeR (TAll _ _   ) = errorstar "Unimplemented: rTypeR - TAll"
rTypeR (TAnd _ )     = errorstar "Unimplemented: rTypeR - TAnd"
rTypeR (TExp _)      = errorstar "Unimplemented: rTypeR - TExp"

-- Set the top-level refinement (wherever applies)
setRTypeR :: RType r -> r -> RType r
setRTypeR (TApp c ts _   ) r = TApp c ts r
setRTypeR (TVar v _      ) r = TVar v r
setRTypeR (TFun xts ot _ ) r = TFun xts ot r
setRTypeR (TArr t _      ) r = TArr t r
setRTypeR t                _ = t

---------------------------------------------------------------------------------------
noUnion :: (F.Reftable r) => RType r -> Bool
---------------------------------------------------------------------------------------
noUnion (TApp TUn _ _)  = False
noUnion (TApp _  rs _)  = and $ map noUnion rs
noUnion (TFun bs rt _)  = and $ map noUnion $ rt : (map b_type bs)
noUnion (TArr t     _)  = noUnion t
noUnion (TAll _ t    )  = noUnion t
noUnion _               = True

unionCheck t | noUnion t = t 
unionCheck t | otherwise = error $ printf "%s found. Cannot have unions." $ ppshow t


instance Eq TCon where
  TInt    == TInt    = True   
  TBool   == TBool   = True           
  TString == TString = True
  TVoid   == TVoid   = True         
  TTop    == TTop    = True
  TRef i1 == TRef i2 = i1 == i2
  TUn     == TUn     = True
  TNull   == TNull   = True
  TUndef  == TUndef  = True
  _       == _       = False
 
instance (Eq r, Ord r, F.Reftable r) => Eq (RType r) where
  TApp TUn t1 _       == TApp TUn t2 _       = (null $ t1 L.\\ t2) && (null $ t2 L.\\ t1)
  TApp c1 t1s r1      == TApp c2 t2s r2      = (c1, t1s, r1)  == (c2, t2s, r2)
  TVar v1 r1          == TVar v2 r2          = (v1, r1)       == (v2, r2)
  TFun b1 t1 r1       == TFun b2 t2 r2       = (b1, t1, r1)   == (b2, t2, r2)
  TArr t1 r1          == TArr t2 r2          = t1 == t2 && r1 == r2
  TAll v1 t1          == TAll v2 t2          = v1 == v2 && t1 == t2   -- Very strict Eq here
  _                   == _                   = False


---------------------------------------------------------------------------------
-- | Nano Program = Code + Types for all function binders
---------------------------------------------------------------------------------

data Nano a t = Nano { code   :: !(Source a)               -- ^ Code to check
                     , externs:: !(Env t)                  -- ^ Imported (unchecked) specifications 
                     , specs  :: !(Env t)                  -- ^ Function specs and 
                     , glVars :: !(Env t)                  -- ^ Global (annotated) vars
                     , consts :: !(Env t)                  -- ^ Measure Signatures
                     , defs   :: !(TDefEnv t)              -- ^ Type definitions
                                                           -- ^ After TC will also include class types
							       , tAlias :: !(TAliasEnv t)            -- ^ Type aliases
                     , pAlias :: !(PAliasEnv)              -- ^ Predicate aliases
                     , quals  :: ![F.Qualifier]            -- ^ Qualifiers
                     , invts  :: ![Located t]              -- ^ Type Invariants
                     } deriving (Functor, Data, Typeable)

type NanoBareR r   = Nano (AnnBare r) (RType r)     -- ^ After Parse
type NanoSSAR r    = Nano (AnnSSA  r) (RType r)     -- ^ After SSA  
type NanoTypeR r   = Nano (AnnType r) (RType r)     -- ^ After TC: Contains an updated TDefEnv
type NanoRefType   = Nano (AnnType F.Reft) (RType F.Reft) -- ^ After Liquid

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
    $+$ text "\n******************* Global vars ***************"
    $+$ pp (glVars pgm)
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
  pp (G s γ c) =  (text "Size:" <+> text (show s))  $$ text "" $$
                  (text "Type definitions:"  $$ nest 2 (pp γ)) $$ text "" $$
                  (text "Named types:"       $$ nest 2 (pp c))

instance PP t => PP (I.IntMap t) where
  pp m = vcat (pp <$> I.toList m)

instance PP t => PP (F.SEnv t) where
  pp m = vcat $ pp <$> F.toListSEnv m

instance (PP t) => PP (TDef t) where
    pp (TD (Just nm) vs Nothing ts) = 
          pp nm 
      <+> ppArgs brackets comma vs 
      <+> braces (
            text " " 
        <+> (vcat $ (\t -> pp t <> text ";") <$> ts) 
        <+> text " ")
    pp (TD (Just nm) vs (Just (p,ps)) ts) = 
          pp nm 
      <+> ppArgs brackets comma vs 
      <+> text "extends" <+> pp p <+> pp ps
      <+> braces (
            text " " 
        <+> (vcat $ (\t -> pp t <> text ";") <$> ts) 
        <+> text " ")
    pp (TD Nothing _ Nothing ts) = 
            braces (
            text " " 
        <+> (vcat $ (\t -> pp t <> text ";") <$> ts) 
        <+> text " ")
    pp _ = error "PP TDEF: case not possible"


instance (PP t) => PP (TElt t) where
  pp (TE s True t) = pp (F.symbol s) <+> text ":" <+> pp t
  pp (TE s False t) = text "private" <+> pp (F.symbol s) <+> text ":" <+> pp t

instance PP Bool where
  pp True   = text "True"
  pp False  = text "False"
    

mapCode :: (a -> b) -> Nano a t -> Nano b t
mapCode f n = n { code = fmap f (code n) }

------------------------------------------------------------------------------------------
-- | Mutability 
------------------------------------------------------------------------------------------

data Mutability 
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
    mGnty         = glVars p  -- guarantees

-- | `immutableVars p` returns symbols that must-not be re-assigned and hence
--    * can appear in refinements

readOnlyVars   :: (IsLocated a, Data a, Typeable a) => Nano a t -> [Id SourceSpan] 
readOnlyVars p = envIds $ mAssm `envUnion` mMeas `envUnion` mExtr
  where 
    mMeas      = consts p     -- measures
    mAssm      = specs  p     -- assumes                      
    mExtr      = externs p    -- externs


---------------------------------------------------------------------------
-- | Pretty Printer Instances ---------------------------------------------
---------------------------------------------------------------------------

instance PP () where 
  pp _ = text ""

instance PP a => PP [a] where 
  pp = ppArgs brackets comma 

instance PP a => PP (Maybe a) where 
  pp = maybe (text "Nothing") pp 

instance PP a => PP (S.Set a) where
  pp = pp . S.toList

instance PP Char where
  pp = char


instance F.Reftable r => PP (RType r) where
  pp (TVar α r)                 = F.ppTy r $ pp α 
  pp (TFun xts t _)             = ppArgs parens comma xts <+> text "=>" <+> pp t 
  pp t@(TAll _ _)               = text "forall" <+> ppArgs id space αs <> text "." <+> pp t' where (αs, t') = bkAll t
  pp (TAnd ts)                  = vcat [text "/\\" <+> pp t | t <- ts]
  pp (TExp e)                   = pprint e 
  pp (TApp TUn ts r)            = F.ppTy r $ ppArgs id (text " +") ts 
  pp (TApp d@(TRef _) ts r)     = F.ppTy r $ pp d <+> ppArgs brackets comma ts 
  pp (TApp c [] r)              = F.ppTy r $ pp c 
  pp (TApp c ts r)              = F.ppTy r $ parens (pp c <+> ppArgs id space ts)  
  pp (TArr t r)                 = F.ppTy r $ brackets (pp t)  
  pp (TCons bs r)               = F.ppTy r $ ppArgs braces comma bs

instance PP TCon where
  pp TInt             = text "number"
  pp TBool            = text "boolean"
  pp TString          = text "string"
  pp TVoid            = text "void"
  pp TTop             = text "top"
  pp TUn              = text "union:"
  pp (TRef x)         = text "REF#" <> int x
  pp TNull            = text "null"
  pp TUndef           = text "undefined"

instance Hashable TCon where
  hashWithSalt s TInt        = hashWithSalt s (0 :: Int)
  hashWithSalt s TBool       = hashWithSalt s (1 :: Int)
  hashWithSalt s TString     = hashWithSalt s (2 :: Int)
  hashWithSalt s TVoid       = hashWithSalt s (3 :: Int)
  hashWithSalt s TTop        = hashWithSalt s (4 :: Int)
  hashWithSalt s TUn         = hashWithSalt s (5 :: Int)
  hashWithSalt s TNull       = hashWithSalt s (6 :: Int)
  hashWithSalt s TUndef      = hashWithSalt s (7 :: Int)
  hashWithSalt s (TRef z)    = hashWithSalt s (8 :: Int) + hashWithSalt s z

instance F.Reftable r => PP (Bind r) where 
  pp (B x t)          = pp x <> colon <> pp t 

ppArgs p sep          = p . intersperse sep . map pp

instance (PP s, PP t) => PP (M.HashMap s t) where
  pp m = vcat $ pp <$> M.toList m


-----------------------------------------------------------------------------
-- | IContext keeps track of context of intersection-type cases -------------
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
-- | Casts ------------------------------------------------------------------
-----------------------------------------------------------------------------

data Cast t  = UCST {castOrigin :: t, castTarget :: t } -- ^ up-cast
             | DCST {castTarget :: t } -- ^ down-cast 
             | DC   {castTarget :: t } -- ^ dead-cast
             deriving (Eq, Ord, Show, Data, Typeable)

instance (PP a) => PP (Cast a) where
  pp (UCST t1 t2) = text "Upcast  : " <+> pp t1 <+> text " => " <+> pp t2
  pp (DCST t)     = text "Downcast: " <+> pp t
  pp (DC   t)     = text "Deadcast: " <+> pp t

-----------------------------------------------------------------------------
-- | Annotations ------------------------------------------------------------
-----------------------------------------------------------------------------

-- | Annotations: Extra-code decorations needed for Refinement Type Checking

data Fact r
  = PhiVar      ![(Id SourceSpan)]
  | TypInst     !IContext ![RType r]
  | Overload    !(Maybe (RType r))
  | TCast       !IContext !(Cast (RType r))
  -- Type annotations
  | TAnnot      !(RType r)
  -- Class annotation
  | CAnnot      !([TVar], Maybe (Id SourceSpan,[RType r]))
    deriving (Eq, Ord, Show, Data, Typeable)

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

instance Eq (Annot a SourceSpan) where 
  (Ann s1 _) == (Ann s2 _) = s1 == s2

instance IsLocated (Annot a SourceSpan) where 
  srcPos = ann

-- instance IsLocated TCon where
--  srcPos (TRef z) = srcPos z
--  srcPos _        = srcPos dummySpan

instance (F.Reftable r, PP r) => PP (Fact r) where
  pp (PhiVar x)       = text "phi"  <+> pp x
  pp (TypInst ξ ts)   = text "inst" <+> pp ξ <+> pp ts 
  pp (Overload i)     = text "overload" <+> pp i
  pp (TCast  ξ c)     = text "cast" <+> pp ξ <+> pp c
  pp (TAnnot t)       = text "annotation" <+> pp t
  pp (CAnnot _)       = error "UNIMPLEMENTED:pp:CAnnot"

instance (F.Reftable r, PP r) => PP (AnnInfo r) where
  pp             = vcat . (ppB <$>) . M.toList 
    where 
      ppB (x, t) = pp x <+> dcolon <+> pp t

instance (PP a, PP b) => PP (Annot b a) where
  pp (Ann x ys) = text "Annot: " <+> pp x <+> pp ys

-- varDeclAnnot v = listToMaybe [ t | TAnnot t <- ann_fact $ getAnnotation v]

phiVarsAnnot l = concat [xs | PhiVar xs <- ann_fact l]

-----------------------------------------------------------------------
-- | Primitive / Base Types -------------------------------------------
-----------------------------------------------------------------------

tVar   :: (F.Reftable r) => TVar -> RType r
tVar   = (`TVar` fTop) 

isTVar (TVar _ _) = True
isTVar _          = False

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

tArr    = (`TArr` fTop)

isArr (TArr _ _ ) = True
isArr _           = False

isTFun (TFun _ _ _) = True
isTFun (TAnd ts)    = all isTFun ts
isTFun (TAll _ t)   = isTFun t
isTFun _            = False


-----------------------------------------------------------------------
-- | Operator Types ---------------------------------------------------
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
builtinOpId BINumArgs       = builtinId "BINumArgs"

-- arrayLitTy :: (IsLocated l) => l -> Int -> Env t -> t
arrayLitTy l n g 
  = case ty of 
      TAll α (TFun [xt] t r) -> TAll α $ TFun (arrayLitBinds n xt) (arrayLitOut n t) r
      _                      -> err 
    where
      ty                     = builtinOpTy l BIArrayLit g
      err                    = die $ bug (srcPos l) $ "Bad Type for ArrayLit Constructor"
      
arrayLitBinds n (B x t) = [B (x_ i) t | i <- [1..n]] 
  where
    xs                  = F.symbolString x
    x_                  = F.symbol . (xs ++) . show 

arrayLitOut n t         = F.subst1 t (F.symbol $ builtinOpId BINumArgs, F.expr (n::Int))

-----------------------------------------------------------------------
infixOpTy :: InfixOp -> Env t -> t 
-----------------------------------------------------------------------
infixOpTy o g = fromMaybe err $ envFindTy ox g
  where 
    err       = errorstar $ printf "Cannot find infixOpTy %s" (ppshow ox) -- (ppshow g)
    ox        = infixOpId o

infixOpId OpLT       = builtinId "OpLT"
infixOpId OpLEq      = builtinId "OpLEq"
infixOpId OpGT       = builtinId "OpGT"
infixOpId OpGEq      = builtinId "OpGEq"
infixOpId OpEq       = builtinId "OpEq"
infixOpId OpStrictEq = builtinId "OpSEq"
infixOpId OpNEq      = builtinId "OpNEq"
infixOpId OpLAnd     = builtinId "OpLAnd"
infixOpId OpLOr      = builtinId "OpLOr"
infixOpId OpSub      = builtinId "OpSub"
infixOpId OpAdd      = builtinId "OpAdd"
infixOpId OpMul      = builtinId "OpMul"
infixOpId OpDiv      = builtinId "OpDiv"
infixOpId OpMod      = builtinId "OpMod"
infixOpId o          = errorstar $ "Cannot handle: infixOpId " ++ ppshow o

-----------------------------------------------------------------------
prefixOpTy :: PrefixOp -> Env t -> t 
-----------------------------------------------------------------------
prefixOpTy o g = fromMaybe err $ envFindTy (prefixOpId o) g
  where 
    err       = convertError "prefixOpTy" o

prefixOpId PrefixMinus  = builtinId "PrefixMinus"
prefixOpId PrefixLNot   = builtinId "PrefixLNot"
prefixOpId PrefixTypeof = builtinId "PrefixTypeof"
prefixOpId o            = errorstar $ "Cannot handle: prefixOpId " ++ ppshow o


builtinId       = mkId . ("builtin_" ++)

-----------------------------------------------------------------------
-- Type and Predicate Aliases -----------------------------------------
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

