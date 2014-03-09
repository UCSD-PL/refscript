{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses     #-}

module Language.Nano.Typecheck.Compare (

  -- * Type comparison/joining/subtyping
    Equivalent
  , equiv
--  , compareTs
--  , alignTs
  , unionParts, unionParts'
  , bkPaddedUnion

  , arrDir
  
  -- * Casting
  , compareTs
  -- , Casts
  , Casts_
  , zipType1
  , zipType2

  , SubDirection (..)

  ) where 

import           Text.Printf

import qualified Data.List                          as L
import qualified Data.HashMap.Strict                as M
import qualified Data.HashSet                       as S
import           Data.Monoid
import           Data.Maybe                         (fromJust)
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.PrettyPrint
import           Language.Nano.Env
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Errors
import           Language.Nano.Misc
import           Language.Nano.Typecheck.Types

import           Language.Fixpoint.Errors
import qualified Language.Fixpoint.Types            as F
import           Language.Fixpoint.Misc
-- import           Language.Fixpoint.PrettyPrint
import           Text.PrettyPrint.HughesPJ 

import           Control.Applicative                hiding (empty)
import           Control.Monad.Error                ()

import           Debug.Trace (trace)


type PPR r = (PP r, F.Reftable r)


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
  equiv (TVar v _   ) (TVar v' _    ) = v == v'
  equiv (TFun b o _ ) (TFun b' o' _ ) = 
    (b_type <$> b) `equiv` (b_type <$> b') && o `equiv` o' 
  equiv (TAll _ _   ) (TAll _ _     ) = error "equiv-tall"
  equiv _             _               = False

instance Equivalent TCon where
  equiv (TRef i) (TRef i')  = i == i'
  equiv c        c'         = c == c'

instance PPR r => Equivalent (TElt (RType r)) where 
  equiv (TE _ b1 t1) (TE _ b2 t2) = b1 == b2 && equiv t1 t2

instance Equivalent (Bind r) where 
  equiv (B s t) (B s' t') = s == s' && equiv t t' 

instance Equivalent (Id a) where 
  equiv i i' = F.symbol i == F.symbol i'



---------------------------------------------------------------------------------------
-- Casts ------------------------------------------------------------------------------
---------------------------------------------------------------------------------------

-- type Casts    = M.Map (Expression (AnnSSA F.Reft)) (Cast RefType)
type Casts_ r = M.HashMap (Expression (AnnSSA r)) (Cast (RType r))


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
  pp EqT  = text "≈"
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


---------------------------------------------------------------------------------
-- SubType API ------------------------------------------------------------------
---------------------------------------------------------------------------------
-- isSubType :: (F.Reftable r, Ord r, PP r) => TDefEnv (RType r) -> RType r -> RType r -> Bool
-- isSubType δ t1 t2 = (fth4 $ compareTs δ t1 t2) `elem` [EqT, SubT]

-- | `alignTs`
-- alignTs δ t1 t2     = (t1', t2')
--   where 
--     (_,t1', t2', _) = compareTs δ t1 t2


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



distinctBs b1 [] = (b_sym <$> b1, []          )
distinctBs [] b2 = ([]          , b_sym <$> b2)
distinctBs b1 b2 = (diff m1 m2  , diff m2 m1  )
  where
    m1           = bindsMap b1
    m2           = bindsMap b2
    diff m m'    = M.keys $ M.difference m m'

bindsMap bs      = M.fromList [(s, t) | B s t <- bs]

-- Direction from distinct keys
distDir xs ys 
  | null (xs ++ ys) = EqT
  | null xs         = SupT
  | null ys         = SubT
  | otherwise       = Nth

meetBinds b1s b2s = M.toList $ M.intersectionWith (,) (bindsMap b1s) (bindsMap b2s)



-- | `compareTs` returns:
-- * A padded version of the upper bound of @t1@ and @t2@
-- * An equivalent version of @t1@ that has the same sort as the second (RJ: first?) output
-- * An equivalent version of @t2@ that has the same sort as the first output
-- * A subtyping direction between @t1@ and @t2@
--
-- Padding the input types gives them the same sort, i.e. makes them compatible. 
---------------------------------------------------------------------------------------
compareTs :: (PPR r) => TDefEnv (RType r) -> RType r -> RType r 
  -> (RType r, RType r, SubDirection, TDefEnv (RType r))
---------------------------------------------------------------------------------------

compareTs δ t1 t2 | t1 `equiv` t2        = (t1, t2, EqT, δ)

compareTs δ t1 t2 | isUndef t1           = setThd4 (compareUnion δ t1 t2) SubT

compareTs δ t1 t2 | isNull t1 &&  
                    not (isUndef t2)     = setThd4 (compareUnion δ t1 t2) SubT

compareTs δ t1 t2 | isTop t2             = setThd4 (compareUnion δ t1 t2) SubT

compareTs δ t1 t2 | any isUnion [t1,t2]  = compareUnion δ t1 t2

compareTs δ t1 t2 | all isArr   [t1,t2]  = compareArray δ t1 t2

-- FIX: This should be updating the result with the new env
compareTs δ t1 t2 | all isTRef  [t1,t2]  = compareTRefs δ t1 t2

compareTs δ t1 t2 | all isTFun  [t1, t2] = compareFun δ t1 t2
compareTs _ (TFun _ _ _)    _            = error "Unimplemented compareTs-1"
compareTs _ _               (TFun _ _ _) = error "Unimplemented compareTs-2"

compareTs _ (TAll _ _  ) _               = error "Unimplemented: compareTs-3"
compareTs _ _            (TAll _ _  )    = error "Unimplemented: compareTs-4"

compareTs δ t1           t2              = compareSimple δ t1 t2 



-- | `compareTRefs`
--
-- Compare defined types by width (only).
--
-- TODO: a private field should be the same as a missing field
-- 
--------------------------------------------------------------------------------
compareTRefs :: (PPR r) => TDefEnv (RType r) -> RType r -> RType r 
    -> (RType r, RType r, SubDirection, TDefEnv (RType r))
--------------------------------------------------------------------------------
compareTRefs δ t1@(TApp (TRef i1) t1s r1) t2@(TApp (TRef i2) t2s r2) 
  -- Same exact type name
  | i1 == i2          = (t1, t2, parEq, δ)
  -- Difference type name but equal types
  | cmnEq && widthEq  = (t1, t2, EqT, δ)
  -- Upcast  : A<S> <: B<T>
  | cmnEq && widthSup = (TApp (TRef i1') t1s r1, t2, SupT, δ')
  -- Downcast: A<S> :> B<T>
  | cmnEq && widthSub = compareSimple δ t1 t2
  -- No relation
  | otherwise         = compareSimple δ t1 t2
  where
    parEq | and (zipWith equiv t1s t2s) = EqT
          | otherwise                   = Nth
    -- Restrict A<S1...> to the fields of B<T1...>,
    e1s''      = filter (\e -> S.member (f_sym e) ks2) e1s
    -- Use B<T1...> 's name and prototype.
    (δ', i1')  = addObjLitTy (TD n2 v1s pro2 e1s'') δ   
    -- Equal types at the common fields
    cmnEq      = all (uncurry depthEq) cmnTs
    cmnTs      = (\k -> (k `lkp` toMap e1s', k `lkp` toMap e2s')) <$> cmnKs
    cmnKs      = S.toList $ S.intersection ks1 ks2
    -- Width eq/sub/super-type
    widthEq    = isEqualSet ks1 ks2
    widthSub   = isProperSubsetOf ks1 ks2
    widthSup   = isProperSubsetOf ks2 ks1
    -- Elements with the type params substituted for the actual params
    e1s'       = apply (fromList $ zip v1s t1s) e1s
    e2s'       = apply (fromList $ zip v2s t2s) e2s
    -- Get the type definitions
    TD n1 v1s pro1 e1s = findTyIdOrDie' "compareTRefs" i1 δ 
    TD n2 v2s pro2 e2s = findTyIdOrDie' "compareTRefs" i2 δ
    -- Field keys
    ks1        = ks e1s
    ks2        = ks e2s
    -- Aux funcs
    toMap      = M.fromList . ((\(TE s b t) -> (s, (b,t))) <$>)
    ks         = S.fromList . (f_sym <$>)
    lkp k      = fromJust . M.lookup k 


-- | depthEq: The input pairs are (access modifier, type)
--------------------------------------------------------------------------------
depthEq :: (Bool, RType r) -> (Bool, RType r) -> Bool
--------------------------------------------------------------------------------
depthEq (b1,t1) (b2, t2) | b1 == b2   = t1 `equiv` t2
depthEq _       _        | otherwise  = False 

-- | Type equivalence: This is equality on the raw type level, 


-- | `compareFun`

compareFun δ (TFun b1s o1 r1) (TFun b2s o2 r2) 
  | length b1s == length b2s && sameTypes = (t1', t2', EqT, δ)
  | otherwise = error "[Unimplemented] compareFun with different types"
    where
      sameTypes         = all (== EqT) $ od:bds
      (t1s',t2s',bds,_) = unzip4 $ zipWith (compareTs δ) (b_type <$> b1s) (b_type <$> b2s)
      (o1',o2',od,_)    = compareTs δ o1 o2
      t1'               = TFun (updTs b1s t1s') o1' r1
      t2'               = TFun (updTs b2s t2s') o2' r2
      updTs             = zipWith (\b t -> b { b_type = t })

compareFun _ _ _ = error "compareFun: no other cases supported"


-- | `compareSimple`
--------------------------------------------------------------------------------
compareSimple :: (PPR r) => TDefEnv (RType r) -> RType r -> RType r 
  -> (RType r, RType r, SubDirection, TDefEnv (RType r))
--------------------------------------------------------------------------------
compareSimple δ t1 t2
  | t1 `equiv` t2 = (t1, t2, EqT, δ)
  | otherwise     = (t1', t2', Nth, δ)
    where t1'     = mkUnion [t1, fmap F.bot t2]  -- Toplevel refs?
          t2'     = mkUnion [fmap F.bot t1, t2]


-- | `compareUnion`

-- Produces an equivalent type for @t1@ (resp. @t2@) that is extended with 
-- the missing type terms to the common upper bound of @t1@ and @t2@. The extra
-- type terms that are added in the union are refined with False to keep them
-- equivalent with the input types.
--
-- The output is the following tuple:
--  * adjusted type for @t1@ to be sort compatible,
--  * adjusted type for @t2@ to be sort compatible
--  * a subtyping direction
--  * the updated environment

--------------------------------------------------------------------------------
compareUnion ::  (PPR r) => 
             TDefEnv (RType r)  -- Type defs
          -> RType r            -- LHS
          -> RType r            -- RHS
          -> (  RType r,        -- The equivalent to @t1@
                RType r,        -- The equivalent to @t2@
                SubDirection,   -- Subtyping relation between LHS and RHS
                TDefEnv (RType r))
--------------------------------------------------------------------------------
compareUnion δ t1 t2 | all (not . isUnion) [t1, t2] 
                     = compareSimple δ t1 t2
compareUnion δ t1 t2 | otherwise                    
                     = (mkUnionR r1 $ t1s, mkUnionR r2 $ t2s, direction, δ)
  where
    -- Extract top-level refinements
    (r1, r2) = mapPair rUnion (t1, t2)
    -- Ignore refinement.
    (t1s, t2s) = unzip $ safeZip "unionParts" t1s' t2s'
    t1s'       = commonT1s ++ d1s ++ (fmap F.bot <$> d2s)
    t2s'       = commonT2s ++ (fmap F.bot <$> d1s) ++ d2s
    commonT1s  = fst4 <$> commonTs
    commonT2s  = snd4 <$> commonTs
    commonTs = map (uncurry $ compareTs δ) cmnPs
    -- To figure out the direction of the subtyping, we must take into account:
    direction  = distSub `mappend` comSub
    -- * The distinct types (the one that has more is a supertype)
    distSub   = case (d1s, d2s) of
                  ([], []) -> EqT
                  ([], _ ) -> SubT  -- <:
                  (_ , []) -> SupT  -- >:
                  (_ , _ ) -> Nth -- no relation
    -- * The common types (recursively call `compareTs` to compare the types
    --   of the parts and join the subtyping relations)
    comSub     = mconcat $ thd4 <$> commonTs
    (cmnPs, d1s, d2s) = unionParts t1 t2


-- | `compareArray`
compareArray δ (TArr t1 r1) (TArr t2 r2) = (TArr t1' r1, TArr t2' r2, arrDir ad, δ)
  where (t1', t2', ad, _) = compareTs δ t1 t2
compareArray _ _ _ = errorstar "BUG: compareArray can only pad Arrays"     


-- | Helper
instance (PP a, PP b, PP c, PP d) => PP (a,b,c,d) where
  pp (a,b,c,d) = pp a <+>  pp b <+> pp c <+> pp d


-- | `zipType1` matches structurally equivalent parts of types @t1@ and @t2@:
-- * Keeping the the structure of @t1@
-- * Applying f on the respective refinements 
-- * f is commutative
-- 
-- E.g. zipType1 (number + { boolean | p } ) { number | v > 0 } meet = 
--        { number | v > 0 } + { boolean | p } 
zipType1 δ f t1 t2 = zipType2 δ f t2 t1


-- | `zipType2` walks through the equivalent parts of types @t1@ and @t2@. It 
-- applies function $f$ on the refinements of the equivalent parts and keeps the
-- output as the resulting refinement. 
-- The shape of @t2@ is preserved in the output.
--------------------------------------------------------------------------------
zipType2 :: (PP r, F.Reftable r) => TDefEnv (RType r) -> (r -> r -> r) ->  RType r -> RType r -> RType r
--------------------------------------------------------------------------------
zipType2 δ f (TApp TUn ts r) (TApp TUn ts' r')  = 
  TApp TUn (zipTypes δ f ts <$> ts') $ f r r'

zipType2 δ f (TApp TUn ts _) t =  
  zipTypes δ f ts t

zipType2 δ f t (TApp TUn ts' r') =  
  TApp TUn (zipTypes δ f [t] <$> ts') r'        -- The top-level refinement for t' should remain

zipType2 δ f (TApp d@(TRef _) ts r) (TApp d'@(TRef _) ts' r') | d == d' =
  TApp d' (zipWith (zipType2 δ f) ts ts') $ f r r'

zipType2 _ f (TApp c [] r) (TApp c' [] r')    | c == c' = 
  TApp c [] $ f r r'

zipType2 _ f (TVar v r) (TVar v' r') | v == v' = TVar v $ f r r'

zipType2 δ f (TFun xts t r) (TFun xts' t' r') = 
  TFun (safeZipWith "zipType2:TFun" (zipBind2 δ f) xts xts') (zipType2 δ f t t') $ f r r'

zipType2 δ f (TArr t r) (TArr t' r') = TArr (zipType2 δ f t t') $ f r r'

zipType2 _ _ t t' = 
  errorstar $ printf "BUG[zipType2]: mis-aligned types:\n\t%s\nand\n\t%s" (ppshow t) (ppshow t')

zipTypes δ f ts t = 
  case filter (equiv t) ts of
    [  ] -> t
    [t'] -> zipType2 δ f t' t
    _    -> errorstar "BUG[zipType]: multiple equivalent types" 
  

zipBind2 δ f (B s t) (B s' t') | s == s' = B s $ zipType2 δ f t t' 
zipBind2 _ _ _       _                   = errorstar "BUG[zipBind2]: mis-matching binders"

