-- | TODO: Add description

{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE NoMonomorphismRestriction   #-}
{-# LANGUAGE MultiParamTypeClasses   #-}

module Language.Nano.Typecheck.Compare (

  -- * Type comparison/joining/subtyping
    Equivalent, equiv
--  , compareTs
--  , alignTs
  , unionParts, unionParts'
  , bkPaddedUnion
  , bkPaddedObject
--  , isSubType

  , (&+&), mconcatS, arrDir
  
  -- * Casting
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
                  | Rel     -- Relatable type: can be thought of in terms of 
                            -- unions as a non-empty intersection of two types,
                            -- but not strong enough to be SubT, SupT, ot EqT
                  | Nth     -- No relation between two types is known
                  deriving (Eq, Show)

instance PP SubDirection where 
  pp SubT = text "<:"
  pp SupT = text ":>"
  pp EqT  = text "≈"
  pp Rel  = text "⨝"
  pp Nth  = text "≠"

-- The Subtyping directions form a monoid both as a `sum` and as a `product`, 
-- cause they are combined in different ways when used in unions and object (the
-- two places where subtyping occurs).

-- Sum: Relaxed version (To be used in unions)
instance Monoid (Sum SubDirection) where
  mempty = Sum EqT

  Sum d    `mappend` Sum d'   | d == d' = Sum d
  
  -- We know nothing about the types so far (Nth), but we can use the other part
  -- to make any assumptions, that's why @d@ is propagated.
  Sum Nth  `mappend` Sum _              = Sum Rel
  Sum _    `mappend` Sum Nth            = Sum Rel

  Sum EqT  `mappend` Sum d              = Sum d
  Sum d    `mappend` Sum EqT            = Sum d
  
  Sum _    `mappend` Sum _              = Sum Rel


(&+&)   :: Monoid (Sum t) => t -> t -> t
a &+& b = getSum $ mappend (Sum a) (Sum b)

mconcatS   :: Monoid (Sum t) => [t] -> t
mconcatS xs = getSum $ mconcat (Sum <$> xs)


-- Product: Strict version (To be used in objects)
instance Monoid (Product SubDirection) where
  mempty = Product EqT

  Product d    `mappend` Product d'   | d == d' = Product d
  
  Product Nth  `mappend` Product _              = Product Nth
  Product _    `mappend` Product Nth            = Product Nth

  Product EqT  `mappend` Product d              = Product d
  Product d    `mappend` Product EqT            = Product d
  
  Product _    `mappend` Product _              = Product Nth



(&*&)   :: Monoid (Product t) => t -> t -> t 
a &*& b = getProduct $ mappend (Product a) (Product b)

mconcatP   :: Monoid (Product t) => [t] -> t
mconcatP xs = getProduct $ mconcat (Product <$> xs)

arrDir     :: SubDirection -> SubDirection
arrDir EqT = EqT
arrDir _   = Nth


---------------------------------------------------------------------------------
-- SubType API ------------------------------------------------------------------
---------------------------------------------------------------------------------
-- isSubType :: (F.Reftable r, Ord r, PP r) => TDefEnv (RType r) -> RType r -> RType r -> Bool
-- isSubType γ t1 t2 = (fth4 $ compareTs γ t1 t2) `elem` [EqT, SubT]




-- | `alignTs`
-- alignTs γ t1 t2     = (t1', t2')
--   where 
--     (_,t1', t2', _) = compareTs γ t1 t2


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
-- ∙ The paired-up types (common as per @eq@)
-- ∙ The types appearing just in @t1@
-- ∙ The types appearing just in @t2@
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

bkPaddedObject l _ _   = die $ bug l $ "bkPaddedObject: can only break objects"


-- | `compareFun`

compareFun γ (TFun b1s o1 r1) (TFun b2s o2 r2) 
  | length b1s == length b2s && sameTypes = (joinT, t1', t2', EqT)
  | otherwise = error "[Unimplemented] compareFun with different types"
    where
      sameTypes              = all (== EqT) $ od:bds
      (tjs, t1s', t2s', bds) = unzip4 $ zipWith (compareTs γ) (b_type <$> b1s) (b_type <$> b2s)
      (oj , o1' , o2' , od ) = compareTs γ o1 o2
      t1'                    = TFun (updTs b1s t1s') o1' r1
      t2'                    = TFun (updTs b2s t2s') o2' r2
      joinT                  = TFun (updTs b1s tjs) oj fTop 
      updTs                  = zipWith (\b t -> b { b_type = t })

compareFun _ _ _ = error "compareFun: no other cases supported"


-- | `compareTs` 
--    returns:
-- ∙ A padded version of the upper bound of @t1@ and @t2@
-- ∙ An equivalent version of @t1@ that has the same sort as the second (RJ: first?) output
-- ∙ An equivalent version of @t2@ that has the same sort as the first output
-- ∙ A subtyping direction between @t1@ and @t2@
--
-- Padding the input types gives them the same sort, i.e. makes them compatible. 
---------------------------------------------------------------------------------------
compareTs :: (PPR r, Ord r) => TDefEnv (RType r) -> RType r -> RType r 
  -> (RType r, RType r, RType r, SubDirection)
---------------------------------------------------------------------------------------

-- Deal with some standard cases of subtyping, e.g.: Top, Null, Undefined ...
compareTs γ t1 t2 | toType t1 == toType t2 = (ofType $ toType t1, t1, t2, EqT)

compareTs γ t1 t2 | isUndefined t1         = setFth4 (compareUnion γ t1 t2) SubT

-- NOTE: Null is NOT considered a subtype of all types. If null is to be 
-- expected this should be explicitly specified by using " + null"
-- compareTs γ t1 t2 | and [isNull t1, not $ isUndefined t2] = setFth4 (compareTs' γ t1 t2) SubT

-- | Top
compareTs γ t1 _  | isTop t1               = errorstar "unimplemented: compareTs - top"
compareTs γ t1 t2 | isTop t2               = (t1', t1, t2', SubT)
  where
    t1' = setRTypeR t1 fTop -- this will be kVared
    -- @t2@ is a Top, so just to make the types compatible we will 
    -- use the base type of @t1@ and stregthen with @t2@'s refinement.
    t2' = setRTypeR t1 $ rTypeR t2

-- Eliminate top-level unions
compareTs γ t1 t2 | any isUnion [t1,t2]  = compareUnion γ t1 t2

-- | Arrays
compareTs γ a@(TArr _ _) a'@(TArr _ _  )   = padArray γ a a'

-- | Type reference: Structural subtyping
--
-- WARNING: cycle
--
compareTs γ (TApp d1@(TRef _) t1s r1) (TApp d2@(TRef _) t2s r2) | d1 == d2 =
   (mk tjs fTop, mk t1s' r1, mk t2s' r2, mconcatP bds)
   where
     (tjs, t1s', t2s', bds)  = unzip4 $ zipWith (compareTs γ) t1s t2s
     mk xs r                 = TApp d1 xs r 

compareTs γ (TApp d1@(TRef _) t1s r1) (TApp d2@(TRef _) t2s r2) | otherwise = undefined
 
-- | Everything else in TApp besides unions and defined types
-- compareTs' _ t1@(TApp _ _ _) t2@(TApp _ _ _) = compareSimple t1 t2 
-- 
-- -- | Type Vars
-- compareTs' _ t1@(TVar _ _)   t2@(TVar _ _)   = compareVar t1 t2





-- | Function Types
compareTs γ t1@(TFun _ _ _) t2@(TFun _ _ _) = compareFun γ t1 t2
compareTs _ (TFun _ _ _)    _               = error "Unimplemented compareTs-1"
compareTs _ _               (TFun _ _ _)    = error "Unimplemented compareTs-2"

-- | TAll
compareTs _ (TAll _ _  ) _                  = error "Unimplemented: compareTs-3"
compareTs _ _            (TAll _ _  )       = error "Unimplemented: compareTs-4"
 
-- | Rest of cases

-- Let these cases be dealt by padding unions
compareTs _ t1           t2                 = compareSimple t1 t2 


-- | Pad objects

compareTRefs γ (Id _ s1, t1s) (Id _ s2, t2s) = undefined
    --  &&  S.null (S.difference (ss e1s) (ss e2s))
    -- &&  S.null (S.difference (ss e2s) (ss e1s))
  where
    (f1s, f2s) = unzip [ (e1, e2) | e1 <- e1s, e2 <- e2s
                                  , f_sym e1 == f_sym e2 && f_acc e1 == f_acc e2 ]
    (d1,d2)    = (TD v1s f1s, TD v2s f2s) 
    {-superType  = S.isProperSubsetOf (ss e1s) (ss e2s)-}
    {-subType    = S.isProperSubsetOf (ss e2s) (ss e1s)-}

    TD v1s e1s = fromJust $ envFindTy s1 γ 
    TD v2s e2s = fromJust $ envFindTy s2 γ
    θ1         = fromList $ zip v1s t1s 
    θ2         = fromList $ zip v2s t2s 
    ss es      = S.fromList $ f_sym <$> es


-- | `compareSimple`

compareSimple t1 t2
  | t1 == t2  = (t1, t1, t2, EqT)
  | otherwise = (joinType, t1', t2', Nth)
    where joinType = (ofType . toType) $ mkUnion [t1,t2]
          t1'      = mkUnion [t1, fmap F.bot t2]  -- Toplevel refs?
          t2'      = mkUnion [fmap F.bot t1, t2]


-- | `compareVar`

compareVar t1@(TVar v1 _ ) t2@(TVar v2 _) | v1 == v2 = (t1, t1, t2, EqT)
compareVar t1 t2 = errorstar $ printf "compareVar: cannot compare %s and %s" (ppshow t1) (ppshow t2)



-- | `compareUnion`

-- Produces an equivalent type for @t1@ (resp. @t2@) that is extended with 
-- the missing type terms to the common upper bound of @t1@ and @t2@. The extra
-- type terms that are added in the union are refined with False to keep them
-- equivalent with the input types.
--
-- The output is the following tuple:
--  ∙ common upper bound type (@t1@ ∪ @t2@) with topped predicates
--  ∙ adjusted type for @t1@ to be sort compatible,
--  ∙ adjusted type for @t2@ to be sort compatible
--  ∙ a subtyping direction

--------------------------------------------------------------------------------
compareUnion ::  (Eq r, Ord r, F.Reftable r, PP r) => 
             TDefEnv (RType r)  -- Type defs
          -> RType r            -- LHS
          -> RType r            -- RHS
          -> (  RType r,        -- The join of the two types
                RType r,        -- The equivalent to @t1@
                RType r,        -- The equivalent to @t2@
                SubDirection)   -- Subtyping relation between LHS and RHS
--------------------------------------------------------------------------------
compareUnion γ t1 t2 = (joinType, mkUnionR r1 $ t1s, mkUnionR r2 $ t2s, direction)
  where
    -- Extract top-level refinements
    (r1, r2) = mapPair rUnion (t1, t2)
    -- Ignore refinement.
    joinType   = mkUnion $ (ofType . toType) <$> ((fst4 <$> commonTs) ++ d1s ++ d2s)
    (t1s, t2s) = unzip $ safeZip "unionParts" t1s' t2s'

    -- It is crucial to sort the types so that they are aligned
    t1s'       = L.sort $ commonT1s ++ d1s ++ (fmap F.bot <$> d2s)
    t2s'       = L.sort $ commonT2s ++ d2s ++ (fmap F.bot <$> d1s)

    commonT1s  = snd4 <$> commonTs
    commonT2s  = thd4 <$> commonTs

    commonTs = 
      {-tracePP "compareUnion: compaTs on common parts" $ -}
      map (uncurry $ compareTs γ) $ cmnPs

    -- To figure out the direction of the subtyping, we must take into account:
    direction  = distSub &+& comSub
    -- * The distinct types (the one that has more is a supertype)
    distSub   = case (d1s, d2s) of
                  ([], []) -> EqT
                  ([], _ ) -> SubT  -- <:
                  (_ , []) -> SupT  -- >:
                  (_ , _ ) -> Nth -- no relation
    -- * The common types (recursively call `compareTs` to compare the types
    --   of the parts and join the subtyping relations)
    comSub     = mconcatS $ fth4 <$> commonTs
    (cmnPs, d1s, d2s) = unionParts t1 t2


-- | `padArray`
padArray γ (TArr t1 r1) (TArr t2 r2) = (TArr tj fTop, TArr t1' r1, TArr t2' r2, arrDir ad)
  where (tj, t1', t2', ad)         = compareTs γ t1 t2
padArray _ _ _ = errorstar "BUG: padArray can only pad Arrays"     










-- | Helper
instance (PP a, PP b, PP c, PP d) => PP (a,b,c,d) where
  pp (a,b,c,d) = pp a <+>  pp b <+> pp c <+> pp d


-- | `zipType1` matches structurally equivalent parts of types @t1@ and @t2@:
-- ∙ Keeping the the structure of @t1@
-- ∙ Applying f on the respective refinements 
-- ∙ f is commutative
-- 
-- E.g. zipType1 (number + { boolean | p } ) { number | v > 0 } meet = 
--        { number | v > 0 } + { boolean | p } 
zipType1 γ f t1 t2 = zipType2 γ f t2 t1


-- | `zipType2` walks through the equivalent parts of types @t1@ and @t2@. It 
-- applies function $f$ on the refinements of the equivalent parts and keeps the
-- output as the resulting refinement. 
-- The shape of @t2@ is preserved in the output.
--------------------------------------------------------------------------------
zipType2 :: (PP r, F.Reftable r) => TDefEnv (RType r) -> (r -> r -> r) ->  RType r -> RType r -> RType r
--------------------------------------------------------------------------------
zipType2 γ f (TApp TUn ts r) (TApp TUn ts' r')  = 
  TApp TUn (zipTypes γ f ts <$> ts') $ f r r'

zipType2 γ f (TApp TUn ts _) t =  
  zipTypes γ f ts t

zipType2 γ f t (TApp TUn ts' r') =  
  TApp TUn (zipTypes γ f [t] <$> ts') r'        -- The top-level refinement for t' should remain

zipType2 γ f (TApp d@(TRef _) ts r) (TApp d'@(TRef _) ts' r') | d == d' =
  TApp d' (zipWith (zipType2 γ f) ts ts') $ f r r'

zipType2 _ f (TApp c [] r) (TApp c' [] r')    | c == c' = 
  TApp c [] $ f r r'

zipType2 _ f (TVar v r) (TVar v' r') | v == v' = TVar v $ f r r'

zipType2 γ f (TFun xts t r) (TFun xts' t' r') = 
  TFun (safeZipWith "zipType2:TFun" (zipBind2 γ f) xts xts') (zipType2 γ f t t') $ f r r'

zipType2 γ f (TArr t r) (TArr t' r') = TArr (zipType2 γ f t t') $ f r r'

zipType2 _ _ t t' = 
  errorstar $ printf "BUG[zipType2]: mis-aligned types:\n\t%s\nand\n\t%s" (ppshow t) (ppshow t')

zipTypes γ f ts t = 
  case filter (equiv t) ts of
    [  ] -> t
    [t'] -> zipType2 γ f t' t
    _    -> errorstar "BUG[zipType]: multiple equivalent types" 
  

zipBind2 γ f (B s t) (B s' t') | s == s' = B s $ zipType2 γ f t t' 
zipBind2 _ _ _       _                   = errorstar "BUG[zipBind2]: mis-matching binders"


-------------------------------------------------------------------------------
unfoldMaybe :: (PPR r) => TDefEnv (RType r) -> RType r -> Either String [TElt (RType r)]
-------------------------------------------------------------------------------
unfoldMaybe γ (TApp (TRef id) pars _) =
  case envFindTy (F.symbol id) γ of
    Just (TD vs ts) -> Right $ apply (fromList $ zip vs pars) ts
    _               -> Left  $ printf "Failed unfolding: %s" (ppshow id)
unfoldMaybe _ t = Left  $ printf "Failed unfolding: %s" (ppshow t)

-------------------------------------------------------------------------------
unfoldSafe :: (PPR r) => TDefEnv (RType r) -> RType r -> [TElt (RType r)]
-------------------------------------------------------------------------------
unfoldSafe env = either error id . unfoldMaybe env


