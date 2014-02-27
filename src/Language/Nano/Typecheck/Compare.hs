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
    Equivalent, equiv, equiv'
--  , compareTs
--  , alignTs
  , unionParts, unionPartsWithEq
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
-- import           Language.Nano.Typecheck.Unfold

import           Language.Fixpoint.Errors
import qualified Language.Fixpoint.Types            as F
import           Language.Fixpoint.Misc
-- import           Language.Fixpoint.PrettyPrint
import           Text.PrettyPrint.HughesPJ 

import           Control.Applicative                hiding (empty)
import           Control.Monad.Error                ()

import           Debug.Trace (trace)


type PPR r = (PP r, F.Reftable r)


-- | Type equivalence: slightly more relaxed version of equality. 

class Equivalent r a where 
  equiv :: TDefEnv (RType r) -> a -> a -> Bool

  equiv' :: String -> TDefEnv (RType r) -> a -> a -> Bool
  equiv' msg a b c = equiv a (trace msg b) c


instance Equivalent r a => Equivalent r [a] where
  equiv γ a b = and $ zipWith (equiv γ) a b 

instance PPR r => Equivalent r (RType r) where 
  equiv _ t t'  | toType t == toType t' = True

  equiv _ t t'  | any isUnion [t,t'] = 
    errorstar (printf "equiv: no unions: %s\n\t\t%s" (ppshow t) (ppshow t'))

  equiv γ (TApp (TRef i) ts _) (TApp (TRef i') ts' _) = equivTRefs γ (i,ts) (i',ts')

  equiv _ (TApp c _ _) (TApp c' _ _)  = c == c'

  equiv _ (TVar v _  ) (TVar v' _  )  = v == v'

  equiv γ (TFun b o _) (TFun b' o' _) = 
    equiv γ (b_type <$> b) (b_type <$> b') && equiv γ o o' 

  equiv _ (TAll _ _   ) (TAll _ _ ) = error "equiv-tall"
    -- t `equiv` apply (fromList [(v',tVar v)]) t'

  equiv _ _                    _              = False


---------------------------------------------------------------------------------------
equivTRefs :: PPR r => TDefEnv (RType r) -> (Id a, [RType r]) -> (Id a, [RType r]) -> Bool
---------------------------------------------------------------------------------------
equivTRefs γ (Id _ s1, t1s) (Id _ s2, t2s) =
    -- Common elements need to be equivalent
        and [ equiv γ e1 e2 | e1 <- e1s, e2 <- e2s, f_sym e1 == f_sym e2 ]
    -- There should be the same exactly fields in each one
    &&  S.null (S.difference (ss e1s) (ss e2s))
    &&  S.null (S.difference (ss e2s) (ss e1s))
  where 
    TD v1s e1s = fromJust $ envFindTy s1 γ 
    TD v2s e2s = fromJust $ envFindTy s2 γ
    θ1         = fromList $ zip v1s t1s 
    θ2         = fromList $ zip v2s t2s 
    ss es      = S.fromList $ f_sym <$> es

instance PPR r => Equivalent r (TElt (RType r)) where 
  equiv γ (TE _ b1 t1) (TE _ b2 t2) = b1 == b2 && equiv γ t1 t2

instance PPR r => Equivalent r (Bind r) where 
  equiv γ (B s t) (B s' t') = s == s' && equiv γ t t' 

instance Equivalent r (Id a) where 
  equiv _ i i' = F.symbol i == F.symbol i'


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
arrDir SubT = Rel
arrDir SupT = Rel
arrDir EqT  = EqT
arrDir Rel  = Rel
arrDir Nth  = Nth



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
bkPaddedUnion :: (Eq r, Ord r, F.Reftable r, PP r) => 
  String -> TDefEnv (RType r) -> RType r -> RType r -> [(RType r, RType r)]
--------------------------------------------------------------------------------
bkPaddedUnion msg γ t1 t2 =
  zipWith check (bkUnion t1) (bkUnion t2)
  where
    check t t' 
      | equiv γ t t' = (t,t')
      | otherwise    = errorstar $ printf "bkPaddedUnion[%s]\n\t%s\nand\n\t%s" 
                                     msg (ppshow t1) (ppshow t2) 



-- | `unionParts`

-- Special case of `unionPartsWithEq` that uses `Equivalent` as the type
-- equivalence relation.
--------------------------------------------------------------------------------
unionParts ::  (PPR r) => 
  TDefEnv (RType r) -> RType r -> RType r -> ([(RType r, RType r)], [RType r], [RType r])
--------------------------------------------------------------------------------

unionParts = unionPartsWithEq . equiv


-- General purpose function that pairs up the components of the two union typed
-- inputs @t1@ and @t2@, based on the Equivalence relation @eq@.
-- Top-level refinements are lost here - use `padUnion` to preserve them.
-- The output consists of 
-- ∙ The paired-up types (common as per @eq@)
-- ∙ The types appearing just in @t1@
-- ∙ The types appearing just in @t2@
--------------------------------------------------------------------------------
unionPartsWithEq ::  (PPR r) => 
             (RType r -> RType r -> Bool)
          -> RType r 
          -> RType r 
          -> ([(RType r, RType r)], [RType r], [RType r])
--------------------------------------------------------------------------------
unionPartsWithEq equal t1 t2 = (common t1s t2s, d1s, d2s)
  where
    -- `common` does a "light" matching - which is determined by `equiv`. 
    -- Right now the only difference is in objects: 
    -- all objects are matched together, so the common parts may not be 
    -- the same in terms of raw type. 
    -- This is why `padObject` is called on the common parts. Non-object types
    -- fall through
    -- Also `common` returns aligned types - so no need to re-align them.
    (t1s, t2s) = sanityCheck $ mapPair bkUnion (t1, t2)

    (d1s, d2s) = distinct t1s t2s

    -- Compare the types based on the Equivalence relation and pair them up into
    -- Type structures that are common in both sides, and ...
    common xs ys | any null [xs,ys] = []
    common xs ys | otherwise        = [(x,y) | x <- xs, y <- ys, x `equal` y ]

    -- ... type structures that are distinct in the two sides
    distinct xs [] = ([], xs)
    distinct [] ys = (ys, [])
    distinct xs ys = ([x | x <- xs, not $ any (x `equal`) ys ],
                      [y | y <- ys, not $ any (y `equal`) xs ])

    sanityCheck ([ ],[ ]) = errorstar "unionPartsWithEq, called on too small input"
    sanityCheck ([_],[ ]) = errorstar "unionPartsWithEq, called on too small input"
    sanityCheck ([ ],[_]) = errorstar "unionPartsWithEq, called on too small input"
    sanityCheck ([_],[_]) = errorstar "unionPartsWithEq, called on too small input"
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


-- | `bkPaddedObject` breaks one level of padded objects

{-bkPaddedObject _ (TObj xt1s _) (TObj xt2s _) = snd <$> cmn-}
{-  where cmn              = meetBinds xt1s xt2s-}

bkPaddedObject l _ _   = die $ bug l $ "bkPaddedObject: can only break objects"


-- | `padFun`

-- padFun γ (TFun b1s o1 r1) (TFun b2s o2 r2) 
--   | length b1s == length b2s && sameTypes = (joinT, t1', t2', EqT)
--   | otherwise                             = 
--       error "Unimplemented: padFun - combining functions with different types"
--     where
--       sameTypes              = all (== EqT) $ od:bds
--       (tjs, t1s', t2s', bds) = unzip4 $ zipWith (compareTs γ) (b_type <$> b1s) (b_type <$> b2s)
--       (oj , o1' , o2' , od ) = compareTs γ o1 o2
--       t1'                    = TFun (updTs b1s t1s') o1' r1
--       t2'                    = TFun (updTs b2s t2s') o2' r2
--       joinT                  = TFun (updTs b1s tjs) oj fTop 
--       updTs                  = zipWith (\b t -> b { b_type = t })
-- 
-- padFun _ _ _ = error "padFun: no other cases supported"






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

-- zipType2 γ f t@(TApp (TDef _) _ _) t' =
--   zipType2 γ f (unfoldSafe γ t) t'
-- 
-- zipType2 γ f t t'@(TApp (TDef _) _ _) =
--   zipType2 γ f (unfoldSafe γ t) t'

zipType2 _ f (TApp c [] r) (TApp c' [] r')    | c == c' = 
  TApp c [] $ f r r'

zipType2 _ f (TVar v r) (TVar v' r') | v == v' = TVar v $ f r r'

zipType2 γ f (TFun xts t r) (TFun xts' t' r') = 
  TFun (safeZipWith "zipType2:TFun" (zipBind2 γ f) xts xts') (zipType2 γ f t t') $ f r r'

-- zipType2 γ f (TObj bs r) (TObj bs' r') = TObj mbs $ f r r'
--   where mbs = (\(s,(t,t')) -> B s $ zipType2 γ f t t') <$> meetBinds bs bs' 

zipType2 γ f (TArr t r) (TArr t' r') = TArr (zipType2 γ f t t') $ f r r'

zipType2 _ _ t t' = 
  errorstar $ printf "BUG[zipType2]: mis-aligned types:\n\t%s\nand\n\t%s" (ppshow t) (ppshow t')

zipTypes γ f ts t = 
  case filter (equiv γ t) ts of
    [  ] -> t
    [t'] -> zipType2 γ f t' t
    _    -> errorstar "BUG[zipType]: multiple equivalent types" 
  

zipBind2 γ f (B s t) (B s' t') | s == s' = B s $ zipType2 γ f t t' 
zipBind2 _ _ _       _                   = errorstar "BUG[zipBind2]: mis-matching binders"

