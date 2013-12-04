-- | TODO: Add description

{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE NoMonomorphismRestriction   #-}
{-# LANGUAGE MultiParamTypeClasses   #-}

module Language.Nano.Typecheck.Compare (

  -- * Type comparison/joining/subtyping
    Equivalent, equiv
  , compareTs
  , alignTs
  , unionParts, unionPartsWithEq
  , bkPaddedUnion, bkPaddedObject
  , isSubType
  , eqType

  
  -- * Casting
  , Cast(..)
  , Casts, Casts_
  , zipType1
  , zipType2

  , SubDirection (..)

  ) where 

import           Text.Printf

import qualified Data.List                          as L
import qualified Data.Map                           as M
import           Data.Monoid
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.PrettyPrint
import           Language.Nano.Errors
import           Language.Nano.Env
import           Language.Nano.Misc
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Liquid.Types

import qualified Language.Fixpoint.Types            as F
import           Language.Fixpoint.Misc
-- import           Language.Fixpoint.PrettyPrint
import           Text.PrettyPrint.HughesPJ 

import           Control.Applicative                hiding (empty)
import           Control.Monad.Error                ()

-- import           Debug.Trace (trace)



-- | Type equivalence

-- This is a slightly more relaxed version of equality. 
class Equivalent e a where 
  equiv :: e -> a -> a -> Bool

instance Equivalent e a => Equivalent e [a] where
  equiv γ a b = and $ zipWith (equiv γ) a b 

instance (PP r, F.Reftable r) => Equivalent (Env (RType r)) (RType r) where 

  equiv _ t t'  | toType t == toType t'               = True
  
  equiv _ t t'  | any isUnion [t,t']                  = errorstar (printf "equiv: no unions: %s\n\t\t%s" (ppshow t) (ppshow t'))
  -- No unions beyond this point!
  
  equiv γ (TApp d@(TDef _) ts _) (TApp d'@(TDef _) ts' _) | d == d' = equiv γ ts ts'

  equiv γ t@(TApp (TDef _) _ _) t' = equiv γ (unfoldSafe γ t) t'
  equiv γ t t'@(TApp (TDef _) _ _) = equiv γ t (unfoldSafe γ t')
  
  equiv _ (TApp c _ _)         (TApp c' _ _)          = c == c'

  equiv _ (TVar v _  )         (TVar v' _  )          = v == v'

  -- Functions need to be exactly the same - no padding can happen here
  equiv γ (TFun b o _)         (TFun b' o' _)         = 
    equiv γ (b_type <$> b) (b_type <$> b') && equiv γ o o' 

  -- Any two objects can be combined - so they should be equivalent
  equiv _ (TObj _ _  )         (TObj _ _   )          = True
  
  equiv _ (TBd _     )         (TBd _      )          = errorstar "equiv: no type bodies"
  
  equiv _ (TAll _ _   )        (TAll _ _ )            = error "equiv-tall"
    -- t `equiv` apply (fromList [(v',tVar v)]) t'
  equiv _ _                    _                      = False

instance (PP r, F.Reftable r) => Equivalent (Env (RType r)) (Bind r) where 
  equiv γ (B s t) (B s' t') = s == s' && equiv γ t t' 

instance Equivalent e (Id a) where 
  equiv _ i i' = F.symbol i == F.symbol i'


---------------------------------------------------------------------------------------
-- Casts ------------------------------------------------------------------------------
---------------------------------------------------------------------------------------

type Casts    = M.Map (Expression (AnnSSA F.Reft)) (Cast RefType)
type Casts_ r = M.Map (Expression (AnnSSA r)) (Cast (RType r))

data Cast t  = UCST t | DCST t | DC t

instance (PP a) => PP (Cast a) where
  pp (UCST t) = text "Upcast  : " <+> pp t
  pp (DCST t) = text "Downcast: " <+> pp t
  pp (DC   t) = text "Deadcast: " <+> pp t


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
isSubType :: (F.Reftable r, Ord r, PP r) => Env (RType r) -> RType r -> RType r -> Bool
isSubType γ t1 t2 = (fth4 $ compareTs γ t1 t2) `elem` [EqT, SubT]


eqType :: (F.Reftable r, Ord r, PP r) => Env (RType r) -> RType r -> RType r -> Bool
eqType γ t1 t2 = (fth4 $ compareTs γ t1 t2) == EqT



-- | `alignTs`
alignTs γ t1 t2     = (t1', t2')
  where 
    (_,t1', t2', _) = compareTs γ t1 t2


-- | `compareTs` returns:
-- ∙ A padded version of the upper bound of @t1@ and @t2@
-- ∙ An equivalent version of @t1@ that has the same sort as the second output
-- ∙ An equivalent version of @t2@ that has the same sort as the first output
-- ∙ A subtyping direction between @t1@ and @t2@
--
-- Padding the input types gives them the same sort, i.e. makes them compatible. 
---------------------------------------------------------------------------------------
compareTs :: (F.Reftable r, Ord r, PP r) => Env (RType r) -> RType r -> RType r -> 
                                  (RType r, RType r, RType r, SubDirection)
---------------------------------------------------------------------------------------
-- Deal with some standard cases of subtyping, e.g.: Top, Null, Undefined ...
compareTs _ t1 t2 | toType t1 == toType t2 = (ofType $ toType t1, t1, t2, EqT)

compareTs γ t1 t2 | isUndefined t1         = setFth4 (compareTs' γ t1 t2) SubT

-- XXX: Null is not considered a subtype of all types. If null is to be 
-- expected this should be explicitly specified by using " + null"
-- compareTs γ t1 t2 | and [isNull t1, not $ isUndefined t2] = setFth4 (compareTs' γ t1 t2) SubT

compareTs γ t1 t2 | otherwise              = compareTs' γ t1 t2
  {-where msg = printf "About to compareTs %s and %s" (ppshow $ toType t1) (ppshow $ toType t2)-}


-- | Top-level Unions

compareTs' _ t1 _  | isTop t1               = errorstar "unimplemented: compareTs - top"
compareTs' _ t1 t2 | isTop t2               = (t1', t1, t2', SubT)
  where
    t1' = setRTypeR t1 F.top -- this will be kVared
    -- @t2@ is a Top, so just to make the types compatible we will 
    -- use the base type of @t1@ and stregthen with @t2@'s refinement.
    t2' = setRTypeR t1 $ rTypeR t2
  

-- Eliminate top-level unions
compareTs' γ t1 t2 | any isUnion [t1,t2]     = padUnion γ t1  t2

-- | Top-level Objects

compareTs' γ t1@(TObj _ _) t2@(TObj _ _)     = 
  {-tracePP (printf "Padding: %s and %s" (ppshow t1) (ppshow t2)) $ -}
  padObject γ ( {- trace ("padding obj " ++ ppshow t1 ++ "\n - " ++ ppshow t2) -} t1) t2

-- | Arrays
compareTs' γ a@(TArr _ _) a'@(TArr _ _  ) = padArray γ a a'
compareTs' _ t1@(TObj _ _) t2@(TArr _ _ ) = error (printf "Unimplemented compareTs-Obj-Arr:\n\t%s\n\t%s" (ppshow t1) (ppshow t2))
compareTs' _ t1@(TArr _ _) t2@(TObj _ _ ) = error (printf "Unimplemented compareTs-Arr-Obj:\n\t%s\n\t%s" (ppshow t1) (ppshow t2))

-- | Type definitions

-- TODO: only handles this case for now - cyclic type defs will loop infinitely
compareTs' γ (TApp d1@(TDef _) t1s r1) (TApp d2@(TDef _) t2s r2) | d1 == d2 = 
  (mk tjs F.top, mk t1s' r1, mk t2s' r2, mconcatP bds)
  where
    (tjs, t1s', t2s', bds)  = unzip4 $ zipWith (compareTs γ) t1s t2s
    mk xs r                 = TApp d1 xs r 

compareTs' γ t1@(TApp (TDef _) _ _) t2       = compareTs γ (unfoldSafe γ t1) t2
compareTs' γ t1 t2@(TApp (TDef _) _ _)       = compareTs γ t1 (unfoldSafe γ t2)

-- | Everything else in TApp besides unions and defined types
compareTs' _ t1@(TApp _ _ _) t2@(TApp _ _ _) = padSimple t1 t2 

-- | Type Vars
compareTs' _ t1@(TVar _ _)   t2@(TVar _ _)   = padVar t1 t2

-- | Function Types
compareTs' γ t1@(TFun _ _ _) t2@(TFun _ _ _) = padFun γ t1 t2
compareTs' _ (TFun _ _ _)    _               = error "Unimplemented compareTs-1"
compareTs' _ _               (TFun _ _ _)    = error "Unimplemented compareTs-2"

-- | TAll
compareTs' _ (TAll _ _  ) _                  = error "Unimplemented: compareTs-3"
compareTs' _ _            (TAll _ _  )       = error "Unimplemented: compareTs-4"

-- | TBd
compareTs' _ _            (TBd  _    )       = error "Unimplemented: compareTs-5"
compareTs' _ (TBd  _    ) _                  = error "Unimplemented: compareTs-6"

-- | Rest of cases

-- Let these cases be dealt by padding unions
compareTs' _ t1           t2                 = padSimple t1 t2 



-- | `padSimple`

-- Not calling padUnion because the inputs are too small
padSimple t1 t2
  | t1 == t2  = (t1, t1, t2, EqT)
  | otherwise = (joinType, t1', t2', Nth)
    where joinType = (ofType . toType) $ mkUnion [t1,t2]
          t1'      = mkUnion [t1, fmap F.bot t2]  -- Toplevel refs?
          t2'      = mkUnion [fmap F.bot t1, t2]


-- | `padVar`

padVar t1@(TVar v1 _ ) t2@(TVar v2 _) | v1 == v2 = ((ofType . toType) t1, t1, t2, EqT)
padVar t1 t2 = errorstar $ printf "padVar: cannot compare %s and %s" (ppshow t1) (ppshow t2)



-- | `padUnion`

-- Produces an equivalent type for @t1@ (resp. @t2@) that is extended with 
-- the missing sorts to the common upper bound of @t1@ and @t2@. The extra
-- types that are added in the union are refined with False to keep the
-- equivalence with the input types.
--
-- The output is the following tuple:
--  ∙ common upper bound type (@t1@ ∪ @t2@) with topped predicates
--  ∙ adjusted type for @t1@ to be sort compatible,
--  ∙ adjusted type for @t2@ to be sort compatible
--  ∙ a subtyping direction

-- Example:
--  {Int | p} ㄩ {Bool | q} => ({Int | ⊥    } ∪ {Bool | ⊥    },
--                              {Int | p    } ∪ {Bool | ⊥    },
--                              {Int | ⊥    } ∪ {Bool | q    },
--                              unrelated )
--------------------------------------------------------------------------------
padUnion ::  (Eq r, Ord r, F.Reftable r, PP r) => 
             Env (RType r)    -- Type defs
          -> RType r          -- LHS
          -> RType r          -- RHS
          -> (  RType r,        -- The join of the two types
                RType r,        -- The equivalent to @t1@
                RType r,        -- The equivalent to @t2@
                SubDirection)   -- Subtyping relation between LHS and RHS
--------------------------------------------------------------------------------
padUnion env t1 t2 = 
  (joinType, mkUnionR topR1 $ t1s, 
             mkUnionR topR2 $ t2s, direction)
  where
    -- Extract top-level refinements
    topR1       = rUnion t1
    topR2       = rUnion t2

    -- No reason to add the kVars here. They will be added in the CGMonad
    joinType   = mkUnion $ (ofType . toType) <$> ((fst4 <$> commonTs) ++ d1s ++ d2s)
    (t1s, t2s) = unzip $ safeZip "unionParts" t1s' t2s'

    -- It is crucial to sort the types so that they are aligned
    t1s'       = L.sort $ commonT1s ++ d1s ++ (fmap F.bot <$> d2s)
    t2s'       = L.sort $ commonT2s ++ d2s ++ (fmap F.bot <$> d1s)

    commonT1s  = snd4 <$> commonTs
    commonT2s  = thd4 <$> commonTs

    commonTs = 
      {-tracePP "padUnion: compaTs on common parts" $ -}
      map (uncurry $ compareTs env) $ cmnPs

    -- To figure out the direction of the subtyping, we must take into account:
    direction  = distSub &+& comSub
    -- ∙ The distinct types (the one that has more is a supertype)
    distSub   = case (d1s, d2s) of
                  ([], []) -> EqT
                  ([], _ ) -> SubT  -- <:
                  (_ , []) -> SupT  -- >:
                  (_ , _ ) -> Nth -- no relation
    -- ∙ The common types (recursively call `compareTs` to compare the types
    --   of the parts and join the subtyping relations)
    comSub     = mconcatS $ fth4 <$> commonTs
    
    (cmnPs, d1s, d2s) =  {- tracePP "padUnion: unionParts" $ -} unionParts env t1 t2


--------------------------------------------------------------------------------
bkPaddedUnion :: (Eq r, Ord r, F.Reftable r, PP r) => 
  String -> Env (RType r) -> RType r -> RType r -> [(RType r, RType r)]
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
unionParts ::  (Eq r, Ord r, F.Reftable r, PP r) => 
            Env (RType r) -> RType r -> RType r 
          -> ([(RType r, RType r)], [RType r], [RType r])
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
unionPartsWithEq ::  (Eq r, Ord r, F.Reftable r, PP r) => 
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

    sanityCheck ([ ],[ ]) = errorstar "unionParts, called on too small input"
    sanityCheck ([_],[ ]) = errorstar "unionParts, called on too small input"
    sanityCheck ([ ],[_]) = errorstar "unionParts, called on too small input"
    sanityCheck ([_],[_]) = errorstar "unionParts, called on too small input"
    sanityCheck p         = p



-- | Pad objects

--  Example: 
--  {{ } | p} ㄩ {{a:Int} | q} => ( { a: { {Int | _     } + {Top | _    } | _ } },
--                                  { a: { {Int | False } + {Top | _    } | p } },
--                                  { a: { {Int | _     } + {Top | False} | q } },
--                                  :> )                              
--------------------------------------------------------------------------------
padObject :: (Eq r, Ord r, F.Reftable r, PP r) => 
             Env (RType r) -> RType r -> RType r -> 
               (RType r, RType r, RType r, SubDirection)
--------------------------------------------------------------------------------
padObject γ (TObj bs1 r1) (TObj bs2 r2) = 
  (TObj jbs' F.top, TObj b1s' r1, TObj b2s' r2, direction)
  where
    -- Total direction
    direction = cmnDir &*& distDir d1s d2s
    -- Direction from all the common keys
    cmnDir    = mconcatP $ (\(_,(t,t')) -> fth4 $ compareTs γ t t') <$> cmn
    -- Direction from distinct keys
    distDir xs ys | null (xs ++ ys) = EqT
                  | null xs         = SupT
                  | null ys         = SubT
                  | otherwise       = Nth

    jbs' = (\(s,(t,t')) -> B s $ fst4 $ compareTs γ t t') <$> cmn ++ d1s ++ d2s
    -- Bindings for 1st object
    b1s' = (\(s,(t,t')) -> B s $ snd4 $ compareTs γ t t') <$> cmn ++ d1s ++ d2s 
    -- Bindings for 2nd object
    b2s' = (\(s,(t,t')) -> B s $ thd4 $ compareTs γ t t') <$> cmn ++ d1s ++ d2s

    (d1s, d2s) = distinct bs1 bs2

    distinct :: (F.Reftable r) => [Bind r] -> [Bind r] -> ([(F.Symbol, (RType r, RType r))],[(F.Symbol, (RType r, RType r))])
    distinct b1 [] = ((\(B s t) -> (s,(t,tTop))) <$> b1, [])
    distinct [] b2 = ([], (\(B s t) -> (s,(tTop,t))) <$> b2)
    distinct b1 b2 = ([(s,(t,tTop)) | B s t <- b1, not $ M.member s (mm b2)],
                      [(s,(tTop,t)) | B s t <- b2, not $ M.member s (mm b1)])
                     
    cmn = M.toList $ M.intersectionWith (,) (mm bs1) (mm bs2) -- bindings in both objects
    mm  = M.fromList . map (\(B s t) -> (s,t))


padObject _ _ _ = error "padObject: Cannot pad non-objects"


-- | Break one level of padded objects

--------------------------------------------------------------------------------
bkPaddedObject :: (F.Reftable r, PP r) => RType r -> RType r -> [(RType r, RType r)]
--------------------------------------------------------------------------------
bkPaddedObject t1@(TObj xt1s _) t2@(TObj xt2s _) =
  safeZipWith (printf "bkPaddedObject: %s vs %s" (ppshow t1) (ppshow t2)) checkB xt1s xt2s
  where 
    checkB b b' | b_sym b == b_sym b' = (b_type b, b_type b')
    checkB _ _                        = 
      errorstar "unimplemented: bkPaddedObject: cannot split these objects"
bkPaddedObject _ _                    = 
  errorstar "bkPaddedObject: can only break objects"


-- | `padFun`

padFun γ (TFun b1s o1 r1) (TFun b2s o2 r2) 
  | length b1s == length b2s && sameTypes = (joinT, t1', t2', EqT)
  | otherwise                             = 
      error "Unimplemented: padFun - combining functions with different types"
    where
      sameTypes              = all (== EqT) $ od:bds
      (tjs, t1s', t2s', bds) = unzip4 $ zipWith (compareTs γ) (b_type <$> b1s) (b_type <$> b2s)
      (oj , o1' , o2' , od ) = compareTs γ o1 o2
      t1'                    = TFun (updTs b1s t1s') o1' r1
      t2'                    = TFun (updTs b2s t2s') o2' r2
      joinT                  = TFun (updTs b1s tjs) oj F.top 
      updTs                  = zipWith (\b t -> b { b_type = t })

padFun _ _ _ = error "padFun: no other cases supported"



-- | `padArray`
padArray γ (TArr t1 r1) (TArr t2 r2) = 
    (TArr tj F.top, TArr t1' r1, TArr t2' r2, arrDir ad)
  where
    (tj, t1', t2', ad) = compareTs γ t1 t2
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
zipType2 :: (PP r, F.Reftable r) => Env (RType r) -> (r -> r -> r) ->  RType r -> RType r -> RType r
--------------------------------------------------------------------------------
zipType2 γ f (TApp TUn ts r) (TApp TUn ts' r')  = 
  TApp TUn (zipTypes γ f ts <$> ts') $ f r r'

zipType2 γ f (TApp TUn ts _) t =  
  zipTypes γ f ts t

zipType2 γ f t (TApp TUn ts' r') =  
  TApp TUn (zipTypes γ f [t] <$> ts') r'        -- The top-level refinement for t' should remain

zipType2 γ f (TApp d@(TDef _) ts r) (TApp d'@(TDef _) ts' r') | d == d' =
  TApp d' (zipWith (zipType2 γ f) ts ts') $ f r r'

zipType2 γ f t@(TApp (TDef _) _ _) t' =
  zipType2 γ f (unfoldSafe γ t) t'

zipType2 γ f t t'@(TApp (TDef _) _ _) =
  zipType2 γ f (unfoldSafe γ t) t'

zipType2 _ f (TApp c [] r) (TApp c' [] r')    | c == c' = 
  TApp c [] $ f r r'

zipType2 _ f (TVar v r) (TVar v' r') | v == v' = TVar v $ f r r'

zipType2 γ f (TFun xts t r) (TFun xts' t' r') = 
  TFun (safeZipWith "zipType2:TFun" (zipBind2 γ f) xts xts') (zipType2 γ f t t') $ f r r'

zipType2 γ f (TObj bs r) (TObj bs' r') = TObj mbs $ f r r'
  where
    mbs = safeZipWith "zipType2:TObj" (zipBind2 γ f) (L.sortBy compB bs) (L.sortBy compB bs')
    compB (B s _) (B s' _) = compare s s'

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

