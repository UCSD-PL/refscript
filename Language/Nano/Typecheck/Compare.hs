-- | TODO: Add description

{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE NoMonomorphismRestriction   #-}

module Language.Nano.Typecheck.Compare (

  ) where 

import           Text.Printf
import           Data.Hashable
import           Data.Maybe             (fromMaybe, isNothing)
import           Data.Monoid            hiding ((<>))            
import qualified Data.List               as L
import qualified Data.HashMap.Strict     as M
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Parser        (SourceSpan (..))
import           Language.Nano.Types
import           Language.Nano.Errors
import           Language.Nano.Env
import           Language.Nano.Misc
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Subst

-- import           Language.Fixpoint.Names (propConName)
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Misc
import           Language.Fixpoint.PrettyPrint
import           Text.PrettyPrint.HughesPJ 

import           Control.Applicative hiding (empty)
import           Control.Monad.Error ()

-- import           Debug.Trace (trace)



-- `compareTs` pads the input types so that he output consists of types of the
-- same sort, i.e. compatible. This function also includes the top-level
-- constraint in the output (which still keeps the "same-sort" invariant as the
-- compatible "joined" version of each of the input types is used (the one
-- returned from padUnion).
--
-- XXX: Matching (pairing up the parts of the top-level constraint into a list 
-- of constraints on compatible parts) is still done here, instead of being 
-- deferrred to splitC, because `fixBase` needs to happen now for constraints 
-- that originate from casts. If they were included to the rest of the constraints 
-- it would be harder to distinguish which constraints need `fixBase` later.
---------------------------------------------------------------------------------------
compareTs :: (F.Reftable r, Ord r, PP r) => Env (RType r) -> RType r -> RType r -> 
                                  (RType r, RType r, RType r, SubDirection)
---------------------------------------------------------------------------------------

-- | Top-level Unions

-- Eliminate top-level unions
compareTs env t1 t2 | any isUnion [t1,t2]     = padUnion env t1  t2

-- | Top-level Objects

-- TODO: toplevel refinement

compareTs env t1@(TObj _ _) t2@(TObj _ _)     = padObject env t1 t2

compareTs env t1@(TApp (TDef _) _ _) t2       = compareTs env (unfoldSafe env t1) t2

compareTs env t1 t2@(TApp (TDef _) _ _)       = compareTs env t1 (unfoldSafe env t2)

-- | Everything else in TApp besides unions and defined types
compareTs env t1@(TApp _ _ _) t2@(TApp _ _ _) = padUnion env t1 t2 

compareTs env t1@(TFun _ _ _) t2@(TFun _ _ _) = undefined
compareTs env t1@(TAll _ _  ) t2@(TAll _ _  ) = undefined
compareTs env t1@(TBd  _    ) t2@(TBd  _    ) = undefined

compareTs _   _               _               = undefined


-- | Pair-up the parts of types @t1@ and @t2@.

-- There is a check that makes sure that thet inpu types are indeed compatible.
-- XXX : Does not add top-level constraint
-- TODO: Object types (?)

{-----------------------------------------------------------------------------------------}
{-alignUnion :: RefType -> RefType -> [(RefType, RefType)]-}
{-----------------------------------------------------------------------------------------}
{-alignUnion t1 t2 | and $ zipWith req t1s t2s = -}
{-  zipWith sanity t1s t2s-}
{-  where-}
{-    t1s = L.sortBy ord $ bkUnion t1-}
{-    t2s = L.sortBy ord $ bkUnion t2-}
{-    -- sorting t1s and t2s by raw-type should alignUnion them !-}
{-    -- by using sanity check anyway to make sure-}
{-    ord a b = compare (toType a) (toType b) -}
{-    req a b = (toType a) == (toType b) -}
{-    sanity a b | toType a == toType b = (a,b)-}
{-    sanity _ _ | otherwise            = errorstar "alignUnion"-}

{-alignUnion t1 t2 | otherwise =-}
{-  errorstar $ printf "alignUnion not alignUnioned: %s - %s" (ppshow t1) (ppshow t2) -}


-- | Type equivalence
-- Make typeclass

-- This is a slightly more relaxed version of equality. 
class Equivalent a where 
  equiv :: a -> a -> Bool

instance Equivalent a => Equivalent [a] where
  equiv a b = and $ zipWith equiv a b 

instance Equivalent (RType r) where 
  equiv t t'                                        | any isUnion [t,t']
                                                    = errorstar "equiv: no unions"
  -- No unions beyond this point!
  equiv (TApp (TDef d) ts _) (TApp (TDef d') ts' _) = d == d' && ts `equiv` ts'
  equiv (TApp (TDef d) ts _) _                      = undefined -- TODO unfold
  equiv _                    (TApp (TDef d) ts _)   = undefined -- TODO unfold
  equiv (TApp c _ _)         (TApp c' _ _)          = c == c'
  equiv (TVar v _  )         (TVar v' _  )          = v == v'
  -- Functions need to be exactly the same - no padding can happen here
  equiv (TFun b o _)         (TFun b' o' _)          = b `equiv` b' && o `equiv` o 
  -- Any two objects can be combined - so they should be equivalent
  equiv (TObj _ _  )         (TObj _ _   )          = True
  equiv (TBd _     )         (TBd _      )          = errorstar "equiv: no type bodies"
  equiv (TAll v t  )         (TAll v' t' )          = undefined
    -- t `equiv` apply (fromList [(v',tVar v)]) t'
  equiv _                    _                      = False

instance Equivalent (Bind r) where 
  equiv (B s t) (B s' t') = s == s' && t `equiv` t' 


---------------------------------------------------------------------------------
-- Type Padding -----------------------------------------------------------------
---------------------------------------------------------------------------------

-- | Subtyping directions
data SubDirection = SubT | SupT | EqT | Nth deriving (Eq, Show)

instance PP SubDirection where 
  pp SubT  = text "<:"
  pp SupT  = text ":>"
  pp EqT  = text "≈"
  pp Nth = text "≠"

joinSub :: SubDirection -> SubDirection -> SubDirection
joinSub c   c'  | c == c' = c
joinSub Nth _             = Nth
joinSub _   Nth           = Nth
joinSub EqT  c             = c
joinSub c   EqT            = c

joinSubs :: [SubDirection] -> SubDirection
joinSubs = foldl joinSub EqT


-- | `padUnion`

-- Produces an equivalent type for @t1@ (resp. @t2@) that has is extended
-- by the missing sorts to the common upper bound of @t1@ and @t2@. The extra
-- types that are added in the union are refined with False to keep the
-- equivalence.
-- The output is the following triplet:
--  o common upper bound type (@t1@ ∪ @t2@) with Top predicates
--  o adjusted type for @t1@ to be sort compatible,
--  o adjusted type for @t2@ to be sort compatible)


-- Example:
--  {Int | p} ㄩ {Bool | q} => ({Int | ⊥    } ∪ {Bool | ⊥    },
--                              {Int | p    } ∪ {Bool | ⊥    },
--                              {Int | ⊥    } ∪ {Bool | q    },
--                              unrelated )
--------------------------------------------------------------------------------
padUnion ::  (Eq r, Ord r, F.Reftable r, PP r) => 
             Env (RType r) 
          -> RType r      -- LHS
          -> RType r      -- RHS
          -> (  RType r,            -- The join of the two types
                RType r,            -- The equivalent to @t1@
                RType r,            -- The equivalent to @t2@
                SubDirection)            -- Subtyping relation between LHS and RHS
--------------------------------------------------------------------------------
padUnion env t1 t2 = 
  (mkUnion        $ (ofType . toType) <$> (commonJoin ++ ds),
  -- No reason to add the kVars here. They will be added in the CGMonad
   mkUnionR topR1 $ commonT1s ++ (fmap F.bot <$> d2s), 
   mkUnionR topR2 $ commonT2s ++ (fmap F.bot <$> d1s), 
   direction)
  where
    -- Extract top-level refinements
    topR1           = rUnion t1 
    topR2           = rUnion t2

    -- Break the union inputs into their parts
    t1s             = bkUnion t1
    t2s             = bkUnion t2

    -- commonTs: types in both t1 and t2
    -- `common` does a "light" matching - which is determined by `equiv`. 
    -- The main difference is in objects: all objects are matched together, so 
    -- the common parts may not be the same in terms of raw type. 
    -- This is why `compareTs` is called on the common parts recursively
    -- This only needs to happen on the common (structurally) types.
    -- The distinct ones are gonna be identical (due to padding).
    -- Also `common` returns aligned types - so no need to re-align them.
    commonTs        = map (uncurry $ compareTs env) $ common t1s t2s

    commonJoin      = fst4 <$> commonTs
    commonT1s       = snd4 <$> commonTs
    commonT2s       = thd4 <$> commonTs
    comSub          = joinSubs $ fth4 <$> commonTs

    -- d1s: types on @t1@ but not @t2@
    -- d2s: types on @t2@ but not @t1@
    -- ds = d1s ++ d2s 
    (ds, d1s, d2s)  = distinct t1s t2s

    -- Compare the types based on the Equivalence relation and pair them up into
    -- Type structures that are common in both sides, and ...
    common xs ys | any null [xs,ys] = []
    common xs ys | otherwise        = [(x,y) | x <- xs, y <- ys, x `equiv` y ]

    -- ... type structures that are distinct in the two sides
    distinct xs [] = (xs, [], xs)
    distinct [] ys = (ys, ys, [])
    distinct xs ys =  let dx = [x | x <- xs, isNothing $ L.find (x `equiv`) ys ]
                          dy = [y | y <- ys, isNothing $ L.find (y `equiv`) xs ] in
                      (L.nub $ dx ++ dy, dx, dy)

    -- To figure out the direction of the subtyping, we must take into account:
    -- ∙ The distinct types (the one that has more is a supertype)
    -- ∙ The common types (recursively call `compareTs` to compare the types
    --   of the parts and join the subtyping relations)
    direction = distSub `joinSub` comSub
    distSub   = case (d1s, d2s) of
                  ([], []) -> EqT
                  ([], _ ) -> SubT  -- <:
                  (_ , []) -> SupT  -- >:
                  (_ , _ ) -> Nth -- no relation


--  Example: 
--  {{ } | p} ㄩ {{a:Int} | q} => ( {{ }        | _},
--                                  {{ a: Top } | p},
--                                  {{ a: Int } | q},
--                                  :> )
                              
--------------------------------------------------------------------------------
padObject ::  (Eq r, Ord r, F.Reftable r) => 
             Env (RType r) 
          -> RType r      -- LHS
          -> RType r      -- RHS
          -> (  RType r,            -- The join of the two types
                RType r,            -- The equivalent to @t1@
                RType r,            -- The equivalent to @t2@
                SubDirection)       -- Subtyping relation between LHS and RHS
--------------------------------------------------------------------------------
padObject env (TObj bs1 r1) (TObj bs2 r2) = undefined
    {-b1s                 = unzip $ split <$> L.sortBy ord xt1s-}
    {-b2s                 = unzip $ split <$> L.sortBy ord xt2s-}
    {-split (B l t)       = (l,t)-}
    {-ord a b             = b_sym a `compare` b_sym b-}


padObject _ _ _ = errorstar "padObject can only happen between objects"









