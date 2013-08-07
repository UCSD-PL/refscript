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

  -- * Type comparison/joining/subtyping
    compareTs
  , unionParts

  
  -- * Casting
  , Cast(..)
  , Casts

  , SubDirection (..)

  ) where 

-- import           Text.Printf
import           Data.Maybe                         (isNothing)
import qualified Data.List                          as L
import qualified Data.Map                           as M
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.PrettyPrint
-- import           Language.Nano.Errors
import           Language.Nano.Env
import           Language.Nano.Misc
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Subst

import qualified Language.Fixpoint.Types            as F
import           Language.Fixpoint.Misc
-- import           Language.Fixpoint.PrettyPrint
import           Text.PrettyPrint.HughesPJ 

import           Control.Applicative                hiding (empty)
import           Control.Monad.Error                ()

-- import           Debug.Trace (trace)




---------------------------------------------------------------------------------------
-- Casts ------------------------------------------------------------------------------
---------------------------------------------------------------------------------------

type Casts   = M.Map (Expression AnnSSA) (Cast Type)

data Cast t  = UCST t | DCST t | DC t

instance (PP a) => PP (Cast a) where
  pp (UCST t) = text "Upcast  : " <+> pp t
  pp (DCST t) = text "Downcast: " <+> pp t
  pp (DC   t) = text "Deadcast: " <+> pp t




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
compareTs γ t1 t2 | any isUnion [t1,t2]     = padUnion γ t1  t2

-- | Top-level Objects

compareTs γ t1@(TObj _ _) t2@(TObj _ _)     = padObject γ t1 t2

-- | Type definitions

compareTs γ t1@(TApp (TDef _) _ _) t2@(TApp (TDef _) _ _) = compareTs γ (unfoldSafe γ t1) t2

compareTs γ t1@(TApp (TDef _) _ _) t2       = compareTs γ (unfoldSafe γ t1) t2

compareTs γ t1 t2@(TApp (TDef _) _ _)       = compareTs γ t1 (unfoldSafe γ t2)

-- | Everything else in TApp besides unions and defined types
compareTs γ t1@(TApp _ _ _) t2@(TApp _ _ _) = padUnion γ t1 t2 

compareTs _ (TFun _ _ _) (TFun _ _ _) = undefined
compareTs _ (TAll _ _  ) (TAll _ _  ) = undefined
compareTs _ (TBd  _    ) (TBd  _    ) = undefined

compareTs _   _           _               = undefined



-- | Type equivalence

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
  equiv (TApp (TDef _) _ _) _                       = undefined -- TODO unfold
  equiv _                    (TApp (TDef _) _ _)    = undefined -- TODO unfold
  equiv (TApp c _ _)         (TApp c' _ _)          = c == c'
  equiv (TVar v _  )         (TVar v' _  )          = v == v'
  -- Functions need to be exactly the same - no padding can happen here
  equiv (TFun b o _)         (TFun b' o' _)          = b `equiv` b' && o `equiv` o' 
  -- Any two objects can be combined - so they should be equivalent
  equiv (TObj _ _  )         (TObj _ _   )          = True
  equiv (TBd _     )         (TBd _      )          = errorstar "equiv: no type bodies"
  equiv (TAll _ _   )         (TAll _ _ )           = undefined
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
  pp SubT = text "<:"
  pp SupT = text ":>"
  pp EqT  = text "≈"
  pp Nth  = text "≠"

joinSub :: SubDirection -> SubDirection -> SubDirection
joinSub c    c'   | c == c' = c
joinSub Nth  _              = Nth
joinSub _    Nth            = Nth
joinSub EqT  c              = c
joinSub c    EqT            = c
joinSub SubT SubT           = Nth 
joinSub SubT SupT           = Nth
joinSub SupT SubT           = Nth
joinSub SupT SupT           = Nth


joinSubs :: [SubDirection] -> SubDirection
joinSubs = foldl joinSub EqT


-- | `padUnion`

-- Produces an equivalent type for @t1@ (resp. @t2@) that has is extended
-- by the missing sorts to the common upper bound of @t1@ and @t2@. The extra
-- types that are added in the union are refined with False to keep the
-- equivalence.
-- The output is the following triplet:
--  ∙ common upper bound type (@t1@ ∪ @t2@) with Top predicates
--  ∙ adjusted type for @t1@ to be sort compatible,
--  ∙ adjusted type for @t2@ to be sort compatible)
--  ∙ a subtyping direction 

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
  (joinType, mkUnionR topR1 $ t1s, mkUnionR topR2 $ t2s, dir)
  where
    (t1s, t2s)          = unzip ts
    (joinType, ts, dir) = unionParts env t1 t2
    -- Extract top-level refinements
    topR1               = rUnion t1 
    topR2               = rUnion t2



-- The top-level refinements will be lost here.
-- Use `padUnion` to keep them
--------------------------------------------------------------------------------
unionParts ::  (Eq r, Ord r, F.Reftable r, PP r) => 
             Env (RType r) 
          -> RType r      -- LHS
          -> RType r      -- RHS
          -> (  RType r,                 -- The join of the two types
                [(RType r, RType r)],    -- 
                SubDirection)            -- Subtyping relation between LHS and RHS
--------------------------------------------------------------------------------
unionParts env t1 t2 = 
  -- No reason to add the kVars here. They will be added in the CGMonad
  ( mkUnion $ (ofType . toType) <$> (commonJoin ++ ds), ts, direction)
  where

    ts  = safeZip "unionParts" t1s t2s
    -- It is crucial to sort the types so that they are aligned
    t1s = L.sort $ commonT1s ++ (fmap F.bot <$> d2s)    
    t2s = L.sort $ commonT2s ++ (fmap F.bot <$> d1s)
    

    -- commonTs: types in both t1 and t2
    -- `common` does a "light" matching - which is determined by `equiv`. 
    -- The main difference is in objects: all objects are matched together, so 
    -- the common parts may not be the same in terms of raw type. 
    -- This is why `compareTs` is called on the common parts recursively
    -- This only needs to happen on the common (structurally) types.
    -- The distinct ones are gonna be identical (due to padding).
    -- Also `common` returns aligned types - so no need to re-align them.
    commonTs   = map (uncurry $ compareTs env) $ common (bkUnion t1) (bkUnion t2)

    commonJoin = fst4 <$> commonTs
    commonT1s  = snd4 <$> commonTs
    commonT2s  = thd4 <$> commonTs
    comSub     = joinSubs $ fth4 <$> commonTs

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



-- | Pad objects

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
-- padObject env (TObj bs1 r1) (TObj bs2 r2) = undefined
    {-b1s                 = unzip $ split <$> L.sortBy ord xt1s-}
    {-b2s                 = unzip $ split <$> L.sortBy ord xt2s-}
    {-split (B l t)       = (l,t)-}
    {-ord a b             = b_sym a `compare` b_sym b-}


padObject _ _ _ = errorstar "padObject can only happen between objects"









