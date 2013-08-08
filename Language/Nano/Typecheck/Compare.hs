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
  , isSubType

  
  -- * Casting
  , Cast(..)
  , Casts

  , SubDirection (..)

  ) where 

import           Text.Printf
import           Data.Maybe                         (isNothing)
import qualified Data.List                          as L
import qualified Data.Map                           as M
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.PrettyPrint
import           Language.Nano.Errors
import           Language.Nano.Env
import           Language.Nano.Misc
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Subst

import qualified Language.Fixpoint.Types            as F
import           Language.Fixpoint.Misc
import           Language.Fixpoint.PrettyPrint
import           Text.PrettyPrint.HughesPJ 

import           Control.Applicative                hiding (empty)
import           Control.Monad.Error                ()

import           Debug.Trace (trace)



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
  equiv (TFun b o _)         (TFun b' o' _)         = b `equiv` b' && o `equiv` o' 
  -- Any two objects can be combined - so they should be equivalent
  equiv (TObj _ _  )         (TObj _ _   )          = True
  equiv (TBd _     )         (TBd _      )          = errorstar "equiv: no type bodies"
  equiv (TAll _ _   )        (TAll _ _ )            = undefined
    -- t `equiv` apply (fromList [(v',tVar v)]) t'
  equiv _                    _                      = False

instance Equivalent (Bind r) where 
  equiv (B s t) (B s' t') = s == s' && t `equiv` t' 




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
compareTs γ t1 t2 | all isTop [t1,t2] = 
  let (j,t1',t2',_) = compareTs' γ t1 t2 in (j, t1', t2', EqT)

compareTs γ t1 t2 | isTop t1 = 
  let (j,t1',t2',_) = compareTs' γ t1 t2 in (j, t1', t2', SupT)

compareTs γ t1 t2 | isTop t2 = 
  let (j,t1',t2',_) = compareTs' γ t1 t2 in (j, t1', t2', SubT)

compareTs γ t1 t2   = compareTs' γ t1 t2


-- | Top-level Unions

-- Eliminate top-level unions
compareTs' γ t1 t2 | any isUnion [t1,t2]     = padUnion γ t1  t2

-- | Top-level Objects

compareTs' γ t1@(TObj _ _) t2@(TObj _ _)     = tracePP (printf "Padding: %s and %s" (ppshow t1) (ppshow t2)) $ padObject γ t1 t2

-- | Type definitions

compareTs' γ t1@(TApp (TDef _) _ _) t2@(TApp (TDef _) _ _) = compareTs γ (unfoldSafe γ t1) t2

compareTs' γ t1@(TApp (TDef _) _ _) t2       = error "Unimplemented: compareTs-3"
  -- compareTs γ (unfoldSafe γ t1) t2

compareTs' γ t1 t2@(TApp (TDef _) _ _)       = error "Unimplemented: compareTs-4"
  -- compareTs γ t1 (unfoldSafe γ t2)

-- | Everything else in TApp besides unions and defined types
compareTs' γ t1@(TApp _ _ _) t2@(TApp _ _ _) = padSimpleApp t1 t2 

compareTs' _ t1@(TVar v1 _) t2@(TVar v2 _)   = padVar t1 t2

compareTs' γ t1@(TFun _ _ _) t2@(TFun _ _ _) = padFun γ t1 t2

compareTs' _ (TAll _ _  ) (TAll _ _  )       = error "Unimplemented: compareTs-7"
compareTs' _ (TBd  _    ) (TBd  _    )       = error "Unimplemented: compareTs-8"

compareTs' _ t1           t2                 = 
  error $ printf "Unimplemented: compareTs-9 for types %s - %s " (ppshow t1) (ppshow t2)



---------------------------------------------------------------------------------
-- SubType API ------------------------------------------------------------------
---------------------------------------------------------------------------------
isSubType :: (F.Reftable r, Ord r, PP r) => Env (RType r) -> RType r -> RType r -> Bool
isSubType γ t1 t2 = (fth4 $ compareTs γ t1 t2) `elem` [EqT, SubT]
{-isSubType γ t1 t2 = (fth4 $ compareTs γ -}
{-                              (tracePP (printf "Comparing: %s - %s" (ppshow t1) (ppshow t2)) t1) -}
{-                              t2) -}
{-                    `elem` [EqT, SubT]-}



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

transposeSub :: SubDirection -> SubDirection
transposeSub SubT = SupT
transposeSub SupT = SubT
transposeSub EqT  = EqT
transposeSub Nth  = Nth


-- | `padSimpleApp`

padSimpleApp t1@(TApp c1 _ _) t2@(TApp c2 _ _) 
  | t1 == t2  = (t1, t1, t2, EqT)
  | otherwise = (joinType, t1', t2', Nth)
    where joinType = (ofType . toType) $ mkUnion [t1,t2]
          t1'      = mkUnion [t1, fmap F.bot t2]
          t2'      = mkUnion [fmap F.bot t1, t2]
padSimpleApp _ _   = error "BUG: padSimpleApp - no other case should be here"


-- | `padVar`

padVar t1@(TVar v1 _ ) t2@(TVar v2 _) | v1 == v2 = ((ofType . toType) t1, t1, t2, EqT)
padVar t1 t2 = errorstar $ printf "padVar: cannot compare %s and %s" (ppshow t1) (ppshow t2)



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
unionParts env t1 t2 = (commonJoin, ts, direction)
  where
    -- No reason to add the kVars here. They will be added in the CGMonad
    commonJoin = mkUnion $ (ofType . toType) <$> ((fst4 <$> commonTs) ++ ds)
    ts         = safeZip "unionParts" t1s' t2s'
    direction  = distSub `joinSub` comSub

    -- It is crucial to sort the types so that they are aligned
    t1s'       = L.sort $ commonT1s ++ d1s ++ (fmap F.bot <$> d2s)    
    t2s'       = L.sort $ commonT2s ++ d2s ++ (fmap F.bot <$> d1s)

    commonT1s  = snd4 <$> commonTs
    commonT2s  = thd4 <$> commonTs

    -- commonTs: types in both t1 and t2
    -- `common` does a "light" matching - which is determined by `equiv`. 
    -- Right now the only difference is in objects: 
    -- all objects are matched together, so the common parts may not be 
    -- the same in terms of raw type. 
    -- This is why `padObject` is called on the common parts. Non-object types
    -- fall through
    -- Also `common` returns aligned types - so no need to re-align them.
    (t1s, t2s) = sanityCheck $ mapPair bkUnion (t1, t2)
    -- Make sure that the recursion will happen on smalled inputs
    -- TODO: make sure the input is getting smaller
    commonTs = map (uncurry $ compareTs env) $ common t1s t2s

    -- The joined subtyping result
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
    distSub   = case (d1s, d2s) of
                  ([], []) -> EqT
                  ([], _ ) -> SubT  -- <:
                  (_ , []) -> SupT  -- >:
                  (_ , _ ) -> Nth -- no relation

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
    direction = cmnDir `joinSub` (distDir d1s d2s)
    -- Direction from all the common keys  
    cmnDir    = joinSubs $ (\(s,(t,t')) -> fth4 $ compareTs γ t t') <$> cmn
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
    mm = M.fromList . map (\(B s t) -> (s,t))


padObject _ _ _ = error "padObject: Cannot pad non-objects"


-- | `padFun`

-- XXX: Not sure if this is the right way to do it 
-- Instead one could create a top-level union with the two function types
padFun γ (TFun b1s o1 r1) (TFun b2s o2 r2) 
  | length b1s == length b2s = (joinT, t1', t2', direction)
    where
      -- XXX: using the first function's bindings - This needs to change !!!
      joinT                  = TFun (updTs b1s tjs) oj F.top 
      t1'                    = TFun (updTs b1s t1s') o1' r1
      t2'                    = TFun (updTs b2s t2s') o2' r2
      (tjs, t1s', t2s', bds) = unzip4 $ zipWith (compareTs γ) (b_type <$> b1s) (b_type <$> b2s)
      (oj , o1' , o2' , od ) = compareTs γ o1 o2
      direction              = transposeSub (joinSubs bds) `joinSub` od
      updTs                  = zipWith (\b t -> b { b_type = t })

padFun _ _ _ = error "padFun: no other cases supported"



-- | Helper
instance (PP a, PP b, PP c) => PP (a,b,c) where
  pp (a,b,c) = pp a <+>  pp b <+> pp c

instance (PP a, PP b, PP c, PP d) => PP (a,b,c,d) where
  pp (a,b,c,d) = pp a <+>  pp b <+> pp c <+> pp d
