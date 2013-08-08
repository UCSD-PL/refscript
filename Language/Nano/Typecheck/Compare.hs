-- | TODO: Add description

{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE NoMonomorphismRestriction   #-}
{-# LANGUAGE MultiParamTypeClasses   #-}

module Language.Nano.Typecheck.Compare (

  -- * Type comparison/joining/subtyping
    Equivalent, equiv
  , compareTs
  , unionParts, unionPartsWithEq
  , isSubType
  , eqType

  
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
class Equivalent e a where 
  equiv :: e -> a -> a -> Bool

instance Equivalent e a => Equivalent e [a] where
  equiv γ a b = and $ zipWith (equiv γ) a b 

instance (PP r, F.Reftable r) => Equivalent (Env (RType r)) (RType r) where 
  equiv _ t t'  | any isUnion [t,t']                  = errorstar "equiv: no unions"
  -- No unions beyond this point!
  
  equiv γ (TApp d@(TDef _) ts _) (TApp d'@(TDef _) ts' _) | d == d' = equiv γ ts ts'

  equiv γ t@(TApp d@(TDef _) _ _) t' = equiv γ (unfoldSafe γ t) t'
  equiv γ t t'@(TApp d@(TDef _) _ _) = equiv γ t (unfoldSafe γ t')
  
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

type Casts   = M.Map (Expression AnnSSA) (Cast Type)

data Cast t  = UCST t | DCST t | DC t

instance (PP a) => PP (Cast a) where
  pp (UCST t) = text "Upcast  : " <+> pp t
  pp (DCST t) = text "Downcast: " <+> pp t
  pp (DC   t) = text "Deadcast: " <+> pp t


---------------------------------------------------------------------------------------
-- Subtyping direction ----------------------------------------------------------------
---------------------------------------------------------------------------------------

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


---------------------------------------------------------------------------------
-- SubType API ------------------------------------------------------------------
---------------------------------------------------------------------------------
isSubType :: (F.Reftable r, Ord r, PP r) => Env (RType r) -> RType r -> RType r -> Bool
isSubType γ t1 t2 = (fth4 $ compareTs γ t1 t2) `elem` [EqT, SubT]
{-isSubType γ t1 t2 = (fth4 $ compareTs γ -}
{-                              (tracePP (printf "Comparing: %s - %s" (ppshow t1) (ppshow t2)) t1) -}
{-                              t2) -}
{-                    `elem` [EqT, SubT]-}


eqType :: (F.Reftable r, Ord r, PP r) => Env (RType r) -> RType r -> RType r -> Bool
eqType γ t1 t2 = (fth4 $ compareTs γ t1 t2) == EqT


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

compareTs γ t1 t2   = 
  {- tracePP (printf "compareTs %s - %s" (ppshow t1) (ppshow t2)) $-}  
  compareTs' γ t1 t2


-- | Top-level Unions

-- Eliminate top-level unions
compareTs' γ t1 t2 | any isUnion [t1,t2]     = padUnion γ t1  t2

-- | Top-level Objects

compareTs' γ t1@(TObj _ _) t2@(TObj _ _)     = 
  {-tracePP (printf "Padding: %s and %s" (ppshow t1) (ppshow t2)) $ -}
  padObject γ ({-trace ("padding obj " ++ ppshow t1 ++ " - " ++ ppshow t2)-} t1) t2

-- | Type definitions

compareTs' γ (TApp d1@(TDef _) t1s r1) (TApp d2@(TDef _) t2s r2) | d1 == d2 = -- only handle this case for now
  (mk tjs F.top, mk t1s' r1, mk t2s' r2, joinSubs bds)
  where
    (tjs, t1s', t2s', bds)  = unzip4 $ zipWith (compareTs γ) t1s t2s
    mk xs r                 = TApp d1 xs r 

compareTs' γ t1@(TApp (TDef _) _ _) t2       = compareTs γ (unfoldSafe γ t1) t2
compareTs' γ t1 t2@(TApp (TDef _) _ _)       = compareTs γ t1 (unfoldSafe γ t2)

-- | Everything else in TApp besides unions and defined types
compareTs' _ t1@(TApp _ _ _) t2@(TApp _ _ _) = padSimpleApp t1 t2 

compareTs' _ t1@(TVar _ _)   t2@(TVar _ _)   = padVar t1 t2

compareTs' γ t1@(TFun _ _ _) t2@(TFun _ _ _) = padFun γ t1 t2

compareTs' _ (TAll _ _  ) (TAll _ _  )       = error "Unimplemented: compareTs-7"
compareTs' _ (TBd  _    ) (TBd  _    )       = error "Unimplemented: compareTs-8"

compareTs' _ t1           t2                 = 
  error $ printf "Unimplemented: compareTs-9 for types %s - %s " (ppshow t1) (ppshow t2)



-- | `padSimpleApp`

padSimpleApp t1@(TApp _ _ _) t2@(TApp _ _ _) 
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
  (joinType, mkUnionR topR1 $ t1s, mkUnionR topR2 $ t2s, direction)
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

    commonTs = map (uncurry $ compareTs env) $ cmnPs

    -- To figure out the direction of the subtyping, we must take into account:
    direction  = distSub `joinSub` comSub
    -- ∙ The distinct types (the one that has more is a supertype)
    distSub   = case (d1s, d2s) of
                  ([], []) -> EqT
                  ([], _ ) -> SubT  -- <:
                  (_ , []) -> SupT  -- >:
                  (_ , _ ) -> Nth -- no relation
    -- ∙ The common types (recursively call `compareTs` to compare the types
    --   of the parts and join the subtyping relations)
    comSub     = joinSubs $ fth4 <$> commonTs
    
    (cmnPs, d1s, d2s) = unionParts env t1 t2



-- | `unionParts`

-- Special case of `unionPartsWithEq` that uses `Equivalent` as the type
-- equivalence relation.
--------------------------------------------------------------------------------
unionParts ::  (Eq r, Ord r, F.Reftable r, PP r) => 
            Env (RType r) -> RType r -> RType r 
          -> ([(RType r, RType r)], [RType r], [RType r])
--------------------------------------------------------------------------------

unionParts γ = unionPartsWithEq (equiv γ) γ


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
          -> Env (RType r) 
          -> RType r 
          -> RType r 
          -> ([(RType r, RType r)], [RType r], [RType r])
--------------------------------------------------------------------------------
unionPartsWithEq eq env t1 t2 = (common t1s t2s, d1s, d2s)
  where
    -- `common` does a "light" matching - which is determined by `equiv`. 
    -- Right now the only difference is in objects: 
    -- all objects are matched together, so the common parts may not be 
    -- the same in terms of raw type. 
    -- This is why `padObject` is called on the common parts. Non-object types
    -- fall through
    -- Also `common` returns aligned types - so no need to re-align them.
    (t1s, t2s) = sanityCheck $ mapPair bkUnion (t1, t2)

    (d1s, d2s)  = distinct t1s t2s

    -- Compare the types based on the Equivalence relation and pair them up into
    -- Type structures that are common in both sides, and ...
    common xs ys | any null [xs,ys] = []
    common xs ys | otherwise        = [(x,y) | x <- xs, y <- ys, x `eq` y ]

    -- ... type structures that are distinct in the two sides
    distinct xs [] = ([], xs)
    distinct [] ys = (ys, [])
    distinct xs ys = ([x | x <- xs, not $ any (x `eq`) ys ],
                      [y | y <- ys, not $ any (y `eq`) xs ])

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
    direction = cmnDir `joinSub` distDir d1s d2s
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



-- | Helper
instance (PP a, PP b, PP c) => PP (a,b,c) where
  pp (a,b,c) = pp a <+> text ",\n" <+> pp b <+> text ",\n" <+> pp c

instance (PP a, PP b, PP c, PP d) => PP (a,b,c,d) where
  pp (a,b,c,d) = pp a <+>  pp b <+> pp c <+> pp d

