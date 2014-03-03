{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Nano.Typecheck.Unify ( 
  
  -- * Unification 
  --   unify
  -- , 
  unifys, unifysV

  ) where 

-- import           Text.PrettyPrint.HughesPJ
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Syntax
import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Errors 
import           Language.Nano.Errors 
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Compare
import           Language.Nano.Typecheck.Subst


import           Language.ECMAScript3.Parser.Type    (SourceSpan (..))
import           Control.Applicative ((<$>))
-- import           Control.Monad
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M 
import           Data.Monoid
import qualified Data.List           as L
import           Text.Printf 
-- import           Debug.Trace
-- import           Language.Nano.Misc (mkEither)


type PPR r = (PP r, F.Reftable r)

-----------------------------------------------------------------------------
-- Unification --------------------------------------------------------------
-----------------------------------------------------------------------------

-- | Unify types @t@ and @t'@, in substitution environment @θ@ and type
-- definition environment @env@.
-----------------------------------------------------------------------------
unify :: (PP r, F.Reftable r, Ord r) => 
  SourceSpan -> TDefEnv (RType r) -> RSubst r -> RType r -> RType r -> Either Error (RSubst r)
-----------------------------------------------------------------------------

unify _ _ θ t@(TApp _ _ _) t'@(TApp _ _ _) | any isTop [t,t'] = Right $ θ

unify l γ θ (TFun xts t _) (TFun xts' t' _)
  = unifys l γ θ (t: (b_type <$> xts)) (t': (b_type <$> xts'))

unify l _ θ (TVar α _) (TVar β _) = varEql l θ α β 
unify l _ θ (TVar α _) t' = varAsn l θ α t'
unify l _ θ t (TVar α _)  = varAsn l θ α t

unify l γ θ (TApp (TRef i) ts _) (TApp (TRef i') ts' _) = 
  unifys l γ θ  
    (f_type <$> (t_elts (sortTDef $ findAndApply i  ts  γ)))
    (f_type <$> (t_elts (sortTDef $ findAndApply i' ts' γ)))

unify l γ θ t t' | any isUnion [t,t'] 
 = uncurry (unifys l γ θ) . unzip . fst3 $ unionParts t t'

unify l γ θ (TArr t _) (TArr t' _) = unify l γ θ t t'

-- The rest of the cases do not cause any unification.
unify _ _ θ _  _ = return θ


-----------------------------------------------------------------------------
findAndApply :: PPR r => TyID -> [RType r] -> TDefEnv (RType r) -> TDef (RType r)
-----------------------------------------------------------------------------
findAndApply i ts γ = TD n [] p $ apply (fromList (zip vs ts)) <$> elts
-- FIXME: apply to p ???
  where TD n vs p elts = findTyIdOrDie i γ 
                

-----------------------------------------------------------------------------
unifys ::  (PP r, F.Reftable r, Ord r) =>  
  SourceSpan -> TDefEnv (RType r) -> RSubst r -> [RType r] -> [RType r] -> Either Error (RSubst r)
-----------------------------------------------------------------------------
unifys loc env θ xs ys = {- tracePP msg $ -} unifys' loc env θ xs ys 
   {-where -}
   {-  msg      = printf "unifys: [xs = %s] [ys = %s]"  (ppshow xs) (ppshow ys)-}

unifysV loc env θ xs ys = tracePP msg <$> unifys' loc env θ (tracePP "xs" xs) (tracePP "ys" ys)
  where 
    msg      = printf "unifys: [xs = %s] [ys = %s]"  (ppshow xs) (ppshow ys)


unifys' loc env θ ts ts' 
  | nTs == nTs' = go env θ ts ts'
  | otherwise   = Left $ errorUnification loc ts ts'
  where 
    nTs           = length ts
    nTs'          = length ts'
    go γ θ ts ts' = foldl safeJoin (Right $ θ) $ zipWith (unify loc γ θ) ts ts'
    -- Only allow joining unifications where the common keys map to identical
    -- types
    safeJoin (Right θ@(Su m)) (Right θ'@(Su m'))
      | check m m'                                = Right $ mappend θ θ'
      | otherwise                                 = Left  $ errorJoinSubsts loc θ θ' 
    safeJoin (Left l        ) _                   = Left l
    safeJoin _                (Left l        )    = Left l
                               

check m m' = vs == vs'
  where vs  = map (toType <$>) $ (`M.lookup` m ) <$> ks
        vs' = map (toType <$>) $ (`M.lookup` m') <$> ks
        ks  = M.keys $ M.intersection (clr m) (clr m')
        clr = M.filterWithKey (\k v -> tVar k /= v)


-----------------------------------------------------------------------------
varEql :: (PP r, F.Reftable r, Ord r) => 
  SourceSpan -> RSubst r -> TVar -> TVar -> Either Error (RSubst r)
-----------------------------------------------------------------------------
varEql l θ α β =  
  case varAsn l θ α $ tVar β of
    Right θ' -> Right θ'
    Left  s1 -> 
      case varAsn l θ β $ tVar α of
        Right θ'' -> Right θ''
        Left  s2  -> Left $ catMessage s1 (errMsg s2) -- s1 { errMsg = (errMsg s1 ++ "\n OR \n" ++ show s2) }


-----------------------------------------------------------------------------
varAsn ::  (PP r, F.Reftable r, Ord r) => 
  SourceSpan -> RSubst r -> TVar -> RType r -> Either Error (RSubst r)
-----------------------------------------------------------------------------
varAsn l θ α t 
  -- Check if previous substs are sufficient 
  | t == apply θ (tVar α)  = Right $ θ 
  -- We are not even checking if t is a subtype of `tVar α`, i.e.:
  -- unifying A with A + B will fail!
  | t == tVar α            = Right $ θ 
  | α `S.member` free t    = Left  $ errorOccursCheck l α t 
  | unassigned α θ         = Right $ θ `mappend` (Su $ M.singleton α t)
  | otherwise              = Left  $ errorRigidUnify l α t
  
unassigned α (Su m) = M.lookup α m == Just (tVar α)

