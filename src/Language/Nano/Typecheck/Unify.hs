{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
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
import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Errors 
import           Language.Nano.Errors 
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Typecheck.Unfold
import           Language.Nano.Typecheck.Compare


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


-----------------------------------------------------------------------------
-- Unification --------------------------------------------------------------
-----------------------------------------------------------------------------

-- | Unify types @t@ and @t'@, in substitution environment @θ@ and type
-- definition environment @env@.
-----------------------------------------------------------------------------
unify :: (PP r, F.Reftable r, Ord r) => 
  SourceSpan -> TDefEnv r -> RSubst r -> RType r -> RType r -> Either Error (RSubst r)
-----------------------------------------------------------------------------

unify _ _ θ t@(TApp _ _ _           ) t'@(TApp _ _ _            )       
  | any isTop [t,t']  
  = Right $ θ

unify l γ θ   (TFun xts t _         )    (TFun xts' t' _        )       
  = unifys l γ θ (t: (b_type <$> xts)) (t': (b_type <$> xts'))

-- TODO: Cycles
unify l γ θ   (TApp d@(TDef _) ts _ )    (TApp d'@(TDef _) ts' _)       
  | d == d'           
  = unifys l γ θ ts ts'

unify l _ θ   (TVar α _             )    (TVar β _)                     
  = varEql l θ α β 
unify l _ θ   (TVar α _)              t'                                
  = varAsn l θ α t'
unify l _ θ t                            (TVar α _              )       
  = varAsn l θ α t

unify l γ θ t@(TApp (TDef _) _ _    ) t'                                
  = unify l γ θ (unfoldSafe γ t) t'
unify l γ θ t                         t'@(TApp (TDef _) _ _     )       
  = unify l γ θ t (unfoldSafe γ t')

-- List[A] + Null `unif` List[T0] + Null => A `unif` T0
-- TODO: make sure other nothing weird is going on with TVars,
-- e.g.  List[A] + B `unif` ... => this should not even be allowed!!!

unify l γ θ t                         t'                                
  | any isUnion [t,t']
  = (uncurry $ unifys l γ θ) $ unzip $ fst3 $ unionPartsWithEq (unifEq γ) t t'

unify l γ θ   (TObj bs1 _           )    (TObj bs2 _            )       
  | s1s == s2s        
  = unifys l γ θ (b_type <$> L.sortBy ord bs1) (b_type <$> L.sortBy ord bs2)
  | otherwise         
  = return θ
    where
      s1s = L.sort $ b_sym <$> bs1 
      s2s = L.sort $ b_sym <$> bs2
      ord b b' = compare (b_sym b) (b_sym b')

unify l γ θ   (TArr t _             )    (TArr t' _                )    
  = unify l γ θ t t'

unify _ _ θ _                         _
  = return θ


{-unify' γ θ t t' = unify γ θ (trace (printf "unify: %s - %s" (ppshow t) (ppshow t')) t) t' -}

-- TODO: cycles
unifEq _ (TApp d@(TDef _) _ _) (TApp d'@(TDef _) _ _) | d == d' = True
unifEq γ t@(TApp (TDef _) _ _) t' = unifEq γ (unfoldSafe γ t) t'
unifEq γ t t'@(TApp (TDef _) _ _) = unifEq γ t (unfoldSafe γ t')
unifEq γ t t'                     = equiv γ t t'
  


-----------------------------------------------------------------------------
unifys ::  (PP r, F.Reftable r, Ord r) =>  
  SourceSpan -> TDefEnv r -> RSubst r -> [RType r] -> [RType r] -> Either Error (RSubst r)
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


