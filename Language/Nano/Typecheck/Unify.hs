{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Nano.Typecheck.Unify ( 
  
  -- * Unification 
  unify, unifys

  ) where 

-- import           Text.PrettyPrint.HughesPJ
-- import           Language.ECMAScript3.PrettyPrint
import           Language.Fixpoint.Misc
-- import qualified Language.Fixpoint.Types as F
import           Language.Nano.Errors 
import           Language.Nano.Env
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Typecheck.Compare

import           Control.Applicative ((<$>))
import           Control.Monad
import           Data.Either         (rights, lefts) 
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M 
import           Data.Monoid
import           Text.Printf 
-- import           Debug.Trace
-- import           Language.Nano.Misc (mkEither)


-----------------------------------------------------------------------------
-- Unification --------------------------------------------------------------
-----------------------------------------------------------------------------

-- | Unify types @t@ and @t'@, using @θ@ as the current substitution and @env@
-- as the current type definition environment.
-----------------------------------------------------------------------------
unify :: Env Type -> Subst -> Type -> Type -> Either String Subst
-----------------------------------------------------------------------------

unify _ θ t@(TApp c _ _) t'@(TApp c' _ _) 
  | any isTop [t,t']                    = Right $ θ
  {- | c /= c'                             = Left $ errorUnification t t'-}

unify env θ (TFun xts t _) (TFun xts' t' _) = 
  unifys env θ (t: (b_type <$> xts)) (t': (b_type <$> xts'))

unify env θ t@(TApp (TDef s) ts _) t'@(TApp (TDef s') ts' _)
  | s == s'                             = unifys env θ ts ts'
  | otherwise                           = Left $ errorUnification t t'

unify env θ t@(TApp (TDef _) _ _) t'    = unify env θ (unfoldSafe env t) t'

unify env θ t t'@(TApp (TDef _) _ _)    = unify env θ t (unfoldSafe env t')

unify _  θ (TVar α _)     (TVar β _)    = varEql θ α β 
unify _  θ (TVar α _)     t             = varAsn θ α t 
unify _  θ t              (TVar α _)    = varAsn θ α t

-- TODO: handle unions ???
-- unify _ _ (TApp TUn _ _) (TApp TUn _ _) = error "Unimplemented: unify unions1"
-- unify _ _ (TApp TUn _ _) _              = error "Unimplemented: unify unions2"
-- unify _ _ _              (TApp TUn _ _) = error "Unimplemented: unify unions3"

unify env θ (TApp c ts _) (TApp c' ts' _)
  | c == c'           = unifys env θ ts ts'

unify _ _ (TBd _) _   = error $ bugTBodiesOccur "unify"
unify _ _ _ (TBd _)   = error $ bugTBodiesOccur "unify"

unify γ θ t t' 
  | t == t'           = Right $ θ
  | isSubType γ t t'  = Right $ θ     -- XXX
  | isSubType γ t' t  = Right $ θ     -- XXX
  | otherwise         = Left  $ errorUnification t t'



-----------------------------------------------------------------------------
unifys ::  Env Type -> Subst -> [Type] -> [Type] -> Either String Subst
-----------------------------------------------------------------------------
unifys env θ xs ys =  {- trace msg $ -} unifys' env θ xs ys 
   {-where -}
   {-  msg      = printf "unifys: [xs = %s] [ys = %s]"  (ppshow xs) (ppshow ys)-}

unifys' env θ ts ts' 
  | nTs == nTs' = go env θ ts ts'
  | otherwise   = Left $ errorUnification ts ts'
  where 
    nTs                      = length ts
    nTs'                     = length ts'
    {-go env θ (t:ts , t':ts') = unify env θ t t' >>= \θ' -> go env θ' (mapPair (apply θ') (ts, ts'))-}
    {-go _   θ (_    , _    )  = return θ -}

    go γ θ ts ts' = foldl (joinEither safeJoin) (Right $ θ) $ zipWith (unify γ θ) ts ts'
    
    -- Only allow joining unifications where the common keys map to identical
    -- types
    safeJoin θ@(Su m) θ'@(Su m') 
      | check m m' = mappend θ θ'
      | otherwise  = errorstar $ printf "Cannot join substs: %s\nand\n%s"
                                 (ppshow θ) (ppshow θ')

check m m' = vs == vs'
  where vs  = (`M.lookup` m ) <$> ks
        vs' = (`M.lookup` m') <$> ks
        ks  = M.keys $ M.intersection (clr m) (clr m')
        clr = M.filterWithKey (\k v -> tVar k /= v)

joinEither f _         (Left  l) = Left  $ l
joinEither f (Left l)  _         = Left  $ l
joinEither f (Right a) (Right b) = Right $ f a b


-----------------------------------------------------------------------------
varEql :: Subst -> TVar -> TVar -> Either String Subst
-----------------------------------------------------------------------------
varEql θ α β =  
  case varAsn θ α $ tVar β of
    Right θ' -> Right θ'
    Left  s1 -> 
      case varAsn θ β $ tVar α of
        Right θ'' -> Right θ''
        Left  s2  -> Left (s1 ++ "\n OR \n" ++ s2)


-----------------------------------------------------------------------------
varAsn :: Subst -> TVar -> Type -> Either String Subst
-----------------------------------------------------------------------------
varAsn θ α t 
  | t == apply θ (tVar α)  = Right $ θ -- Check if previous substs are sufficient 
  | t == tVar α            = Right $ θ 
  | α `S.member` free t    = Left  $ errorOccursCheck α t 
  | unassigned α θ         = Right $ θ `mappend` (Su $ M.singleton α t)
  | otherwise              = Left  $ errorRigidUnify α t
  
unassigned α (Su m) = M.lookup α m == Just (tVar α)


