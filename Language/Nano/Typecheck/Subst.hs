{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Nano.Typecheck.Subst ( 
  
  -- * Substitutions
    RSubst (..)
  , Subst 
  , toList
  , fromList

  -- * Free Type Variables
  , Free (..)

  -- * Type-class with operations
  , Substitutable (..)


  ) where 

import           Text.PrettyPrint.HughesPJ
import           Language.ECMAScript3.PrettyPrint
import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Types as F
import           Language.Nano.Errors 
import           Language.Nano.Env
import           Language.Nano.Typecheck.Types

import           Control.Applicative ((<$>))
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M 
import           Data.Monoid
import           Text.Printf 
-- import           Debug.Trace
-- import           Language.Nano.Misc (mkEither)

---------------------------------------------------------------------------
-- | Substitutions --------------------------------------------------------
---------------------------------------------------------------------------

-- | Type alias for Map from @TVar@ to @Type@. Hidden

data RSubst r = Su (M.HashMap TVar (RType r))
type Subst    = RSubst ()

toList        :: RSubst r -> [(TVar, RType r)]
toList (Su m) =  M.toList m 

fromList      :: [(TVar, RType r)] -> RSubst r
fromList      = Su . M.fromList 

-- | Substitutions form a monoid; not commutative

instance (F.Reftable r, Substitutable r (RType r)) => Monoid (RSubst r) where 
  mempty                    = Su M.empty
  mappend (Su m) θ'@(Su m') = Su $ (apply θ' <$> m) `M.union` m'

instance (F.Reftable r, PP r) => PP (RSubst r) where 
  pp (Su m) = if M.null m then text "empty" else vcat $ (ppBind <$>) $ M.toList m 

ppBind (x, t) = pp x <+> text ":=" <+> pp t

---------------------------------------------------------------------------
-- | Substitutions --------------------------------------------------------
---------------------------------------------------------------------------

class Free a where 
  free  :: a -> S.HashSet TVar

class Substitutable r a where 
  apply :: (RSubst r) -> a -> a 

instance Free a => Free [a] where 
  free = S.unions . map free

instance Substitutable r a => Substitutable r [a] where 
  apply = map . apply 

instance (Substitutable r a, Substitutable r b) => Substitutable r (a,b) where 
  apply f (x,y) = (apply f x, apply f y)

instance (PP r, F.Reftable r) => Substitutable r (RType r) where 
  apply θ t = appTy θ t
--     where 
--       msg   = printf "apply [θ = %s] [t = %s]" (ppshow θ) (ppshow t)

instance (PP r, F.Reftable r) => Substitutable r (Bind r) where 
  apply θ (B z t) = B z $ appTy θ t

instance Free (RType r) where
  free (TApp _ ts _)        = S.unions   $ free <$> ts
  free (TVar α _)           = S.singleton α 
  free (TFun xts t _)       = S.unions   $ free <$> t:ts where ts = b_type <$> xts
  free (TAll α t)           = S.delete α $ free t 
  free (TObj bs _)          = S.unions   $ free <$> b_type <$> bs
  free (TBd (TD _ α t _ ))  = foldr S.delete (free t) α

instance Substitutable () Fact where
  apply _ x@(PhiVar _)  = x
  apply θ (TypInst ts)  = TypInst $ apply θ ts
  apply θ (Assume  t )  = Assume  $ apply θ t

instance Free Fact where
  free (PhiVar _)       = S.empty
  free (TypInst ts)     = free ts
  free (Assume t)       = free t
 
------------------------------------------------------------------------
-- appTy :: RSubst r -> RType r -> RType r
------------------------------------------------------------------------
appTy θ (TApp c ts z)            = TApp c (apply θ ts) z 
appTy θ (TObj bs z)              = TObj (map (\b -> B { b_sym = b_sym b, b_type = appTy θ $ b_type b } ) bs ) z
appTy (Su m) t@(TVar α r)        = (M.lookupDefault t α m) `strengthen` r
appTy θ (TFun ts t r)            = TFun  (apply θ ts) (apply θ t) r
appTy (Su m) (TAll α t)          = apply (Su $ M.delete α m) t 
appTy (Su m) (TBd (TD c α t s))  = TBd $ TD c α (apply (Su $ foldr M.delete m α) t) s



-----------------------------------------------------------------------------
-- Unfolding ----------------------------------------------------------------
-----------------------------------------------------------------------------

-- | Unfold the FIRST TDef at any part of the type @t@.
-------------------------------------------------------------------------------
unfoldFirst :: Type -> Env Type -> Type
-------------------------------------------------------------------------------
unfoldFirst t env = go t
  where 
    go (TFun its ot r)         = TFun (appTBi go <$> its) (go ot) r
    go (TObj bs r)             = TObj (appTBi go <$> bs) r
    go (TBd  _)                = error "unfoldTDefDeep: there should not be a TBody here"
    go (TAll v t)              = TAll v $ go t
    go (TApp (TDef id) acts _) = 
      case envFindTy (F.symbol id) env of
        Just (TBd (TD _ vs bd _ )) -> apply (fromList $ zip vs acts) bd
        _                          -> error $ errorUnboundId id
    go (TApp c a r)            = TApp c (go <$> a) r
    go t@(TVar _ _ )           = t
    appTBi f (B s t)           = B s $ f t


-- | Unfold a top-level type definition once. 
-- Return @Right t@, where @t@ is the unfolded type if the unfolding is succesful.
-- This includes the case where the input type @t@ is not a type definition in
-- which case the same type is returned.
-- If it is a type literal for which no definition exists return 
-- @Left "<Error message>".
--
-- TODO: Make sure toplevel refinements are the same.
-------------------------------------------------------------------------------
unfoldMaybe :: (PP r, F.Reftable r) => RType r -> Env (RType r) -> Either String (RType r)
-------------------------------------------------------------------------------
unfoldMaybe t@(TApp (TDef id) acts _) env =
      case envFindTy (F.symbol id) env of
        Just (TBd (TD _ vs bd _ )) -> Right $ apply (fromList $ zip vs acts) bd
        _                          -> Left  $ (printf "Failed unfolding: %s" $ ppshow t)
-- The only thing that is unfoldable is a TDef.
-- The rest are just returned as they are.
unfoldTDefMaybe t                       _   = Right t


-- | Force a successful unfolding
-------------------------------------------------------------------------------
unfoldSafe :: (PP r, F.Reftable r) => RType r -> Env (RType r) -> RType r
-------------------------------------------------------------------------------
unfoldSafe t env = either error id $ unfoldTDefMaybe t env



-----------------------------------------------------------------------------
-- Unification --------------------------------------------------------------
-----------------------------------------------------------------------------

-- | Unify types @t@ and @t'@, using @θ@ as the current substitution and @env@
-- as the current type definition environment.
-----------------------------------------------------------------------------
unify :: Env Type -> Subst -> Type -> Type -> Either String Subst
-----------------------------------------------------------------------------

unify env θ t@(TApp c _ _) t'@(TApp c' _ _) 
  | c /= c' = Left $ errorUnification t t'

unify env θ (TFun xts t _) (TFun xts' t' _) = 
  unifys env θ (t: (b_type <$> xts)) (t': (b_type <$> xts'))

unify env θ t@(TApp (TDef s) ts _) t'@(TApp (TDef s') ts' _)
  | s == s'   = unifys env θ ts ts'
  | otherwise = Left $ errorUnification t t'

unify env θ t@(TApp (TDef _) _ _) t' =
  unify env θ (unfoldSafe t env) t'

unify env θ t t'@(TApp (TDef _) _ _)        =
  unify env θ t (unfoldSafe t' env)

unify _  θ (TVar α _)     (TVar β _)       = varEql θ α β 
unify _  θ (TVar α _)     t                = varAsn θ α t 
unify _  θ t              (TVar α _)       = varAsn θ α t

-- TODO: handle unions
unify env θ (TApp c ts _) (TApp c' ts' _)
  | c == c' = unifys env θ ts ts'

unify _ _ (TBd _) _ = error $ bugTBodiesOccur "unify"
unify _ _ _ (TBd _) = error $ bugTBodiesOccur "unify"

unify _ θ t t' 
  | t == t'   = Right $ θ
  | otherwise = Left  $ errorUnification t t'


-----------------------------------------------------------------------------
unifys ::  Env Type -> Subst -> [Type] -> [Type] -> Either String Subst
-----------------------------------------------------------------------------
unifys env θ xs ys =  {- trace msg $ -} unifys' env θ xs ys 
   {-where -}
   {-  msg      = printf "unifys: [xs = %s] [ys = %s]"  (ppshow xs) (ppshow ys)-}

unifys' env θ ts ts' 
  | nTs == nTs' = go env θ (ts, ts') 
  | otherwise   = Left $ errorUnification ts ts'
  where 
    nTs                      = length ts
    nTs'                     = length ts'
    go env θ (t:ts , t':ts') = unify env θ t t' >>= \θ' -> go env θ' (mapPair (apply θ') (ts, ts'))
    go env θ (_    , _    )  = return θ 


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
  | α `S.member` free t   = Left  $ errorOccursCheck α t 
  | unassigned α θ         = Right $ θ `mappend` (Su $ M.singleton α t)
  | otherwise              = Left  $ errorRigidUnify α t
  
unassigned α (Su m) = M.lookup α m == Just (tVar α)


