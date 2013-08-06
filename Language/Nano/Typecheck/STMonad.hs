{- LANGUAGE TypeSynonymInstances #-}
{- LANGUAGE FlexibleInstances    #-}
{- LANGUAGE NoMonomorphismRestriction #-}
{- LANGUAGE ScopedTypeVariables  #-}


-- RJ: Please add documentation for what this module is 
-- supposed to do. I have no idea from the name.

module Language.Nano.Typecheck.STMonad (
  
  -- * Casting
    Cast(..)
  , Casts

  -- * Subtyping
  , subType,  subTypeCast
  , subTypes, subTypesCast
  , isSubType
 
  -- * Unfold type definition
  , unfoldTDefSafe, unfoldTDefDeep, unfoldTDefSafeST, unfoldTDefDeepST


  )  where 

import           Text.Printf
import           Text.PrettyPrint.HughesPJ          (text, (<+>), ($+$))
import           Control.Applicative                ((<$>))
import           Control.Monad.State
import           Control.Monad.Error
import           Language.Fixpoint.Misc             hiding (traceShow) 
import qualified Language.Fixpoint.Types            as F

import           Language.Nano.Env
import           Language.Nano.Types
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Errors
import           Language.Nano.Misc
import           Data.Monoid                  
import qualified Data.HashSet                       as HS
import qualified Data.HashMap.Strict                as HM
import qualified Data.Map                           as M
import qualified Data.List                          as L
import           Language.ECMAScript3.Parser        (SourceSpan (..))
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations
import           Language.ECMAScript3.PrettyPrint

import           Debug.Trace

--------------------------------------------------------------------------
-- | Subtype Monad  ------------------------------------------------------
--------------------------------------------------------------------------

data STState = STS {
                   -- Errors
                     st_err     :: !STError
                   -- Type definitions
                   , st_tdefs   :: !(Env Type)
                   -- st_docasts: True if casts are allowed
                   , st_docasts :: Bool
                   -- st_casts: the cast map 
                   , st_casts   :: Casts
                   -- Keep track of mutually recursive type constructors
                   , st_aliases :: [(TCon,TCon)]
                   -- Current expression
                   , st_expr    :: Maybe (Expression AnnSSA) 
                   }

type STM     = ErrorT String (State STState)

type STError = [(SourceSpan, String)]

type Casts   = M.Map (Expression AnnSSA) (Cast Type)

data Cast t  = UCST t | DCST t | DC  t


-------------------------------------------------------------------------------
-- API ------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- | Simple invocation of subtyping using an EMPTY substitution.
-- Useful when the types have been substitions have been applied to the types
-- Also, casts are not going to be added here - otherwise trivially true...
-- TODO:
-- TODO: Make sure that the subs have been applies !!!
-- TODO:
isSubType :: Env (RType r) -> RType r -> RType r -> Bool
isSubType env t1 t2 = either2Bool $ subType (envMap toType env) mempty (toType t1) (toType t2)


-- | Decides if @t1@ is a subtype of @t2@. If yes, it returns the the 
-- succeful substitution that does this. Otherwise, a list of error messages.
-- Casts are disabled in this version. 
-------------------------------------------------------------------------------
subType :: Env Type             -- Type definitions
        -> Subst                -- Initial substitution
        -> Type                 -- LHS type
        -> Type                 -- RHS type
        -> Either STError Subst -- Result
-------------------------------------------------------------------------------
subType m = execSubTypeE $ STS [] (envMap toType m) False M.empty [] Nothing 
 
 
-- | Decides if @t1@ is a subtype of @t2@. If true, it returns the the 
-- succeful substitution that does this. Otherwise, a list of error messages.
-- Casts are enabled in this version, so with the addition of "dead code casts"
-- this version is unlikely to fail.
-- TODO: Cast with an empty expression will fail
-------------------------------------------------------------------------------
subTypeCast  :: Env Type                      -- Type definitions
             -> Subst                         -- Initial substitution
             -> Maybe (Expression AnnSSA)     -- Current expression
             -> Type                          -- LHS type
             -> Type                          -- RHS type
             -> Either STError (Subst, Casts) -- Result
-------------------------------------------------------------------------------
subTypeCast m θ e t t' = either Left (\b -> Right (b, st_casts s)) r
  where (r,s) = execSubTypeS (STS [] (envMap toType m) True M.empty [] e) θ t t'
 

-------------------------------------------------------------------------------
subTypes :: Env Type                    -- Type definitions
         -> Subst                       -- Initial substitution
         -> [Type]                      -- LHS type
         -> [Type]                      -- RHS type
         -> Either STError Subst        -- Result
-------------------------------------------------------------------------------
subTypes m = execSubTypesE $ STS [] (envMap toType m) False M.empty [] Nothing
 
 
-- TODO: Cast with an empty expression will fail
-------------------------------------------------------------------------------
subTypesCast :: Env Type                      -- Type definitions
             -> Subst                         -- Initial substitution
             -> [Maybe (Expression AnnSSA)]   -- Current expression
             -> [Type]                        -- LHS type
             -> [Type]                        -- RHS type
             -> Either STError (Subst, Casts) -- Result
-------------------------------------------------------------------------------
subTypesCast m θ es t t' = either Left (\b -> Right (b, st_casts s)) r
  where (r,s) = execSubTypesS (STS [] (envMap toType m) True M.empty [] Nothing) es θ t t'



-------------------------------------------------------------------------------
-- Misc / Aux -----------------------------------------------------------------
-------------------------------------------------------------------------------

-----------------------------------------------------------------------------
executeST :: (PP a) => STState -> STM a -> (Either STError a, STState)
-----------------------------------------------------------------------------
executeST st action =
  case runState (runErrorT $ action) st of 
    (Left err, s) -> (Left [(dummySpan, err)], s)
    (Right x , s) | null (st_err s) -> (Right x, s)
                  | otherwise       -> (Left $ reverse $ st_err s, s)
      
-- (applyNonNull (Right x) Left (trace (printf "Errs: %d" (length $ st_err s)) $ reverse $ st_err s), s)


-----------------------------------------------------------------------------
execute :: (PP a) => STState -> STM a -> Either STError a
-----------------------------------------------------------------------------
execute s = fst . executeST s


-----------------------------------------------------------------------------
tryWithBackup :: (PP a) => STM a -> STM a -> STM a
-----------------------------------------------------------------------------
tryWithBackup action backup =
  do  s <- get
      case executeST s action of 
        -- If the action fails, just do the backup and IGNORE ALL ERROR MESSAGES
        (Left _  , _ ) -> backup
        -- Otherwise, apply the latest state and return the successful result
        (Right r , s') -> modify (const s') >> return r


-----------------------------------------------------------------------------
tryWithBackups :: (PP a) => STM a -> [STM a] -> STM a
-----------------------------------------------------------------------------
tryWithBackups = foldl tryWithBackup


-------------------------------------------------------------------------------
setExpr   :: Maybe (Expression AnnSSA) -> STM () 
-------------------------------------------------------------------------------
setExpr eo = modify $ \st -> st { st_expr = eo }


-------------------------------------------------------------------------------
getExpr   :: STM (Maybe (Expression AnnSSA))
-------------------------------------------------------------------------------
getExpr    = st_expr <$> get



-----------------------------------------------------------------------------
withExpr  :: Maybe (Expression AnnSSA) -> STM a -> STM a
-----------------------------------------------------------------------------
withExpr e action = 
  do  eold  <- getExpr 
      setExpr  e 
      r     <- action 
      setExpr  eold
      return $ r


-----------------------------------------------------------------------------
addTAlias   :: (TCon, TCon) -> STM ()
-----------------------------------------------------------------------------
addTAlias cc = modify (\s -> s { st_aliases = cc : st_aliases s })


----------------------------------------------------------------------------------
-- | Join a list of Subst 
----------------------------------------------------------------------------------
joinSubsts :: Subst -> [Subst] -> STM Subst
----------------------------------------------------------------------------------
joinSubsts θ θs =  foldM (joinSubst θ) mempty θs
   {-foldM (\θ1 θ2 -> tracePP (printf "Joining substs: %s ++ %s ++ %s" (ppshow θ) (ppshow θ1) (ppshow θ2)) -}
   {- <$> joinSubst θ θ1 θ2) mempty θs -}


----------------------------------------------------------------------------------
-- | Join two substitutions: When a key is present in both substitutionsthen use 
-- the supertype of the bindings *if one exists*, otherwise flag an error
----------------------------------------------------------------------------------
joinSubst :: Subst -> Subst -> Subst -> STM Subst
----------------------------------------------------------------------------------
joinSubst θ (Su m1) (Su m2) =
  do 
    s     <- get
    cmnV  <- zipWithM (\t t' -> haltCasts (join s θ t t')) (safeMap commonK m1) (safeMap commonK m2)
    {-tracePP (printf "Joining types: (%s <: %s)" (ppshow t1) (ppshow t2)) <$> -}
    return $ Su $ only1 `HM.union` only2 `HM.union` (HM.fromList $ zip commonK cmnV)
  where 
    commonK       = HM.keys $ (rr m1) `HM.intersection` (rr m2)
    rr m         = HM.filterWithKey (\α t -> not $ tVar α == t) m
    only1         = foldr HM.delete m1 commonK
    only2         = foldr HM.delete m2 commonK
    safeMap s m   = (\k -> mfromJust "joinSubst" $ HM.lookup k m) <$> s
    join s θ t t' | t == t'   = return t
                  | otherwise = addError (printf "Cannot unify %s and %s" (ppshow t) (ppshow t')) t


-------------------------------------------------------------------------------
addError   :: String -> a -> STM a
-------------------------------------------------------------------------------
addError msg x = modify f >> return x
  where
    f st = st { st_err = (l st, msg) : (st_err st) }
    l st = maybe dummySpan (ann . getAnnotation) $ st_expr st


-------------------------------------------------------------------------------
either2Bool :: Either a b -> Bool
-------------------------------------------------------------------------------
either2Bool = either (const False) (const True)



-----------------------------------------------------------------------------
-- SubTyping ----------------------------------------------------------------
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
execSubTypeB :: STState -> Subst -> Type -> Type -> Bool
-----------------------------------------------------------------------------
execSubTypeB st θ t1 t2 = either2Bool $ execSubTypeE st θ t1 t2 


-----------------------------------------------------------------------------
execSubTypeE :: STState -> Subst -> Type -> Type -> Either STError Subst
-----------------------------------------------------------------------------
execSubTypeE st θ t1 t2 = execute st $ subty θ t1 t2


-----------------------------------------------------------------------------
execSubTypesE :: STState -> Subst -> [Type] -> [Type] -> Either STError Subst
-----------------------------------------------------------------------------
execSubTypesE st θ t1s t2s = execute st $ subtys θ (repeat Nothing) t1s t2s


-----------------------------------------------------------------------------
execSubTypeS :: STState -> Subst -> Type -> Type -> (Either STError Subst, STState)
-----------------------------------------------------------------------------
execSubTypeS st θ t1 t2 = executeST st $ subty θ t1 t2


-----------------------------------------------------------------------------
execSubTypesS :: STState -> [Maybe (Expression AnnSSA)] -> Subst -> [Type] -> [Type] -> (Either STError Subst, STState)
-----------------------------------------------------------------------------
execSubTypesS st es θ t1 t2 = executeST st $ subtys θ es t1 t2


-- | The pattern followed below:
-- ∙ Try to apply regular subtyping (for unions or non-unions
-- ∙ If this succeeds, add an upcast (if needed)
-- ∙ If subtyping fails, try unification
-- ∙ if unification fails, add a down-cast (or, dead code annotation)
--
-- For everything there is both a "casting" and a "non-casting" version.
-----------------------------------------------------------------------------
subty :: Subst -> Type -> Type -> STM Subst
-----------------------------------------------------------------------------
subty θ (TApp TUn ts _ ) (TApp TUn ts' _) = 
  subtyIfCast (tryWithBackup (subtyUnions θ ts  ts' >>= upCastTs ts ts') 
                             (castTs θ ts ts'),
               subtyUnions θ ts ts')

subty θ t@(TApp TUn ts _ ) t' = 
  subtyIfCast (tryWithBackups (subtyUnions θ ts [t'] >>= upCastTs ts [t']) 
                              [unify θ t t', castTs θ ts [t']], 
               tryWithBackups (subtyUnions θ ts [t']) 
                              [unify θ t t'])

subty θ t t'@(TApp TUn ts' _) = 
  subtyIfCast (tryWithBackups (subtyUnions θ [t] ts' >>= upCastTs [t] ts') 
                              [unify θ t t', castTs θ [t] ts'],
               tryWithBackups (subtyUnions θ [t] ts') 
                              [unify θ t t'])

subty θ t t' = 
  subtyIfCast (tryWithBackups (subtyNoUnion θ t t' >>= upCastTs [t] [t'])
                              [castTs θ [t] [t']], 
               subtyNoUnion θ t t')

-- | If casts are set, do @a@, otherwise do @b@
subtyIfCast (a,b) = get >>= \s -> if (st_docasts s) then a else b



-----------------------------------------------------------------------------
subtys :: Subst -> [Maybe (Expression AnnSSA)] -> [Type] -> [Type] -> STM Subst
-----------------------------------------------------------------------------
subtys θ es ts ts'
  | nTs == nTs' = go $ zip3 es ts ts'
  | otherwise   = addError (errorSubType "" (toType <$> ts) (toType <$> ts)) θ
  where
    nTs  = length ts
    nTs' = length ts'
    go l = foldM (\θ' (e,t,t') -> withExpr e $ subty θ' t t') θ l  
      {-do  θs <- mapM (\(e,t,t') -> withExpr e $ subty θ t t') l-}
      {-    case θs of [] -> return θ-}
      {-               _  -> joinSubsts θ θs-}


-----------------------------------------------------------------------------
subtyUnions :: Subst -> [Type] -> [Type] -> STM Subst
-----------------------------------------------------------------------------
subtyUnions θ xs ys
  | any isTop ys  = return θ
  -- Do not allow casts here
  | otherwise     = haltCasts $ allM θ xs ys
    where
      allM θ (x:xs) ys  = anyM θ x ys >>= \θ -> allM θ xs ys
      allM θ []     _   = return θ
      anyM θ t (t':ts') = tryWithBackup (haltCasts $ subtyNoUnion θ t t') (anyM θ t ts')
      anyM θ _      []  = addError (errorSubType "U" (toType <$> xs) (toType <$> ys)) θ


-----------------------------------------------------------------------------
subtyNoUnion :: Subst -> Type -> Type -> STM Subst
-----------------------------------------------------------------------------
-- | Reject union types
subtyNoUnion' _ (TApp TUn _ _ ) _ = error $ bugBadUnions "subtyNoUnion-1"
subtyNoUnion' _ _ (TApp TUn _ _ ) = error $ bugBadUnions "subtyNoUnion-2"
-- | Top
subtyNoUnion' θ _ t' 
  | isTop t'       = return θ
-- | Undefined
subtyNoUnion' θ t t'
  | isUndefined t && isUndefined t'  = return θ
  | isUndefined t && isNull t'       = return θ
  | isUndefined t                    = addError (errorSubType "subtyNoUnion" t t') θ
-- | Null
subtyNoUnion' θ t t'
  | isNull t && isNull t'       = return θ
  | isNull t                    = addError (errorSubType "subtyNoUnion" t t') θ

-- | Defined types
subtyNoUnion' θ t@(TApp (TDef _) _ _) t'@(TApp (TDef _) _ _) = subTyDef θ t t'

-- | Expand the type definitions
subtyNoUnion' θ t@(TApp (TDef _) _ _) t' = liftM (unfoldTDefSafe t) (st_tdefs <$> get) >>= subty θ t'
subtyNoUnion' θ t t'@(TApp (TDef _) _ _) = liftM (unfoldTDefSafe t') (st_tdefs <$> get) >>= subty θ t

-- | Object subtyping
subtyNoUnion' θ t@(TObj bs _) t'@(TObj bs' _)
  | l < l'          = addError (errorObjSubtyping t t') θ
  -- All keys in the right hand should also be in the left hand
  | k' L.\\ k == [] = 
    do  eo <- st_expr <$> get 
        case eo of 
          Just (ObjectLit _ bl) -> subtys θ (es bl) ts ts'
          _                     -> subtys θ (repeat Nothing) ts ts' 
    where 
      (k,k')  = (map b_sym) `mapPair` (bs, bs')
      (l,l')  = length      `mapPair` (bs, bs')
      es bl   = [Just $ snd b | b <- bl, (F.symbol $ fst b) `elem` k']
      ts      = [b_type b     | b <- bs, (b_sym b) `elem` k']
      ts'     = b_type <$> bs'

-- | Function subtyping: Contravariant in argumnets, Covariant in return type
subtyNoUnion' θ (TFun xts t _) (TFun xts' t' _) = 
  do  e <- st_expr <$> get
      θ' <- subtys θ (es e) ts' ts  -- contravariant argument types
      subty θ' t t'                 -- covariant return type
  where
    ts   = b_type <$> xts
    ts'  = b_type <$> xts'
    es e = case e of Just (CallExpr _ _ es) -> Just <$> es
                     _                      -> repeat Nothing

subtyNoUnion' θ t t' = undefined

subtyNoUnion θ t t' = subtyNoUnion' θ t t'
{-subtyNoUnion θ t t' = -}
{-  tracePP "subTyNoUnion Returns" <$> subtyNoUnion' θ (trace -}
{-  (printf "subtyNoUnion: %s <: %s" (ppshow t) (ppshow t')) t) t'-}


--------------------------------------------------------------------------------
subTyDef :: Subst -> Type -> Type -> STM Subst
--------------------------------------------------------------------------------
subTyDef θ t@(TApp d@(TDef _) ts _) t'@(TApp d'@(TDef _) ts' _) =
  do 
    seen <- tracePP "knonw aliases" <$> st_aliases <$> get 
    if d == d' || (d,d') `elem` seen
      then unifys θ ts ts'
      else 
        do  u  <- unfoldTDefSafeST $ tracePP "Unfolding 1" t
            u' <- unfoldTDefSafeST $ tracePP "Unfolding 2" t'
            -- Populate the state for mutual recursive types
            addTAlias (d,d')
            subTyDef θ (tracePP "recurse 1 " u) (tracePP "recurse 2" u')
subTyDef θ t@(TApp (TDef _) _ _) t'  = unfoldTDefSafeST t >>= \u  -> subtyNoUnion θ u t'
subTyDef θ t t'@(TApp (TDef _) _ _)  = unfoldTDefSafeST t'>>= \u' -> subtyNoUnion θ t u'
subTyDef θ t t'                      = subtyNoUnion θ (trace "subTyDef" t) t'




--------------------------------------------------------------------------------
-- Adding Casts ----------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Add a cast from the disjunction of types @fromTs@ to the disjunction of 
--   @toTs@. Resort to dead code annotation if this fails.
--------------------------------------------------------------------------------
castTs :: Subst -> [Type] -> [Type] -> STM Subst
--------------------------------------------------------------------------------
castTs θ fromTs toTs = get >>= use . subs
  where
    -- If there is no possiblity for a subtype, require that this is dead 
    -- code, and freely give exactly the type that is expected
    use [] = addDeadCast (mkUnion toTs) >> return θ
    use ts = addDownCast (mkUnion ts)   >> return θ
    -- IMPORTANT: Don't allow casts when checking subtyping here
    subs s = L.nub [ a | a <- fromTs, b <- toTs, execSubTypeB (noCasts s) θ a b ]


upCastTs ts ts' θ = go (mkUnion ts) (mkUnion ts') >> return θ
  -- The only case where an upcast will not be added
  where go t t' | apply θ t == apply θ t'   = return ()
  -- t <: t' so it's safe to cast the current expression to t'
        -- go t t' | otherwise                 = addUpCast t'
        go t t' | otherwise                 = 
          addUpCast (tracePP ("UpCasting from " ++ ppshow t ++ "to") t')


--------------------------------------------------------------------------------
addDownCast :: Type -> STM ()
--------------------------------------------------------------------------------
addDownCast t = 
  do  e <- mfromJust "addDownCast" <$> getExpr 
      let l = ann $ getAnnotation e
      modify $ \st -> st { st_casts = M.insert e (DCST t) (st_casts st) }


--------------------------------------------------------------------------------
addUpCast :: Type -> STM ()
--------------------------------------------------------------------------------
addUpCast t = 
  do  e <- mfromJust "addUpCast" <$> getExpr 
      let l = ann $ getAnnotation e
      modify $ \st -> st { st_casts = M.insert e (UCST t) (st_casts st) }


--------------------------------------------------------------------------------
addDeadCast :: Type -> STM ()
--------------------------------------------------------------------------------
addDeadCast t = 
  do  e <- mfromJust "addDeadCast" <$> getExpr 
      let l = ann $ getAnnotation e
      modify $ \st -> st { st_casts = M.insert e (DC t) (st_casts st) } 



-- | Ensure that casts are disabled while @action@ is executed.
-----------------------------------------------------------------------------
haltCasts :: STM a -> STM a
-----------------------------------------------------------------------------
haltCasts action = 
  do  c <- st_docasts <$> get       
      modify noCasts
      r <- action
      modify (\st -> st { st_docasts = c     })
      return r


-----------------------------------------------------------------------------
noCasts :: STState -> STState
-----------------------------------------------------------------------------
noCasts st = st { st_docasts = False }


instance (PP a) => PP (Cast a) where
  pp (UCST t) = text "Upcast  : " <+> pp t
  pp (DCST t) = text "Downcast: " <+> pp t
  pp (DC   t) = text "Deadcast: " <+> pp t




-----------------------------------------------------------------------------
-- Unfolding ----------------------------------------------------------------
-----------------------------------------------------------------------------

-- | Unfold the FIRST TDef at any part of the type @t@.
-------------------------------------------------------------------------------
unfoldTDefDeep :: Type -> Env Type -> Type
-------------------------------------------------------------------------------
unfoldTDefDeep t env = go t
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


-- | Unfold a type definition once. Return Just t, where t is the unfolded type 
-- if there was an unfolding, otherwise Nothing.
-- TODO: Restore toplevel refinements
-------------------------------------------------------------------------------
unfoldTDefMaybe :: (PP r, F.Reftable r) => RType r -> Env (RType r) -> Either String (RType r)
-------------------------------------------------------------------------------
unfoldTDefMaybe t@(TApp (TDef id) acts _) env =
      case envFindTy (F.symbol id) env of
        Just (TBd (TD _ vs bd _ )) -> Right $ apply (fromList $ zip vs acts) bd
        _                          -> Left  $ (printf "Failed unfolding: %s" $ ppshow t)
-- The only thing that is unfoldable is a TDef.
-- The rest are just returned as they are.
unfoldTDefMaybe t                       _   = Right t


-- | Force a successful unfolding
-------------------------------------------------------------------------------
unfoldTDefSafe :: (PP r, F.Reftable r) => RType r -> Env (RType r) -> RType r
-------------------------------------------------------------------------------
unfoldTDefSafe t env = either error id $ unfoldTDefMaybe t env


-- | Monadic versions
-------------------------------------------------------------------------------
unfoldTDefDeepST :: Type -> STM Type
-------------------------------------------------------------------------------
unfoldTDefDeepST t = liftM (unfoldTDefDeep t) (st_tdefs <$> get)


-------------------------------------------------------------------------------
unfoldTDefSafeST :: Type -> STM Type
-------------------------------------------------------------------------------
unfoldTDefSafeST t = liftM (unfoldTDefSafe t) (st_tdefs <$> get)

