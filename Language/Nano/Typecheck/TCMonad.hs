{- LANGUAGE TypeSynonymInstances #-}
{- LANGUAGE FlexibleInstances    #-}
{- LANGUAGE NoMonomorphismRestriction #-}
{- LANGUAGE ScopedTypeVariables  #-}

-- | This module has the code for the Type-Checker Monad. 
--   You /need not/ modify the code here, just use the exported API.

module Language.Nano.Typecheck.TCMonad (
  -- * TC Monad
    TCM
 
  -- * Execute 
  , execute

  -- * Log Errors
  , logError
  
  -- * Error Action
  , tcError

  -- * Freshness
  , freshTyArgs

  -- * Substitutions
  , getSubst, setSubst

  -- * Annotations
  , accumAnn
  , getAllAnns
  , addCast

  -- * Unification
  , unifyType, unifyTypes

  -- * Subtyping
  , subType, subTypes, isSubtype

  -- * Get Type Signature 
  , getDefType 

  -- * Unfold type definition
  , unfoldTDefSafe, unfoldTDefDeep, unfoldTDefSafeM, unfoldTDefDeepM

  -- * Patch the program with assertions
  , patchPgmM

  -- * Set the current expression
  , setExpr
  )  where 

import           Text.Printf
import           Control.Applicative            ((<$>))
import           Control.Monad.State
import           Control.Monad.Error
import           Language.Fixpoint.Misc 
import qualified Language.Fixpoint.Types as F

import           Language.Nano.Env
import           Language.Nano.Types
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Errors
import           Language.Nano.Visitor.Visitor
import           Language.Nano.Misc             (mapSndM)
import           Data.Monoid                  
import qualified Data.HashSet             as HS
import qualified Data.HashMap.Strict      as HM
import qualified Data.Map                 as M
import qualified Data.List                as L
import qualified Data.Set                 as S
import           Data.Maybe                     (fromJust)
import           Data.Generics                   
import           Data.Generics.Aliases
import           Data.Generics.Schemes
import           Data.Typeable
import           Language.ECMAScript3.Parser    (SourceSpan (..))
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations

import           Debug.Trace hiding (traceShow)
import           Language.Nano.Misc               ()

-------------------------------------------------------------------------------
-- | Typechecking monad -------------------------------------------------------
-------------------------------------------------------------------------------

data TCState = TCS { 
                   -- Errors
                     tc_errss :: ![(SourceSpan, String)]
                   , tc_errs  :: ![String]
                   , tc_subst :: !Subst
                   , tc_cnt   :: !Int
                   -- Annotations
                   , tc_anns  :: AnnInfo
                   , tc_annss :: [AnnInfo]
                   -- Assertions (back the comparison on SourceSpan with 
                   -- a check on the AST Expression node
                   , tc_casts :: M.Map SourceSpan (Expression AnnSSA, Type)
                   -- Dead casts: this will require from the liquid system 
                   -- to prove that this code is dead
                   , tc_dead  :: M.Map SourceSpan (Expression AnnSSA, Type)
                   -- Function definitions
                   , tc_defs  :: !(Env Type) 
                   -- Type definitions
                   , tc_tdefs :: !(Env Type)
                   -- The currently typed expression 
                   , tc_expr  :: Maybe (Expression AnnSSA)
                   -- Keep track of mutually recursive type constructors
                   , tc_mut   :: [(TCon,TCon)]
                   }

type TCM     = ErrorT String (State TCState)

-------------------------------------------------------------------------------
getSubst :: TCM Subst
-------------------------------------------------------------------------------
getSubst = tc_subst <$> get 

-------------------------------------------------------------------------------
setSubst   :: Subst -> TCM () 
-------------------------------------------------------------------------------
setSubst θ = modify $ \st -> st { tc_subst = θ }

-------------------------------------------------------------------------------
extSubst :: [TVar] -> TCM ()
-------------------------------------------------------------------------------
extSubst βs = getSubst >>= setSubst . (`mappend` θ')
  where 
    θ'      = fromList $ zip βs (tVar <$> βs)


-------------------------------------------------------------------------------
tcError :: (IsLocated l) => l -> String -> TCM a
-------------------------------------------------------------------------------
tcError l msg = throwError $ printf "TC-ERROR at %s : %s" (ppshow $ srcPos l) msg


-------------------------------------------------------------------------------
logError   :: SourceSpan -> String -> a -> TCM a
-------------------------------------------------------------------------------
logError l msg x = (modify $ \st -> st { tc_errss = (l,msg):(tc_errss st)}) >> return x


-------------------------------------------------------------------------------
freshTyArgs :: SourceSpan -> ([TVar], Type) -> TCM Type 
-------------------------------------------------------------------------------
freshTyArgs l (αs, t) 
  = (`apply` t) <$> freshSubst l αs

freshSubst :: SourceSpan -> [TVar] -> TCM Subst
freshSubst l αs
  = do βs <- mapM (freshTVar l) αs
       setTyArgs l βs
       extSubst βs 
       return $ fromList $ zip αs (tVar <$> βs)

setTyArgs l βs 
  = do m <- tc_anns <$> get 
       when (HM.member l m) $ tcError l "Multiple Type Args"
       addAnn l $ TypInst (tVar <$> {- tracePP ("setTA" ++ show l) -} βs)



-------------------------------------------------------------------------------
-- | Managing Annotations: Type Instantiations --------------------------------
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
getAnns :: TCM AnnInfo  
-------------------------------------------------------------------------------
getAnns = do θ     <- tc_subst <$> get
             m     <- tc_anns  <$> get
             let m' = fmap (apply θ . sortNub) m
             _     <- modify $ \st -> st { tc_anns = m' }
             return m' 

-------------------------------------------------------------------------------
addAnn :: SourceSpan -> Fact -> TCM () 
-------------------------------------------------------------------------------
addAnn l f = modify $ \st -> st { tc_anns = inserts l f (tc_anns st) } 

-------------------------------------------------------------------------------
getAllAnns :: TCM [AnnInfo]  
-------------------------------------------------------------------------------
getAllAnns = tc_annss <$> get


-------------------------------------------------------------------------------
accumAnn :: (AnnInfo -> [(SourceSpan, String)]) -> TCM () -> TCM ()
-------------------------------------------------------------------------------
accumAnn check act 
  = do m     <- tc_anns <$> get 
       modify $ \st -> st {tc_anns = HM.empty}
       act
       m'    <- getAnns
       forM_ (check m') $ \(l, s) -> logError l s ()
       modify $ \st -> st {tc_anns = m} {tc_annss = m' : tc_annss st}

-------------------------------------------------------------------------------
execute     :: Nano z (RType r) -> TCM a -> Either [(SourceSpan, String)] a
-------------------------------------------------------------------------------
execute pgm act 
  = case runState (runErrorT act) $ initState pgm of 
      (Left err, _) -> Left [(dummySpan,  err)]
      (Right x, st) ->  applyNonNull (Right x) Left (reverse $ tc_errss st)


-- TODO: write an executeST for the subtype monad -- replace those try...

initState :: Nano z (RType r) -> TCState
initState pgm = TCS tc_errss tc_errs tc_subst tc_cnt tc_anns tc_annss 
                    tc_casts tc_dead tc_defs tc_tdefs tc_expr tc_mut 
  where
    tc_errss = []
    tc_errs  = []
    tc_subst = mempty 
    tc_cnt   = 0
    tc_anns  = HM.empty
    tc_annss = []
    tc_casts = M.empty
    tc_dead  = M.empty 
    tc_defs  = envMap toType $ defs pgm
    tc_tdefs = envMap toType $ tDefs pgm
    tc_expr  = Nothing
    tc_mut   = []


-- | Instantiate the type body pointed to by "TDef id" with the actual types in
-- "acts". Here is the only place we shall allow TDef, so after this part TDefs
-- should be eliminated. 


-- | Unfold the first TDef at any part of the type @t@.
-------------------------------------------------------------------------------
unfoldTDefDeep :: Type -> Env Type -> Type
-------------------------------------------------------------------------------
unfoldTDefDeep t env = go t
  where 
    go (TFun its ot r)         = TFun ((appTBi go) <$> its) (go ot) r
    go (TObj bs r)             = TObj ((appTBi go) <$> bs) r
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
-------------------------------------------------------------------------------
unfoldTDefMaybe :: (PP r, F.Reftable r) => RType r -> Env (RType r) -> Maybe (RType r)
-------------------------------------------------------------------------------
unfoldTDefMaybe (TApp (TDef id) acts _) env = 
      case envFindTy (F.symbol id) env of
        Just (TBd (TD _ vs bd _ )) -> Just $ apply (fromList $ zip vs acts) bd
        _                          -> Nothing
unfoldTDefMaybe _                       _   = Nothing


-- | Force a successful unfolding
-------------------------------------------------------------------------------
unfoldTDefSafe :: (PP r, F.Reftable r) => RType r -> Env (RType r) -> RType r
-------------------------------------------------------------------------------
unfoldTDefSafe t env = maybe (error $ printf "Unfolding of %s failed" $ ppshow t) 
                             id (unfoldTDefMaybe t env)


-- | Monadic versions
-------------------------------------------------------------------------------
unfoldTDefDeepM :: Type -> STS Type
-------------------------------------------------------------------------------
unfoldTDefDeepM t = st_tdefs <$> get >>= return . unfoldTDefDeep t

-------------------------------------------------------------------------------
unfoldTDefSafeM :: Type -> STS Type
-------------------------------------------------------------------------------
unfoldTDefSafeM t = st_tdefs <$> get >>= return . unfoldTDefSafe t



getDefType f 
  = do m <- tc_defs <$> get
       maybe err return $ envFindTy f m 
    where 
       err = tcError l $ errorMissingSpec l f
       l   = srcPos f


-------------------------------------------------------------------------------
accumErrs :: AnnSSA -> TCM ()
-------------------------------------------------------------------------------
accumErrs l
  = do m     <- tc_errs <$> get 
       modify   $ \st -> st {tc_errs = []}
       forM_  m $ \s -> logError (ann l) s ()


--------------------------------------------------------------------------
-- | Generating Fresh Values ---------------------------------------------
--------------------------------------------------------------------------

tick :: TCM Int
tick = do st    <- get 
          let n  = tc_cnt st
          put    $ st { tc_cnt = n + 1 }
          return n 

class Freshable a where 
  fresh :: a -> TCM a 

-- instance Freshable TVar where 
--   fresh _ = TV . F.intSymbol "T" <$> tick

instance Freshable a => Freshable [a] where 
  fresh = mapM fresh

freshTVar l _ =  ((`TV` l). F.intSymbol "T") <$> tick
              



--------------------------------------------------------------------------
-- | Subtype Monad  ------------------------------------------------------
--------------------------------------------------------------------------

data STState = STS { 
                   -- Errors
                     st_err   :: ![(SourceSpan, String)]

                   -- Type definitions
                   , st_tdefs :: !(Env Type)

                   -- st_casts: True if casts are allowed
                   , st_casts :: Bool

                   -- Keep track of mutually recursive type constructors
                   , tc_mut   :: [(TCon,TCon)]

                   ---------------------------------------  
                   , tc_subst :: !Subst
                   , tc_cnt   :: !Int
                   -- Annotations
                   , tc_anns  :: AnnInfo
                   , tc_annss :: [AnnInfo]
                   -- Assertions (back the comparison on SourceSpan with 
                   -- a check on the AST Expression node
                   , tc_casts :: M.Map SourceSpan (Expression AnnSSA, Type)
                   -- Dead casts: this will require from the liquid system 
                   -- to prove that this code is dead
                   , tc_dead  :: M.Map SourceSpan (Expression AnnSSA, Type)
                   -- Function definitions
                   , tc_defs  :: !(Env Type) 
                   -- Type definitions
                   , tc_expr  :: Maybe (Expression AnnSSA)
                   }

type STM     = ErrorT String (State STState)

newSubType θ t t' = 
  case runState (runErrorT $ subTypes θ t t') initState) of
                      (Left err, s) -> Left err
                      (Right x , s) -> 
  where
    initState = STS {}
                  
isSubType






----------------------------------------------------------------------------------
unifyTypes :: AnnSSA -> String -> [Type] -> [Type] -> STM Subst
----------------------------------------------------------------------------------
unifyTypes l _ t1s t2s
  | length t1s /= length t2s = getSubst >>= logError (ann l) errorArgMismatch
  | otherwise                = do θ  <- getSubst 
                                  θ' <- unifys θ t1s t2s
                                  {-addError msg-}
                                  accumErrs l
                                  setSubst θ' 
                                  return θ'
                                  {- Left msg' -> logError (ann l) (msg ++ "\n" ++ msg') θ-}
                                  {- Right θ'  -> setSubst θ' >> return θ' -}

unifyType l m e t t' = unifyTypes l msg [t] [t'] >> return ()
  where 
    msg              = errorWrongType m e t t'


-- | STState.st_casts determines if casts are allowed 
----------------------------------------------------------------------------------
subTypeM :: AnnSSA -> Maybe (Expression AnnSSA) -> Type -> Type -> STM Subst
----------------------------------------------------------------------------------
subTypeM l eo t t' = subTypesM l [eo] [t] [t']

----------------------------------------------------------------------------------
subTypesM :: AnnSSA -> [Maybe (Expression AnnSSA)] -> [Type] -> [Type] -> STM Subst
----------------------------------------------------------------------------------
subTypesM l es t1s t2s
  | length t1s /= length t2s = tcError (ann l) errorArgMismatch
  | otherwise = 
    do
      θ  <- getSubst 
      θ' <- tracePP (printf "SubTypes %s <: %s" (ppshow t1s) (ppshow t2s)) <$> 
              subtys b θ es t1s t2s
      accumErrs l
      setSubst θ'
      return θ'



-- | Join a list of Subst 
----------------------------------------------------------------------------------
joinSubsts :: [Subst] -> STM Subst
----------------------------------------------------------------------------------
joinSubsts θs = foldM (\θ1 θ2 -> 
  {- tracePP (printf "Joining substs: %s ++ %s" (ppshow θ1) (ppshow θ2)) <$> -} 
  joinSubst θ1 θ2) mempty θs


-- | Join two substitutions
-- When a key is present in both Subst then be conservative, i.e. use the
-- supertype of the bindings *if one exists*, otherwise flag an error
----------------------------------------------------------------------------------
joinSubst :: Subst -> Subst -> STM Subst
----------------------------------------------------------------------------------
joinSubst (Su m1) (Su m2) =
  do 
    θ     <- getSubst 
    e     <- getExpr
    s     <- get
    cmnV  <- zipWithM (\t1 t2 ->  
               {-tracePP (printf "Joining types: (%s <: %s)" (ppshow t1) (ppshow t2)) <$> -}
               join s θ e t1 t2) (sureMap cmnK m1) (sureMap cmnK m2)
    return $ Su $ only1 `HM.union` only2 `HM.union` (HM.fromList $ zip cmnK cmnV)
      where 
        cmnK         = HM.keys $ m1 `HM.intersection` m2
        only1        = foldr HM.delete m1 cmnK
        only2        = foldr HM.delete m2 cmnK
        sureMap s m  = map (\k -> fromJust $ HM.lookup k m) s
        join s θ e t t' | success s $ subty False θ e t t' = return t'
                        | success s $ subty False θ e t' t = return t
                        | otherwise                  = addError (printf "Cannot join %s with %s" (ppshow t) (ppshow t')) t


-----------------------------------------------------------------------------
unify :: Subst -> Type -> Type -> STM Subst
-----------------------------------------------------------------------------
unify θ (TFun xts t _) (TFun xts' t' _) = 
  unifys θ (t: (b_type <$> xts)) (t': (b_type <$> xts'))

unify θ t@(TApp (TDef s) ts _) t'@(TApp (TDef s') ts' _)
  | s == s'   = unifys θ ts ts'
  | otherwise = Left $ errorUnification t t'

unify θ t@(TApp (TDef _) _ _) t' =
  st_tdefs <$> get >>= return . unfoldTDefSafe t >>= unify θ t'

unify θ t t'@(TApp (TDef _) _ _)        =
  st_tdefs <$> get >>= return . unfoldTDefSafe t' >>= unify θ t

unify θ (TVar α _)     (TVar β _)       = varEqlM θ α β 
unify θ (TVar α _)     t                = varAsnM θ α t 
unify θ t              (TVar α _)       = varAsnM θ α t

--
--  XXX: Objects, Unions, will have been broken down by subtype 
--  unless unify is being called directly
--
-- unify θ (TApp TUn ts _) (TApp TUn ts' _)
--   | unifiable ts && unifiable ts' 
--       && subset ts ts'                  = return $ θ
--   where
--     -- Simple check to prohibit multiple type vars in a union type
--     -- Might need a stronger check here.
--     unifiable ts = length (filter var ts) < 2 
--     var (TVar _ _) = True
--     var _          = False

unify θ (TApp c ts _) (TApp c' ts' _)
  | c == c'                             = unifys  θ ts ts'

unify _ (TBd _) _ = error $ bugTBodiesOccur "unify"
unify _ _ (TBd _) = error $ bugTBodiesOccur "unify"

unify θ t t' 
  | t == t'                             = return θ
  | isTop t                             = go θ $ strip t'
  | isTop t'                            = go θ $ strip t
  | otherwise                           = addError (errorUnification t t') θ
  where 
    strip (TApp _ xs _ )            = xs
    strip x@(TVar _ _)              = [x]
    strip (TFun xs y _)             = (b_type <$> xs) ++ [y]
    strip (TAll _ x)                = [x]
    strip t                         = error (printf "%s: Not supported in unify - strip" $ ppshow t)
    tops = map $ const tTop
    go θ ts = unifys θ ts $ tops ts



-----------------------------------------------------------------------------
unifys         ::  Subst -> [Type] -> [Type] -> STM Subst
-----------------------------------------------------------------------------
unifys θ xs ys =  {- trace msg $ -} unifys' θ xs ys 
   {-where -}
   {-  msg      = printf "unifys: [xs = %s] [ys = %s]"  (ppshow xs) (ppshow ys)-}

unifys' θ ts ts' 
  | nTs == nTs' = go θ (ts, ts') 
  | otherwise   = addError (errorUnification ts ts') θ
  where 
    nTs                  = length ts
    nTs'                 = length ts'
    go θ (t:ts , t':ts') = unify θ t t' >>= \θ' -> go θ' (mapPair (apply θ') (ts, ts'))
    go θ (_    , _    )  = return θ 


-----------------------------------------------------------------------------
varEqlM :: Subst -> TVar -> TVar -> STM Subst
-----------------------------------------------------------------------------
varEqlM θ α β =  
  do 
    s <- get
    case tryError s $ varAsnM θ α $ tVar β of
      (Right θ1, s1) -> modify (const s1) >> return θ1
      (Left  e1, _ ) -> case tryError s $ varAsnM θ β $ tVar α of
                          (Right θ2, s2) -> modify (const s2) >> return θ2
                          (Left  e2, _ ) -> addError (unlines e1 ++ "\n OR \n" ++ unlines e2) θ


-----------------------------------------------------------------------------
varAsn :: Subst -> TVar -> Type -> STM Subst
-----------------------------------------------------------------------------
varAsn θ α t 
  | t == tVar α          = Right $ θ 
  | α `HS.member` free t = Left  $ errorOccursCheck α t 
  | unassigned α θ       = Right $ θ `mappend` (Su $ HM.singleton α t) 
  | otherwise            = Left  $ errorRigidUnify α t
  
unassigned α (Su m) = HM.lookup α m == Just (tVar α)

-----------------------------------------------------------------------------
varAsnM :: Subst -> TVar -> Type -> STM Subst
-----------------------------------------------------------------------------
varAsnM θ a t = 
  case varAsn θ a t of 
    Left s -> addError s θ
    Right θ' -> return θ'


-- | Subtyping without unions
-- TypeScript lists its subtyping rules in § 3.8.2 of the manual.
-- The rules for subtyping with Null or Undefined type are unsound, so we're
-- using a sound version instead, i.e. Null and Undefined are not subtypes of
-- every type T.
-----------------------------------------------------------------------------
subtyNoUnion :: Subst -> Maybe (Expression AnnSSA) -> Type -> Type -> STM Subst
-----------------------------------------------------------------------------

-- | Reject union types here
subtyNoUnion _ _ (TApp TUn _ _ ) _ = error $ bugBadUnions "subtyNoUnion-1"
subtyNoUnion _ _ _ (TApp TUn _ _ ) = error $ bugBadUnions "subtyNoUnion-2"

-- | Top 
subtyNoUnion θ _ _ t' 
  | isTop t'       = return θ

-- | Undefined
subtyNoUnion θ _ t t'
  | isUndefined t && isUndefined t'  = return θ
  | isUndefined t && isNull t'       = return θ
  | isUndefined t                    = addError (errorSubType "subtyNoUnion" t t') θ

-- | Null
subtyNoUnion θ _ t t'
  | isNull t && isNull t'       = return θ
  | isNull t                    = addError (errorSubType "subtyNoUnion" t t') θ

-- | Defined types
subtyNoUnion θ e t@(TApp (TDef _) _ _) t'@(TApp (TDef _) _ _) = 
  do  st <- get
      case subTyDef st θ e t t' of 
        Just θ  -> return θ
        Nothing -> addError "different type definitions" θ
  

-- | Expand the type definitions
subtyNoUnion θ e t@(TApp (TDef _) _ _) t'        =
  tc_tdefs <$> get >>= return . unfoldTDefSafe t >>= subty θ e t'

subtyNoUnion θ e t t'@(TApp (TDef _) _ _)        =
  tc_tdefs <$> get >>= return . unfoldTDefSafe t' >>= subty θ e t

-- | Object subtyping
subtyNoUnion θ e@(Just (ObjectLit _ bl)) t@(TObj bs _) t'@(TObj bs' _)
  | l < l'          = addError (errorObjSubtyping t t') θ
  -- All keys in the right hand should also be in the left hand
  | k' L.\\ k == [] = subtys θ es ts ts'
    where 
      (k,k')  = (map b_sym) `mapPair` (bs, bs')
      (l,l')  = length `mapPair` (bs, bs')
      es      = [Just $ snd b | b <- bl, (F.symbol $ fst b) `elem` k']
      ts      = [b_type b     | b <- bs, (b_sym b) `elem` k']
      ts'     = b_type <$> bs'




subtyNoUnion θ Nothing t@(TObj bs _) t'@(TObj bs' _) 
  | l < l'          = addError (errorObjSubtyping t t') θ
  | k' L.\\ k == [] = subtys θ es ts ts'
    where 
      (k,k')  = (map b_sym) `mapPair` (bs, bs')
      (l,l')  = length `mapPair` (bs, bs')
      es      = replicate (length ts) Nothing
      ts      = [b_type b | b <- bs, (b_sym b) `elem` k']
      ts'     = b_type <$> bs'



subtyNoUnion θ Nothing t@(TObj bs _) t'@(TObj bs' _)
  | l < l'          = addError (errorObjSubtyping t t') θ
  | k' L.\\ k == [] = castTs θ (tracePP "obj cast 1" [t]) (tracePP "obj cast 2" [t'])
    where
      (k,k')  = (map b_sym) `mapPair` (bs, bs')
      (l,l')  = length `mapPair` (bs, bs')

  -- error (printf "Unimplemented: subtyping on %s :: %s <: %s" 
  --         (ppshow e) (ppshow t) (ppshow t'))
        





-- | Function subtyping: 
--  - Contravariant in argumnets
--  - Covariant in return type
subtyNoUnion b θ (Just e@(CallExpr _ _ es)) (TFun xts t _) (TFun xts' t' _) = 
  subtys θ (Just <$> es) (b_type <$> xts') (b_type <$> xts) >>= 
    \θ' -> subty False θ' (Just e) t t'

subtyNoUnion θ _ t t' = unify θ t t'

subtyNoUnion' θ e t t' = subtyNoUnion θ e t t'



-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
subTyDef θ e t@(TApp d@(TDef _) ts _) t'@(TApp d'@(TDef _) ts' _) =
  do 
    seen <- tracePP "knonw aliases" <$> tc_mut <$> get 
    -- Proved subtyping -- no need to clear the state for mutual recursive
    -- types, as this could be used later on as well
    if d == d' || (d,d') `elem` seen
      then tracePP "Unifying" <$> unifys θ (tracePP "ts" ts) (tracePP "ts'" ts')
      -- invariant in type arguments
      else 
      do  u  <- unfoldTDefSafeM $ tracePP "Unfolding 1" t
          u' <- unfoldTDefSafeM $ tracePP "Unfolding 2" t'
          -- Populate the state for mutual recursive types
          modify (\s -> s { st_mut = (d,d') : (st_mut s) })
          subTyDef θ e (tracePP "recurse 1 "u) (tracePP "recurse 2" u')

subTyDef θ e t@(TApp (TDef _) _ _) t'  = unfoldTDefSafeM t >>= \u  -> subtyNoUnion θ e u t'
subTyDef θ e t t'@(TApp (TDef _) _ _)  = unfoldTDefSafeM t'>>= \u' -> subtyNoUnion θ e t u'
subTyDef θ e t t'                      = subtyNoUnion' θ e t t'


-----------------------------------------------------------------------------
-- | General Subtyping -- including unions
-----------------------------------------------------------------------------
subty :: Subst -> Maybe (Expression AnnSSA) -> Type -> Type -> STM Subst
-----------------------------------------------------------------------------
subty θ e   (TApp TUn ts _ ) t'@(TApp TUn ts' _) = tryWithBackups (subtyUnions b θ e ts ts') [castTs θ ts ts']
subty θ e t@(TApp TUn ts _ ) t'                  = tryWithBackups (subtyUnions b θ e ts [t']) [unify θ t t', castTs θ ts [t']]
subty θ e t t'@(TApp TUn ts' _)                  = tryWithBackups (subtyUnions b θ e [t] ts') [unify θ t t', castTs θ [t] ts']
subty θ e t t'                                   = tryWithBackups (subtyNoUnion' b θ e t t') 
      [trace (printf "(Dead)Cast %s :: %s - %s" (ppshow e) (ppshow t) (ppshow t')) $ castTs θ [t] [t']]


-- | Add a cast from the disjunction of types @fromTs@ to the disjunction of 
--   @toTs@. Resort to dead code annotation if this fails.
-----------------------------------------------------------------------------
castTs :: Subst -> [Type] -> [Type] -> STM Subst
-----------------------------------------------------------------------------
castTs θ fromTs toTs = 
  do proceed <- st_casts <$> get
     if proceed 
      then 
        st <- get
        use $ L.nub [ a | a <- fromTs, b <- toTs, isSubtype (Right st) a b ]
      else
        return θ

  where
-- If there is no possiblity for a subtype, require that this is dead 
-- code, and freely give exactly the type that is expected
    use [] = addDeadCast (mkUnion toTs) >> return θ 
    use ts = addCast (mkUnion ts)       >> return θ


-----------------------------------------------------------------------------
subtyUnions :: Subst -> Maybe (Expression AnnSSA) -> [Type] -> [Type] -> STM Subst
-----------------------------------------------------------------------------
subtyUnions b θ e xs ys
  | any isTop ys  = return θ
  | otherwise     = allM θ e xs ys
    where
      allM θ e (x:xs) ys  = anyM θ e x ys >>= \θ -> allM θ e xs ys
      allM θ _ [] _       = return θ
      -- Do not allow casts in this check
      anyM θ e t (t':ts') = tryWithBackup (haltCasts $ subtyNoUnion θ e t t') (anyM θ e t ts')
      anyM θ _ _ []       = addError (errorSubType "U" xs ys) θ




-- | Ensure that casts are disabled while @action@ is executed.
-----------------------------------------------------------------------------
haltCasts :: STM a -> STM a
-----------------------------------------------------------------------------
haltCasts action = 
  do  c <- st_casts <$> get       
      modify (\st -> STS { st_casts = False })
      r <- action
      modify (\st -> STS { st_casts = c     })
      return r



-- | Try to execute the operation in the first argument's monad. 
-- And if it fails try successively the operations in the second 
-- argument list.
-----------------------------------------------------------------------------
tryWithBackups :: STM a -> [STM a] -> STM a
-----------------------------------------------------------------------------
tryWithBackups = foldl tryWithBackup


-- | Try to execute the operation in the first argument's monad. 
-- And if it fails try the operations in the second argument.
-----------------------------------------------------------------------------
tryWithBackup :: TCM a -> TCM a -> TCM a
-----------------------------------------------------------------------------
tryWithBackup act bkup = tryWithSuccessAndBackup act return bkup
   

-- | Try to execute the operation in the first argument's monad
-- and return true if it succeeds, false otherwise.
-- Ignores the outstate
-----------------------------------------------------------------------------
success:: TCState -> TCM a -> Bool
-----------------------------------------------------------------------------
success s action = 
  case tryError s action of 
    (Left _  , _) -> False
    (Right _ , _) -> True


-----------------------------------------------------------------------------
-- | isSubtype will call subty without letting it resort to casts,
-- otherwise it would always return tyre trivially. Also the state we 
-- use for it is initially empty. 
-----------------------------------------------------------------------------
isSubtype :: Either (Nano z (RType r)) TCState -> RType r -> RType r -> Bool 
-----------------------------------------------------------------------------
isSubtype (Left pgm) t t' = success (initState pgm)
                            $ subty False mempty Nothing (toType t) (toType t')
isSubtype (Right st) t t' = success st
                            $ subty False mempty Nothing
                              (tracePP "isSubtype 1" $ toType t)
                              (tracePP "isSubtype 2" $ toType t')


-----------------------------------------------------------------------------
tryWithSuccessAndBackup :: TCM a -> (a -> TCM a) -> TCM a -> TCM a
-----------------------------------------------------------------------------
tryWithSuccessAndBackup act succ bkup =
  do  s <- get
      case tryError s $ act of 
        (Left _  , _ ) -> bkup
        (Right r , s') -> modify (const s') >> succ r


-----------------------------------------------------------------------------
tryError :: TCState -> TCM a -> (Either [String] a, TCState)
-----------------------------------------------------------------------------
tryError = tryIt clearError applyError


-----------------------------------------------------------------------------
tryIt ::     
             (TCState -> (b, TCState))    -- Clear the initial state and get some info
          -> (TCState -> b -> TCState)    -- Restore state
          -> TCState                      -- The initial state 
          -> TCM a                        -- The monadic computation 
          -> (Either [String] a, TCState) -- Result of computation
-----------------------------------------------------------------------------
tryIt clear restore state action = 
  let (errs, stc) = clear state in
  let (res , st') = case runState (runErrorT $ action ) stc of 
                      (Left err, s) -> (Left [err], s)
                      (Right x , s) -> 
                        (applyNonNull (Right x) Left (reverse $ tc_errs s), s)
  in
  let st''        = restore st' errs in
  (res, st'')

-----------------------------------------------------------------------------
clearError ::     TCState -> ([String], TCState)
-----------------------------------------------------------------------------
clearError s@(TCS {tc_errs=e}) = (e, s {tc_errs=[]})

-----------------------------------------------------------------------------
applyError ::     TCState -> [String] -> TCState
-----------------------------------------------------------------------------
applyError s@(TCS {}) e        = s {tc_errs= e ++ tc_errs s}


-- | Subtype lists of types
-----------------------------------------------------------------------------
subtys :: Bool -> Subst -> [Maybe (Expression AnnSSA)] -> [Type] -> [Type] -> TCM Subst
-----------------------------------------------------------------------------
subtys b θ es ts ts'
  | nTs == nTs' = go $ zip3 es ts ts'
  | otherwise   = addError (errorSubType "" ts ts) θ
  where
    nTs  = length ts
    nTs' = length ts'
    go l = 
      do  θs <- mapM (\(e,t,t') -> setExpr e >> subty b θ e t t') l
          case θs of [] -> return θ
                     _  -> joinSubsts θs


-------------------------------------------------------------------------------
addError   :: String -> a -> TCM a
-------------------------------------------------------------------------------
addError msg x = (modify $ \st -> st { tc_errs = msg:(tc_errs st)}) >> return x


-------------------------------------------------------------------------------
setExpr   :: Maybe (Expression AnnSSA) -> TCM () 
-------------------------------------------------------------------------------
setExpr eo = modify $ \st -> st { tc_expr = eo }

getExpr = tc_expr <$> get


-------------------------------------------------------------------------------
addCast :: Type -> TCM ()
-------------------------------------------------------------------------------
addCast t = 
  do  e <- fromJust <$> getExpr 
      let l = ann $ getAnnotation e
      modify $ \st -> st { tc_casts = M.insert l (e,t) (tc_casts st) } 


-------------------------------------------------------------------------------
addDeadCast :: Type -> TCM ()
-------------------------------------------------------------------------------
addDeadCast t = 
  do  e <- fromJust <$> getExpr 
      let l = ann $ getAnnotation e
      modify $ \st -> st { tc_dead = M.insert l (tracePP "Adding dc" (e,t)) (tc_dead st) } 


--------------------------------------------------------------------------------
-- | Insert casts and dead code casts in the AST
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
patchPgmM :: (Typeable r, Data r) => Nano AnnSSA (RType r) -> TCM (Nano AnnSSA (RType r))
--------------------------------------------------------------------------------
patchPgmM pgm = 
  do  c <- tc_casts <$> get
      d <- tc_dead  <$> get
      return $ everywhere (mkT $ patchExpr c d) pgm

--------------------------------------------------------------------------------
patchExpr :: 
  M.Map SourceSpan (Expression AnnSSA, Type)  ->    -- Cast map
  M.Map SourceSpan (Expression AnnSSA, Type)  ->    -- Dead map
  Expression (Annot Fact SourceSpan) -> Expression (Annot Fact SourceSpan)
--------------------------------------------------------------------------------
patchExpr c d = (castExpr c) . (deadExpr d)

castExpr m e =
  case M.lookup ss m of
    Just (e',t) | e == e' -> Cast (a { ann_fact = (Assume t):fs }) e
    _                     -> e
  where 
    ss = ann a
    fs = ann_fact a
    a  = getAnnotation e

deadExpr m e =
  case M.lookup ss m of
  -- WARNING: checking for expression equality will be skewed by 
  -- the presence of Cast and DeadCast nodes.
    Just  (e',t) -> DeadCast (a { ann_fact = (Assume t):fs }) e
    _            -> e
  where 
    ss = ann a
    fs = ann_fact a
    a  = getAnnotation e

