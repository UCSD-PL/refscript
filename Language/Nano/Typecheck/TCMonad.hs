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
  , SCache, subType, subTypes, getCache, isSubtype

  -- * Get Type Signature 
  , getDefType 

  -- * Unfold type definition
  , unfoldTDef, unfoldTDefM, unfoldTDefMaybe

  -- * Patch the program with assertions
  , patchPgm

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
import           Language.Nano.Misc             (mapSndM)
import           Data.Monoid                  
import qualified Data.HashSet             as S
import qualified Data.HashMap.Strict      as HM
import qualified Data.Map                 as M
import qualified Data.List                as L
import           Data.Maybe                     (fromJust)
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
                   -- Assertions
                   , tc_asrt  :: M.Map SourceSpan Type
                   -- Function definitions
                   , tc_defs  :: !(Env Type) 
                   -- Type definitions
                   , tc_tdefs :: !(Env Type)
                   -- The currently typed expression 
                   , tc_expr  :: Maybe (Expression AnnSSA)
                   -- Keep track of mutually recursive type constructors
                   , tc_mut   :: [(TCon,TCon)]
                   -- Cache subtyping relations
                   , tc_cache :: SCache
                   }

type TCM     = ErrorT String (State TCState)

-- TODO: replace with something more efficient
type SCache  = [(Type, Type)]

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

initState :: Nano z (RType r) -> TCState
initState pgm = TCS tc_errss tc_errs tc_subst tc_cnt tc_anns tc_annss 
                    tc_asrt tc_defs tc_tdefs tc_expr tc_mut tc_cache
  where
    tc_errss = []
    tc_errs  = []
    tc_subst = mempty 
    tc_cnt   = 0
    tc_anns  = HM.empty
    tc_annss = []
    tc_asrt  = M.empty
    tc_defs  = envMap toType $ defs pgm
    tc_tdefs = envMap toType $ tDefs pgm
    tc_expr  = Nothing
    tc_mut   = []
    tc_cache = []


-- | Instantiate the type body pointed to by "TDef id" with the actual types in
-- "acts". Here is the only place we shall allow TDef, so after this part TDefs
-- should be eliminated. 

-------------------------------------------------------------------------------
unfoldTDefM :: Type -> TCM Type
-------------------------------------------------------------------------------
unfoldTDefM t = tc_tdefs <$> get >>= return . unfoldTDef t

-------------------------------------------------------------------------------
unfoldTDef :: Type -> Env Type -> Type
-------------------------------------------------------------------------------
unfoldTDef t env = go t
  where 
    go (TFun its ot r)         = TFun ((appTBi go) <$> its) (go ot) r
    go (TObj bs r)             = TObj ((appTBi go) <$> bs) r
    go (TBd  _)                = error "unfoldTDef: there should not be a TBody here"
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
        _                          -> error $ errorUnboundId id
unfoldTDefMaybe _                       _   = Nothing
   


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
              

----------------------------------------------------------------------------------
unifyTypes :: AnnSSA -> String -> [Type] -> [Type] -> TCM Subst
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


----------------------------------------------------------------------------------
subType :: AnnSSA -> Maybe (Expression AnnSSA) -> Type -> Type -> TCM ()
----------------------------------------------------------------------------------
subType l eo t t' = subTypes l [eo] [t] [t'] >> return ()

----------------------------------------------------------------------------------
subTypes :: AnnSSA -> [Maybe (Expression AnnSSA)] -> [Type] -> [Type] -> TCM Subst
----------------------------------------------------------------------------------
subTypes l es t1s t2s
  | length t1s /= length t2s = getSubst >>= logError (ann l) errorArgMismatch
  | otherwise = 
    do
      θ  <- getSubst 
      θ' <- tracePP (printf "SubTypes %s <: %s in %s" (ppshow t1s) (ppshow t2s) (ppshow θ)) <$> subtys θ es t1s t2s
      accumErrs l
      setSubst θ'
      return θ'

getCache :: TCM ([(Type, Type)])
getCache = tc_cache <$> get



-- | Join a list of Subst 
----------------------------------------------------------------------------------
joinSubsts :: [Subst] -> TCM Subst
----------------------------------------------------------------------------------
joinSubsts θs = foldM (\θ1 θ2 -> tracePP (printf "Joining substs: %s ++ %s" (ppshow θ1) (ppshow θ2)) <$> joinSubst θ1 θ2) mempty θs


-- | Join two substitutions
-- When a key is present in both Subst then be conservative, i.e. use the
-- supertype of the bindings *if one exists*, otherwise flag an error
----------------------------------------------------------------------------------
joinSubst :: Subst -> Subst -> TCM Subst
----------------------------------------------------------------------------------
joinSubst (Su m1) (Su m2) =
  do 
    θ     <- getSubst 
    e     <- getExpr
    s     <- get
    cmnV  <- zipWithM (\t1 t2 ->  tracePP (printf "Joining types: (%s <: %s)" (ppshow t1) (ppshow t2)) <$> join s θ e t1 t2) (sureMap cmnK m1) (sureMap cmnK m2)
    return $ Su $ only1 `HM.union` only2 `HM.union` (HM.fromList $ zip cmnK cmnV)
      where 
        cmnK         = HM.keys $ m1 `HM.intersection` m2
        only1        = foldr HM.delete m1 cmnK
        only2        = foldr HM.delete m2 cmnK
        sureMap s m  = map (\k -> fromJust $ HM.lookup k m) s
        join s θ e t t' | success s $ subty θ e t t' = return t'
                        | success s $ subty θ e t' t = return t
                        | otherwise                  = addError (printf "Cannot join %s with %s" (ppshow t) (ppshow t')) t


-----------------------------------------------------------------------------
unify :: Subst -> Type -> Type -> TCM Subst
-----------------------------------------------------------------------------
unify θ (TFun xts t _) (TFun xts' t' _) = 
  unifys θ (t: (b_type <$> xts)) (t': (b_type <$> xts'))

unify θ t@(TApp (TDef s) ts _) t'@(TApp (TDef s') ts' _) 
  | s == s'   = unifys θ ts ts'
  | otherwise = addError (errorUnification t t') θ

unify θ t@(TApp (TDef _) _ _) t'        =
  tc_tdefs <$> get >>= return . unfoldTDef t >>= unify θ t'

unify θ t t'@(TApp (TDef _) _ _)        =
  tc_tdefs <$> get >>= return . unfoldTDef t' >>= unify θ t

unify θ (TVar α _)     (TVar β _)       = varEqlM θ α β 
unify θ (TVar α _)     t                = varAsnM θ α t 
unify θ t              (TVar α _)       = varAsnM θ α t

unify θ (TApp TUn ts _) (TApp TUn ts' _)
  | unifiable ts && unifiable ts' 
      && subset ts ts'                  = return $ θ
  where
    -- Simple check to prohibit multiple type vars in a union type
    -- Might need a stronger check here.
    unifiable ts = length (filter var ts) < 2 
    var (TVar _ _) = True
    var _          = False

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
unifys         ::  Subst -> [Type] -> [Type] -> TCM Subst
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
varEqlM :: Subst -> TVar -> TVar -> TCM Subst
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
varAsn :: Subst -> TVar -> Type -> Either String Subst
-----------------------------------------------------------------------------
varAsn θ α t 
  | t == tVar α          = Right $ θ 
  | α `S.member` free t  = Left  $ errorOccursCheck α t 
  | unassigned α θ       = Right $ θ `mappend` (Su $ HM.singleton α t) 
  | otherwise            = Left  $ errorRigidUnify α t
  
unassigned α (Su m) = HM.lookup α m == Just (tVar α)

-----------------------------------------------------------------------------
varAsnM :: Subst -> TVar -> Type -> TCM Subst
-----------------------------------------------------------------------------
varAsnM θ a t = 
  case varAsn θ a t of 
    Left s -> addError s θ
    Right θ' -> return θ'

-- | Subtyping without unions
-----------------------------------------------------------------------------
subtyNoUnion :: Subst -> Maybe (Expression AnnSSA) -> Type -> Type -> TCM Subst
-----------------------------------------------------------------------------

-- | Reject union types here
subtyNoUnion _ _ (TApp TUn _ _ ) _ = error $ bugBadUnions "subtyNoUnion-1"
subtyNoUnion _ _ _ (TApp TUn _ _ ) = error $ bugBadUnions "subtyNoUnion-2"

-- | Top 
subtyNoUnion θ _ _ t' 
  | isTop t'       = return θ

-- | Undefined
subtyNoUnion θ _ t _
  | isUndefined t  = return θ 

-- | Null
subtyNoUnion θ _ t t'
  | isNull t && isUndefined t' = addError errorNullUndefined θ
  | isNull t       = return θ 

-- | Defined types
subtyNoUnion θ e t@(TApp (TDef _) _ _) t'@(TApp (TDef _) _ _) = 
  subtdef θ e t t'

-- | Expand the type definitions
subtyNoUnion θ e t@(TApp (TDef _) _ _) t'        =
  tc_tdefs <$> get >>= return . unfoldTDef t >>= subty θ e t'

subtyNoUnion θ e t t'@(TApp (TDef _) _ _)        =
  tc_tdefs <$> get >>= return . unfoldTDef t' >>= subty θ e t

-- | Object subtyping
subtyNoUnion θ e t@(TObj bs _) t'@(TObj bs' _)
  | l < l'          = addError (errorObjSubtyping t t') θ
  -- All keys in the right hand should also be in the left hand
  | k' L.\\ k == [] = subtys θ es ts ts'
    where 
      (k,k')   = {- tracePP "subObjKeys" $ -} (map b_sym) `mapPair` (bs, bs')
      l        = length bs
      l'       = length bs'
      es       = replicate l' e
      (ts,ts') = {- tracePP "subObjTypes" -}
        ([b_type b | b <- bs, (b_sym b) `elem` k'], b_type <$> bs')

subtyNoUnion θ _ t t' = unify θ t t'

subtyNoUnion' θ e t t' = subtyNoUnion θ e t t'


-----------------------------------------------------------------------------
subtdef :: Subst -> Maybe (Expression AnnSSA) -> Type -> Type -> TCM Subst
-----------------------------------------------------------------------------
subtdef θ e t@(TApp d@(TDef _) ts _) t'@(TApp d'@(TDef _) ts' _) = 
  do 
    seen <- tc_mut <$> get 
    -- Proved subtyping -- no need to clear the state for mutual recursive
    -- types, as this could be used later on as well
    if d == d' || (d,d') `elem` seen 
      then unifys θ ts ts' -- invariant in type arguments
      else 
      do  u  <- unfoldTDefM t
          u' <- unfoldTDefM t'
          -- Populate the state for mutual recursive types
          modify (\s -> s { tc_mut = (d,d'):(tc_mut s) })
          subtdef θ e u u'

subtdef θ e t@(TApp (TDef _) _ _) t'  = unfoldTDefM t >>= \u  -> subtyNoUnion θ e u t'
subtdef θ e t t'@(TApp (TDef _) _ _)  = unfoldTDefM t'>>= \u' -> subtyNoUnion θ e t u'                                                      
subtdef θ e t t'                      = subtyNoUnion' θ e t t'


-----------------------------------------------------------------------------
-- | General Subtyping -- including unions
-----------------------------------------------------------------------------
subty' :: Subst -> Maybe (Expression AnnSSA) -> Type -> Type -> TCM Subst
-----------------------------------------------------------------------------
subty' θ e   (TApp TUn ts _ ) t'@(TApp TUn ts' _) = tryWithBackup (foldM (\θ t -> subty θ e t t') θ ts) (cast θ ts ts')
subty' θ e t@(TApp TUn ts _ ) t'                  = tryWithBackups (subtyUnions θ e ts [t']) [unify θ t t', cast θ ts [t']]
subty' θ e t                  t'@(TApp TUn ts' _) = tryWithBackups (subtyUnions θ e [t] ts') [unify θ t t', cast θ [t] ts']
subty' θ e t                  t'                  = subtyNoUnion' θ e (traceShow "sub no union lhs" t) (traceShow "sub no union rhs" t')

subty  θ e t t' = tryWithSuccessAndBackup (subty' θ e t t') succ (return θ)
  where 
    succ θ' = addSubCache t t' >> return θ'
    addSubCache t t' = modify $ \st -> st {tc_cache = if (t,t')  `L.elem` (tc_cache st) then tc_cache st else (t,t'):(tc_cache st)}

cast θ xs ys 
-- TOGGLE CASTS 
--  | S.size (isct xs ys) > 0 = addCast (mkUnion $ S.toList $ isct xs ys) >> return θ
  | otherwise               = addError (errorSubType "No Cast" xs ys) θ
  where
    isct xs ys = (S.fromList xs) `S.intersection` (S.fromList ys)


-----------------------------------------------------------------------------
subtyUnions :: Subst -> Maybe (Expression AnnSSA) -> [Type] -> [Type] -> TCM Subst
-----------------------------------------------------------------------------
subtyUnions θ e xs ys
  | any isTop ys  = return θ
  | otherwise     = allM θ e xs ys
    where
      allM θ e (x:xs) ys  = anyM θ e x ys >>= \θ -> allM θ e xs ys
      allM θ _ [] _       = return θ
      anyM θ e t (t':ts') = tryWithBackup (subtyNoUnion θ e t t') (anyM θ e t ts')
      anyM θ _ _ []        = addError (errorSubType "U" xs ys) θ

-- | Try to execute the operation in the first argument's monad. 
-- And if it fails try successively the operations in the second 
-- argument list.
-----------------------------------------------------------------------------
tryWithBackups :: TCM a -> [TCM a] -> TCM a
-----------------------------------------------------------------------------
tryWithBackups = foldl tryWithBackup
  

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
isSubtype :: Nano z (RType r) -> Type -> Type -> Bool 
-----------------------------------------------------------------------------
isSubtype pgm t t' = success (initState pgm) $ subty' mempty Nothing (trace (printf "lhs: %s" (show t)) $ t) (trace (printf "rhs: %s" (show t')) $ t')


-- | Try to execute the operation in the first argument's monad. 
-- And if it fails try the operations in the second argument.
-----------------------------------------------------------------------------
tryWithBackup :: TCM a -> TCM a -> TCM a
-----------------------------------------------------------------------------
tryWithBackup act bkup = tryWithSuccessAndBackup act return bkup

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


-- TODO Make this more generic
-----------------------------------------------------------------------------
tryIt ::     
             (TCState -> (b, TCState))    -- Clear the initial state and get some info
          -> (TCState -> b -> TCState)    -- Restore state
          -> TCState                      -- The initial state 
          -> TCM a                        -- The monadic computation 
          -> (Either [String] a, TCState) -- Result of computation
-----------------------------------------------------------------------------
tryIt c a st f = 
  let (errs, stc) = c st in
  let (res , st') = case runState (runErrorT $ f ) stc of 
                      (Left err, s) -> (Left [err], s)
                      (Right x , s) -> 
                        (applyNonNull (Right x) Left (reverse $ tc_errs s), s)
  in
  let st''        = a st' errs in
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
subtys ::  Subst -> [Maybe (Expression AnnSSA)] -> [Type] -> [Type] -> TCM Subst
-----------------------------------------------------------------------------
subtys θ es ts ts'
  | nTs == nTs' = go $ zip3 es ts ts'
  | otherwise   = addError (errorSubType "" ts ts) θ
  where
    nTs  = length ts
    nTs' = length ts'
    {-sub θ e t t' = tryError (subty θ e t t') -}
    go l = 
      do  θs <- mapM (\(e,t,t') -> setExpr e >> subty θ e t t') l
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
  do  eo <- getExpr
      case eo of 
      -- Add the cast (assertion) to the state
      -- Not the AST
        Just e -> addAsrt e $ tracePP (printf "Casting %s (%s)" (ppshow e) (ppshow $ getAnnotation e)) t
        _      -> logError dummySpan "NO CAST" ()

addAsrt e t = modify $ \st -> st { tc_asrt = M.insert ss t (tc_asrt st) } 
  where 
    ss = {- tracePP "Adding" $ -} ann $ getAnnotation e


--------------------------------------------------------------------------------
-- | Insert the assertions as annotations in the AST
--------------------------------------------------------------------------------

-------------------------------------------------------------------------------
patchPgm  :: Nano AnnType (RType r) -> TCM (Nano AnnAsrt (RType r))
-------------------------------------------------------------------------------
patchPgm p@(Nano {code = Src fs})
  = do fs' <- patchFuns fs
       return p {code = Src fs'}


patchFuns  = mapM patchFun

patchFun   = patchStmt

patchStmts = mapM patchStmt'

patchStmt' s = patchStmt {- $ tracePP "patch stmt" -} s

patchStmt (BlockStmt a sts)         = BlockStmt a <$> patchStmts sts
patchStmt e@(EmptyStmt _)           = return $ e
patchStmt (ExprStmt a e)            = ExprStmt a <$> patchExpr e
patchStmt (IfStmt a e s1 s2)        = liftM3 (IfStmt a) (patchExpr e) (patchStmt s1) (patchStmt s2)
patchStmt (IfSingleStmt a e s)      = liftM2 (IfSingleStmt a) (patchExpr e) (patchStmt s)
patchStmt (ReturnStmt a (Just e))   = ReturnStmt a . Just <$> patchExpr e
patchStmt r@(ReturnStmt _ _ )       = return $ r
patchStmt (VarDeclStmt a vds)       = VarDeclStmt a <$> mapM patchVarDecl vds
patchStmt (FunctionStmt a id as bd) = FunctionStmt a id as <$> patchStmts bd
patchStmt s                         = return $ error $ "Does not support patchStmt for: " 
                                                  ++ ppshow s

patchExprs = mapM patchExpr

annt e a = 
  do  m <- tc_asrt <$> get
      case M.lookup key m of
        Just t -> return $ a { ann_fact = (Assert $ trace (printf "Found %s" $ ppshow key) t) : (ann_fact a) } 
        _      -> return $ a
        where 
          key = ann $ getAnnotation e

patchExpr' e@(StringLit _ _ )        = return $ e 
patchExpr' e@(NumLit _ _ )           = return $ e 
patchExpr' e@(IntLit _ _ )           = return $ e 
patchExpr' e@(BoolLit _ _)           = return $ e 
patchExpr' e@(NullLit _)             = return $ e 
patchExpr' e@(ArrayLit a es)         = liftM2 ArrayLit (annt e a) (patchExprs es)
patchExpr' e@(ObjectLit a pes)       = liftM2 ObjectLit (annt e a) $
                                        mapM (mapSndM patchExpr) pes
patchExpr' e@(ThisRef _)             = return $ e
patchExpr' e@(VarRef a id)           = do a' <- annt e a
                                          return $ VarRef a' id
patchExpr' e@(PrefixExpr a p e')     = do a' <- annt e a
                                          PrefixExpr a' p <$> patchExpr e'
patchExpr' e@(InfixExpr a o e1 e2)   = do a' <- annt e a 
                                          liftM2 (InfixExpr a' o) (patchExpr e1) (patchExpr e2)
patchExpr' e@(AssignExpr a o lv e')  = do a' <- annt e a 
                                          AssignExpr a' o lv <$> patchExpr e'
patchExpr' e@(CallExpr a e' el)      = do a' <- annt e a
                                          liftM2 (CallExpr a') (patchExpr e') (patchExprs el)
patchExpr' e@(FuncExpr a oi is ss)   = do a' <- annt e a
                                          FuncExpr a' oi is <$> patchStmts ss
patchExpr' e@(DotRef a b f)          = do a' <- annt e a 
                                          b' <- patchExpr b
                                          return $ DotRef a' b' f
patchExpr' e                         = return $ error $ "Does not support patchExpr for: "
                                                  ++ ppshow e
                                                  
-------------------------------------------------------------------------------
patchExpr :: Expression AnnType -> TCM (Expression AnnType)
-------------------------------------------------------------------------------
patchExpr = liftM go . patchExpr'
  where 
  go e = 
    case L.find asrt $ {- tracePP ("patching " ++ ppshow e) $ -}  ann_fact $ getAnnotation e of
      Just (Assert t) -> CallExpr ann name {- $ tracePP "adding" -} $ arg e t
      _               -> e
  asrt (Assert _) = True
  asrt _          = False
  ann = Ann dummySpan []
  name = VarRef ann (Id ann "__cast")
  arg e t = [e, StringLit ann $ ppshow t]

patchVarDecl (VarDecl a id (Just e)) = do e' <- patchExpr' e
                                          return $ VarDecl a id $ Just e'
patchVarDecl v                         = return v
