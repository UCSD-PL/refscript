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

  -- * Unification
  , unifyType, unifyTypes

  -- * Subtyping
  , subType, subTypes

  -- * Get Type Signature 
  , getDefType 

  -- * Unfold type definition
  , unfoldTDef, unfoldTDefM

  -- * Patch the program with assertions
  , patchPgm
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
import qualified Data.HashSet             as HS
import qualified Data.Set                 as S
import qualified Data.HashMap.Strict      as HM
import qualified Data.Map                 as M
import qualified Data.List                as L
import           Language.ECMAScript3.Parser    (SourceSpan (..))
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations

import           Debug.Trace hiding (traceShow)
import           Language.Nano.Misc               ()

-------------------------------------------------------------------------------
-- | Typechecking monad -------------------------------------------------------
-------------------------------------------------------------------------------

data TCState = TCS { tc_errss :: ![(SourceSpan, String)]
                   , tc_errs  :: ![String]
                   , tc_subst :: !Subst
                   , tc_cnt   :: !Int
                   , tc_anns  :: AnnInfo
                   , tc_annss :: [AnnInfo]
                   , tc_asrt  :: M.Map SourceSpan Type
                   , tc_defs  :: !(Env Type) 
                   , tc_tdefs :: !(Env Type)
                   , tc_expr  :: Maybe (Expression AnnSSA)
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

initState :: Nano z (RType r) -> TCState
initState pgm = TCS tc_errss tc_errs tc_subst tc_cnt tc_anns 
                    tc_annss tc_asrt tc_defs tc_tdefs tc_expr 
  where
    tc_errss = []
    tc_errs  = []
    tc_subst = mempty 
    tc_cnt   = 0
    tc_anns  = HM.empty
    tc_annss = []
    tc_asrt  = M.empty
    -- Refinements are lost here ... 
    tc_defs  = tracePP "tc_defs" 
      {- $ envMap (unfoldTDef $ envMap toType $ tDefs pgm) 
      $ tracePP "tc_defs before"-} $ envMap toType $ defs pgm
    tc_tdefs = tracePP "tc_tdefs" $ envMap toType $ tDefs pgm
    tc_expr  = Nothing


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
        _                          -> 
          error $printf "Symbol: %s has not been defined" (show id)
    go (TApp c a r)            = TApp (traceShow "Cons" c) (go <$> (tracePP "Go rec" a)) r
    go t                       = error $ printf "Missed case %s" (ppshow t)
    appTBi f (B s t)           = B s $ f t
    


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
subTypes :: AnnSSA -> [Maybe (Expression AnnSSA)] -> [Type] -> [Type] -> TCM Subst
----------------------------------------------------------------------------------
subTypes l es t1s t2s
  | length t1s /= length t2s = getSubst >>= logError (ann l) errorArgMismatch
  | otherwise                = do θ  <- getSubst
                                  θ' <- subtys θ es t1s t2s 
                                  -- θ' <- subtys θ 
                                  --   (trace (printf "ST: %s :: %s - %s" 
                                  --     (ppshow es) 
                                  --     (ppshow t1s)
                                  --     (ppshow t2s)) es) t1s t2s
                                  accumErrs l
                                  setSubst θ' 
                                  return θ'

----------------------------------------------------------------------------------
subType :: AnnSSA -> Maybe (Expression AnnSSA) -> Type -> Type -> TCM ()
----------------------------------------------------------------------------------
subType l eo t t' = subTypes l [eo] [t] [t'] >> return ()


-----------------------------------------------------------------------------
unify :: Subst -> Type -> Type -> TCM Subst
-----------------------------------------------------------------------------
unify θ (TFun xts t _) (TFun xts' t' _) = 
  unifys θ (t: (b_type <$> xts)) (t': (b_type <$> xts'))

unify θ t@(TApp (TDef s) ts _) t'@(TApp (TDef s') ts' _) 
  | tracePP "s" s == tracePP "sp" s'   = unifys θ ts ts'
  | otherwise = addError (errorUnification t t') θ

unify θ t@(TApp (TDef _) _ _) t'        =
  tc_tdefs <$> get >>= return . unfoldTDef t >>= unify θ t'

unify θ t t'@(TApp (TDef _) _ _)        =
  tc_tdefs <$> get >>= return . unfoldTDef t' >>= unify θ t

unify θ (TVar α _)     (TVar β _)       = trace "unifying" <$> varEql θ α β 
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

unify _ (TBd _) _ = error "NO TBD"  
unify _ _ (TBd _) = error "NO TBD"  

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
    strip _                         = error "Not supported in unify - strip"
    tops = map $ const tTop
    go θ ts = unifys θ ts $ tops ts



unifys         ::  Subst -> [Type] -> [Type] -> TCM Subst
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
varEql :: Subst -> TVar -> TVar -> TCM Subst
-----------------------------------------------------------------------------
varEql θ α β = case varAsn θ α (tVar β) of 
                 Right θ' -> return θ'
                 Left e1  -> case varAsn θ β (tVar α) of
                                  Right θ' -> return θ'
                                  Left e2  -> addError (e1 ++ "\n OR \n" ++ e2) θ
 
-----------------------------------------------------------------------------
varAsn :: Subst -> TVar -> Type -> Either String Subst
-----------------------------------------------------------------------------
varAsn θ α t 
  | t == tVar α          = Right $ θ 
  | α `HS.member` free t = Left  $ errorOccursCheck α t 
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
subtyNoUnion _ _ (TApp TUn _ _ ) _ = error "No union type is allowed here"
subtyNoUnion _ _ _ (TApp TUn _ _ ) = error "No union type is allowed here"


-- | Top type gets simplified
subtyNoUnion θ _ t t' 
  | isTop t'       = unify θ t tTop


-- | Subtyping named TDefs: for the moment require that the 
-- names of the bodies be the same - otherwise flag as error
-- TODO: This is a complete nominal and restrictive approach.
-- Will probably need to unfold and do structural subtyping here
subtyNoUnion θ _ t@(TApp (TDef s) ts _) t'@(TApp (TDef s') ts' _) 
  -- for the moment keep type parameters invariant (i.e. unify them)
  | TDef s == TDef s' = unifys θ ts ts' 
  | otherwise         = addError (errorSubType "NoUnion" t t') θ

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



-----------------------------------------------------------------------------
-- | General Subtyping -- including unions
-----------------------------------------------------------------------------
subty :: Subst -> Maybe (Expression AnnSSA) -> Type -> Type -> TCM Subst
-----------------------------------------------------------------------------
subty θ e (TApp TUn ts _ ) t'@(TApp TUn _ _)  = 
  foldM (\θ t -> subty θ e t t') θ ts 

subty θ e t@(TApp TUn ts _ ) t'                =
  do  s <- get
      case tryError s (subtyUnions θ e ts [t']) of 
        (Left _  , _ ) -> unify θ t t'
        (Right θ', s') -> modify (const s') >> return θ'

subty θ e t                t'@(TApp TUn ts' _)  = 
  do  s <- get
      case tryError s (subtyUnions θ e [t] ts') of 
        (Left _  , _ ) -> unify θ t t' 
        (Right θ', s') -> modify (const s') >> return θ'

subty θ e t                t'                = subtyNoUnion θ e t t'  
-----------------------------------------------------------------------------
subtyUnions :: Subst -> Maybe (Expression AnnSSA) -> [Type] -> [Type] -> TCM Subst
-----------------------------------------------------------------------------
subtyUnions θ e xs ys
  | isTop ys  = return θ
  | otherwise = loop θ e xs ys
    where

      go θ e t (t':ts') =
        do  st <- get
            case tryError st (subtyNoUnion θ e t t') of
              -- Left means that subtyping was unsuccessful
              (Left _  , _ ) -> go θ e t {- $ tracePP "will check next" -} ts'
              -- Right means we found a candidate
              (Right θ', s') ->
                do  modify (const s')
                    return $ θ'
      -- TODO: ADD A CASTS !!!
      go θ _ _ _       = addError (errorSubType "U" xs ys) θ
      -- -- Disabling for the moment 
      --  | S.size (tracePP "∩" isct) > 0 = addCast (mkUnion $ S.toList isct) >> return θ
      --  | otherwise = addError (errorSubType "U" xs ys) θ
      -- isct = (S.fromList xs) `S.intersection` (S.fromList ys)

      loop θ e (x:xs) ys = 
        do  θ' <- go θ e x ys
            loop θ' e xs ys
      loop θ _ [] _  = return θ


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
subtys θ es xs ys =  {- trace msg <$> -} applyToList subty θ es xs ys 

applyToList f θ es ts ts'
  | nTs == nTs' = go θ (es, ts, ts')
  | otherwise   = addError (errorSubType "" ts ts) θ
  where
    nTs                  = length ts
    nTs'                 = length ts'
    go θ (eo:eos, t:ts , t':ts') = do setExpr eo
                                      θ' <- f θ eo t t'
                                      go θ' (eos, apply θ' ts, apply θ' ts')
    go θ (_, _  , _  )   = return θ



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
      case {- trace (printf "Casting %s to %s"  (ppshow eo) (ppshow t)) -} eo of 
      -- Right now nothing is added explicitly to . The casted type is just
      -- propagated. 
        Just e                -> addAsrt e t 
        _                     -> logError dummySpan "NO CAST" ()

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
        Just t -> return $ a { ann_fact = (Assert $ t) : (ann_fact a) } 
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
patchExpr' e@(DotRef a e' i)         = do a' <- annt e a 
                                          return $ DotRef a' e' i 
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
