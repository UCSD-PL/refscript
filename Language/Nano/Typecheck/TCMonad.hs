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
  , unifyType
  , unifyTypes

  -- * Subtyping
  , subTypes
  , subType

  -- * Get Type Signature 
  , getDefType 

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
import           Language.ECMAScript3.Parser    (SourceSpan (..))
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Syntax.Annotations
import           Text.PrettyPrint.HughesPJ          (render)

import           Debug.Trace
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
                   , tc_asrt  :: M.Map (Expression AnnSSA) Type
                   , tc_defs  :: !(Env Type) 
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
initState pgm = TCS [] [] mempty 0 HM.empty [] M.empty (envMap toType $ defs pgm) Nothing


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
subTypes :: AnnSSA -> Env Type -> [Maybe (Expression AnnSSA)] -> [Type] -> [Type] -> TCM (Env Type, Subst)
----------------------------------------------------------------------------------
subTypes l γ es t1s t2s
  | length t1s /= length t2s = getSubst >>= logError (ann l) errorArgMismatch >>= return <$> (γ,)

  | otherwise                = do θ         <- getSubst
                                  (γ', θ')  <- subtys θ γ es t1s t2s
                                  accumErrs l
                                  setSubst θ' 
                                  return (γ', θ')

----------------------------------------------------------------------------------
subType :: AnnSSA -> Env Type -> Maybe (Expression AnnSSA) -> Type -> Type -> TCM ()
----------------------------------------------------------------------------------
subType l γ eo t t' = subTypes l γ [eo] [t] [t'] >> return ()


-----------------------------------------------------------------------------
unify :: Subst -> Type -> Type -> TCM Subst
-----------------------------------------------------------------------------
unify θ (TFun xts t _) (TFun xts' t' _) = unifys θ (t: (b_type <$> xts)) (t': (b_type <$> xts'))
unify θ (TVar α _)     (TVar β _)       = varEql θ α β 
unify θ (TVar α _)     t                = varAsnM θ α t 
unify θ t              (TVar α _)       = varAsnM θ α t

unify θ (TApp TUn ts _) (TApp TUn ts' _)
  | unifiable ts && unifiable ts' 
      && subset ts ts'                  = return $ tracePP "ts c ts\'" θ
  where
    -- Simple check to prohibit multiple type vars in a union type
    -- Might need a stronger check here.
    unifiable ts = length (filter var ts) < 2 
    var (TVar _ _) = True
    var _          = False

unify θ (TApp c ts _) (TApp c' ts' _)
  | c == c'                             = unifys  θ (tracePP "ts" ts) (tracePP "ts\'" ts')

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
    tops = map $ const tTop
    go θ ts = unifys θ ts $ tops ts

unifys         ::  Subst -> [Type] -> [Type] -> TCM Subst
unifys θ xs ys =  trace msg $ unifys' θ xs ys 
   where 
     msg      = printf "unifys: [xs = %s] [ys = %s]"  (ppshow xs) (ppshow ys)

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

-----------------------------------------------------------------------------
subty :: Subst -> Env Type -> Maybe (Expression AnnSSA) -> Type -> Type -> TCM (Env Type, Subst)
-----------------------------------------------------------------------------
subty θ γ _ t t'                                   
  | isTop t'       = (γ,) <$> unify θ t tTop

subty θ γ _ (TApp TUn ts _ ) t'                     
  | subset [t'] ts  = do  γ' <- addCast γ t'
                          return (γ', θ)

subty θ γ _ (TApp TUn ts _ ) (TApp TUn ts' _) 
  | subset ts  ts'            = return (γ, θ)
  | S.size (tracePP "intersection" (isc ts ts')) > 0 = 
      do  γ' <- addCast γ $ mkUnion $ S.toList (isc ts ts')
          return (γ', θ)
  where 
    isc a b = (S.fromList a) `S.intersection` (S.fromList b)

subty θ γ _ t (TApp TUn ts' _) 
  | subset [t] ts' = return (γ, θ)

subty θ γ _ t t1 = do θ <- trace "unifying..." $ unify θ t t1
                      return (γ, θ)


-----------------------------------------------------------------------------
subtys ::  Subst -> Env Type -> [Maybe (Expression AnnSSA)] -> [Type] -> [Type] -> TCM (Env Type, Subst)
-----------------------------------------------------------------------------
subtys θ γ es xs ys =  trace msg <$> applyToList subty θ γ es xs ys 
   where 
     msg      = printf "subtys: [xs = %s] [ys = %s]"  (ppshow xs) (ppshow ys)

  {-| nTs == nTs' = go θ (es, ts, ts')-}
  {-| otherwise   = addError (errorSubType "" ts ts) θ-}
  {-where -}
  {-  nTs                  = length ts-}
  {-  nTs'                 = length ts'-}
  {-  go θ (eo:eos, t:ts , t':ts') = do setExpr eo-}
  {-                                    θ' <- subty θ t t' -}
  {-                                    go θ' $ (eos, apply θ' ts, apply θ' ts')-}
  {-  go θ (_, _  , _  )   = return θ-}

applyToList f θ γ es ts ts'
  | nTs == nTs' = go θ γ (es, ts, ts')
  | otherwise   = addError (errorSubType "" ts ts) θ >>= return <$> (γ,)
  where 
    nTs                  = length ts
    nTs'                 = length ts'
    go θ γ (eo:eos, t:ts , t':ts') = do setExpr eo
                                        (γ', θ') <- f θ γ eo t t' 
                                        go θ' γ' (eos, apply θ' ts, apply θ' ts')
    go θ γ (_, _  , _  )   = return (γ, θ)



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
addCast :: Env Type -> Type -> TCM (Env Type)
-------------------------------------------------------------------------------
addCast γ t = 
  do  eo <- getExpr
      case {- tracePP "Casting A" -} eo of 
        Just e@(VarRef _ id)  -> addAsrt e t >> return (envAdds [(id,t)] γ)
        _                     -> logError dummySpan "NO CAST" () >> return γ

addAsrt e t = modify $ \st -> st { tc_asrt = M.insert e t (tc_asrt st) } 




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

patchStmts = mapM patchStmt 

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
                                                  ++ (render (pp s))

patchExprs = mapM patchExpr

annt e a = 
  do  m <- tc_asrt <$> get
      case M.lookup e m of
        Just t -> return $ a { ann_fact = (Assert $ tracePP "patching" t) : (ann_fact a) } 
        _      -> return $ a

patchExpr e@(StringLit _ _ )        = return $ e 
patchExpr e@(NumLit _ _ )           = return $ e 
patchExpr e@(IntLit _ _ )           = return $ e 
patchExpr e@(BoolLit _ _)           = return $ e 
patchExpr e@(NullLit _)             = return $ e 
patchExpr e@(ArrayLit a es)         = liftM2 ArrayLit (annt e a) (patchExprs es)
patchExpr e@(ObjectLit a pes)       = liftM2 ObjectLit (annt e a) (mapM (mapSndM patchExpr) pes)
patchExpr e@(ThisRef _)             = return $ e
patchExpr e@(VarRef a id)           = do a' <- annt e a
                                         return $ VarRef a' id
patchExpr e@(PrefixExpr a p e')     = do a' <- annt e a
                                         PrefixExpr a' p <$> patchExpr e'
patchExpr e@(InfixExpr a o e1 e2)   = do a' <- annt e a 
                                         liftM2 (InfixExpr a' o) (patchExpr e1) (patchExpr e2)
patchExpr e@(AssignExpr a o lv e')  = do a' <- annt e a 
                                         AssignExpr a' o lv <$> patchExpr e'
patchExpr e@(CallExpr a e' el)      = do a' <- annt e a
                                         liftM2 (CallExpr a') (patchExpr e') (patchExprs el)
patchExpr e@(FuncExpr a oi is ss)   = do a' <- annt e a
                                         FuncExpr a' oi is <$> patchStmts ss
patchExpr e                         = return $ error $ "Does not support patchExpr for."
                                                  ++ (render (pp e))
                                                  
patchExpr' :: Expression a -> ()
patchExpr' e = ()
  where a = getAnnotation e 



patchVarDecl vd = return vd    


