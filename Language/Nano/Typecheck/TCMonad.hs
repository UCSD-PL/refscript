{- LANGUAGE TypeSynonymInstances      #-}
{- LANGUAGE FlexibleInstances         #-}
{- LANGUAGE NoMonomorphismRestriction #-}
{- LANGUAGE ScopedTypeVariables       #-}

-- | This module has the code for the Type-Checker Monad. 

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

  -- * Type definitions
  , getTDefs

  -- * Substitutions
  , getSubst, setSubst

  -- * Annotations
  , accumAnn
  , getAllAnns

  -- * Unfolding
  , unfoldFirstTC, unfoldSafeTC

  -- * Subtyping
  , subTypeM  , subTypeM'
  , subTypesM

  -- * Unification
  , unifyTypeM, unifyTypesM

  -- * Casts
  , getCasts
  , castM, castsM
  
  -- * Get Type Signature 
  , getDefType 

  -- * Expression Getter/Setter
  , getExpr, setExpr

  -- * Patch the program with assertions
  , patchPgmM

  )  where 

import           Text.Printf
import           Control.Applicative            ((<$>))
import           Control.Monad.State
import           Control.Monad.Error
import           Language.Fixpoint.Misc 
import qualified Language.Fixpoint.Types as F

import           Language.Nano.Env
import           Language.Nano.Misc             (unique, everywhereM')
import           Language.Nano.Types
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Typecheck.Compare
import           Language.Nano.Errors
import           Data.Monoid                  
import qualified Data.HashMap.Strict            as HM
import qualified Data.Map                       as M
import           Data.Generics                  (Data(..))
import           Data.Generics.Aliases
import           Data.Generics.Schemes
import           Data.Typeable                  (Typeable (..))
import           Language.ECMAScript3.Parser    (SourceSpan (..))
-- import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations

-- import           Debug.Trace                      (trace)

-------------------------------------------------------------------------------
-- | Typechecking monad -------------------------------------------------------
-------------------------------------------------------------------------------

data TCState = TCS { 
                   -- Errors
                     tc_errss :: ![(SourceSpan, String)]
                   , tc_subst :: !Subst
                   , tc_cnt   :: !Int
                   -- Annotations
                   , tc_anns  :: AnnInfo
                   , tc_annss :: [AnnInfo]
                   -- Cast map: SourceSpan alone is not enough as key, 
                   -- so backing up with expression
                   , tc_casts :: M.Map (Expression AnnSSA) (Cast Type)
                   -- Function definitions
                   , tc_defs  :: !(Env Type) 
                   -- Type definitions
                   , tc_tdefs :: !(Env Type)
                   -- The currently typed expression 
                   , tc_expr  :: Maybe (Expression AnnSSA)
                   }

type TCM     = ErrorT String (State TCState)


-------------------------------------------------------------------------------
getTDefs :: TCM (Env Type)
-------------------------------------------------------------------------------
getTDefs = tc_tdefs <$> get 

-------------------------------------------------------------------------------
getSubst :: TCM Subst
-------------------------------------------------------------------------------
getSubst = tc_subst <$> get 

getCasts = do c <- tc_casts <$> get 
              return $ M.toList c

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
  = do
      fUnique αs
      βs        <- mapM (freshTVar l) αs
      setTyArgs l βs
      extSubst βs 
      return     $ fromList $ zip αs (tVar <$> βs)
    where
      fUnique xs = when (not $ unique xs) $ logError l errorUniqueTypeParams ()

setTyArgs l βs 
  = do m <- tc_anns <$> get 
       when (HM.member l m) $ tcError l "Multiple Type Args"
       addAnn l $ TypInst (tVar <$> βs)







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
initState pgm = TCS tc_errss tc_subst tc_cnt tc_anns tc_annss 
                    tc_casts tc_defs tc_tdefs tc_expr  
  where
    tc_errss = []
    tc_subst = mempty 
    tc_cnt   = 0
    tc_anns  = HM.empty
    tc_annss = []
    tc_casts = M.empty
    tc_defs  = envMap toType $ defs pgm
    tc_tdefs = envMap toType $ tDefs pgm
    tc_expr  = Nothing


getDefType f 
  = do m <- tc_defs <$> get
       maybe err return $ envFindTy f m 
    where 
       err = tcError l $ errorMissingSpec l f
       l   = srcPos f


-------------------------------------------------------------------------------
setExpr   :: Maybe (Expression AnnSSA) -> TCM () 
-------------------------------------------------------------------------------
setExpr eo = modify $ \st -> st { tc_expr = eo }


-------------------------------------------------------------------------------
getExpr   :: TCM (Maybe (Expression AnnSSA))
-------------------------------------------------------------------------------
getExpr = tc_expr <$> get

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
              



{--- | SubTyping within the TCMonad invokes casting by default-}
{----------------------------------------------------------------------------------}
{-subTypeM :: AnnSSA -> Maybe (Expression AnnSSA) -> Type -> Type -> TCM Subst-}
{----------------------------------------------------------------------------------}
{-subTypeM _  eo t t' = subTAux subTypeCast eo t t'-}


{--- | SubTyping within the TCMonad invokes casting by default-}
{----------------------------------------------------------------------------------}
{-subTypeM_ :: AnnSSA -> Maybe (Expression AnnSSA) -> Type -> Type -> TCM ()-}
{----------------------------------------------------------------------------------}
{-subTypeM_ l eo t t' = subTypeM l eo t t' >> return ()-}

      
{--- | SubTyping within the TCMonad invokes casting by default-}
{----------------------------------------------------------------------------------}
{-subTypesM :: AnnSSA -> [Maybe (Expression AnnSSA)] -> [Type] -> [Type] -> TCM Subst-}
{----------------------------------------------------------------------------------}
{-subTypesM l es t1s t2s-}
{-  | length t1s /= length t2s = tcError (ann l) errorArgMismatch-}
{-  | otherwise   = subTAux subTypesCast es t1s t2s-}

      
{--- | SubTyping variant that returns void.-}
{--- Invokes casting by default.-}
{----------------------------------------------------------------------------------}
{-subTypesM_ :: AnnSSA -> [Maybe (Expression AnnSSA)] -> [Type] -> [Type] -> TCM ()-}
{----------------------------------------------------------------------------------}
{-subTypesM_ l es t1s t2s = subTypesM l es t1s t2s >> return ()-}


{-subTAux action e t t' =-}
{-  do  θ <- getSubst-}
{-      m <- tc_tdefs <$> get-}
{-      case action m θ e t t' of-}
{-        Left  s      -> forM_  s (\(l,m) -> logError l m θ) >> return θ-}
{-        Right (θ',c) -> setSubst θ' >> addCasts c >> return θ'-}


-- | Monadic unfolding
-------------------------------------------------------------------------------
unfoldFirstTC :: Type -> TCM Type
-------------------------------------------------------------------------------
unfoldFirstTC t = getTDefs >>= \γ -> return $ unfoldFirst γ t


-------------------------------------------------------------------------------
unfoldSafeTC :: Type -> TCM Type
-------------------------------------------------------------------------------
unfoldSafeTC   t = getTDefs >>= \γ -> return $ unfoldSafe γ t



--------------------------------------------------------------------------------
--  Unification and Subtyping --------------------------------------------------
--------------------------------------------------------------------------------


----------------------------------------------------------------------------------
unifyTypesM :: (IsLocated l) => l -> String -> [Type] -> [Type] -> TCM Subst
----------------------------------------------------------------------------------
unifyTypesM l msg t1s t2s
  -- TODO: This check might be done multiple times
  | length t1s /= length t2s = tcError l errorArgMismatch 
  | otherwise                = do θ <- getSubst 
                                  γ <- getTDefs
                                  case unifys γ θ t1s t2s of
                                    Left msg' -> tcError l $ msg ++ msg'
                                    Right θ'  -> setSubst θ' >> return θ' 

----------------------------------------------------------------------------------
--unifyTypeM :: (IsLocated l) => l -> String -> Expression AnnSSA -> Type -> Type -> TCM Subst
----------------------------------------------------------------------------------
unifyTypeM l m e t t' = unifyTypesM l msg [t] [t']
  where 
    msg              = errorWrongType m e t t'


----------------------------------------------------------------------------------
subTypeM :: (IsLocated l) => l -> Expression AnnSSA -> Type -> Type -> TCM SubDirection
----------------------------------------------------------------------------------
subTypeM l e t t' = undefined

----------------------------------------------------------------------------------
subTypeM' :: (IsLocated l) => l -> Type -> Type -> TCM ()
----------------------------------------------------------------------------------
subTypeM' l t t' = undefined
 
----------------------------------------------------------------------------------
subTypesM :: (IsLocated l) => l -> [Expression AnnSSA] -> [Type] -> [Type] -> TCM [SubDirection]
----------------------------------------------------------------------------------
subTypesM l es ts ts' = mapM (\(e,t,t') -> subTypeM l e t t') $ zip3 es ts ts'



--------------------------------------------------------------------------------
--  Cast Helpers ---------------------------------------------------------------
--------------------------------------------------------------------------------


-----------------------------------------------------------------------------
withExpr  :: Maybe (Expression AnnSSA) -> TCM a -> TCM a
-----------------------------------------------------------------------------
withExpr e action = 
  do  eold  <- getExpr 
      setExpr  e 
      r     <- action 
      setExpr  eold
      return $ r



castM  e  t  = undefined
castsM es ts = undefined 


-------------------------------------------------------------------------------
addCasts :: Casts -> TCM ()
-------------------------------------------------------------------------------
addCasts cs = 
  do  cs'  <- tc_casts <$> get
      {-let err e _ _ -}
      {-      = error $ printf "There should be no prior cast on %s\nNew Casts: %s\nOld casts: %s" -}
      {-                (ppshow e)-}
      {-                (ppshow $ M.toList cs)-}
      {-                (ppshow $ M.toList cs')-}
      let err e _ _ = error $ printf "Erro at TCMonad addCasts on %s" (ppshow e)
      modify $ \st -> st {tc_casts = M.unionWithKey err cs cs' }



--------------------------------------------------------------------------------
patchPgmM :: (Typeable r, Data r) => Nano AnnSSA (RType r) -> TCM (Nano AnnSSA (RType r))
--------------------------------------------------------------------------------
patchPgmM pgm = 
  do  c <- tc_casts <$> get
      return $ fst $ runState (everywhereM' (mkM transform) pgm) (PS c)


data PState = PS { m :: Casts }
type PM     = State PState

--------------------------------------------------------------------------------
transform :: Expression AnnSSA -> PM (Expression AnnSSA)
--------------------------------------------------------------------------------
transform e = 
  do  c  <- m <$> get      
      put (PS $ M.delete e c)
      return $ patchExpr c e

--------------------------------------------------------------------------------
patchExpr :: Casts -> Expression AnnSSA -> Expression AnnSSA
--------------------------------------------------------------------------------
patchExpr m e =
  case M.lookup e m of
    Just (UCST t) -> UpCast   (a { ann_fact = (Assume t):fs }) e
    Just (DCST t) -> DownCast (a { ann_fact = (Assume t):fs }) e
    Just (DC   t) -> DeadCast (a { ann_fact = (Assume t):fs }) e
    _             -> e
  where 
    fs = ann_fact a
    a  = getAnnotation e




