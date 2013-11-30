{- LANGUAGE TypeSynonymInstances       #-}
{- LANGUAGE FlexibleInstances          #-}
{- LANGUAGE NoMonomorphismRestriction  #-}
{- LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE LiberalTypeSynonyms       #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

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
  , freshTArray

  -- * Dot Access
  , safeGetProp
  , safeGetIdx
  , indexType

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
  , checkAnnotation

  -- * Unification
  , unifyTypeM, unifyTypesM

  -- * Casts
  , getCasts
  , castM, castsM
  
  -- * Get Type Signature 
  , getDefType 

  -- * Expression Getter/Setter
  , getExpr, setExpr, withExpr

  -- * Patch the program with assertions
  , patchPgmM

  -- * Verbosity
  , whenLoud', whenLoud
  , whenQuiet', whenQuiet

  )  where 

import           Text.Printf
import           Language.ECMAScript3.PrettyPrint
import           Control.Applicative            ((<$>))
import           Control.Monad.State
import           Control.Monad.Error hiding (Error)
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc 
import qualified Language.Fixpoint.Types as F

import           Language.Nano.Env
import           Language.Nano.Misc             (unique, everywhereM', zipWith3M_)

import           Language.Nano.Types
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Typecheck.Compare
import           Language.Nano.Typecheck.Unify
import           Language.Nano.Errors
import           Data.Monoid                  
import qualified Data.HashMap.Strict            as HM
import qualified Data.Map                       as M
import           Data.Generics                  (Data(..))
import           Data.Maybe                     (fromJust)
import           Data.Generics.Aliases
import           Data.Typeable                  (Typeable (..))
import           Language.ECMAScript3.Parser    (SourceSpan (..))
-- import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations

-- import           Debug.Trace                      (trace)
import qualified System.Console.CmdArgs.Verbosity as V

-------------------------------------------------------------------------------
-- | Typechecking monad -------------------------------------------------------
-------------------------------------------------------------------------------

data TCState r = TCS {
                   -- Errors
                     tc_errss :: ![Error]
                   , tc_subst :: !(RSubst r)
                   , tc_cnt   :: !Int
                   -- Annotations
                   , tc_anns  :: AnnInfo r
                   , tc_annss :: [AnnInfo r]
                   -- Cast map: 
                   , tc_casts :: M.Map (Expression (AnnSSA r)) (Cast (RType r))
                   -- Function definitions
                   , tc_defs  :: !(Env (RType r))
                   -- Type definitions
                   , tc_tdefs :: !(Env (RType r))
                   -- The currently typed expression 
                   , tc_expr  :: Maybe (Expression (AnnSSA r))
                   -- Verbosity
                   , tc_verb  :: V.Verbosity
                   }

type TCM r     = ErrorT Error (State (TCState r))


-------------------------------------------------------------------------------
whenLoud :: TCM r () -> TCM r ()
-------------------------------------------------------------------------------
whenLoud  act = whenLoud' act $ return ()

-------------------------------------------------------------------------------
whenLoud' :: TCM r a -> TCM r a -> TCM r a
-------------------------------------------------------------------------------
whenLoud' loud other = do  v <- tc_verb <$> get
                           case v of
                             V.Loud -> loud 
                             _      -> other

-------------------------------------------------------------------------------
whenQuiet :: TCM r () -> TCM r ()
-------------------------------------------------------------------------------
whenQuiet  act = whenQuiet' act $ return ()

-------------------------------------------------------------------------------
whenQuiet' :: TCM r a -> TCM r a -> TCM r a
-------------------------------------------------------------------------------
whenQuiet' quiet other = do  v <- tc_verb <$> get
                             case v of
                               V.Quiet -> quiet
                               _       -> other



-------------------------------------------------------------------------------
getTDefs :: TCM r (Env (RType r))
-------------------------------------------------------------------------------
getTDefs = tc_tdefs <$> get 

-------------------------------------------------------------------------------
getSubst :: TCM r (RSubst r)
-------------------------------------------------------------------------------
getSubst = tc_subst <$> get 

getCasts = do c <- tc_casts <$> get 
              return $ M.toList c

-------------------------------------------------------------------------------
setSubst   :: RSubst r -> TCM r () 
-------------------------------------------------------------------------------
setSubst θ = modify $ \st -> st { tc_subst = θ }

-------------------------------------------------------------------------------
extSubst :: (F.Reftable r, PP r) => [TVar] -> TCM r ()
-------------------------------------------------------------------------------
extSubst βs = getSubst >>= setSubst . (`mappend` θ')
  where 
    θ'      = fromList $ zip βs (tVar <$> βs)


-------------------------------------------------------------------------------
tcError     :: Error -> TCM r a
-------------------------------------------------------------------------------
tcError err = throwError $ catMessage err "TC-ERROR "


-------------------------------------------------------------------------------
logError   :: Error -> a -> TCM r a
-------------------------------------------------------------------------------
logError err x = (modify $ \st -> st { tc_errss = err : tc_errss st}) >> return x


-------------------------------------------------------------------------------
freshTyArgs :: (PP r, F.Reftable r) => SourceSpan -> ([TVar], RType r) -> TCM r (RType r)
-------------------------------------------------------------------------------
freshTyArgs l (αs, t) 
  = (`apply` t) <$> freshSubst l αs

freshSubst :: (PP r, F.Reftable r) => SourceSpan -> [TVar] -> TCM r (RSubst r)
freshSubst l αs
  = do
      fUnique αs
      βs        <- mapM (freshTVar l) αs
      setTyArgs l βs
      extSubst βs 
      return     $ fromList $ zip αs (tVar <$> βs)
    where
      fUnique xs = when (not $ unique xs) $ logError (errorUniqueTypeParams l) ()

setTyArgs l βs
  = do m <- tc_anns <$> get
       when (HM.member l m) $ tcError $ errorMultipleTypeArgs l
       addAnn l $ TypInst (tVar <$> βs)


-------------------------------------------------------------------------------
-- | Field access -------------------------------------------------------------
-------------------------------------------------------------------------------

-- Access field @f@ of type @t@, adding a cast if needed to avoid errors.
-------------------------------------------------------------------------------
safeGetProp :: (Ord r, PP r, F.Reftable r) => String -> RType r -> TCM r (RType r)
-------------------------------------------------------------------------------
safeGetProp f t = do
    γ <- getTDefs
    e <- fromJust <$> getExpr
    case getProp γ f t of
      Just (t',tf) -> castM e t t' >> return tf
      Nothing      -> error "safeGetProp" --TODO: deadcode
 
-- DEPRECATE:
-- Access index @i@ of type @t@.
-- In JavaScript semantics, all types can be indexed, besides null and undefined. 
-- So we're not gonna cast for those types
-- the accessed type here. Instead
-------------------------------------------------------------------------------
safeGetIdx :: (Ord r, PP r, F.Reftable r) => Int -> RType r -> TCM r (RType r)
-------------------------------------------------------------------------------
safeGetIdx f t = do  
    γ <- getTDefs
    e <- fromJust <$> getExpr
    case getIdx γ f t of
      Just (t',tf) -> castM e t t' >> return tf
      Nothing      -> error "safeGetIdx" --TODO: deadcode

-- Only support indexing in arrays atm. Discharging array bounds checks makes
-- sense only for array types. 
-------------------------------------------------------------------------------
indexType :: (PP r, Ord r, F.Reftable r) => RType r -> TCM r (RType r)
-------------------------------------------------------------------------------
indexType (TArr t _) = return t 
indexType t@(TApp TUn ts _) = do
    e <- fromJust <$> getExpr
    castM e t t'
    mkUnion <$> mapM indexType arrs
  where
    t'   = mkUnion arrs
    arrs = (filter isArr ts) 

indexType _          = errorstar "Unimplemented: indexing type other than array."


-------------------------------------------------------------------------------
-- | Managing Annotations: Type Instantiations --------------------------------
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
getAnns :: (Ord r, F.Reftable r, Substitutable r (Fact r)) => TCM r (AnnInfo r)
-------------------------------------------------------------------------------
getAnns = do θ     <- tc_subst <$> get
             m     <- tc_anns  <$> get
             let m' = fmap (apply θ . sortNub) m
             _     <- modify $ \st -> st { tc_anns = m' }
             return m' 

-------------------------------------------------------------------------------
addAnn :: SourceSpan -> Fact r -> TCM r () 
-------------------------------------------------------------------------------
addAnn l f = modify $ \st -> st { tc_anns = inserts l f (tc_anns st) } 

-------------------------------------------------------------------------------
getAllAnns :: TCM r [AnnInfo r]  
-------------------------------------------------------------------------------
getAllAnns = tc_annss <$> get


-------------------------------------------------------------------------------
accumAnn :: (Ord r, F.Reftable r, Substitutable r (Fact r)) =>
  (AnnInfo r -> [Error]) -> TCM r () -> TCM r ()
-------------------------------------------------------------------------------
accumAnn check act 
  = do m     <- tc_anns <$> get 
       modify $ \st -> st {tc_anns = HM.empty}
       act
       m'    <- getAnns
       forM_ (check m') (`logError` ())
       modify $ \st -> st {tc_anns = m} {tc_annss = m' : tc_annss st}

-------------------------------------------------------------------------------
execute     ::  (PP r, F.Reftable r) => 
  V.Verbosity -> Nano z (RType r) -> TCM r a -> Either [Error] a
-------------------------------------------------------------------------------
execute verb pgm act 
  = case runState (runErrorT act) $ initState verb pgm of 
      (Left err, _) -> Left [err]
      (Right x, st) ->  applyNonNull (Right x) Left (reverse $ tc_errss st)


initState ::  (PP r, F.Reftable r) => V.Verbosity -> Nano z (RType r) -> TCState r
initState verb pgm = TCS tc_errss tc_subst tc_cnt tc_anns tc_annss 
                       tc_casts tc_defs tc_tdefs tc_expr tc_verb 
  where
    tc_errss = []
    tc_subst = mempty 
    tc_cnt   = 0
    tc_anns  = HM.empty
    tc_annss = []
    tc_casts = M.empty
    tc_defs  = defs pgm
    tc_tdefs = tDefs pgm
    tc_expr  = Nothing
    tc_verb  = verb


getDefType f 
  = do m <- tc_defs <$> get
       maybe err return $ envFindTy f m 
    where 
       err = tcError $ errorMissingSpec l f
       l   = srcPos f


-------------------------------------------------------------------------------
setExpr   :: Maybe (Expression (AnnSSA r)) -> TCM r () 
-------------------------------------------------------------------------------
setExpr eo = modify $ \st -> st { tc_expr = eo }


-------------------------------------------------------------------------------
getExpr   :: TCM r (Maybe (Expression (AnnSSA r)))
-------------------------------------------------------------------------------
getExpr = tc_expr <$> get

--------------------------------------------------------------------------
-- | Generating Fresh Values ---------------------------------------------
--------------------------------------------------------------------------

tick :: TCM r Int
tick = do st    <- get 
          let n  = tc_cnt st
          put    $ st { tc_cnt = n + 1 }
          return n 

class Freshable a where 
  fresh :: a -> TCM r a

-- instance Freshable TVar where 
--   fresh _ = TV . F.intSymbol "T" <$> tick

instance Freshable a => Freshable [a] where 
  fresh = mapM fresh

freshTVar l _ =  ((`TV` l). F.intSymbol "T") <$> tick


freshTArray l = 
  do  v <- ((`TV` l). F.intSymbol "A") <$> tick
      extSubst [v]
      let t = tVar v
      addAnn l $ TypInst [t]
      return $ tArr t
              



-- | Monadic unfolding
-------------------------------------------------------------------------------
unfoldFirstTC :: (PP r, F.Reftable r) => RType r -> TCM r (RType r)
-------------------------------------------------------------------------------
unfoldFirstTC t = getTDefs >>= \γ -> return $ unfoldFirst γ t


-------------------------------------------------------------------------------
unfoldSafeTC :: (PP r, F.Reftable r) => RType r -> TCM r (RType r)
-------------------------------------------------------------------------------
unfoldSafeTC   t = getTDefs >>= \γ -> return $ unfoldSafe γ t



--------------------------------------------------------------------------------
--  Unification and Subtyping --------------------------------------------------
--------------------------------------------------------------------------------

----------------------------------------------------------------------------------
unifyTypesM :: (Ord r, PP r, F.Reftable r) => 
  SourceSpan -> String -> [RType r] -> [RType r] -> TCM r (RSubst r)
----------------------------------------------------------------------------------
unifyTypesM l msg t1s t2s
  -- TODO: This check might be done multiple times
  | length t1s /= length t2s = tcError $ errorArgMismatch l 
  | otherwise                = do θ <- getSubst 
                                  γ <- getTDefs
                                  case unifys l γ θ t1s t2s of
                                    Left err' -> tcError $ catMessage err' msg 
                                    Right θ'  -> setSubst θ' >> return θ' 

----------------------------------------------------------------------------------
unifyTypeM :: (Ord r, PrintfArg t1, PP r, PP a, F.Reftable r) =>
  SourceSpan -> t1 -> a -> RType r -> RType r -> TCM r (RSubst r)
----------------------------------------------------------------------------------
unifyTypeM l m e t t' = unifyTypesM l msg [t] [t']
  where 
    msg               = ppshow $ errorWrongType l m e t t'


----------------------------------------------------------------------------------
subTypeM :: (Ord r, PP r, F.Reftable r) => RType r -> RType r -> TCM r SubDirection
----------------------------------------------------------------------------------
subTypeM t t' 
  = do  θ            <- getTDefs 
        let (_,_,_,d) = compareTs θ t t'
        return $  {- trace (printf "subTypeM: %s %s %s" (ppshow t) (ppshow d) (ppshow t')) -}  d

----------------------------------------------------------------------------------
subTypeM' :: (IsLocated l, Ord r, PP r, F.Reftable r) => l -> RType r -> RType r -> TCM r ()
----------------------------------------------------------------------------------
subTypeM' _ _ _  = error "unimplemented: subTypeM\'"
 
----------------------------------------------------------------------------------
subTypesM :: (Ord r, PP r, F.Reftable r) => [RType r] -> [RType r] -> TCM r [SubDirection]
----------------------------------------------------------------------------------
subTypesM ts ts' = zipWithM subTypeM ts ts'

----------------------------------------------------------------------------------
checkAnnotation :: (F.Reftable r, PP r, Ord r) => 
  String -> RType r -> Expression (AnnSSA r) -> RType r -> TCM r () 
----------------------------------------------------------------------------------
checkAnnotation msg ta e t = do
    subTypeM t ta >>= sub
  where
    sub SubT = return () 
    sub EqT  = return () 
    sub _    = tcError $ catMessage err msg' 
    err      = errorAnnotation (srcPos $ getAnnotation e) e t ta
    msg'     = "[" ++ msg ++ "]"

--------------------------------------------------------------------------------
--  Cast Helpers ---------------------------------------------------------------
--------------------------------------------------------------------------------

-----------------------------------------------------------------------------
withExpr  :: Maybe (Expression (AnnSSA r)) -> TCM r a -> TCM r a
-----------------------------------------------------------------------------
withExpr e action = 
  do  eold  <- getExpr 
      setExpr  e 
      r     <- action 
      setExpr  eold
      return $ r


-- For the expression @e@, check the subtyping relation between the type @t@
-- which is the actual type for @e@ and @t'@ which is the desired (cast) type
-- and insert the right kind of cast. 
--------------------------------------------------------------------------------
castM     :: (Ord r, PP r, F.Reftable r) => Expression (AnnSSA r) -> RType r -> RType r -> TCM r ()
--------------------------------------------------------------------------------
castM e t t'    = subTypeM t t' >>= go
  where go SupT = addDownCast e t t'
        go Rel  = addDownCast e t t'
        go SubT = addUpCast e t'
        go EqT  = return ()
        go Nth  = addDeadCast e t'


--------------------------------------------------------------------------------
castsM    :: (Ord r, PP r, F.Reftable r) => 
  [Expression (AnnSSA r)] -> [RType r] -> [RType r] -> TCM r ()
--------------------------------------------------------------------------------
castsM     = zipWith3M_ castM 


--------------------------------------------------------------------------------
addUpCast :: (F.Reftable r, PP r) => 
  Expression (AnnSSA r) -> RType r -> TCM r ()
--------------------------------------------------------------------------------
addUpCast e t = modify $ \st -> st { tc_casts = M.insert e (UCST t) (tc_casts st) }

--------------------------------------------------------------------------------
addDownCast :: (Ord r, PP r, F.Reftable r) => 
  Expression (AnnSSA r) -> RType r -> RType r -> TCM r ()
--------------------------------------------------------------------------------
-- addDownCast e _ cast = modify $ \st -> st { tc_casts = M.insert e (DCST cast) (tc_casts st) }
  
-- Down casts will not be k-vared later - so pass the refinements here!
addDownCast e base cast = 
  do  γ <- getTDefs
      let cast' = zipType2 γ F.meet base cast    -- copy the refinements from the base type 
          {-msg   =  printf "DOWN CAST ADDS: %s\ninstead of just:\n%s" (ppshow cast') (ppshow cast)-}
      modify $ \st -> st { tc_casts = M.insert e (DCST $ {- trace msg -} cast') (tc_casts st) }

--------------------------------------------------------------------------------
addDeadCast :: Expression (AnnSSA r) -> RType r -> TCM r ()
--------------------------------------------------------------------------------
addDeadCast e t = modify $ \st -> st { tc_casts = M.insert e (DC t) (tc_casts st) }


--------------------------------------------------------------------------------
patchPgmM :: (Data b, Typeable r) => b -> TCM r b
--------------------------------------------------------------------------------
patchPgmM pgm = 
  do  c <- tc_casts <$> get
      return $ fst $ runState (everywhereM' (mkM transform) pgm) (PS c)


data PState r = PS { m :: Casts_ r }
type PM     r = State (PState r)

--------------------------------------------------------------------------------
transform :: Expression (AnnSSA r) -> PM r (Expression (AnnSSA r))
--------------------------------------------------------------------------------
transform e = 
  do  c  <- m <$> get      
      put (PS $ M.delete e c)
      return $ patchExpr c e

--------------------------------------------------------------------------------
patchExpr :: Casts_ r -> Expression (AnnSSA r) -> Expression (AnnSSA r)
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


