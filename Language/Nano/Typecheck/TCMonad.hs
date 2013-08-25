{- LANGUAGE TypeSynonymInstances       #-}
{- LANGUAGE FlexibleInstances          #-}
{- LANGUAGE NoMonomorphismRestriction  #-}
{- LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE LiberalTypeSynonyms       #-}
{-# LANGUAGE FlexibleContexts          #-}

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

  -- * Dot Access
  , dotAccess

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
  , getExpr, setExpr, withExpr

  -- * Patch the program with assertions
  , patchPgmM

  -- * Verbosity
  , whenLoud', whenLoud
  , whenQuiet', whenQuiet

  , AnnSSAR

  )  where 

import           Text.Printf
import           Language.ECMAScript3.PrettyPrint
import           Control.Applicative            ((<$>))
import           Control.Monad.State
import           Control.Monad.Error
import           Language.Fixpoint.Misc 
import qualified Language.Fixpoint.Types as F

import           Language.Nano.Env
import           Language.Nano.Misc             (unique, everywhereM', zipWith3M_)

import           Language.Nano.Liquid.Types

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
import           Data.List                      (find)
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
                     tc_errss :: ![(SourceSpan, String)]
                   , tc_subst :: !(Subst_ r)
                   , tc_cnt   :: !Int
                   -- Annotations
                   , tc_anns  :: AnnInfo_ r
                   , tc_annss :: [AnnInfo_ r]
                   -- Cast map: 
                   , tc_casts :: M.Map (Expression (AnnSSA_ r)) (Cast (RType r))
                   -- Function definitions
                   , tc_defs  :: !(Env (RType r))
                   -- Type definitions
                   , tc_tdefs :: !(Env (RType r))
                   -- The currently typed expression 
                   , tc_expr  :: Maybe (Expression (AnnSSA_ r))

                   -- Verbosiry
                   , tc_verb  :: V.Verbosity
                   }

type TCM r     = ErrorT String (State (TCState r))

type AnnSSAR  = forall r. AnnSSA_  r
type AnnInfoR = forall r. AnnInfo_ r  
type FactR    = forall r. Fact_    r 
type SubstR   = forall r. Subst_   r 
type RTypeR   = forall r. RType    r
type EnvTypeR = forall r. Env (RType r)

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
getSubst :: TCM r (Subst_ r)
-------------------------------------------------------------------------------
getSubst = tc_subst <$> get 

getCasts = do c <- tc_casts <$> get 
              return $ M.toList c

-------------------------------------------------------------------------------
setSubst   :: Subst_ r -> TCM r () 
-------------------------------------------------------------------------------
setSubst θ = modify $ \st -> st { tc_subst = θ }

-------------------------------------------------------------------------------
extSubst :: (F.Reftable r, PP r) => [TVar] -> TCM r ()
-------------------------------------------------------------------------------
extSubst βs = getSubst >>= setSubst . (`mappend` θ')
  where 
    θ'      = fromList $ zip βs (tVar <$> βs)


-------------------------------------------------------------------------------
tcError :: (IsLocated l) => l -> String -> TCM r a
-------------------------------------------------------------------------------
tcError l msg = throwError $ printf "TC-ERROR at %s : %s" (ppshow $ srcPos l) msg


-------------------------------------------------------------------------------
logError   :: SourceSpan -> String -> a -> TCM r a
-------------------------------------------------------------------------------
logError l msg x = (modify $ \st -> st { tc_errss = (l,msg):(tc_errss st)}) >> return x


-------------------------------------------------------------------------------
freshTyArgs :: (PP r, F.Reftable r) => SourceSpan -> ([TVar], RType r) -> TCM r (RType r)
-------------------------------------------------------------------------------
freshTyArgs l (αs, t) 
  = (`apply` t) <$> freshSubst l αs

freshSubst :: (PP r, F.Reftable r) => SourceSpan -> [TVar] -> TCM r (Subst_ r)
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
-- | Field access -------------------------------------------------------------
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
dotAccess ::  (Ord r, PP r, F.Reftable r) => Id (AnnSSA_ r) -> RType r -> TCM r (Maybe (RType r))
-------------------------------------------------------------------------------
dotAccess f   (TObj bs _) = 
  return $ Just $ maybe tUndef b_type $ find (match $ F.symbol f) bs
  where match s (B f _)  = s == f

dotAccess f t@(TApp c ts _ ) = go c
  where  go TUn      = dotAccessUnion f ts
         go TInt     = return $ Just tUndef
         go TBool    = return $ Just tUndef
         go TString  = return $ Just tUndef
         go TUndef   = return   Nothing
         go TNull    = return   Nothing
         go (TDef _) = unfoldSafeTC t >>= dotAccess f
         go TTop     = error "dotAccess top"
         go TVoid    = error "dotAccess void"

dotAccess _   (TFun _ _ _ ) = return $ Just tUndef
dotAccess _ t               = error $ "dotAccess " ++ (ppshow t) 


-------------------------------------------------------------------------------
dotAccessUnion ::  (Ord r, PP r, F.Reftable r) => Id (AnnSSA_ r) -> [RType r] -> TCM r (Maybe (RType r))
-------------------------------------------------------------------------------
dotAccessUnion f ts = 
  do  e              <- fromJust <$> getExpr
      tfs            <- mapM (dotAccess f) ts
      -- Gather all the types that do not throw errors, and the type of 
      -- the accessed expression that yields them
      let (ts', tfs') = unzip [(t,tf) | (t, Just tf) <- zip ts tfs]
      castM e (mkUnion ts) (mkUnion ts')
      case tfs' of
        [] -> return Nothing
        _  -> return $ Just $ mkUnion tfs'

      

-------------------------------------------------------------------------------
-- | Managing Annotations: Type Instantiations --------------------------------
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
getAnns :: (Ord r, F.Reftable r, Substitutable r (Fact_ r)) => TCM r (AnnInfo_ r)
-------------------------------------------------------------------------------
getAnns = do θ     <- tc_subst <$> get
             m     <- tc_anns  <$> get
             let m' = fmap (apply θ . sortNub) m
             _     <- modify $ \st -> st { tc_anns = m' }
             return m' 

-------------------------------------------------------------------------------
addAnn :: SourceSpan -> Fact_ r -> TCM r () 
-------------------------------------------------------------------------------
addAnn l f = modify $ \st -> st { tc_anns = inserts l f (tc_anns st) } 

-------------------------------------------------------------------------------
getAllAnns :: TCM r [AnnInfo_ r]  
-------------------------------------------------------------------------------
getAllAnns = tc_annss <$> get


-------------------------------------------------------------------------------
accumAnn :: (Ord r, F.Reftable r, Substitutable r (Fact_ r)) =>
  (AnnInfo_ r -> [(SourceSpan, String)]) -> TCM r () -> TCM r ()
-------------------------------------------------------------------------------
accumAnn check act 
  = do m     <- tc_anns <$> get 
       modify $ \st -> st {tc_anns = HM.empty}
       act
       m'    <- getAnns
       forM_ (check m') $ \(l, s) -> logError l s ()
       modify $ \st -> st {tc_anns = m} {tc_annss = m' : tc_annss st}

-------------------------------------------------------------------------------
execute     ::  (PP r, F.Reftable r) => V.Verbosity -> Nano z (RType r) -> TCM r a -> Either [(SourceSpan, String)] a
-------------------------------------------------------------------------------
execute verb pgm act 
  = case runState (runErrorT act) $ initState verb pgm of 
      (Left err, _) -> Left [(dummySpan,  err)]
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
       err = tcError l $ errorMissingSpec l f
       l   = srcPos f


-------------------------------------------------------------------------------
setExpr   :: Maybe (Expression (AnnSSA_ r)) -> TCM r () 
-------------------------------------------------------------------------------
setExpr eo = modify $ \st -> st { tc_expr = eo }


-------------------------------------------------------------------------------
getExpr   :: TCM r (Maybe (Expression (AnnSSA_ r)))
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
unifyTypesM :: (IsLocated l, Ord r, PP r, F.Reftable r) => l -> String -> [RType r] -> [RType r] -> TCM r (Subst_ r)
----------------------------------------------------------------------------------
unifyTypesM l msg t1s t2s
  -- TODO: This check might be done multiple times
  | length t1s /= length t2s = tcError l errorArgMismatch 
  | otherwise                = do θ <- getSubst 
                                  γ <- getTDefs
                                  case unifys γ θ t1s t2s of
                                    Left msg' -> tcError l $ msg ++ "\n" ++ msg'
                                    Right θ'  -> setSubst θ' >> return θ' 

----------------------------------------------------------------------------------
unifyTypeM :: (Ord r, PrintfArg t1, PP r, PP a, F.Reftable r, IsLocated l) =>
  l -> t1 -> a -> RType r -> RType r -> TCM r (Subst_ r)
----------------------------------------------------------------------------------
unifyTypeM l m e t t' = unifyTypesM l msg [t] [t']
  where 
    msg              = errorWrongType m e t t'


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


--------------------------------------------------------------------------------
--  Cast Helpers ---------------------------------------------------------------
--------------------------------------------------------------------------------


-----------------------------------------------------------------------------
withExpr  :: Maybe (Expression (AnnSSA_ r)) -> TCM r a -> TCM r a
-----------------------------------------------------------------------------
withExpr e action = 
  do  eold  <- getExpr 
      setExpr  e 
      r     <- action 
      setExpr  eold
      return $ r


--------------------------------------------------------------------------------
castM     :: (Ord r, PP r, F.Reftable r) => Expression (AnnSSA_ r) -> RType r -> RType r -> TCM r ()
--------------------------------------------------------------------------------
castM e t t'    = subTypeM t t' >>= go
  where go SupT = addDownCast e t'
        go Rel  = addDownCast e t'
        go SubT = addUpCast e t'
        go EqT  = return ()
        go Nth  = addDeadCast e t'


--------------------------------------------------------------------------------
castsM    :: (Ord r, PP r, F.Reftable r) => [Expression (AnnSSA_ r)] -> [RType r] -> [RType r] -> TCM r ()
--------------------------------------------------------------------------------
castsM     = zipWith3M_ castM 


--------------------------------------------------------------------------------
addUpCast :: Expression (AnnSSA_ r) -> RType r -> TCM r ()
--------------------------------------------------------------------------------
addUpCast e t = modify $ \st -> st { tc_casts = M.insert e (UCST t) (tc_casts st) }

--------------------------------------------------------------------------------
addDownCast :: (Ord r, PP r, F.Reftable r) => Expression (AnnSSA_ r) -> RType r -> TCM r ()
--------------------------------------------------------------------------------
addDownCast e t = modify $ \st -> st { tc_casts = M.insert e (DCST t) (tc_casts st) }


--------------------------------------------------------------------------------
addDeadCast :: Expression (AnnSSA_ r) -> RType r -> TCM r ()
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
transform :: Expression (AnnSSA_ r) -> PM r (Expression (AnnSSA_ r))
--------------------------------------------------------------------------------
transform e = 
  do  c  <- m <$> get      
      put (PS $ M.delete e c)
      return $ patchExpr c e

--------------------------------------------------------------------------------
patchExpr :: Casts_ r -> Expression (AnnSSA_ r) -> Expression (AnnSSA_ r)
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




