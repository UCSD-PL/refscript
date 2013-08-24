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

data TCState = TCS {
                   -- Errors
                     tc_errss :: ![(SourceSpan, String)]
                   , tc_subst :: !SubstR
                   , tc_cnt   :: !Int
                   -- Annotations
                   , tc_anns  :: AnnInfoR
                   , tc_annss :: [AnnInfoR]
                   -- Cast map: 
                   , tc_casts :: M.Map (Expression AnnSSAR) (Cast RefType)
                   -- Function definitions
                   , tc_defs  :: !(Env RefType) 
                   -- Type definitions
                   , tc_tdefs :: !(Env RefType)
                   -- The currently typed expression 
                   , tc_expr  :: Maybe (Expression AnnSSAR)

                   -- Verbosiry
                   , tc_verb  :: V.Verbosity
                   }

type TCM      = ErrorT String (State TCState)

type AnnSSAR  = AnnSSA_  F.Reft
type AnnInfoR = AnnInfo_ F.Reft
type FactR    = Fact_    F.Reft
type SubstR   = Subst_   F.Reft

-------------------------------------------------------------------------------
whenLoud :: TCM () -> TCM ()
-------------------------------------------------------------------------------
whenLoud  act = whenLoud' act $ return ()

-------------------------------------------------------------------------------
whenLoud' :: TCM a -> TCM a -> TCM a
-------------------------------------------------------------------------------
whenLoud' loud other = do  v <- tc_verb <$> get
                           case v of
                             V.Loud -> loud 
                             _      -> other

-------------------------------------------------------------------------------
whenQuiet :: TCM () -> TCM ()
-------------------------------------------------------------------------------
whenQuiet  act = whenQuiet' act $ return ()

-------------------------------------------------------------------------------
whenQuiet' :: TCM a -> TCM a -> TCM a
-------------------------------------------------------------------------------
whenQuiet' quiet other = do  v <- tc_verb <$> get
                             case v of
                               V.Quiet -> quiet
                               _       -> other



-------------------------------------------------------------------------------
getTDefs :: TCM (Env RefType)
-------------------------------------------------------------------------------
getTDefs = tc_tdefs <$> get 

-------------------------------------------------------------------------------
getSubst :: TCM SubstR
-------------------------------------------------------------------------------
getSubst = tc_subst <$> get 

getCasts = do c <- tc_casts <$> get 
              return $ M.toList c

-------------------------------------------------------------------------------
setSubst   :: SubstR -> TCM () 
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
freshTyArgs :: (PP r, F.Reftable r) => SourceSpan -> ([TVar], RefType) -> TCM RefType
-------------------------------------------------------------------------------
freshTyArgs l (αs, t) 
  = (`apply` t) <$> freshSubst l αs

freshSubst :: SourceSpan -> [TVar] -> TCM SubstR
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
dotAccess :: Id AnnSSAR -> RefType -> TCM (Maybe RefType)
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
dotAccessUnion :: Id AnnSSAR -> [RefType] -> TCM (Maybe RefType)
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
getAnns :: TCM AnnInfoR
-------------------------------------------------------------------------------
getAnns = do θ     <- tc_subst <$> get
             m     <- tc_anns  <$> get
             let m' = fmap (apply θ . sortNub) m
             _     <- modify $ \st -> st { tc_anns = m' }
             return m' 

-------------------------------------------------------------------------------
addAnn :: SourceSpan -> FactR -> TCM () 
-------------------------------------------------------------------------------
addAnn l f = modify $ \st -> st { tc_anns = inserts l f (tc_anns st) } 

-------------------------------------------------------------------------------
getAllAnns :: TCM [AnnInfoR]  
-------------------------------------------------------------------------------
getAllAnns = tc_annss <$> get


-------------------------------------------------------------------------------
accumAnn :: (AnnInfoR -> [(SourceSpan, String)]) -> TCM () -> TCM ()
-------------------------------------------------------------------------------
accumAnn check act 
  = do m     <- tc_anns <$> get 
       modify $ \st -> st {tc_anns = HM.empty}
       act
       m'    <- getAnns
       forM_ (check m') $ \(l, s) -> logError l s ()
       modify $ \st -> st {tc_anns = m} {tc_annss = m' : tc_annss st}

-------------------------------------------------------------------------------
execute     :: V.Verbosity -> Nano z RefType -> TCM a -> Either [(SourceSpan, String)] a
-------------------------------------------------------------------------------
execute verb pgm act 
  = case runState (runErrorT act) $ initState verb pgm of 
      (Left err, _) -> Left [(dummySpan,  err)]
      (Right x, st) ->  applyNonNull (Right x) Left (reverse $ tc_errss st)


initState :: V.Verbosity -> Nano z RefType -> TCState
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
setExpr   :: Maybe (Expression AnnSSAR) -> TCM () 
-------------------------------------------------------------------------------
setExpr eo = modify $ \st -> st { tc_expr = eo }


-------------------------------------------------------------------------------
getExpr   :: TCM (Maybe (Expression AnnSSAR))
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
              



-- | Monadic unfolding
-------------------------------------------------------------------------------
unfoldFirstTC :: RefType -> TCM RefType
-------------------------------------------------------------------------------
unfoldFirstTC t = getTDefs >>= \γ -> return $ unfoldFirst γ t


-------------------------------------------------------------------------------
unfoldSafeTC :: RefType -> TCM RefType
-------------------------------------------------------------------------------
unfoldSafeTC   t = getTDefs >>= \γ -> return $ unfoldSafe γ t



--------------------------------------------------------------------------------
--  Unification and Subtyping --------------------------------------------------
--------------------------------------------------------------------------------


----------------------------------------------------------------------------------
unifyTypesM :: (IsLocated l) => l -> String -> [RefType] -> [RefType] -> TCM SubstR
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
--unifyTypeM :: (IsLocated l) => l -> String -> Expression AnnSSA -> Type -> Type -> TCM Subst
----------------------------------------------------------------------------------
unifyTypeM l m e t t' = unifyTypesM l msg [t] [t']
  where 
    msg              = errorWrongType m e t t'


----------------------------------------------------------------------------------
subTypeM :: RefType -> RefType -> TCM SubDirection
----------------------------------------------------------------------------------
subTypeM t t' 
  = do  θ            <- getTDefs 
        let (_,_,_,d) = compareTs θ t t'
        return $  {- trace (printf "subTypeM: %s %s %s" (ppshow t) (ppshow d) (ppshow t')) -}  d

----------------------------------------------------------------------------------
subTypeM' :: (IsLocated l) => l -> Type -> Type -> TCM ()
----------------------------------------------------------------------------------
subTypeM' _ _ _  = error "unimplemented: subTypeM\'"
 
----------------------------------------------------------------------------------
subTypesM :: [RefType] -> [RefType] -> TCM [SubDirection]
----------------------------------------------------------------------------------
subTypesM ts ts' = zipWithM subTypeM ts ts'


--------------------------------------------------------------------------------
--  Cast Helpers ---------------------------------------------------------------
--------------------------------------------------------------------------------


-----------------------------------------------------------------------------
withExpr  :: Maybe (Expression AnnSSAR) -> TCM a -> TCM a
-----------------------------------------------------------------------------
withExpr e action = 
  do  eold  <- getExpr 
      setExpr  e 
      r     <- action 
      setExpr  eold
      return $ r


--------------------------------------------------------------------------------
castM     :: Expression AnnSSAR -> RefType -> RefType -> TCM ()
--------------------------------------------------------------------------------
castM e t t'    = subTypeM t t' >>= go
  where go SupT = addDownCast e t'
        go Rel  = addDownCast e t'
        go SubT = addUpCast e t'
        go EqT  = return ()
        go Nth  = addDeadCast e t'


--------------------------------------------------------------------------------
castsM    :: [Expression AnnSSAR] -> [RefType] -> [RefType] -> TCM ()
--------------------------------------------------------------------------------
castsM     = zipWith3M_ castM 


--------------------------------------------------------------------------------
addUpCast :: Expression AnnSSAR -> RefType -> TCM ()
--------------------------------------------------------------------------------
addUpCast e t = modify $ \st -> st { tc_casts = M.insert e (UCST t) (tc_casts st) }

--------------------------------------------------------------------------------
addDownCast :: Expression AnnSSAR -> RefType -> TCM ()
--------------------------------------------------------------------------------
addDownCast e t = modify $ \st -> st { tc_casts = M.insert e (DCST t) (tc_casts st) }


--------------------------------------------------------------------------------
addDeadCast :: Expression AnnSSAR -> RefType -> TCM ()
--------------------------------------------------------------------------------
addDeadCast e t = modify $ \st -> st { tc_casts = M.insert e (DC t) (tc_casts st) } 


--------------------------------------------------------------------------------
patchPgmM :: Nano AnnSSAR RefType -> TCM (Nano AnnSSAR RefType)
--------------------------------------------------------------------------------
patchPgmM pgm = 
  do  c <- tc_casts <$> get
      return $ fst $ runState (everywhereM' (mkM transform) pgm) (PS c)


data PState = PS { m :: Casts }
type PM     = State PState

--------------------------------------------------------------------------------
transform :: Expression AnnSSAR -> PM (Expression AnnSSAR)
--------------------------------------------------------------------------------
transform e = 
  do  c  <- m <$> get      
      put (PS $ M.delete e c)
      return $ patchExpr c e

--------------------------------------------------------------------------------
patchExpr :: Casts -> Expression AnnSSAR -> Expression AnnSSAR
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




