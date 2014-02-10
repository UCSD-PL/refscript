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
  -- , freshTArray

  -- * Substitutions
  , getSubst
  , setSubst

  -- * Function Types
  , tcFunTys

  -- * Annotations
  , addAnn    -- TEMP
  , accumAnn
  , getAllAnns
  , remAnn

  -- * Subtyping
  , subTypeM  , subTypeM'
  , subTypesM
  , checkAnnotation

  -- * Unification
  , unifyTypeM
  , unifyTypesM

  -- * Casts
  , castM
  , addDeadCast 

  -- * Get Type Signature 
  , getSpecOrDie
  , getSpecM
  , addSpec

  -- * Expression Getter/Setter
  -- , withExpr

  -- * Patch the program with assertions
  -- , patchPgmM

  -- * Verbosity
  , whenLoud', whenLoud
  , whenQuiet', whenQuiet

  -- * This
  , tcPeekThis
  , tcWithThis

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
import           Language.Nano.Misc             (unique, fth4)

import           Language.Nano.Types
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Typecheck.Unify
import           Language.Nano.Typecheck.Compare
import           Language.Nano.Errors
import           Data.Monoid                  
import qualified Data.HashMap.Strict            as HM
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
                   -- Function definitions
                   , tc_specs  :: !(Env (RType r))
                   -- Verbosity
                   , tc_verb  :: V.Verbosity
                   -- This stack
                   , tc_this  :: ![RType r]
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
getSubst :: TCM r (RSubst r)
-------------------------------------------------------------------------------
getSubst = tc_subst <$> get 

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
freshTyArgs :: (PP r, F.Reftable r)
            => SourceSpan -> IContext -> [TVar] -> RType r -> TCM r (RType r)
-------------------------------------------------------------------------------
freshTyArgs l ξ αs t 
  = (`apply` t) <$> freshSubst l ξ αs

-------------------------------------------------------------------------------
freshSubst :: (PP r, F.Reftable r) => SourceSpan -> IContext -> [TVar] -> TCM r (RSubst r)
-------------------------------------------------------------------------------
freshSubst l ξ αs
  = do when (not $ unique αs) $ logError (errorUniqueTypeParams l) ()
       βs        <- mapM (freshTVar l) αs
       setTyArgs l ξ βs
       extSubst   $ βs 
       return     $ fromList $ zip αs (tVar <$> βs)

setTyArgs l ξ βs
  = do  {-m <- tc_anns <$> get-}
        {-when (hasTI l m) $ tcError $ errorMultipleTypeArgs l-}
        case map tVar βs of 
          [] -> return ()
          vs -> addAnn l $ TypInst ξ vs
    {-where-}
    {-   hasTI l m  = not $ null [ i | i@(TypInst _ _) <- HM.lookupDefault [] l m ]-}


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
addAnn :: (F.Reftable r) => SourceSpan -> Fact r -> TCM r () 
-------------------------------------------------------------------------------
addAnn l f = modify $ \st -> st { tc_anns = inserts l f (tc_anns st) } 

-------------------------------------------------------------------------------
-- remAnn :: (F.Reftable r) => SourceSpan -> TCM r () 
-------------------------------------------------------------------------------
remAnn l   = modify $ \st -> st { tc_anns = delLst l (tc_anns st) } 
  where
    delLst k m | not (HM.member k m)                  = m
    delLst k m | null (stl $ HM.lookupDefault [] k m) = HM.delete k m
    delLst _ _ | otherwise                            = errorstar "BUG remAnn"
    stl []     = []
    stl (_:xs) = xs

-------------------------------------------------------------------------------
getAllAnns :: TCM r [AnnInfo r]  
-------------------------------------------------------------------------------
getAllAnns = tc_annss <$> get


-------------------------------------------------------------------------------
accumAnn :: (Ord r, F.Reftable r, Substitutable r (Fact r)) =>
  (AnnInfo r -> [Error]) -> TCM r a -> TCM r a
-------------------------------------------------------------------------------
-- RJ: this function is gross. Why is it being used? why are anns not just
-- accumulated monotonically?
accumAnn check act 
  = do m     <- tc_anns <$> get 
       modify $ \st -> st {tc_anns = HM.empty}
       z     <- act
       m'    <- getAnns
       forM_ (check m') (`logError` ())
       modify $ \st -> st {tc_anns = m} {tc_annss = m' : tc_annss st}
       return z
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
                          tc_specs tc_verb tc_this
  where
    tc_errss = []
    tc_subst = mempty 
    tc_cnt   = 0
    tc_anns  = HM.empty
    tc_annss = []
    tc_specs = specs pgm
    tc_verb  = verb
    tc_this  = [tTop]


getSpecOrDie f = tc_specs <$> get >>= maybe e return . envFindTy f
  where e = tcError $ errorMissingSpec (srcPos f) f

getSpecM f = tc_specs <$> get >>= return . envFindTy f

addSpec x t = modify $ \st -> st { tc_specs = envAdds [(x,t)] (tc_specs st) } 


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


--------------------------------------------------------------------------------
--  Unification and Subtyping --------------------------------------------------
--------------------------------------------------------------------------------

----------------------------------------------------------------------------------
unifyTypesM :: (Ord r, PP r, F.Reftable r) => 
  TDefEnv r -> SourceSpan -> String -> [RType r] -> [RType r] -> TCM r (RSubst r)
----------------------------------------------------------------------------------
unifyTypesM γ l msg t1s t2s
  -- TODO: This check might be done multiple times
  | length t1s /= length t2s = tcError $ errorArgMismatch l 
  | otherwise                = do θ <- getSubst 
                                  case unifys l γ θ t1s t2s of
                                    Left err' -> tcError $ catMessage err' msg 
                                    Right θ'  -> setSubst θ' >> return θ' 

----------------------------------------------------------------------------------
unifyTypeM :: (Ord r, PrintfArg t1, PP r, PP a, F.Reftable r) =>
  TDefEnv r -> SourceSpan -> t1 -> a -> RType r -> RType r -> TCM r (RSubst r)
----------------------------------------------------------------------------------
unifyTypeM γ l m e t t' = unifyTypesM γ l msg [t] [t']
  where 
    msg               = ppshow $ errorWrongType l m e t t'


----------------------------------------------------------------------------------
subTypeM :: (Ord r, PP r, F.Reftable r) => 
  TDefEnv r -> RType r -> RType r -> TCM r SubDirection
----------------------------------------------------------------------------------
subTypeM γ t t' = return $ fth4 (compareTs γ t t')

----------------------------------------------------------------------------------
subTypeM' :: (IsLocated l, Ord r, PP r, F.Reftable r) => 
  TDefEnv r -> l -> RType r -> RType r -> TCM r ()
----------------------------------------------------------------------------------
subTypeM' _ _ _  = error "unimplemented: subTypeM\'"
 
----------------------------------------------------------------------------------
subTypesM :: (Ord r, PP r, F.Reftable r) => 
  TDefEnv r -> [RType r] -> [RType r] -> TCM r [SubDirection]
----------------------------------------------------------------------------------
subTypesM γ ts ts' = zipWithM (subTypeM γ) ts ts'

----------------------------------------------------------------------------------
checkAnnotation :: (F.Reftable r, PP r, Ord r) => 
  TDefEnv r -> String -> Expression (AnnSSA r) -> RType r -> RType r ->  TCM r (RType r) 
----------------------------------------------------------------------------------
checkAnnotation γ msg e t ta = do
    subTypeM γ t ta >>= sub
  where
    sub SubT = return ta 
    sub EqT  = return ta
    sub _    = tcError $ catMessage err msg' 
    err      = errorAnnotation (srcPos $ getAnnotation e) e t ta
    msg'     = "[" ++ msg ++ "]"

--------------------------------------------------------------------------------
--  Cast Helpers ---------------------------------------------------------------
--------------------------------------------------------------------------------

-- | For the expression @e@, check the subtyping relation between the type @t@
-- which is the actual type for @e@ and @t'@ which is the desired (cast) type
-- and insert the right kind of cast. 
--------------------------------------------------------------------------------
castM :: (Ord r, PP r, F.Reftable r) => 
  TDefEnv r -> IContext -> Expression (AnnSSA r) -> RType r -> RType r 
    -> TCM r (Expression (AnnSSA r))
--------------------------------------------------------------------------------
-- Special case casting for objects 
castM γ ξ e fromT toT | any isObj [fromT, toT] = subTypeM γ fromT toT >>= go
  where
    go EqT          = return e
    go SubT         = addUpCast   ξ e toT
    go _            = addDeadCast ξ e toT

castM γ ξ e fromT toT = subTypeM γ fromT toT >>= go
  where 
    go SupT         = addDownCast ξ e toT    
    go Rel          = addDownCast ξ e toT   
    go SubT         = addUpCast   ξ e toT   
    go Nth          = addDeadCast ξ e toT   
    go EqT          = return e 

addUpCast   ξ e t = addCast ξ e (UCST t)
addDownCast ξ e t = addCast ξ e (DCST t) 
addDeadCast ξ e t = addCast ξ e (DC t)

addCast     ξ e c = addAnn loc fact >> return (wrapCast loc fact e)
  where 
    loc           = srcPos e
    fact          = TCast ξ c

wrapCast _ f (Cast (Ann l fs) e) = Cast (Ann l (f:fs)) e
wrapCast l f e                   = Cast (Ann l [f])    e


tcFunTys l f xs ft = 
  case funTys l f xs ft of 
    Left e  -> tcError e 
    Right a -> return a


-- | `this`

tcPeekThis     = safeHead "get 'this'" <$> (tc_this <$> get)

tcPushThis t   = modify $ \st -> st { tc_this = t : tc_this st } 

tcPopThis      = modify $ \st -> st { tc_this = tail $ tc_this st } 

tcWithThis t p = do { tcPushThis t; a <- p; tcPopThis; return a } 

