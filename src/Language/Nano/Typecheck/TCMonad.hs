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

  -- * Dot Access
  -- , safeGetProp
  -- , safeGetIdx
  -- , indexType

  -- * Type definitions
  , getTDefs

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

  -- * Unfolding
  , unfoldFirstTC
  , unfoldSafeTC

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
  , getDefType 

  -- * Expression Getter/Setter
  , setExpr
  -- , getExpr
  -- , withExpr

  -- * Patch the program with assertions
  -- , patchPgmM

  -- * Verbosity
  , whenLoud', whenLoud
  , whenQuiet', whenQuiet

  -- * This
  , peekThis

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
import           Language.Nano.Typecheck.Unify
import           Language.Nano.Typecheck.Compare
import           Language.Nano.Typecheck.Unfold
import           Language.Nano.Errors
import           Data.Monoid                  
import qualified Data.HashMap.Strict            as HM
import qualified Data.Map                       as M
import           Data.Generics                  (Data(..))
import           Data.Maybe                     (fromJust)
import           Data.Generics.Aliases
import           Data.Typeable                  (Typeable (..))
import           Language.ECMAScript3.Parser.Type    (SourceSpan (..))
-- import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations
import           Language.Fixpoint.Misc

import           Debug.Trace                      (trace)
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
                   , tc_defs  :: !(Env (RType r))
                   -- Type definitions
                   , tc_tdefs :: !(Env (RType r))
                   -- The currently typed expression 
                   , tc_expr  :: Maybe (Expression (AnnSSA r))
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
getTDefs :: TCM r (Env (RType r))
-------------------------------------------------------------------------------
getTDefs = tc_tdefs <$> get 

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

freshSubst :: (PP r, F.Reftable r) => SourceSpan -> IContext -> [TVar] -> TCM r (RSubst r)
freshSubst l ξ αs
  = do when (not $ unique αs) $ logError (errorUniqueTypeParams l) ()
       βs        <- mapM (freshTVar l) αs
       setTyArgs l ξ βs
       extSubst βs 
       return    $ fromList $ zip αs (tVar <$> βs)

setTyArgs l ξ βs
  = do m <- tc_anns <$> get
       {-when (hasTI l m) $ tcError $ errorMultipleTypeArgs l-}
       addAnn l $ TypInst ξ (tVar <$> βs)
    where 
       hasTI l m = not $ null [ i | i@(TypInst _ _) <- HM.lookupDefault [] l m ]
       msg = printf "setTyArgs: l = %s ξ = %s" (ppshow l) (ppshow ξ) 


-------------------------------------------------------------------------------
-- | Field access -------------------------------------------------------------
-------------------------------------------------------------------------------

-- Access field @f@ of type @t@, adding a cast if needed to avoid errors.
-------------------------------------------------------------------------------
-- RJ: TODO FIELD safeGetProp :: (Ord r, PP r, F.Reftable r) => IContext -> String -> RType r -> TCM r (RType r)
-- RJ: TODO FIELD -------------------------------------------------------------------------------
-- RJ: TODO FIELD safeGetProp ξ f t = do
-- RJ: TODO FIELD      γ <- getTDefs
-- RJ: TODO FIELD      e <- fromJust <$> getExpr
-- RJ: TODO FIELD      case getProp γ f t of
-- RJ: TODO FIELD        Just (t', tf) -> castM ξ e t t' >> return tf
-- RJ: TODO FIELD        Nothing       -> error "safeGetProp" --TODO: deadcode
 
-- DEPRECATE:
-- Access index @i@ of type @t@.
-- In JavaScript semantics, all types can be indexed, besides null and undefined. 
-- So we're not gonna cast for those types
-- the accessed type here. Instead
-------------------------------------------------------------------------------
-- safeGetIdx :: (Ord r, PP r, F.Reftable r) => IContext -> Int -> RType r -> TCM r (RType r)
-- -------------------------------------------------------------------------------
-- safeGetIdx ξ f t = do  
--     γ <- getTDefs
--     e <- fromJust <$> getExpr
--     case getIdx γ f t of
--       Just (t',tf) -> castM ξ e t t' >> return tf
--       Nothing      -> error "safeGetIdx" --TODO: deadcode

-- Only support indexing in arrays atm. Discharging array bounds checks makes
-- sense only for array types. 
-------------------------------------------------------------------------------
-- RJ: TODO FIELD indexType :: (PP r, Ord r, F.Reftable r) => IContext -> RType r -> TCM r (RType r)
-- RJ: TODO FIELD -------------------------------------------------------------------------------
-- RJ: TODO FIELD indexType _ (TArr t _)        = return t 
-- RJ: TODO FIELD 
-- RJ: TODO FIELD indexType ξ t@(TApp TUn ts _) = do
-- RJ: TODO FIELD     e <- fromJust <$> getExpr
-- RJ: TODO FIELD     castM ξ e t t'
-- RJ: TODO FIELD     mkUnion <$> mapM (indexType ξ) arrs
-- RJ: TODO FIELD   where
-- RJ: TODO FIELD     t'   = mkUnion arrs
-- RJ: TODO FIELD     arrs = filter isArr ts
-- RJ: TODO FIELD 
-- RJ: TODO FIELD indexType _ _                 = errorstar "Unimplemented: indexing type other than array."


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
    delLst _ m | otherwise                            = errorstar "BUG remAnn"
    stl []     = []
    stl (x:xs) = xs

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
                       tc_defs tc_tdefs tc_expr tc_verb tc_this
  where
    tc_errss = []
    tc_subst = mempty 
    tc_cnt   = 0
    tc_anns  = HM.empty
    tc_annss = []
    tc_defs  = sigs pgm
    tc_tdefs = defs pgm
    tc_expr  = Nothing
    tc_verb  = verb
    tc_this  = [tTop]


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
        -- let (_,_,_,d) = compareTs θ (trace ("CompareTs:\n" ++ ppshow t ++ "\nvs\n" ++ ppshow t' ++ "\n") t) t'
        -- let (_,_,_,d) = tracePP ("CompareTs " ++ ppshow t ++ " : " ++ ppshow t') $ compareTs θ t t'
        let (_,_,_,d) = compareTs θ t t'
        return d

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
  String -> Expression (AnnSSA r) -> RType r -> RType r ->  TCM r (RType r) 
----------------------------------------------------------------------------------
checkAnnotation msg e t ta = do
    subTypeM t ta >>= sub
  where
    sub SubT = return ta 
    sub EqT  = return ta
    sub _    = tcError $ catMessage err msg' 
    err      = errorAnnotation (srcPos $ getAnnotation e) e t ta
    msg'     = "[" ++ msg ++ "]"

--------------------------------------------------------------------------------
--  Cast Helpers ---------------------------------------------------------------
--------------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- withExpr  :: Maybe (Expression (AnnSSA r)) -> TCM r a -> TCM r a
-- -----------------------------------------------------------------------------
-- withExpr e action = 
--   do  eold  <- getExpr 
--       setExpr  e 
--       r     <- action 
--       setExpr  eold
--       return $ r


-- | For the expression @e@, check the subtyping relation between the type @t@
-- which is the actual type for @e@ and @t'@ which is the desired (cast) type
-- and insert the right kind of cast. 
--------------------------------------------------------------------------------
castM :: (Ord r, PP r, F.Reftable r) => 
           IContext -> Expression (AnnSSA r) -> RType r -> RType r -> TCM r (Expression (AnnSSA r))
--------------------------------------------------------------------------------
-- Special case casting for objects 
castM ξ e fromT toT | any isObj [fromT, toT] = subTypeM fromT toT >>= go
  where
    go EqT          = return e
    go SubT         = addUpCast   ξ e toT
    go _            = addDeadCast ξ e toT

castM ξ e fromT toT = subTypeM fromT toT >>= go
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

peekThis = safeHead "get 'this'" <$> (tc_this <$> get)
