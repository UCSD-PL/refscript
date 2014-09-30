{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE LiberalTypeSynonyms       #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DoAndIfThenElse           #-}

-- | This module has the code for the Type-Checker Monad. 

module Language.Nano.Typecheck.TCMonad (
  -- * TC Monad
    TCM
 
  -- * Execute 
  , execute
  , runFailM, runMaybeM
              
  -- * Errors
  , logError, tcError, tcWrap

  -- * Freshness
  , freshTyArgs, freshenAnn

  -- * Substitutions
  , getSubst, setSubst, addSubst

  -- * Function Types
  , tcFunTys -- , tcMethTys, tcCtorTys

  -- * Annotations
  , addAnn {-TEMP-}, getAnns --, getDef, setDef

  -- * Unification
  , unifyTypeM, unifyTypesM

  -- * Subtyping
  , subtypeM, isSubtype, checkTypes

  -- * Casts
  , castM, deadcastM, freshId

  -- * Verbosity / Options
  , whenLoud', whenLoud, whenQuiet', whenQuiet, getOpts

  )  where 


import           Control.Applicative                ((<$>), (<*>))
import           Control.Monad.State
import           Control.Monad.Trans.Except
import           Control.Monad.Except               (catchError)
import           Data.Function                      (on)
import           Data.Generics
import qualified Data.HashMap.Strict                as M
import           Data.Maybe                         (catMaybes, isJust, maybeToList)
import           Data.Monoid                  
import qualified Data.IntMap.Strict                 as I

import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc 
import qualified Language.Fixpoint.Types            as F

import           Language.Nano.Annots
import           Language.Nano.CmdLine
import           Language.Nano.Env
import           Language.Nano.Locations
import           Language.Nano.Misc
import           Language.Nano.Program
import           Language.Nano.Types
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Environment
import           Language.Nano.Typecheck.Sub
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Typecheck.Unify
import           Language.Nano.Typecheck.Resolve
import           Language.Nano.Errors

import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Syntax.Annotations

-- import           Debug.Trace                      (trace)
--
import qualified System.Console.CmdArgs.Verbosity   as V

type PPR r = (PP r, F.Reftable r, Data r)
type PPRSF r = (PPR r, Substitutable r (Fact r), Free (Fact r)) 


-------------------------------------------------------------------------------
-- | Typechecking monad 
-------------------------------------------------------------------------------

data TCState r = TCS {
  -- 
  -- ^ Errors
  --
    tc_errors   :: ![Error]
  -- 
  -- ^ Substitutions
  --
  , tc_subst    :: !(RSubst r)
  -- 
  -- ^ Freshness counter
  --
  , tc_cnt      :: !Int
  -- 
  -- ^ Annotations
  --
  , tc_anns     :: AnnInfo r
  -- 
  -- ^ Verbosity
  --
  , tc_verb     :: V.Verbosity
  -- 
  -- ^ configuration options
  --
  , tc_opts     :: Config               
  -- 
  -- ^ AST Counter
  --
  , tc_ast_cnt  :: NodeId

  }

type TCM r     = ExceptT Error (State (TCState r))

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
whenQuiet' quiet other = do  tc_verb <$> get >>= \case 
                               V.Quiet -> quiet
                               _       -> other

getOpts :: TCM r Config
getOpts = tc_opts <$> get


-------------------------------------------------------------------------------
getSubst :: TCM r (RSubst r)
-------------------------------------------------------------------------------
getSubst = tc_subst <$> get 

-------------------------------------------------------------------------------
setSubst   :: RSubst r -> TCM r () 
-------------------------------------------------------------------------------
setSubst θ = modify $ \st -> st { tc_subst = θ }


-------------------------------------------------------------------------------
addSubst :: (PPR r, IsLocated a) => a -> RSubst r -> TCM r ()
-------------------------------------------------------------------------------
addSubst l θ 
  = do θ0 <- appSu θ <$> getSubst 
       case checkSubst θ0 θ of 
         [] -> return ()
         ts -> forM_ ts $ (\(t1,t2) -> tcError $ errorMergeSubst (srcPos l) t1 t2)
       setSubst $ θ0 `mappend` θ
  where
    appSu θ                   = fromList . (mapSnd (apply θ) <$>) . toList 
    checkSubst (Su m) (Su m') = checkIntersection m m' 
    checkIntersection m n     = catMaybes $ check <$> (M.toList $ M.intersectionWith (,) m n)
    check (k, (t,t'))         | uninstantiated k t = Nothing
                              | eqT t t'           = Nothing
                              | otherwise          = Just (t,t')
    eqT                       = on (==) toType
    uninstantiated k t        = eqT (tVar k) t


-------------------------------------------------------------------------------
extSubst :: (F.Reftable r, PP r) => [TVar] -> TCM r ()
-------------------------------------------------------------------------------
extSubst βs = getSubst >>= setSubst . (`mappend` θ)
  where 
    θ       = fromList $ zip βs (tVar <$> βs)


-- | Error handling

-------------------------------------------------------------------------------
tcError     :: Error -> TCM r a
-------------------------------------------------------------------------------
tcError err = throwE $ catMessage err "TC-ERROR\n"

-------------------------------------------------------------------------------
tcWrap :: TCM r a -> TCM r (Either Error a)
-------------------------------------------------------------------------------
tcWrap act = (Right <$> act) `catchError` (return . Left)

-------------------------------------------------------------------------------
logError   :: Error -> a -> TCM r a
-------------------------------------------------------------------------------
logError err x = (modify $ \st -> st { tc_errors = err : tc_errors st}) >> return x

-------------------------------------------------------------------------------
freshTyArgs :: PPR r => AnnSSA r -> Int -> IContext -> [TVar] -> RType r -> TCM r (RType r)
-------------------------------------------------------------------------------
freshTyArgs a n ξ αs t = (`apply` t) <$> freshSubst a n ξ αs

-------------------------------------------------------------------------------
freshSubst :: PPR r => AnnSSA r -> Int -> IContext -> [TVar] -> TCM r (RSubst r)
-------------------------------------------------------------------------------
freshSubst (Ann i l _) n ξ αs
  = do when (not $ unique αs) $ logError (errorUniqueTypeParams l) ()
       βs        <- mapM (freshTVar l) αs
       setTyArgs i n ξ βs
       extSubst   $ βs 
       return     $ fromList $ zip αs (tVar <$> βs)

-------------------------------------------------------------------------------
setTyArgs :: PPR r => NodeId -> Int -> IContext -> [TVar] -> TCM r ()
-------------------------------------------------------------------------------
setTyArgs l i ξ βs
  = case map tVar βs of 
      [] -> return ()
      vs -> addAnn l $ TypInst i ξ vs


-------------------------------------------------------------------------------
-- | Managing Annotations: Type Instantiations
-------------------------------------------------------------------------------

-- PV: is sortNub needed here?
-------------------------------------------------------------------------------
getAnns :: (F.Reftable r, Substitutable r (Fact r)) => TCM r (AnnInfo r)
-------------------------------------------------------------------------------
getAnns = do θ     <- tc_subst <$> get
             m     <- tc_anns  <$> get
             let m' = fmap (apply θ {-. sortNub-}) m
             _     <- modify $ \st -> st { tc_anns = m' }
             return m' 

-------------------------------------------------------------------------------
addAnn :: PPR r => NodeId -> Fact r -> TCM r () 
-------------------------------------------------------------------------------
addAnn i f = modify $ \st -> st { tc_anns = I.insertWith (++) i [f] $ tc_anns st } 
 
-------------------------------------------------------------------------------
execute ::  PPR r => Config -> V.Verbosity -> NanoSSAR r -> TCM r a -> Either (F.FixResult Error) a
-------------------------------------------------------------------------------
execute cfg verb pgm act 
  = case runState (runExceptT act) $ initState cfg verb pgm of 
      (Left err, _) -> Left $ F.Unsafe [err]
      (Right x, st) -> applyNonNull (Right x) (Left . F.Unsafe) (reverse $ tc_errors st)

-------------------------------------------------------------------------------
initState :: PPR r => Config -> V.Verbosity -> NanoSSAR r -> TCState r
-------------------------------------------------------------------------------
initState cfg verb pgm = TCS tc_errors tc_subst tc_cnt tc_anns tc_verb tc_opts tc_ast_cnt
  where
    tc_errors   = []
    tc_subst    = mempty 
    tc_cnt      = 0
    tc_anns     = I.empty
    tc_verb     = verb
    tc_opts     = cfg
    tc_ast_cnt  = max_id pgm


--------------------------------------------------------------------------
-- | Generating Fresh Values 
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

freshTVar l _ =  ((`TV` l). F.intSymbol (F.symbol "T")) <$> tick

freshId l =  Id l . ("__cast_" ++) . show <$> tick


--------------------------------------------------------------------------------
-- | Unification and Subtyping
--------------------------------------------------------------------------------

----------------------------------------------------------------------------------
unifyTypesM :: PPR r => SourceSpan -> TCEnv r -> FuncInputs (RType r) 
                     -> FuncInputs (RType r) -> TCM r (RSubst r)
----------------------------------------------------------------------------------
unifyTypesM l γ t1s t2s 
  | sameLength t1s t2s = do θ <- getSubst
                            case unifys l γ θ (toList t1s) (toList t2s) of
                              Left err -> tcError $ err 
                              Right θ' -> setSubst θ' >> return θ' 
  | otherwise          = tcError $ errorArgMismatch l 
  where 
    sameLength (FI to ts) (FI to' ts') = isJust to == isJust to' && length ts == length ts'
    toList (FI to ts)                  = maybeToList to ++ ts

----------------------------------------------------------------------------------
unifyTypeM :: PPR r => SourceSpan -> TCEnv r -> RType r -> RType r -> TCM r (RSubst r)
----------------------------------------------------------------------------------
unifyTypeM l γ t t' = unifyTypesM l γ (FI Nothing [t]) (FI Nothing [t'])


--------------------------------------------------------------------------------
--  Cast Helpers
--------------------------------------------------------------------------------

-- | @deadcastM@ wraps an expression @e@ with a dead-cast around @e@. 
--------------------------------------------------------------------------------
deadcastM :: (PPR r) => IContext -> Error -> Expression (AnnSSA r) -> TCM r (Expression (AnnSSA r))
--------------------------------------------------------------------------------
deadcastM ξ err e
  = addCast ξ e $ CDead err tNull 


-- | For the expression @e@, check the subtyping relation between the type @t1@
--   (the actual type for @e@) and @t2@ (the target type) and insert the cast.
--------------------------------------------------------------------------------
castM :: PPR r => TCEnv r -> Expression (AnnSSA r) -> RType r -> RType r -> TCM r (Expression (AnnSSA r))
--------------------------------------------------------------------------------
castM γ e t1 t2 
  = case convert (srcPos e) γ t1 t2 of
      Left  e   -> tcError e
      Right CNo -> return e
      Right c   -> addCast (tce_ctx γ) e c


-- | Run the monad `a` in the current state. This action will not alter the
-- state.
--------------------------------------------------------------------------------
runFailM :: PPR r => TCM r a -> TCM r (Either Error a)
--------------------------------------------------------------------------------
runFailM a = fst . runState (runExceptT a) <$> get


--------------------------------------------------------------------------------
runMaybeM :: PPR r => TCM r a -> TCM r (Maybe a)
--------------------------------------------------------------------------------
runMaybeM a = runFailM a >>= \case 
                Right rr -> return $ Just rr
                Left _   -> return $ Nothing


-- | subTypeM will throw error if subtyping fails
--------------------------------------------------------------------------------
subtypeM :: PPR r => SourceSpan -> TCEnv r -> RType r -> RType r -> TCM r ()
--------------------------------------------------------------------------------
subtypeM l γ t1 t2 
  = case convert l γ t1 t2 of
      Left e          -> tcError e
      Right CNo       -> return  ()
      Right (CUp _ _) -> return  ()
      Right _         -> tcError $ errorSubtype l t1 t2


addCast ξ e c = addAnn i fact >> wrapCast loc fact e
  where
    i         = ann_id $ getAnnotation e
    loc       = ann    $ getAnnotation e
    fact      = TCast ξ c

wrapCast _ f (Cast_ (Ann i l fs) e) = Cast_ <$> freshenAnn (Ann i l (f:fs)) <*> return e
wrapCast l f e                      = Cast_ <$> freshenAnn (Ann (-1) l [f]) <*> return e

freshenAnn :: AnnSSA r -> TCM r (AnnSSA r)
freshenAnn (Ann _ l a)
  = do n     <- tc_ast_cnt <$> get 
       modify $ \st -> st {tc_ast_cnt = 1 + n}
       return $ Ann n l a



-- | tcFunTys: "context-sensitive" function signature
--------------------------------------------------------------------------------
tcFunTys :: (PPRSF r, F.Subable (RType r), F.Symbolic s, PP a) 
         => AnnSSA r -> a -> [s] -> RType r -> TCM r [(Int, ([TVar], Maybe (RType r), [RType r], RType r))]
--------------------------------------------------------------------------------
tcFunTys l f xs ft = either tcError return $ funTys l f xs ft 


-- --------------------------------------------------------------------------------
-- tcMethTys :: (PPRSF r, F.Subable (RType r), PP a) 
--           => AnnSSA r -> a -> (Mutability, RType r)
--           -> TCM r [(Int, ([TVar], Maybe (RType r), [RType r], RType r))]
-- --------------------------------------------------------------------------------
-- tcMethTys l f (m,t) 
--   = zip [0..] <$> mapM (methTys l f) (bkAnd t)
-- 
-- 
-- methTys l f ft0
--   = case remThisBinding ft0 of
--       Nothing          -> tcError $ errorNonFunction (srcPos l) f ft0 
--       Just (vs,s,bs,t) -> return  $ (vs,s,b_type <$> bs,t)
-- 

-- --------------------------------------------------------------------------------
-- tcCtorTys :: (PPRSF r, F.Subable (RType r), PP a) 
--           => AnnSSA r -> a -> RType r
--           -> TCM r [(Int, ([TVar], Maybe (RType r), [RType r], RType r))]
-- --------------------------------------------------------------------------------
-- tcCtorTys l f t = zip [0..] <$> mapM (methTys l f) (bkAnd t)


--------------------------------------------------------------------------------
checkTypes :: PPR r => TCEnv r -> TCM r ()
--------------------------------------------------------------------------------
checkTypes γ = mapM_ (safeExtends γ) types
  where 
    types = concatMap envToList $ m_types . snd <$> qenvToList (tce_mod γ)
    

--------------------------------------------------------------------------------
safeExtends :: (IsLocated l, PPR r) => TCEnv r -> (l, IfaceDef r) -> TCM r ()
--------------------------------------------------------------------------------
safeExtends _ p@(l,    ID _ _ _ Nothing        _  ) = return ()
safeExtends γ   (l, t@(ID _ c _ (Just (p, ts)) es)) = 
    case flatten' Nothing False γ t of
      Just ms -> 
        case flatten Nothing False γ . (,ts) =<< resolveRelNameInEnv γ p of
          Just ps  | isSubtype γ (mkTCons t_immutable ms) (mkTCons t_immutable ps) 
                  -> return ()
                   | otherwise         
                  -> tcError $ errorClassExtends (srcPos l) c p 
                               (mkTCons t_immutable ms) (mkTCons t_immutable ps) 
          Nothing -> tcError $ bugFlattenType (srcPos l) p
      Nothing -> tcError $ bugFlattenType (srcPos l) c
  where

-- Local Variables:
-- flycheck-disabled-checkers: (haskell-liquid)
-- End:
