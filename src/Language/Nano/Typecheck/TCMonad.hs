{- LANGUAGE TypeSynonymInstances       #-}
{- LANGUAGE FlexibleInstances          #-}
{- LANGUAGE NoMonomorphismRestriction  #-}
{- LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds           #-}
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
import           Control.Applicative                ((<$>))
import qualified Data.HashSet                       as S
import qualified Data.List                          as L
import           Data.Maybe                         (fromJust)
import           Control.Monad.State
import           Control.Monad.Error                hiding (Error)
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc 
import qualified Language.Fixpoint.Types            as F

import           Language.Nano.Env
import           Language.Nano.Misc                 (unique, snd4, thd4, fst4, fth4, setFth4)

import           Language.Nano.Types
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Typecheck.Unify
import           Language.Nano.Typecheck.Compare
import           Language.Nano.Errors
import           Data.Monoid                  
import qualified Data.HashMap.Strict                as HM
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations

-- import           Debug.Trace                      (trace)
import qualified System.Console.CmdArgs.Verbosity   as V

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
                   -- Defined types
                   , tc_tdefs  :: !(TDefEnv (RType r))
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
getTDef  :: TCM r (TDefEnv (RType r)) 
-------------------------------------------------------------------------------
getTDef = tc_tdefs <$> get

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
                          tc_specs tc_tdefs tc_verb tc_this
  where
    tc_errss = []
    tc_subst = mempty 
    tc_cnt   = 0
    tc_anns  = HM.empty
    tc_annss = []
    tc_specs = specs pgm
    tc_tdefs = mempty
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
  TDefEnv (RType r) -> SourceSpan -> String -> [RType r] -> [RType r] -> TCM r (RSubst r)
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
  TDefEnv (RType r) -> SourceSpan -> t1 -> a -> RType r -> RType r -> TCM r (RSubst r)
----------------------------------------------------------------------------------
unifyTypeM γ l m e t t' = unifyTypesM γ l msg [t] [t']
  where 
    msg               = ppshow $ errorWrongType l m e t t'



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
-- castM γ ξ e fromT toT | any isObj [fromT, toT] = subTypeM γ fromT toT >>= go
--   where
--     go EqT          = return e
--     go SubT         = addUpCast   ξ e toT
--     go _            = addDeadCast ξ e toT

castM γ ξ e fromT toT = do
    (_,_,_,d) <- compareTs fromT toT
    case d of 
      SupT -> addDownCast ξ e toT    
      Rel  -> addDownCast ξ e toT   
      SubT -> addUpCast   ξ e toT   
      Nth  -> addDeadCast ξ e toT   
      EqT  -> return e 


type PPR r = (PP r, F.Reftable r)


-- RJ: 
-- | `compareTs` 
--    returns:
-- ∙ A padded version of the upper bound of @t1@ and @t2@
-- ∙ An equivalent version of @t1@ that has the same sort as the second (RJ: first?) output
-- ∙ An equivalent version of @t2@ that has the same sort as the first output
-- ∙ A subtyping direction between @t1@ and @t2@
--
-- Padding the input types gives them the same sort, i.e. makes them compatible. 
---------------------------------------------------------------------------------------
compareTs :: (PPR r, Ord r) => RType r -> RType r -> TCM r (RType r, RType r, RType r, SubDirection)
---------------------------------------------------------------------------------------

-- Deal with some standard cases of subtyping, e.g.: Top, Null, Undefined ...
compareTs t1 t2 | toType t1 == toType t2 = return (ofType $ toType t1, t1, t2, EqT)

compareTs t1 t2 | isUndefined t1         = return $ setFth4 (padSimple t1 t2) SubT

-- NOTE: Null is NOT considered a subtype of all types. If null is to be 
-- expected this should be explicitly specified by using " + null"
-- compareTs γ t1 t2 | and [isNull t1, not $ isUndefined t2] = setFth4 (compareTs' γ t1 t2) SubT

-- | Top
compareTs t1 _  | isTop t1               = errorstar "unimplemented: compareTs - top"
compareTs t1 t2 | isTop t2               = return (t1', t1, t2', SubT)
  where
    t1' = setRTypeR t1 fTop -- this will be kVared
    -- @t2@ is a Top, so just to make the types compatible we will 
    -- use the base type of @t1@ and stregthen with @t2@'s refinement.
    t2' = setRTypeR t1 $ rTypeR t2
  

-- Eliminate top-level unions
compareTs t1 t2 | any isUnion [t1,t2]    = getTDef >>= \γ -> return (padUnion γ t1 t2)

-- | Arrays
compareTs a@(TArr _ _) a'@(TArr _ _  )   = padArray a a'

-- | Type definitions

-- TODO: cyclic types

compareTs (TApp d1@(TRef _) t1s r1) (TApp d2@(TRef _) t2s r2) | d1 == d2 = undefined 





--   (mk tjs fTop, mk t1s' r1, mk t2s' r2, mconcatP bds)
--   where
--     (tjs, t1s', t2s', bds)  = unzip4 $ zipWith (compareTs γ) t1s t2s
--     mk xs r                 = TApp d1 xs r 

-- TODO : remove these - they can't hold.
-- compareTs' γ t1@(TApp (TDef _) _ _) t2       = compareTs γ (unfoldSafe γ t1) t2
-- compareTs' γ t1 t2@(TApp (TDef _) _ _)       = compareTs γ t1 (unfoldSafe γ t2)
 
-- | Everything else in TApp besides unions and defined types
-- compareTs' _ t1@(TApp _ _ _) t2@(TApp _ _ _) = padSimple t1 t2 
-- 
-- -- | Type Vars
-- compareTs' _ t1@(TVar _ _)   t2@(TVar _ _)   = padVar t1 t2
-- 
-- -- | Function Types
-- compareTs' γ t1@(TFun _ _ _) t2@(TFun _ _ _) = undefined -- padFun γ t1 t2
-- compareTs' _ (TFun _ _ _)    _               = error "Unimplemented compareTs-1"
-- compareTs' _ _               (TFun _ _ _)    = error "Unimplemented compareTs-2"
-- 
-- -- | TAll
-- compareTs' _ (TAll _ _  ) _                  = error "Unimplemented: compareTs-3"
-- compareTs' _ _            (TAll _ _  )       = error "Unimplemented: compareTs-4"
-- 
-- -- | Rest of cases
-- 
-- -- Let these cases be dealt by padding unions
-- compareTs' _ t1           t2                 = padSimple t1 t2 


-- | Pad objects

padTRefs γ (Id _ s1, t1s) (Id _ s2, t2s) = undefined 
    --  &&  S.null (S.difference (ss e1s) (ss e2s))
    -- &&  S.null (S.difference (ss e2s) (ss e1s))
  where 
    (f1s, f2s) = unzip [ (e1, e2) | e1 <- e1s, e2 <- e2s
                                  , f_sym e1 == f_sym e2 && f_acc e1 == f_acc e2 ]
    (d1,d2)    = (TD v1s f1s, TD v2s f2s) 
    {-superType  = S.isProperSubsetOf (ss e1s) (ss e2s)-}
    {-subType    = S.isProperSubsetOf (ss e2s) (ss e1s)-}

    TD v1s e1s = fromJust $ envFindTy s1 γ 
    TD v2s e2s = fromJust $ envFindTy s2 γ
    θ1         = fromList $ zip v1s t1s 
    θ2         = fromList $ zip v2s t2s 
    ss es      = S.fromList $ f_sym <$> es


-- | `padSimple`

-- Not calling padUnion because the inputs are too small
padSimple t1 t2
  | t1 == t2  = (t1, t1, t2, EqT)
  | otherwise = (joinType, t1', t2', Nth)
    where joinType = (ofType . toType) $ mkUnion [t1,t2]
          t1'      = mkUnion [t1, fmap F.bot t2]  -- Toplevel refs?
          t2'      = mkUnion [fmap F.bot t1, t2]


-- | `padVar`

padVar t1@(TVar v1 _ ) t2@(TVar v2 _) | v1 == v2 = ((ofType . toType) t1, t1, t2, EqT)
padVar t1 t2 = errorstar $ printf "padVar: cannot compare %s and %s" (ppshow t1) (ppshow t2)



-- | `padUnion`

-- Produces an equivalent type for @t1@ (resp. @t2@) that is extended with 
-- the missing sorts to the common upper bound of @t1@ and @t2@. The extra
-- types that are added in the union are refined with False to keep the
-- equivalence with the input types.
--
-- The output is the following tuple:
--  ∙ common upper bound type (@t1@ ∪ @t2@) with topped predicates
--  ∙ adjusted type for @t1@ to be sort compatible,
--  ∙ adjusted type for @t2@ to be sort compatible
--  ∙ a subtyping direction

-- Example:
--  {Int | p} ㄩ {Bool | q} => ({Int | ⊥    } ∪ {Bool | ⊥    },
--                              {Int | p    } ∪ {Bool | ⊥    },
--                              {Int | ⊥    } ∪ {Bool | q    },
--                              unrelated )
--------------------------------------------------------------------------------
padUnion ::  (Eq r, Ord r, F.Reftable r, PP r) => 
             TDefEnv (RType r)  -- Type defs
          -> RType r            -- LHS
          -> RType r            -- RHS
          -> (  RType r,        -- The join of the two types
                RType r,        -- The equivalent to @t1@
                RType r,        -- The equivalent to @t2@
                SubDirection)   -- Subtyping relation between LHS and RHS
--------------------------------------------------------------------------------
padUnion env t1 t2 = 
  (joinType, mkUnionR topR1 $ t1s, 
             mkUnionR topR2 $ t2s, direction)
  where
    -- Extract top-level refinements
    topR1       = rUnion t1
    topR2       = rUnion t2

    -- No reason to add the kVars here. They will be added in the CGMonad
    joinType   = mkUnion $ (ofType . toType) <$> ((fst4 <$> commonTs) ++ d1s ++ d2s)
    (t1s, t2s) = unzip $ safeZip "unionParts" t1s' t2s'

    -- It is crucial to sort the types so that they are aligned
    t1s'       = L.sort $ commonT1s ++ d1s ++ (fmap F.bot <$> d2s)
    t2s'       = L.sort $ commonT2s ++ d2s ++ (fmap F.bot <$> d1s)

    commonT1s  = snd4 <$> commonTs
    commonT2s  = thd4 <$> commonTs

    commonTs = 
      {-tracePP "padUnion: compaTs on common parts" $ -}
      map (uncurry $ compareTs env) $ cmnPs

    -- To figure out the direction of the subtyping, we must take into account:
    direction  = distSub &+& comSub
    -- ∙ The distinct types (the one that has more is a supertype)
    distSub   = case (d1s, d2s) of
                  ([], []) -> EqT
                  ([], _ ) -> SubT  -- <:
                  (_ , []) -> SupT  -- >:
                  (_ , _ ) -> Nth -- no relation
    -- ∙ The common types (recursively call `compareTs` to compare the types
    --   of the parts and join the subtyping relations)
    comSub     = mconcatS $ fth4 <$> commonTs
    
    (cmnPs, d1s, d2s) =  {- tracePP "padUnion: unionParts" $ -} unionParts env t1 t2


-- | `padArray`
padArray (TArr t1 r1) (TArr t2 r2) = do
    (tj, t1', t2', ad) <- compareTs t1 t2
    return (TArr tj fTop, TArr t1' r1, TArr t2' r2, arrDir ad)
padArray _ _ _ = errorstar "BUG: padArray can only pad Arrays"     







--------------------
-- Move the compareTs here and add it into the monad so that it updates the
-- class Env
------------------


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

