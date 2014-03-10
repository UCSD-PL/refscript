{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections             #-}
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
  , getDef
  , setDef
  , addObjLitTyM
  , getExts
  , getClasses

  -- * Unification
  , unifyTypeM
  , unifyTypesM

  -- * Casts
  , castM
  , addDeadCast 

  -- * TDefEnv
  , findTyIdOrDieM, findTyIdOrDieM'
  , findTySymM, findTySymOrDieM

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

  , getPropM, getPropTDefM

  )  where 

import           Text.Printf
import           Language.ECMAScript3.PrettyPrint
import           Control.Applicative                ((<$>))
import qualified Data.HashSet                       as S
import qualified Data.List                          as L
import           Data.Maybe                         (fromMaybe)
import           Data.Function                      (on)
import           Control.Monad.State
import           Control.Monad.Error                hiding (Error)
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc 
import qualified Language.Fixpoint.Types            as F

import           Language.Nano.Env
import           Language.Nano.Misc                 (unique, snd4, thd4, fst4, fth4, setFth4)

import           Language.Nano.Types
import           Language.Nano.Misc
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Lookup
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Typecheck.Unify
import           Language.Nano.Errors
import           Data.Monoid                  
import qualified Data.HashMap.Strict                as M
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations

-- import           Debug.Trace                      (trace)
import qualified System.Console.CmdArgs.Verbosity   as V

type PPR r = (PP r, F.Reftable r)
type PPRSF r = (PPR r, Substitutable r (Fact r), Free (Fact r)) 


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
  , tc_specs :: !(Env (RType r))

  -- Defined types
  , tc_defs  :: !(TDefEnv (RType r))

  -- Extern (unchecked) declarations
  , tc_ext   :: !(Env (RType r))

  -- Class definitions
  , tc_cls   :: Env (Statement (AnnSSA r))

  -- Verbosity
  , tc_verb  :: V.Verbosity

  -- This stack: if empty, assume top
  , tc_this  :: ![RType r]
  }

type TCM r     = ErrorT Error (State (TCState r))

-- type TCME r    = ErrorT Error (StateT (TCState r) (State (TDefEnv (RType r))))

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
getDef  :: TCM r (TDefEnv (RType r)) 
-------------------------------------------------------------------------------
getDef = tc_defs <$> get

-------------------------------------------------------------------------------
getExts  :: TCM r (Env (RType r)) 
-------------------------------------------------------------------------------
getExts = tc_ext <$> get

-------------------------------------------------------------------------------
getClasses  :: TCM r (Env (Statement (AnnSSA r)))
-------------------------------------------------------------------------------
getClasses = tc_cls <$> get

-------------------------------------------------------------------------------
setDef  :: TDefEnv (RType r) -> TCM r ()
-------------------------------------------------------------------------------
setDef γ = modify $ \u -> u { tc_defs = γ } 

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
    {-   hasTI l m  = not $ null [ i | i@(TypInst _ _) <- M.lookupDefault [] l m ]-}


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
    delLst k m | not (M.member k m)                  = m
    delLst k m | null (stl $ M.lookupDefault [] k m) = M.delete k m
    delLst _ _ | otherwise                            = errorstar "BUG remAnn"
    stl []     = []
    stl (_:xs) = xs

-------------------------------------------------------------------------------
getAllAnns :: TCM r [AnnInfo r]  
-------------------------------------------------------------------------------
getAllAnns = tc_annss <$> get


-------------------------------------------------------------------------------
accumAnn :: (Ord r, PPRSF r) => (AnnInfo r -> [Error]) -> TCM r a -> TCM r a
-------------------------------------------------------------------------------
-- RJ: this function is gross. Why is it being used? why are anns not just
-- accumulated monotonically?
accumAnn check act 
  = do m     <- tc_anns <$> get 
       modify $ \st -> st {tc_anns = M.empty}
       z     <- act
       m'    <- getAnns
       forM_ (check m') (`logError` ())
       modify $ \st -> st {tc_anns = m} {tc_annss = m' : tc_annss st}
       return z

-------------------------------------------------------------------------------
execute ::  PPR r => V.Verbosity -> NanoSSAR r -> TCM r a -> Either [Error] a
-------------------------------------------------------------------------------
execute verb pgm act 
  = case runState (runErrorT act) $ initState verb pgm of 
      (Left err, _) -> Left [err]
      (Right x, st) ->  applyNonNull (Right x) Left (reverse $ tc_errss st)

-------------------------------------------------------------------------------
initState :: PPR r => V.Verbosity -> NanoSSAR r -> TCState r
-------------------------------------------------------------------------------
initState verb pgm = TCS tc_errss tc_subst tc_cnt tc_anns tc_annss 
                          tc_specs tc_defs tc_exts tc_class tc_verb tc_this
  where
    tc_errss = []
    tc_subst = mempty 
    tc_cnt   = 0
    tc_anns  = M.empty
    tc_annss = []
    tc_specs = specs pgm
    tc_exts  = externs pgm
    tc_defs  = tDefEmpty
    tc_verb  = verb
    tc_this  = []
    tc_class = envFromList [ (s, ClassStmt l s e i b) | let Src ss = code pgm
                                                      , ClassStmt l s e i b <- ss ]


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
  SourceSpan -> String -> [RType r] -> [RType r] -> TCM r (RSubst r)
----------------------------------------------------------------------------------
unifyTypesM l msg t1s t2s
  -- TODO: This check might be done multiple times
  | length t1s /= length t2s = tcError $ errorArgMismatch l 
  | otherwise                = do θ <- getSubst 
                                  δ <- getDef
                                  case unifys l δ θ t1s t2s of
                                    Left err' -> tcError $ catMessage err' msg 
                                    Right θ'  -> setSubst θ' >> return θ' 

----------------------------------------------------------------------------------
unifyTypeM :: (Ord r, PrintfArg t1, PP r, PP a, F.Reftable r) =>
  SourceSpan -> t1 -> a -> RType r -> RType r -> TCM r (RSubst r)
----------------------------------------------------------------------------------
unifyTypeM l m e t t' = unifyTypesM l msg [t] [t']
  where 
    msg               = ppshow $ errorWrongType l m e t t'



--------------------------------------------------------------------------------
--  Cast Helpers ---------------------------------------------------------------
--------------------------------------------------------------------------------

-- | For the expression @e@, check the subtyping relation between the type @t@
-- which is the actual type for @e@ and @t'@ which is the desired (cast) type
-- and insert the right kind of cast. 
--------------------------------------------------------------------------------
castM :: (PPR r) => AnnSSA r -> IContext -> Expression (AnnSSA r) 
  -> RType r -> RType r -> TCM r (Expression (AnnSSA r))
--------------------------------------------------------------------------------
castM l ξ e fromT toT = go =<< thd3 <$> convert (ann l) fromT toT
  where go SupT = addDownCast ξ e toT    
        go SubT = addUpCast   ξ e toT   
        go Nth  = addDeadCast ξ e toT   
        go EqT  = return e 


-- | Monad versions of TDefEnv operations

updTDefEnv f = f <$> getDef >>= \(δ', a) -> setDef δ' >> return a

addSymM         = updTDefEnv . addSym
addObjLitTyM    = updTDefEnv . addObjLitTy

findTyIdOrDieM' :: String -> TyID -> TCM r (TDef (RType r))
findTyIdOrDieM' m i = findTyIdOrDie' m i <$> getDef

findTyIdOrDieM i = findTyIdOrDie i <$> getDef

findTySymM i = findTySym i <$> getDef
findTySymOrDieM i = findTySymOrDie i <$> getDef


-- | `convert` returns:
-- * An equivalent version of @t1@ that has the same sort as the second (RJ: first?) output
-- * An equivalent version of @t2@ that has the same sort as the first output
-- * A subtyping direction between @t1@ and @t2@
--
-- Padding the input types gives them the same sort, i.e. makes them compatible. 
---------------------------------------------------------------------------------------
convert :: (PPR r) => 
  SourceSpan -> RType r -> RType r -> TCM r (RType r, RType r, SubDirection)
---------------------------------------------------------------------------------------

convert _ t1 t2 | t1 `equiv` t2        = return $ (t1, t2, EqT)

convert l t1 t2 | isUndef t1           = (`setThd3` SubT) <$> convertUnion l t1 t2

convert l t1 t2 | isNull t1 &&  
                  not (isUndef t2)     = (`setThd3` SubT) <$> convertUnion l t1 t2

convert l t1 t2 | isTop t2             = (`setThd3` SubT) <$> convertUnion l t1 t2

convert l t1 t2 | any isUnion [t1,t2]  = convertUnion l t1 t2

convert l t1 t2 | all isArr   [t1,t2]  = convertArray l t1 t2

convert l t1 t2 | all isTRef  [t1,t2]  = convertTRefs l t1 t2

convert l t1 t2 | all isTFun  [t1, t2] = convertFun l t1 t2
convert _ (TFun _ _ _)    _            = error "Unimplemented convert-1"
convert _ _               (TFun _ _ _) = error "Unimplemented convert-2"

convert _ (TAll _ _  ) _               = error "Unimplemented: convert-3"
convert _ _            (TAll _ _  )    = error "Unimplemented: convert-4"

convert _ t1           t2              = convertSimple t1 t2 


-- | `convertTRefs`
-- 
--------------------------------------------------------------------------------
convertTRefs :: (PPR r) => 
  SourceSpan -> RType r -> RType r -> TCM r (RType r, RType r, SubDirection)
--------------------------------------------------------------------------------
convertTRefs l t1@(TApp (TRef i1) t1s r1) t2@(TApp (TRef i2) t2s r2) 
  -- Same exact type name
  | i1 == i2 = 
      if parEq then
        return (t1, t2, EqT)
      else
        tcError $ errorConvDefInvar l t1 t2

  | otherwise = do
      -- Get the type definitions
      d1@(TD n1 v1s pro1 _) <- findTyIdOrDieM' "convertTRefs" i1
      d2@(TD n2 v2s pro2 _) <- findTyIdOrDieM' "convertTRefs" i2

      -- Gather all fields from current and parent classes
      e1s <- getDef >>= return . apply (fromList $ zip v1s t1s) . flatten d1
      e2s <- getDef >>= return . apply (fromList $ zip v2s t2s) . flatten d2
    
      -- Field keys
      let ks1 = ks e1s
          ks2 = ks e2s

      -- Width eq/sub/super-type
          widthEq    = isEqualSet ks1 ks2
          widthSub   = isProperSubsetOf ks1 ks2
          widthSup   = isProperSubsetOf ks2 ks1

      -- Equal types at the common fields
          cmnKs = S.intersection ks1 ks2
          e1s'  = ord $ filter ((`S.member` cmnKs) . f_sym) e1s 
          e2s'  = ord $ filter ((`S.member` cmnKs) . f_sym) e2s 
          -- e1s' and e2s' should have the same length
          cmnEq = and $ zipWith depthEq e1s' e2s' 

      case (cmnEq, widthEq, widthSup, widthSub) of
        (True, True, _  , _   ) -> return (t1, t2, EqT)
        (True, _,   True, _   ) -> do
          -- UPCAST: Restrict A<S1...> to the fields of B<T1...>
          -- Here we are using a flat (object literal) type in place 
          -- of could have been a class type (part of hiararchy). This 
          -- should be fine since this type will only be used locally 
          -- for the constraint generation.
            let e1s' = filter (\e -> S.member (f_sym e) ks2) e1s
            i1' <- addObjLitTyM (TD n2 v1s pro2 e1s')
            return (TApp (TRef i1') t1s r1, t2, SupT)
        (True, _   , _  , True) -> tcError $ errorMissFlds l d1 d2 (S.toList $ S.difference ks2 ks1)
        (True, _   , _  , _   ) -> tcError $ errorConvDef l d1 d2
        (_   , _   , _  , _   ) -> tcError $ errorConvDefDepth l d1 d2
  where
      -- Aux funcs

      ord     = L.sortBy (compare `on` f_sym)
      parEq   = and (zipWith equiv t1s t2s) 
      toMap   = M.fromList . ((\(TE s b t) -> (s, (b,t))) <$>)
      ks      = S.fromList . (f_sym <$>)


-- | depthEq: The input pairs are (access modifier, type)
--------------------------------------------------------------------------------
depthEq :: (TElt (RType r)) -> (TElt (RType r)) -> Bool
--------------------------------------------------------------------------------
depthEq (TE _ b1 t1) (TE _ b2 t2) | b1 == b2   = t1 `equiv` t2
depthEq _       _                 | otherwise  = False 


-- | `convertFun`

convertFun l (TFun b1s o1 r1) (TFun b2s o2 r2) = do
    (t1s',t2s',bds) <- unzip3 <$> zipWithM (convert l) (b_type <$> b1s) (b_type <$> b2s)
    (o1',o2',od)  <- convert l o1 o2
    let updTs        = zipWith (\b t -> b { b_type = t })
    case length b1s == length b2s && all (== EqT)(od:bds) of
      True  -> let t1'  = TFun (updTs b1s t1s') o1' r1
                   t2'  = TFun (updTs b2s t2s') o2' r2 in
                return (t1', t2', EqT)
      False -> error "[Unimplemented] convertFun with different types"

convertFun _ _ _ = error "convertFun: no other cases supported"


-- | `convertSimple`
--------------------------------------------------------------------------------
convertSimple :: (PPR r) => RType r -> RType r -> TCM r (RType r, RType r, SubDirection)
--------------------------------------------------------------------------------
convertSimple t1 t2
  | t1 `equiv` t2 = return (t1, t2, EqT)
  | otherwise     = return (t1', t2', Nth)
    where t1'     = mkUnion [t1, fmap F.bot t2]  -- Toplevel refs?
          t2'     = mkUnion [fmap F.bot t1, t2]


-- | `convertUnion`

-- Produces an equivalent type for @t1@ (resp. @t2@) that is extended with 
-- the missing type terms to the common upper bound of @t1@ and @t2@. The extra
-- type terms that are added in the union are refined with False to keep them
-- equivalent with the input types.
--
-- The output is the following tuple:
--  * adjusted type for @t1@ to be sort compatible,
--  * adjusted type for @t2@ to be sort compatible
--  * a subtyping direction
--------------------------------------------------------------------------------
convertUnion :: (PPR r) => 
  SourceSpan -> RType r -> RType r -> TCM r (RType r, RType r, SubDirection)   
--------------------------------------------------------------------------------
convertUnion l t1 t2 | all (not . isUnion) [t1, t2] = convertSimple t1 t2
convertUnion l t1 t2 | otherwise = do
     
    -- * The common types (recursively call `convert` to convert the types
    --   of the parts and join the subtyping relations)
    (cmn1, cmn2, dirs) <- unzip3 <$> mapM (uncurry $ convert l) cmnPs
                     
    let t1s' = cmn1 ++ d1s ++ (fmap F.bot <$> d2s)
    let t2s' = cmn2 ++ (fmap F.bot <$> d1s) ++ d2s
                     
    -- Ignore refinement.
    let (t1s, t2s) = unzip $ safeZip "unionParts" t1s' t2s'

    let comSub     = mconcat dirs
        direction  = distSub `mappend` comSub
                     
    return $ (mkUnionR r1 $ t1s, mkUnionR r2 $ t2s, direction)
  where
    -- Extract top-level refinements
    (r1, r2) = mapPair rUnion (t1, t2)

    -- Break the input types into pieces
    (cmnPs, d1s, d2s) = unionParts t1 t2
    -- To figure out the direction of the subtyping, we must take into account:
    -- * The distinct types (the one that has more is a supertype)
    distSub   = case (d1s, d2s) of
                  ([], []) -> EqT
                  ([], _ ) -> SubT  -- <:
                  (_ , []) -> SupT  -- >:
                  (_ , _ ) -> Nth -- no relation


-- | `convertArray`
convertArray l (TArr t1 r1) (TArr t2 r2) = do
  (t1', t2', ad) <- convert l t1 t2
  return (TArr t1' r1, TArr t2' r2, arrDir ad)
convertArray _ _ _ = errorstar "BUG: convertArray can only pad Arrays"


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




getPropM l s t = do
  ε <- getExts
  δ <- getDef 
  return $ getProp l ε δ (F.symbol s) t

getPropTDefM l s t ts = do 
  ε <- getExts
  δ <- getDef 
  return $ getPropTDef l ε δ (F.symbol s) ts t


