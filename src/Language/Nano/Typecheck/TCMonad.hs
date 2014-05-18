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
  , logError, tcError

  -- * Freshness
  , freshTyArgs

  -- * Substitutions
  , getSubst, setSubst, addSubst

  -- * Function Types
  , tcFunTys

  -- * Annotations
  , addAnn {-TEMP-}, accumAnn, getAllAnns, getDef, setDef
  , getExts, getClasses

  -- * Unification
  , unifyTypeM, unifyTypesM

  -- * Subtyping
  , subtypeM, isSubtypeM

  -- * Casts
  , castM

  -- * TDefEnv
  , findSymM, findSymOrDieM

  -- * Get Type Signature 
  , getSpecOrDie, getSpecM, addSpec

  -- * Verbosity
  , whenLoud', whenLoud, whenQuiet', whenQuiet
  
  -- * This
  , tcPeekThis, tcWithThis

  -- * Super
  , getSuperM, getSuperDefM

  -- * Prop
  , getPropTDefM, getPropM

  )  where 

import           Language.ECMAScript3.PrettyPrint
import           Control.Applicative                ((<$>), (<*>))
import           Data.Function                      (on)
import qualified Data.HashSet                       as S
import           Data.Maybe                         (fromJust, catMaybes)
import           Data.List                          (elem)
import           Control.Monad.State
import           Control.Monad.Error                hiding (Error)
import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc 
import qualified Language.Fixpoint.Types            as F

import           Language.Nano.Env
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

-- import           Debug.Trace                      (trace)
import qualified System.Console.CmdArgs.Verbosity   as V

type PPR r = (PP r, F.Reftable r)
type PPRSF r = (PPR r, Substitutable r (Fact r), Free (Fact r)) 


-------------------------------------------------------------------------------
-- | Typechecking monad 
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
whenQuiet' quiet other = do  tc_verb <$> get >>= \case 
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


-- Q: Do we need this anymore?
-- A: Yes! 
-------------------------------------------------------------------------------
addSubst :: (PPR r, IsLocated a) => a -> RSubst r -> TCM r ()
-------------------------------------------------------------------------------
addSubst l θ 
  = do θ0 <- getSubst 
       case checkSubst θ0 θ of 
         [] -> return ()
         ts -> forM_ ts $ (\(t1,t2) -> tcError $ errorUnification (srcPos l) t1 t2)
       setSubst $ θ0 `mappend` θ
  where
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
-- | Managing Annotations: Type Instantiations
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
getAnns :: (F.Reftable r, Substitutable r (Fact r)) => TCM r (AnnInfo r)
-------------------------------------------------------------------------------
getAnns = do θ     <- tc_subst <$> get
             m     <- tc_anns  <$> get
             let m' = fmap (apply θ . sortNub) m
             _     <- modify $ \st -> st { tc_anns = m' }
             return m' 

-------------------------------------------------------------------------------
addAnn :: (PPR r, F.Reftable r) => SourceSpan -> Fact r -> TCM r () 
-------------------------------------------------------------------------------
addAnn l f = modify $ \st -> st { tc_anns = inserts l f (tc_anns st) } 

-------------------------------------------------------------------------------
getAllAnns :: TCM r [AnnInfo r]  
-------------------------------------------------------------------------------
getAllAnns = tc_annss <$> get


-------------------------------------------------------------------------------
accumAnn :: (PP a, PPRSF r) => (AnnInfo r -> [Error]) -> TCM r a -> TCM r a
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

freshTVar l _ =  ((`TV` l). F.intSymbol "T") <$> tick


--------------------------------------------------------------------------------
-- | Unification and Subtyping
--------------------------------------------------------------------------------

----------------------------------------------------------------------------------
unifyTypesM :: PPR r => SourceSpan -> String -> [RType r] -> [RType r] -> TCM r (RSubst r)
----------------------------------------------------------------------------------
unifyTypesM l msg t1s t2s
  | on (/=) length t1s t2s = tcError $ errorArgMismatch l 
  | otherwise = do (δ, θ) <- (,) <$> getDef <*> getSubst
                   case unifys l δ θ t1s t2s of
                     Left err -> tcError $ catMessage err msg
                     Right θ' -> setSubst θ' >> return θ' 
                     -- Right θ' -> setSubst (tracePP (ppshow θ ++ " ===> " ++ ppshow t1s ++ " ~ " ++ ppshow t2s) θ') >> return θ' 

----------------------------------------------------------------------------------
unifyTypeM :: PPR r => SourceSpan -> RType r -> RType r -> TCM r (RSubst r)
----------------------------------------------------------------------------------
unifyTypeM l t t' = unifyTypesM l (ppshow "") [t] [t']


--------------------------------------------------------------------------------
--  Cast Helpers
--------------------------------------------------------------------------------

-- | For the expression @e@, check the subtyping relation between the type @t1@
-- which is the actual type for @e@ and @t2@ which is the desired (cast) type
-- and insert the right kind of cast.
--------------------------------------------------------------------------------
castM :: (PPR r) => AnnSSA r -> IContext -> Expression (AnnSSA r) 
  -> RType r -> RType r -> TCM r (Expression (AnnSSA r))
--------------------------------------------------------------------------------
castM l ξ e t1 t2 = convert (ann l) t1 t2 >>= patch
  where patch CNo = return e
        patch c   = addCast ξ e c


-- | Monad versions of TDefEnv operations

findSymM i = findSym i <$> getDef
findSymOrDieM i = findSymOrDie i <$> getDef


-- | Run the monad `a` in the current state. This action will not alter the
-- state.
--------------------------------------------------------------------------------
runFailM :: (PPR r) => TCM r a -> TCM r (Either Error a)
--------------------------------------------------------------------------------
runFailM a = fst . runState (runErrorT a) <$> get


--------------------------------------------------------------------------------
runMaybeM :: (PPR r) => TCM r a -> TCM r (Maybe a)
--------------------------------------------------------------------------------
runMaybeM a = runFailM a >>= \case 
                Right rr -> return $ Just rr
                Left _   -> return $ Nothing


isSubtypeM l t1 t2 = runFailM (subtypeM l t1 t2) >>= \case
                        Right _ -> return True
                        Left  _ -> return False

-- | subTypeM will throw error if subtyping fails
--------------------------------------------------------------------------------
subtypeM :: (PPR r) => SourceSpan -> RType r -> RType r -> TCM r ()
--------------------------------------------------------------------------------
subtypeM l t1 t2 = convert l t1 t2 >>= \case
  CNo       -> return ()
  (CUp _ _) -> return ()
  _         -> tcError $ errorStrictSubtype l -- No casts allowed


-- | @convert@ returns:
-- * An equivalent version of @t1@ that has the same sort as the first input type
-- * An equivalent version of @t2@ that has the same sort as the second input type
-- * A subtyping direction between @t1@ and @t2@
--  
-- Padding the input types gives them the same sort, i.e. makes them compatible. 
--------------------------------------------------------------------------------
convert :: (PPR r) => SourceSpan -> RType r -> RType r -> TCM r (Cast r)
--------------------------------------------------------------------------------

convert _ t1 t2 | t1 `equiv` t2        = return CNo

convert _ t1 _  | isUndef t1           = return CNo

convert _ t1 t2 | isNull t1 &&  
                  not (isUndef t2)     = return CNo

convert _ _  t2 | isTop t2             = return CNo

convert l t1 t2 | any isUnion [t1,t2]  = convertUnion l t1 t2

convert l t1 t2 | all isTObj  [t1,t2]  = convertObj l t1 t2

convert l t1 t2 | all isTFun  [t1, t2] = convertFun l t1 t2

convert l t1 t2                        = convertSimple l t1 t2 


-- | `convertObj`
--------------------------------------------------------------------------------
convertObj :: (PPR r) => SourceSpan -> RType r -> RType r -> TCM r (Cast r)
--------------------------------------------------------------------------------
convertObj l t1@(TCons e1s _) t2@(TCons e2s _)
  -- LHS and RHS are object literal types
  | all (not . isIndSig) [t1,t2] = zipWithM (convert l) t1s t2s >>= kk
  -- LHS and RHS are index signatures
  | all isIndSig [t1,t2] = mapM (uncurry $ convert l) ind >>= nc
  -- RHS is index signature:
  -- We ASSUME all fields are string-indexed and so fall under the 
  -- string index signature (which is the only one supported anyway).
  | isIndSig t2          = zipWithM (convert l) (ts e1s) (repeat $ ti e2s) >>= nc
  | isIndSig t1          = zipWithM (convert l) (repeat $ ti e1s) (ts e2s) >>= nc
  | otherwise            = g4 
  where
    -- Only consider non-static properties
    ts es      = [ eltType e | e <- es, nonStaticElt e ]
    ti es      = safeHead "convertObj" [ t | IndexSig _ _ t <- es ]
    
    -- NOT: Take all (non-static) properties into account. 
    p1s        = [ eltToPair e | e <- e1s, nonStaticElt e ]
    p2s        = [ eltToPair e | e <- e2s, nonStaticElt e ]
    
    (m1, m2)   = mapPair M.fromList (p1s, p2s)
    (ks1, ks2) = mapPair (S.fromList . map fst) (p1s, p2s)
    cmnKs      = S.toList $ S.intersection ks1 ks2
    t1s        = fromJust . (`M.lookup` m1) <$> cmnKs
    t2s        = fromJust . (`M.lookup` m2) <$> cmnKs

    ind   = [ (t1, t2) | IndexSig _ s1 t1 <- e1s
                       , IndexSig _ s2 t2 <- e2s
                       , s1 == s2 ]
    kk cs | m1 `equalKeys` m2        = cc cs 
          -- LHS has more fields than RHS
          | m2 `isProperSubmapOf` m1 = nc cs
          -- LHS has fewer fields than RHS
          | m1 `isProperSubmapOf` m2 = g3
          | otherwise                = g4
    cc cs | all noCast cs = return   $ CNo
          | all upCast cs = return   $ CUp t1 t2
          -- NOTE: Assuming covariance here!!!
          | all dnCast cs = return   $ CDn t1 t2
          -- TOGGLE dead-code
          -- | any ddCast cs = return   $ CDead t2
          | otherwise     = tcError  $ errorConvDef l t1 t2
    nc cs | all noCast cs = return   $ CNo
          | all upCast cs = return   $ CUp t1 t2
          | otherwise     = tcError  $ errorConvDef l t1 t2
    g3    = tcError $ errorMissFlds l t1 t2 (S.toList $ S.difference ks2 ks1)
    g4    = tcError $ errorConvDef l t1 t2

convertObj l t1@(TApp (TRef i1) t1s r1) t2@(TApp (TRef i2) t2s r2) 
  | i1 == i2 
  = do -- FIXME: Using covariance here !!!
       bs <- zipWithM (isSubtypeM l) t1s t2s 
       if and bs then return CNo
                 else tcError $ errorSimpleSubtype l t1 t2
  | (s1,b1) /= (s2,b2)
  = do δ  <- getDef 
       case weaken δ (findSymOrDie s1 δ,t1s) s2 of
         Just (_, t1s') -> return $ CUp (TApp (TRef (s2,b1)) t1s' r1) t2
         Nothing        -> convertObj l (c1 δ) (c2 δ)
  where
    (s1,b1) = i1
    (s2,b2) = i2
    c1 δ    = TCons (flattenTRef δ t1) r1
    c2 δ    = TCons (flattenTRef δ t2) r2
                          
convertObj l t1@(TApp (TRef _) _ _) t2 
  = do δ <- getDef 
       convertObj l (flattenType δ t1) t2

convertObj l t1 t2@(TApp (TRef _) _ _)
  = do δ <- getDef
       convertObj l t1 (flattenType δ t2) 

convertObj _ _ _ =  error "BUG: Case not supported in convertObj"


instance PP a => PP (S.HashSet a) where
  pp = pp . S.toList 


-- | `convertFun`
--
-- TODO: add arg length check
--
--------------------------------------------------------------------------------
convertFun :: (PPR r) => SourceSpan -> RType r -> RType r -> TCM r (Cast r)
--------------------------------------------------------------------------------
convertFun l t1@(TFun b1s o1 _) t2@(TFun b2s o2 _) = do
    cs <- zipWithM (convert l) (b_type <$> b2s) (b_type <$> b1s)
    co <- convert l o1 o2
    if      all noCast cs && noCast co then return $ CNo
    else if all dnCast cs && upCast co then return $ CUp t1 t2
    else tcError $ errorFuncSubtype l t1 t2

convertFun _ _ _ = error "convertFun: no other cases supported"


-- | `convertSimple`
--------------------------------------------------------------------------------
convertSimple :: (PPR r) => SourceSpan -> RType r -> RType r -> TCM r (Cast r)
--------------------------------------------------------------------------------
convertSimple l t1 t2
  | t1 `equiv` t2 = return CNo
  -- TOGGLE dead-code
  -- | otherwise     = return $ CDead t2
  | otherwise     = tcError  $ errorSigNotFound l t1 t2


-- | `convertUnion`
--------------------------------------------------------------------------------
convertUnion :: (PPR r) => SourceSpan -> RType r -> RType r -> TCM r (Cast r)
--------------------------------------------------------------------------------
convertUnion l t1 t2 = parts   $ unionParts t1 t2
  where 
    parts (_,[],[])  = return  $ CNo
    parts (_,[],_ )  = return  $ CUp t1 t2
    parts (_,_ ,[])  = return  $ CDn t1 t2
    parts (_, _ ,_)  = tcError $ errorUnionSubtype l t1 t2


addCast     ξ e c = addAnn loc fact >> return (wrapCast loc fact e)
  where loc       = srcPos e
        fact      = TCast ξ c

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

getPropTDefM b l s t ts = do 
  δ <- getDef
  return $ getPropTDef b l δ (F.symbol s) ts t

getPropM _ l s t = do 
  (δ, ε) <- (,) <$> getDef <*> getExts
  return  $ snd <$> getProp l ε δ (F.symbol s) t

--------------------------------------------------------------------------------
getSuperM :: (PPRSF r, IsLocated a) => a -> RType r -> TCM r (RType r)
--------------------------------------------------------------------------------
getSuperM l (TApp (TRef (i, s)) ts _) = fromTdef =<< findSymOrDieM i
  where fromTdef (TD _ _ vs (Just (p,ps)) _) = do
          return  $ apply (fromList $ zip vs ts) 
                  $ TApp (TRef (F.symbol p,s)) ps fTop
        fromTdef (TD _ _ _ Nothing _) = tcError $ errorSuper (srcPos l) 
getSuperM l _  = tcError $ errorSuper (srcPos l) 

--------------------------------------------------------------------------------
getSuperDefM :: (PPRSF r, IsLocated a) => a -> RType r -> TCM r (TDef (RType r))
--------------------------------------------------------------------------------
getSuperDefM l (TApp (TRef (i,_)) ts _) = fromTdef =<< findSymOrDieM i
  where 
    fromTdef (TD _ _ vs (Just (p,ps)) _) = 
      do TD c n ws pp ee <- findSymOrDieM p
         return  $ apply (fromList $ zip vs ts) 
                 $ apply (fromList $ zip ws ps)
                 $ TD c n [] pp ee
    fromTdef (TD _ _ _ Nothing _) = tcError $ errorSuper (srcPos l) 
getSuperDefM l _  = tcError $ errorSuper (srcPos l)

