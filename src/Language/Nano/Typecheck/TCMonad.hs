{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections             #-}
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

  -- * Errors
  , logError, tcError

  -- * Freshness
  , freshTyArgs

  -- * Substitutions
  , getSubst, setSubst

  -- * Function Types
  , tcFunTys

  -- * Annotations
  , addAnn {-TEMP-}, accumAnn, getAllAnns, remAnn, getDef, setDef
  , getExts, getClasses

  -- * Unification
  , unifyTypeM, unifyTypesM

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
  , getPropM, getPropTDefM

  )  where 

import           Text.Printf
import           Language.ECMAScript3.PrettyPrint
import           Control.Applicative                ((<$>))
import           Data.Function                      (on)
import qualified Data.HashSet                       as S
import           Data.Maybe                         (fromJust)
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
extSubst βs = getSubst >>= setSubst . (`mappend` θ)
  where 
    θ       = fromList $ zip βs (tVar <$> βs)

-- addSubst θ  = getSubst >>= setSubst . (`mappend` θ)

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
unifyTypesM :: (Ord r, PP r, F.Reftable r) => 
  SourceSpan -> String -> [RType r] -> [RType r] -> TCM r (RSubst r)
----------------------------------------------------------------------------------
unifyTypesM l msg t1s t2s
  | on (/=) length t1s t2s = tcError $ errorArgMismatch l 
  | otherwise              = do θ <- getSubst 
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

convert l t1 t2 | all isArr   [t1,t2]  = convertArray l t1 t2

convert l t1 t2 | all isTCons [t1,t2]  = convertCons l t1 t2

convert l t1 t2 | all isTFun  [t1, t2] = convertFun l t1 t2

convert _ (TFun _ _ _)    _            = error "Unimplemented convert-1"
convert _ _               (TFun _ _ _) = error "Unimplemented convert-2"

convert _ (TAll _ _  ) _               = error "Unimplemented: convert-3"
convert _ _            (TAll _ _  )    = error "Unimplemented: convert-4"

convert l t1           t2              = convertSimple l t1 t2 


-- | `convertCons`
--------------------------------------------------------------------------------
convertCons :: (PPR r) => SourceSpan -> RType r -> RType r -> TCM r (Cast r)
--------------------------------------------------------------------------------
convertCons l t1@(TCons e1s _) t2@(TCons e2s _)
  = do
      -- Take all elements into account, excluding constructors.
      let (l1, l2)   = mapPair (\es -> [ (s, t) | TE s _ t <- es
                                                , s /= F.symbol "constructor" ]) (e1s, e2s)
          (m1, m2)   = mapPair M.fromList (l1, l2)
          (ks1, ks2) = mapPair (S.fromList . map fst) (l1, l2)
          cmnKs      = S.toList $ S.intersection ks1 ks2
          t1s        = fromJust . (`M.lookup` m1) <$> cmnKs
          t2s        = fromJust . (`M.lookup` m2) <$> cmnKs

      cs <- zipWithM (convert l) t1s t2s

      if m1 `equalKeys` m2 then 
        if      all noCast cs then return $ CNo
        else if all upCast cs then return $ CUp t1 t2
        -- NOTE: Assuming covariance here!!!
        else if all dnCast cs then return $ CDn t1 t2
        else if any ddCast cs then return $ CDead t2
        else tcError $ errorConvDef l t1 t2

      -- LHS has more fields than RHS
      else if m2 `isProperSubmapOf` m1 then
        if      all noCast cs then return $ CUp t1 t2
        else if all upCast cs then return $ CUp t1 t2
        else tcError $ errorConvDef l t1 t2

      -- LHS has fewer fields than RHS
      else if m1 `isProperSubmapOf` m2 then 
        tcError $ errorMissFlds l t1 t2 (S.toList $ S.difference ks2 ks1)

      else 
        tcError $ errorConvDef l t1 t2

convertCons l t1@(TApp (TRef i1) t1s r1) t2@(TApp (TRef i2) t2s r2) 
  -- FIXME: type argument subtyping
  -- Same exact type name
  | i1 == i2 && and (zipWith equiv t1s t2s) 
  = return CNo
  -- Same type - incompatible arguments
  | i1 == i2   
  = tcError $ errorConvDefInvar l t1 t2
  | otherwise  
  = do δ <- getDef
       let c1 = TCons (flattenTRef δ t1) r1
       let c2 = TCons (flattenTRef δ t2) r2
       convertCons l c1 c2  

convertCons l t1@(TApp (TRef _) _ _) t2 
  = do δ <- getDef 
       convertCons l (flattenType δ t1) t2

convertCons l t1 t2@(TApp (TRef _) _ _)
  = do δ <- getDef
       convertCons l t1 (flattenType δ t2) 

convertCons _ _ _ =  error "BUG: Case not supported in convertCons"


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
convertSimple _ t1 t2
  | t1 `equiv` t2 = return CNo
  | otherwise     = return $ CDead t2


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


-- | `convertArray`
--------------------------------------------------------------------------------
convertArray :: (PPR r) => SourceSpan -> RType r -> RType r -> TCM r (Cast r)
--------------------------------------------------------------------------------
convertArray l t1@(TArr τ1 _) t2@(TArr τ2 _) = do
  c1 <- convert l τ1 τ2
  c2 <- convert l τ2 τ1
  -- FIXME: Too Strict
  case (c1, c2) of
    (CNo, CNo) -> return CNo
    _          -> tcError $ errorArraySubtype l t1 t2 
convertArray _ _ _ = errorstar "BUG: convertArray can only pad Arrays"


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


getPropM l s t = do
  ε <- getExts
  δ <- getDef 
  return $ getProp l ε δ (F.symbol s) t

getPropTDefM l s t ts = do 
  ε <- getExts
  δ <- getDef 
  return $ getPropTDef l ε δ (F.symbol s) ts t


--------------------------------------------------------------------------------
getSuperM :: (PPRSF r, IsLocated a) => a -> RType r -> TCM r (RType r)
--------------------------------------------------------------------------------
getSuperM l (TApp (TRef i) ts _) = fromTdef =<< findSymOrDieM i
  where fromTdef (TD _ vs (Just (p,ps)) _) = do
          return  $ apply (fromList $ zip vs ts) 
                  $ TApp (TRef $ F.symbol p) ps fTop
        fromTdef (TD _ _ Nothing _) = tcError $ errorSuper (srcPos l) 
getSuperM l _  = tcError $ errorSuper (srcPos l) 


--------------------------------------------------------------------------------
getSuperDefM :: (PPRSF r, IsLocated a) => a -> RType r -> TCM r (TDef (RType r))
--------------------------------------------------------------------------------
getSuperDefM l (TApp (TRef i) ts _) = fromTdef =<< findSymOrDieM i
  where 
    fromTdef (TD _ vs (Just (p,ps)) _) = 
      do TD n ws pp ee <- findSymOrDieM p
         return  $ apply (fromList $ zip vs ts) 
                 $ apply (fromList $ zip ws ps)
                 $ TD n [] pp ee
    fromTdef (TD _ _ Nothing _) = tcError $ errorSuper (srcPos l) 
getSuperDefM l _  = tcError $ errorSuper (srcPos l)

