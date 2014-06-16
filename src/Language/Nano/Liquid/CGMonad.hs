{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE DoAndIfThenElse           #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Operations pertaining to Constraint Generation

module Language.Nano.Liquid.CGMonad (
    
  -- * Constraint Generation Monad
    CGM

  -- * Constraint Information
  , CGInfo (..)

  -- * Execute Action and Get FInfo
  , getCGInfo 

  -- * Get Defined Function Type Signature
  , getDefType, getDef, setDef, getPropTDefM, getPropM

  -- * Throw Errors
  , cgError      

  -- * Fresh Templates for Unknown Refinement Types 
  , freshTyFun, freshTyVar, freshTyInst, freshTyPhis
  , freshTyPhisWhile, freshTyObj

  -- * Freshable
  , Freshable (..), refreshValueVar

  -- * Environment API
  , envAddFresh, envAdds, envAddReturn, envAddGuard, envFindTy
  , envGlobAnnot, envFieldAnnot
  , envRemSpec, isGlobalVar, envToList, envFindReturn, envPushContext
  , envGetContextCast, envGetContextTypArgs, arrayQualifiers

  , findSymM, findSymOrDieM

  -- * Add Subtyping Constraints
  , subType, wellFormed, safeExtends
  
  -- * Add Type Annotations
  , addAnnot


  -- * Function Types
  , cgFunTys

  -- * This
  , cgPeekThis, cgWithThis

  -- * Super 
  , getSuperM, getSuperDefM


  ) where

import           Data.Maybe                     (fromMaybe, listToMaybe, catMaybes, isJust, fromJust)
import           Data.Monoid                    (mempty)
import qualified Data.HashMap.Strict            as M
import qualified Data.List                      as L
import           Data.Function                  (on)
import           Text.PrettyPrint.HughesPJ
import qualified Data.Traversable                   as T 
import           Language.Nano.Types
import           Language.Nano.Errors
import qualified Language.Nano.Annots           as A
import qualified Language.Nano.Env              as E
import           Language.Nano.Typecheck.Types  hiding (quals)
import           Language.Nano.Typecheck.Lookup
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Liquid.Types
import           Language.Nano.Liquid.Qualifiers
import           Language.Nano.Misc


import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Errors
import           Control.Applicative 
import           Control.Exception (throw)

import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Error hiding (Error)
import           Text.Printf 

import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.PrettyPrint

-- import           Debug.Trace                        (trace)

-------------------------------------------------------------------------------
-- | Top level type returned after Constraint Generation
-------------------------------------------------------------------------------

data CGInfo = CGI { cgi_finfo :: F.FInfo Cinfo
                  , cgi_annot :: A.UAnnInfo RefType  
                  }

-- Dump the refinement subtyping constraints
instance PP CGInfo where
  pp (CGI finfo _) = cat (map pp (M.elems $ F.cm finfo))

instance PP (F.SubC c) where
  pp s = pp (F.lhsCs s) <+> text " <: " <+> pp (F.rhsCs s)


-------------------------------------------------------------------------------
getCGInfo :: Config -> NanoRefType -> CGM a -> CGInfo
-------------------------------------------------------------------------------
getCGInfo cfg pgm = cgStateCInfo pgm . execute cfg pgm . (>> fixCWs)
  where 
    fixCWs       = (,) <$> fixCs <*> fixWs
    fixCs        = get >>= concatMapM splitC . cs
    fixWs        = get >>= concatMapM splitW . ws

execute :: Config -> NanoRefType -> CGM a -> (a, CGState)
execute cfg pgm act
  = case runState (runErrorT act) $ initState cfg pgm of 
      (Left err, _) -> throw err
      (Right x, st) -> (x, st)  

initState       :: Config -> Nano AnnTypeR RefType -> CGState
initState c p   = CGS F.emptyBindEnv (specs p) (defs p) (externs p)
                    [] [] 0 mempty invs c [this] M.empty []
  where 
    invs        = M.fromList [(tc, t) | t@(Loc _ (TApp tc _ _)) <- invts p]
    this        = tTop

getDefType f 
  = do m <- cg_sigs <$> get
       maybe err return $ E.envFindTy f m 
    where 
       err = cgError l $ errorMissingSpec l f
       l   = srcPos f

-- cgStateFInfo :: Nano a1 (RType F.Reft)-> (([F.SubC Cinfo], [F.WfC Cinfo]), CGState) -> CGInfo
cgStateCInfo pgm ((fcs, fws), cg) = CGI (patchSymLits fi) (cg_ann cg)
  where 
    fi   = F.FI { F.cm    = M.fromList $ F.addIds fcs  
                , F.ws    = fws
                , F.bs    = binds cg
                , F.gs    = measureEnv pgm
                , F.lits  = []
                , F.kuts  = F.ksEmpty
                , F.quals = nanoQualifiers pgm ++ quals cg
                }

patchSymLits fi = fi { F.lits = F.symConstLits fi ++ F.lits fi }


-- | Get binding from object type

---------------------------------------------------------------------------------------
measureEnv   ::  Nano a (RType F.Reft) -> F.SEnv F.SortedReft
---------------------------------------------------------------------------------------
measureEnv   = fmap rTypeSortedReft . E.envSEnv . consts 

---------------------------------------------------------------------------------------
-- | Constraint Generation Monad 
---------------------------------------------------------------------------------------

data CGState 
  = CGS { binds    :: F.BindEnv            -- ^ global list of fixpoint binders
        , cg_sigs  :: !(E.Env RefType)     -- ^ type sigs for all defined functions
        , cg_defs  :: !(TDefEnv RefType)   -- ^ defined types 
        , cg_ext   :: !(E.Env RefType)     -- ^ Extern (unchecked) declarations
        , cs       :: ![SubC]              -- ^ subtyping constraints
        , ws       :: ![WfC]               -- ^ well-formedness constraints
        , count    :: !Integer             -- ^ freshness counter
        , cg_ann   :: A.UAnnInfo RefType   -- ^ recorded annotations
        , invs     :: TConInv              -- ^ type constructor invariants
        , cg_opts  :: Config               -- ^ configuration options
        , cg_this  :: ![RefType]           -- ^ a stack holding types for 'this' 
        , globs    :: !GlobEnv             -- ^ bindings of globals
        , quals    :: ![F.Qualifier]       -- ^ qualifiers that arise at typechecking
        }

type CGM     = ErrorT Error (State CGState)

type TConInv = M.HashMap TCon (Located RefType)

type GlobEnv = M.HashMap F.Symbol [F.BindId]

-------------------------------------------------------------------------------
getDef  :: CGM (TDefEnv RefType)
-------------------------------------------------------------------------------
getDef   = cg_defs <$> get

setDef d = modify $ \st -> st { cg_defs  = d } 

-- XXX: This is not really used 
-------------------------------------------------------------------------------
getExts  :: CGM (E.Env RefType)
-------------------------------------------------------------------------------
getExts = cg_ext <$> get


getPropTDefM b l s t ts = do 
  δ <- getDef 
  return $ getPropTDef b l δ (F.symbol s) ts t

getPropM l s t = do 
  (δ, ε) <- (,) <$> getDef <*> getExts
  return  $ snd <$> getProp l ε δ (F.symbol s) t

---------------------------------------------------------------------------------------
cgError     :: a -> Error -> CGM b 
---------------------------------------------------------------------------------------
cgError _ e = throwError e

---------------------------------------------------------------------------------------
-- | Environment API
---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------
envPushContext :: (CallSite a) => a -> CGEnv -> CGEnv
---------------------------------------------------------------------------------------
envPushContext c g = g {cge_ctx = pushContext c (cge_ctx g)}

---------------------------------------------------------------------------------------
envGetContextCast :: CGEnv -> AnnTypeR -> Cast F.Reft
---------------------------------------------------------------------------------------
envGetContextCast g a 
  = case [c | TCast cx c <- ann_fact a, cx == cge_ctx g] of
      [ ] -> CNo
      [c] -> c
      cs  -> die $ errorMultipleCasts (srcPos a) cs


---------------------------------------------------------------------------------------
envGetContextOverload :: CGEnv -> AnnTypeR -> [RefType]
---------------------------------------------------------------------------------------
envGetContextOverload g a 
  = [t | Overload cx t <- ann_fact a, cx == cge_ctx g] 


---------------------------------------------------------------------------------------
envGetContextTypArgs :: CGEnv -> AnnTypeR -> [TVar] -> [RefType]
---------------------------------------------------------------------------------------
-- NOTE: If we do not need to instantiate any type parameter (i.e. length αs ==
-- 0), DO NOT attempt to compare that with the TypInst that might hide withing
-- the expression, cause those type instantiations might serve anothor reason
-- (i.e. might be there for a separate instantiation).  
envGetContextTypArgs _ _ []        = []
envGetContextTypArgs g a αs
  = case [i | TypInst ξ' i <- ann_fact a, ξ' == cge_ctx g] of 
      [i] | length i == length αs -> i 
      _                           -> die $ bugMissingTypeArgs $ srcPos a


---------------------------------------------------------------------------------------
envAddFresh :: (IsLocated l) => l -> RefType -> CGEnv -> CGM (Id AnnTypeR, CGEnv) 
---------------------------------------------------------------------------------------
envAddFresh l t g 
  = do x  <- freshId loc
       g' <- envAdds False [(x, t)] g
       addAnnot (srcPos l) x t
       return (x, g')
    where loc = srcPos l
   
freshId l = Id (Ann l []) <$> fresh


---------------------------------------------------------------------------------------
envAdds      :: (PP x, F.Symbolic x, IsLocated x) => Bool -> [(x, RefType)] -> CGEnv -> CGM CGEnv
---------------------------------------------------------------------------------------
envAdds glob xts' g
  = do xts      <- zip xs  <$> mapM addInvariant ts
       is       <- forM xts $  addFixpointBind 
       _        <- forM xts $  \(x, t) -> addAnnot (srcPos x) x t
       when glob $ forM_ (zip xs is) addGlobBind

       return    $ g { renv  = E.envAdds xts        (renv g) } 
                     { fenv  = F.insertsIBindEnv is (fenv g) }
    where 
       (xs,ts) = unzip xts'
       exists x g = E.envMem x (renv g) 


---------------------------------------------------------------------------------------
addGlobBind :: (F.Symbolic x) => (x, F.BindId) -> CGM ()
---------------------------------------------------------------------------------------
addGlobBind (x,i) 
  = do let s     = F.symbol x
       modify    $ \st -> st { globs = upd s i (globs st) }
    where
       upd s i m = case M.lookup s m of
           Just is -> M.insert s (i:is) m
           Nothing -> M.insert s [i] m

---------------------------------------------------------------------------------------
addFixpointBind :: (F.Symbolic x) => (x, RefType) -> CGM F.BindId
---------------------------------------------------------------------------------------
addFixpointBind (x, t) 
  = do let s     = F.symbol x
       let r     = rTypeSortedReft t
       (i, bs') <- F.insertBindEnv s r . binds <$> get 
       modify    $ \st -> st { binds = bs' }
       return    $ i

---------------------------------------------------------------------------------------
addInvariant              :: RefType -> CGM RefType
---------------------------------------------------------------------------------------
addInvariant t             = {- tagStrn <$> -} ((`tx` t) . invs) <$> get
  where 
    tx i t@(TApp tc _ o)   = maybe t (\i -> strengthenOp t o $ rTypeReft $ val i) $ M.lookup tc i
    tx _ t                 = t 
    strengthenOp t o r     | L.elem r (ofRef o) = t
    strengthenOp t _ r     | otherwise          = strengthen t r
    ofRef (F.Reft (s, as)) = (F.Reft . (s,) . single) <$> as
    -- tagStrn t              = t `strengthen` tagR t


---------------------------------------------------------------------------------------
arrayQualifiers    :: RefType -> CGM RefType -- [F.Qualifier]
---------------------------------------------------------------------------------------
arrayQualifiers t   = do 
    modify $ \st -> st { quals = qs ++ quals st }
    return t 
   where
     qs             = concatMap (refTypeQualifiers γ0) $ [t]
     γ0             = E.envEmpty :: F.SEnv F.Sort 


refTypeQualifiers γ0 t = efoldRType rTypeSort addQs γ0 [] t 
  where addQs γ t qs   = (mkQuals γ t) ++ qs

mkQuals γ t      = [ mkQual γ v so pa | let (F.RR so (F.Reft (v, ras))) = rTypeSortedReft t 
                                      , F.RConc p                    <- ras                 
                                      , pa                         <- atoms p
                   ]

mkQual γ v so p = F.Q "Auto" [(v, so)] $ F.subst θ p
  where 
    θ             = F.mkSubst [(x, F.eVar y)   | (x, y) <- xys]
    xys           = zipWith (\x i -> (x, F.stringSymbol ("~A" ++ show i))) xs [0..] 
    xs            = L.delete v $ orderedFreeVars γ p


orderedFreeVars γ = L.nub . filter (`F.memberSEnv` γ) . F.syms 

atoms (F.PAnd ps)   = concatMap atoms ps
atoms p           = [p]


---------------------------------------------------------------------------------------
addAnnot       :: (F.Symbolic x) => SourceSpan -> x -> RefType -> CGM () 
---------------------------------------------------------------------------------------
addAnnot l x t = modify $ \st -> st {cg_ann = A.addAnnot l x t (cg_ann st)}

---------------------------------------------------------------------------------------
envAddReturn        :: (IsLocated f)  => f -> RefType -> CGEnv -> CGEnv 
---------------------------------------------------------------------------------------
envAddReturn f t g  = g { renv = E.envAddReturn f t (renv g) } 

---------------------------------------------------------------------------------------
envAddGuard       :: (F.Symbolic x, IsLocated x) => x -> Bool -> CGEnv -> CGEnv  
---------------------------------------------------------------------------------------
envAddGuard x b g = g { guards = guard b x : guards g }
  where 
    guard True    = F.eProp 
    guard False   = F.PNot . F.eProp


-- | A helper that returns the actual @RefType@ of variable @x@. Interstring cases:
--   
--   * Global variables (that can still be assigned) should not be strengthened
--     with single
--   * Class names (they might contain static fields)
--   * Local (non-assignable) variables (strengthened with singleton for base-types)
---------------------------------------------------------------------------------------
envFindTy :: (IsLocated x, F.Symbolic x, F.Expression x) => x -> CGEnv -> RefType 
---------------------------------------------------------------------------------------
envFindTy x g = fromMaybe err $ listToMaybe $ catMaybes [globalSpec, className, local]
  where 
    -- Check for global spec
    globalSpec  | isJust $ E.envFindTy x $ cge_spec g = E.envFindTy x $ renv g
                | otherwise                           = Nothing
    -- Check for static fields
    className   = case findSym x $ cge_defs g of    
        Just t  | t_class t -> Just $ TApp (TRef (F.symbol x) True) [] fTop
        _                   -> Nothing 
    -- Check for local variable
    local       = fmap (`eSingleton` x) $ E.envFindTy x $ renv g
    err         = throw $ bugUnboundVariable (srcPos x) (F.symbol x) 


envGlobAnnot _ x g = E.envFindTy x $ renv g

envFieldAnnot l    =  listToMaybe [ t | FieldAnn (_,t) <- ann_fact l ]


---------------------------------------------------------------------------------------
envRemSpec     :: (IsLocated x, F.Symbolic x, F.Expression x, PP x) 
               => x -> CGEnv -> CGM CGEnv
---------------------------------------------------------------------------------------
envRemSpec x g = do 
      gls   <- concat . M.elems . globs <$> get
      return $ g { cge_spec = E.envDel x $ cge_spec g 
                 , renv     = E.envDel x $ renv     g
                 , fenv     = foldr F.deleteIBindEnv (fenv g) gls }


-- | A global variable should have an entry in `cge_spec`.
---------------------------------------------------------------------------------------
isGlobalVar :: F.Symbolic x => x -> CGEnv -> Bool
---------------------------------------------------------------------------------------
isGlobalVar x g = E.envMem x $ cge_spec g
    

---------------------------------------------------------------------------------------
envToList     ::  CGEnv -> [(Id SourceSpan, RefType)]
---------------------------------------------------------------------------------------
envToList g = E.envToList $ renv g


---------------------------------------------------------------------------------------
envFindReturn :: CGEnv -> RefType 
---------------------------------------------------------------------------------------
envFindReturn = E.envFindReturn . renv


-- | Monad versions of TDefEnv operations
findSymOrDieM i = findSymOrDie i <$> getDef
findSymM i      = findSym i      <$> getDef



---------------------------------------------------------------------------------------
-- | Fresh Templates
---------------------------------------------------------------------------------------

-- | Instantiate Fresh Type (at Function-site)
---------------------------------------------------------------------------------------
freshTyFun :: (IsLocated l) => CGEnv -> l -> RefType -> CGM RefType 
---------------------------------------------------------------------------------------
freshTyFun g l t = freshTyFun' g l t . kVarInst . cg_opts =<< get  

freshTyFun' g l t b
  | b && isTrivialRefType t = freshTy "freshTyFun" (toType t) >>= wellFormed l g
  | otherwise               = return t

freshTyVar g l t 
  | isTrivialRefType t      = freshTy "freshTyVar" (toType t) >>= wellFormed l g
  | otherwise               = return t

-- | Instantiate Fresh Type (at Call-site)
freshTyInst l g αs τs tbody
  = do ts    <- mapM (freshTy "freshTyInst") τs
       _     <- mapM (wellFormed l g) ts
       return $ apply (fromList $ zip αs ts) tbody

-- | Instantiate Fresh Type (at Phi-site) 
---------------------------------------------------------------------------------------
freshTyPhis :: (PP l, IsLocated l) => l -> CGEnv -> [Id l] -> [Type] -> CGM (CGEnv, [RefType])  
---------------------------------------------------------------------------------------
freshTyPhis l g xs τs 
  = do ts <- mapM    (freshTy "freshTyPhis")  τs
       g' <- envAdds False (safeZip "freshTyPhis" xs ts) g
       _  <- mapM    (wellFormed l g') ts
       return (g', ts)

---------------------------------------------------------------------------------------
freshTyPhisWhile :: (PP l, IsLocated l) => l -> CGEnv -> [Id l] -> [Type] -> CGM (CGEnv, [RefType])  
---------------------------------------------------------------------------------------
freshTyPhisWhile l g xs τs 
  = do ts <- mapM    (freshTy "freshTyPhis")  τs
       g' <- envAdds False (safeZip "freshTyPhis" xs ts) g
       _  <- mapM    (wellFormed l g) ts
       return (g', ts)

-- | Fresh Object Type
---------------------------------------------------------------------------------------
freshTyObj :: (IsLocated l) => l -> CGEnv -> RefType -> CGM RefType 
---------------------------------------------------------------------------------------
freshTyObj l g t = freshTy "freshTyArr" t >>= wellFormed l g 



---------------------------------------------------------------------------------------
-- | Adding Subtyping Constraints
---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------
subType :: (IsLocated l) => l -> CGEnv -> RefType -> RefType -> CGM ()
---------------------------------------------------------------------------------------
subType l g t1 t2 =
  do t1'   <- addInvariant t1  -- enhance LHS with invariants
     g'    <- envAdds False [(symbolId l x, t) | (x, Just t) <- rNms t1' ++ rNms t2 ] g
     modify $ \st -> st {cs = c g' (t1', t2) : (cs st)}
  where
    c g     = uncurry $ Sub g (ci l)
    rNms t  = (\n -> (n, n `E.envFindTy` renv g)) <$> names t
    names   = foldReft rr []
    rr r xs = F.syms r ++ xs


safeExtends sub δ (TD _ c _ (Just (p, ts)) es) = zipWithM_ sub t1s t2s
  where
    (t1s, t2s) = unzip [ (t1,t2) | pe <- flatten δ (findSymOrDie p δ, ts)
                                 , ee <- undefined
                                 , sameBinder pe ee 
                                 , let t1 = eltType ee
                                 , let t2 = eltType pe ]

safeExtends _ _ (TD _ _ _ Nothing _) = return ()


--------------------------------------------------------------------------------
-- | Adding Well-Formedness Constraints
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
wellFormed       :: (IsLocated l) => l -> CGEnv -> RefType -> CGM RefType  
--------------------------------------------------------------------------------
wellFormed l g t = do modify $ \st -> st { ws = (W g (ci l) t) : ws st }
                      return t


--------------------------------------------------------------------------------
-- | Generating Fresh Values 
--------------------------------------------------------------------------------

class Freshable a where
  fresh   :: CGM a
  true    :: a -> CGM a
  true    = return . id
  refresh :: a -> CGM a
  refresh = return . id

instance Freshable Integer where
  fresh = do modify $ \st -> st { count = 1 + (count st) }
             count <$> get 

instance Freshable F.Symbol where
  fresh = F.tempSymbol "nano" <$> fresh

instance Freshable String where
  fresh = F.symbolString <$> fresh

-- | Freshen up
freshTy :: RefTypable a => s -> a -> CGM RefType
freshTy _ τ = refresh $ rType τ

instance Freshable F.Refa where
  fresh = (`F.RKvar` mempty) <$> (F.intKvar <$> fresh)

instance Freshable [F.Refa] where
  fresh = single <$> fresh

instance Freshable F.Reft where
  fresh                  = errorstar "fresh Reft"
  true    (F.Reft (v,_)) = return $ F.Reft (v, []) 
  refresh (F.Reft (_,_)) = curry F.Reft <$> freshVV <*> fresh
    where freshVV        = F.vv . Just  <$> fresh

instance Freshable F.SortedReft where
  fresh                  = errorstar "fresh Reft"
  true    (F.RR so r)    = F.RR so <$> true r 
  refresh (F.RR so r)    = F.RR so <$> refresh r

instance Freshable RefType where
  fresh   = errorstar "fresh RefType"
  refresh = refreshRefType
  true    = trueRefType 

trueRefType    :: RefType -> CGM RefType
trueRefType    = mapReftM true

refreshRefType :: RefType -> CGM RefType
refreshRefType = mapReftM refresh

refreshValueVar :: RefType -> CGM RefType
refreshValueVar t = T.mapM freshR t
  where 
    freshR (F.Reft (v,r)) = do 
      v' <- freshVV
      return $ F.Reft (v', F.substa (ss v v') r)
    freshVV = F.vv . Just  <$> fresh
    ss old new w | F.symbol old == F.symbol w = new
                 | otherwise                  = w

--------------------------------------------------------------------------------
-- | Splitting Subtyping Constraints 
--------------------------------------------------------------------------------
--
-- The types that are split should be aligned by type
--
-- Premise: |t1| = |t2| -- raw type equality
--
--------------------------------------------------------------------------------
splitC :: SubC -> CGM [FixSubC]
--------------------------------------------------------------------------------

-- | Function types
splitC (Sub g i tf1@(TFun xt1s t1 _) tf2@(TFun xt2s t2 _))
  = do bcs       <- bsplitC g i tf1 tf2
       g'        <- envTyAdds i xt2s g 
       cs        <- concatMapM splitC $ safeZipWith "splitc-1" (Sub g' i) t2s t1s' 
       cs'       <- splitC $ Sub g' i (F.subst su t1) t2      
       return     $ bcs ++ cs ++ cs'
    where 
       t2s        = b_type <$> xt2s
       t1s'       = F.subst su (b_type <$> xt1s)
       su         = F.mkSubst $ safeZipWith "splitc-2" bSub xt1s xt2s
       bSub b1 b2 = (b_sym b1, F.eVar $ b_sym b2)

-- | TAlls
splitC (Sub g i (TAll α1 t1) (TAll α2 t2))
  | α1 == α2 
  = splitC $ Sub g i t1 t2
  | otherwise   
  = splitC $ Sub g i t1 t2' 
  where 
    θ   = fromList [(α2, tVar α1 :: RefType)]
    t2' = apply θ t2

-- | TVars
splitC (Sub g i t1@(TVar α1 _) t2@(TVar α2 _)) 
  | α1 == α2
  = bsplitC g i t1 t2
  | otherwise
  = cgError l $ bugBadSubtypes l t1 t2 where l = srcPos i

-- | Unions
splitC (Sub g i t1@(TApp TUn t1s _) t2@(TApp TUn t2s _))
  = (++) <$> bsplitC g i t1 t2 
         <*> concatMapM splitC (safeZipWith "splitc-3" (Sub g i) s1s s2s)
    where 
       s1s = L.sortBy (compare `on` toType) t1s
       s2s = L.sortBy (compare `on` toType) t2s

-- |Type references
splitC (Sub g i t1@(TApp (TRef x1 s1) (μ1:t1s) _) t2@(TApp (TRef x2 s2) (μ2:t2s) _)) 
  | (x1,s1) /= (x2,s2) 
  = cgError l $ bugBadSubtypes l t1 t2
  | isImmutable μ2
  = do  cs    <- bsplitC g i t1 t2
        δ     <- getDef
        cs'   <- splitWithVariance (varianceTDef $ findSymOrDie x1 δ) t1s t2s
        return $ cs ++ cs' 
  | otherwise 
  = do  cs    <- bsplitC g i t1 t2
        cs'   <- concatMapM splitC $ safeZipWith "splitc-4" (Sub g i) t1s t2s
        cs''  <- concatMapM splitC $ safeZipWith "splitc-5" (Sub g i) t2s t1s
        return $ cs ++ cs' ++ cs''
  where
    splitWithVariance vs t1s t2s = concat <$> zipWith3M splitCov vs t1s t2s
    splitCov True  t1 t2 = splitC (Sub g i t1 t2)
    splitCov False t1 t2 = splitC (Sub g i t2 t1)
    l = srcPos i

-- -- FIXME: Add constraint for null
-- splitC (Sub _ _ (TApp (TRef _ _) _ _) (TApp TNull _ _)) 
--   = return []
-- 
-- splitC (Sub _ _ (TApp TNull _ _) (TApp (TRef _ _) _ _)) 
--   = return []

-- | Rest of TApp
splitC (Sub g i t1@(TApp c1 t1s _) t2@(TApp c2 t2s _))
  | c1 == c2  = (++) <$> bsplitC g i t1 t2
                     <*> concatMapM splitC 
                           (safeZipWith (printf "splitc-5: %s - %s" (ppshow t1) (ppshow t2)) 
                             (Sub g i) t1s t2s)
  | otherwise = cgError l $ bugBadSubtypes l t1 t2 where l = srcPos i

-- | TCons
splitC (Sub g i t1@(TCons e1s μ1 _ ) t2@(TCons e2s μ2 _ ))
  = (++) <$> bsplitC g i t1 t2
         <*> splitEs g i μ1 μ2 e1s e2s
  
splitC x@(Sub g i t1 t2)
  = cgError l $ bugBadSubtypes l t1 t2 where l = srcPos x


-- FIXME: 
--  * Include mutability
--  * Add special cases: IndexSig ...
---------------------------------------------------------------------------------------
-- splitEs :: CGEnv -> Cinfo -> [TElt RefType] -> [TElt RefType] -> CGM [FixSubC]
---------------------------------------------------------------------------------------
splitEs g i μ1 μ2 e1s e2s
  | length t1s == length t2s 
  = concatMapM splitC $ zipWith (Sub g i) t1s t2s
  | otherwise
  = cgError l $ bugMalignedFields l e1s e2s 
  where
    l     = srcPos i
    t1s   = f_type <$> L.sortBy (compare `on` F.symbol) (filter flt e1s)
    t2s   = f_type <$> L.sortBy (compare `on` F.symbol) (filter flt e2s)
    flt x = nonStaticElt x && nonConstrElt x


---------------------------------------------------------------------------------------
bsplitC :: CGEnv -> a -> RefType -> RefType -> CGM [F.SubC a]
---------------------------------------------------------------------------------------
-- NOTE: removing addInvariant from RHS
bsplitC g ci t1 t2 = bsplitC' g ci <$> addInvariant t1 <*> return t2

bsplitC' g ci t1 t2
  | F.isFunctionSortedReft r1 && F.isNonTrivialSortedReft r2
  = F.subC (fenv g) F.PTrue (r1 {F.sr_reft = fTop}) r2 Nothing [] ci
  | F.isNonTrivialSortedReft r2
  = F.subC (fenv g) p r1 r2 Nothing [] ci
  | otherwise
  = []
  where
    p  = F.pAnd $ guards g
    -- Use common sort for top or named type
    (r1,r2) = sorts t1 t2

-- BUGGY!!!
--     sorts t1 t2@(TApp TTop _ _ )
--       = (rTypeSortedReft t2, rTypeSortedReft t2)
--     sorts (TApp (TRef _) _ _ ) t2@(TApp (TRef _) _ _ ) 
--       = (rTypeSortedReft t2, rTypeSortedReft t2)
    sorts t1 t2
      = (rTypeSortedReft t1, rTypeSortedReft t2)

instance PP (F.SortedReft) where
  pp (F.RR _ b) = pp b

---------------------------------------------------------------------------------------
-- | Splitting Well-Formedness Constraints
---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------
splitW :: WfC -> CGM [FixWfC]
---------------------------------------------------------------------------------------
splitW (W g i ft@(TFun ts t _)) 
  = do let bws = bsplitW g ft i
       g'     <- envTyAdds i ts g 
       ws     <- concatMapM splitW [W g' i ti | B _ ti <- ts]
       ws'    <-            splitW (W g' i t)
       return  $ bws ++ ws ++ ws'

splitW (W g i (TAll _ t)) 
  = splitW (W g i t)

splitW (W g i t@(TVar _ _))
  = return $ bsplitW g t i 

splitW (W g i t@(TApp _ ts _))
  =  do let ws = bsplitW g t i
        ws'   <- concatMapM splitW [W g i ti | ti <- ts]
        return $ ws ++ ws'

splitW (W g i (TAnd ts))
  = concatMapM splitW [W g i t | t <- ts]

splitW (W g i t@(TCons es _ _))
  = do let bws = bsplitW g t i
       -- FIXME: add field bindings in g?
       ws     <- concatMapM splitW [ W g i $ eltType e | e <- es ]
       return  $ bws ++ ws

splitW (W _ _ t) = error $ render $ text "Not supported in splitW: " <+> pp t

bsplitW g t i 
  | F.isNonTrivialSortedReft r'
  = [F.wfC (fenv g) r' Nothing i] 
  | otherwise
  = []
  where r' = rTypeSortedReft t

envTyAdds l xts = envAdds False [(symbolId l x, t) | B x t <- xts]

cgFunTys l f xs ft = 
  case funTys l f xs ft of 
    Left e  -> cgError l e 
    Right a -> return a


--------------------------------------------------------------------------------
-- | `this`

cgPeekThis = safeHead "get 'this'" <$> (cg_this <$> get)

cgPushThis t = modify $ \st -> st { cg_this = t : cg_this st } 

cgPopThis    = modify $ \st -> st { cg_this = tail $ cg_this st } 

cgWithThis t p = do { cgPushThis t; a <- p; cgPopThis; return a } 


--------------------------------------------------------------------------------
getSuperM :: IsLocated a => a -> RefType -> CGM RefType
--------------------------------------------------------------------------------
getSuperM l (TApp (TRef i s) ts _) = fromTdef =<< findSymOrDieM i
  where fromTdef (TD _ _ vs (Just (p,ps)) _) = do
          return  $ apply (fromList $ zip vs ts) 
                  $ TApp (TRef (F.symbol p) s) ps fTop
        fromTdef (TD _ _ _ Nothing _) = cgError l $ errorSuper (srcPos l) 
getSuperM l _  = cgError l $ errorSuper (srcPos l) 

--------------------------------------------------------------------------------
getSuperDefM :: IsLocated a => a -> RefType -> CGM (TDef RefType)
--------------------------------------------------------------------------------
getSuperDefM l (TApp (TRef i _) ts _) = fromTdef =<< findSymOrDieM i
  where 
    fromTdef (TD _ _ vs (Just (p,ps)) _) = 
      do TD c n ws pp ee <- findSymOrDieM p
         return  $ apply (fromList $ zip vs ts) 
                 $ apply (fromList $ zip ws ps)
                 $ TD c n [] pp ee
    fromTdef (TD _ _ _ Nothing _) = cgError l $ errorSuper (srcPos l) 
getSuperDefM l _  = cgError l $ errorSuper (srcPos l)

