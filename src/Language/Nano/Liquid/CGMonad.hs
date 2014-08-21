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
  , runFailM

  -- * Throw Errors
  , cgError      

  -- * Fresh Templates for Unknown Refinement Types 
  , freshTyFun, freshTyVar, freshTyInst, freshTyPhis
  , freshTyPhisWhile, freshTyObj, freshenCGEnvM

  -- * Freshable
  , Freshable (..), refreshValueVar

  -- * Environment API
  , envAddFresh, envAdds, envAddReturn, envAddGuard, envFindTy, safeEnvFindTy
  , isGlobalVar 
  , envRemSpec, isGlobalVar, envFindReturn, envPushContext
  , envGetContextCast, envGetContextTypArgs
  , scrapeQualifiers

  -- * Add Subtyping Constraints
  , subType, wellFormed -- , safeExtends
  
  -- * Add Type Annotations
  , addAnnot

  -- * Function Types
  , cgFunTys, cgMethTys


  -- * Super 
  , getSuperM, getSuperDefM

  -- * Zip type wrapper
  , zipTypeM


  ) where

import           Data.Maybe                     (fromMaybe, listToMaybe, catMaybes, isJust)
import           Data.Monoid                    (mempty)
import qualified Data.HashMap.Strict            as M
-- import qualified Data.HashSet            as S
import qualified Data.List                      as L
import           Data.Function                  (on)
import           Text.PrettyPrint.HughesPJ
import qualified Data.Traversable                   as T 
import           Language.Nano.Types
import           Language.Nano.Errors
import           Language.Nano.Annots
import qualified Language.Nano.Env              as E
import           Language.Nano.Locations
import           Language.Nano.Misc
import           Language.Nano.Names
import           Language.Nano.CmdLine
import           Language.Nano.Program
import           Language.Nano.Typecheck.Resolve
import qualified Language.Nano.SystemUtils      as S
import           Language.Nano.Typecheck.Types  hiding (quals)
import           Language.Nano.Typecheck.Lookup
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Environment
import           Language.Nano.Liquid.Environment
import           Language.Nano.Liquid.Types
import           Language.Nano.Liquid.Qualifiers


import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Errors
import           Control.Applicative 
import           Control.Exception (throw)

import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Error hiding (Error)

import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.PrettyPrint

-- import           Debug.Trace                        (trace)

-------------------------------------------------------------------------------
-- | Top level type returned after Constraint Generation
-------------------------------------------------------------------------------

data CGInfo = CGI { cgi_finfo :: F.FInfo Cinfo
                  , cgi_annot :: S.UAnnInfo RefType  
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

  
runFailM a = fst . runState (runErrorT a) <$> get

-------------------------------------------------------------------------------
initState       :: Config -> Nano AnnTypeR F.Reft -> CGState
-------------------------------------------------------------------------------
initState c p   = CGS F.emptyBindEnv [] [] 0 mempty invs c [] 
  where 
    invs        = M.fromList [(tc, t) | t@(Loc _ (TApp tc _ _)) <- invts p]


-------------------------------------------------------------------------------
cgStateCInfo :: NanoRefType -> (([F.SubC Cinfo], [F.WfC Cinfo]), CGState) -> CGInfo
-------------------------------------------------------------------------------
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
measureEnv   ::  Nano a F.Reft -> F.SEnv F.SortedReft
---------------------------------------------------------------------------------------
measureEnv   = fmap rTypeSortedReft . E.envSEnv . consts 

---------------------------------------------------------------------------------------
-- | Constraint Generation Monad 
---------------------------------------------------------------------------------------

data CGState = CGS { 
  -- 
  -- ^ global list of fixpoint binders
  --
    binds    :: F.BindEnv
  -- 
  -- ^ subtyping constraints
  --
  , cs       :: ![SubC]
  -- 
  -- ^ well-formedness constraints
  --
  , ws       :: ![WfC]               
  -- 
  -- ^ freshness counter
  --
  , count    :: !Integer             
  -- 
  -- ^ recorded annotations
  --
  , cg_ann   :: S.UAnnInfo RefType   
  -- 
  -- ^ type constructor invariants
  --
  , invs     :: TConInv              
  -- 
  -- ^ configuration options
  --
  , cg_opts  :: Config               
  -- 
  -- ^ qualifiers that arise at typechecking
  --
  , quals    :: ![F.Qualifier]       
  }

type CGM     = ErrorT Error (State CGState)

type TConInv = M.HashMap TCon (Located RefType)
 

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
  = do x  <- freshId $ srcPos l
       g' <- envAdds [(x, t)] g
       addAnnot (srcPos l) x t
       return (x, g')
   
freshId l = Id (Ann l []) <$> fresh


---------------------------------------------------------------------------------------
envAdds :: (PP x, F.Symbolic x, IsLocated x) => [(x, RefType)] -> CGEnv -> CGM CGEnv
---------------------------------------------------------------------------------------
envAdds xts' g
  = do xts      <- zip xs  <$> mapM addInvariant ts
       is       <- forM xts $  addFixpointBind 
       _        <- forM xts $  \(x, t) -> addAnnot (srcPos x) x t
       return    $ g { cge_names = E.envAdds xts $ cge_names g
                     , cge_fenv  = F.insertsIBindEnv is $ cge_fenv g 
                     -- Update the binders...
                     , cge_globs = foldl adjust (cge_globs g) (zip xs is) }
    where
       (xs,ts)        = unzip xts'
       adjust m (k,v) = M.adjust (v:) (F.symbol k) m



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
addInvariant t               = (ty . (`tx` t) . invs) <$> get
  where
    tx i t@(TApp tc _ o)     = maybe t (strengthenOp t o . rTypeReft . val) $ M.lookup tc i
    tx _ t                   = t 
    strengthenOp t o r       | L.elem r (ofRef o) = t
    strengthenOp t _ r       | otherwise          = strengthen t r
    ofRef (F.Reft (s, as))   = (F.Reft . (s,) . single) <$> as

    -- instanceof(v,"C")
    ty t@(TApp (TRef c) _ _) = t `strengthen` reftIO t (name c)
    ty t                     = t 
    name (RN (QName _ _ s))  = s
    reftIO t c               = F.Reft (vv t, [refaIO t c])
    refaIO t c               = F.RConc $ F.PBexp $ F.EApp sym [F.expr $ vv t, F.expr   $ F.symbolText c]
    vv                       = rTypeValueVar
    sym                      = F.dummyLoc $ F.symbol "instanceof"


---------------------------------------------------------------------------------------
scrapeQualifiers   :: RefType -> CGM RefType 
---------------------------------------------------------------------------------------
scrapeQualifiers t  = do 
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

mkQual γ v so p = F.Q (F.symbol "Auto") [(v, so)] (F.subst θ p) l0
  where 
    θ             = F.mkSubst [(x, F.eVar y)   | (x, y) <- xys]
    xys           = zipWith (\x i -> (x, F.symbol ("~A" ++ show i))) xs [0..] 
    xs            = L.delete v $ orderedFreeVars γ p
    l0            = F.dummyPos "RSC.CGMonad.mkQual"


orderedFreeVars γ = L.nub . filter (`F.memberSEnv` γ) . F.syms 

atoms (F.PAnd ps)   = concatMap atoms ps
atoms p           = [p]


---------------------------------------------------------------------------------------
addAnnot       :: (F.Symbolic x) => SourceSpan -> x -> RefType -> CGM () 
---------------------------------------------------------------------------------------
addAnnot l x t = modify $ \st -> st {cg_ann = S.addAnnot l x t (cg_ann st)}

---------------------------------------------------------------------------------------
envAddReturn        :: (IsLocated f)  => f -> RefType -> CGEnv -> CGEnv 
---------------------------------------------------------------------------------------
envAddReturn f t g  = g { cge_names = E.envAddReturn f t (cge_names g) } 

---------------------------------------------------------------------------------------
envAddGuard       :: (F.Symbolic x, IsLocated x) => x -> Bool -> CGEnv -> CGEnv  
---------------------------------------------------------------------------------------
envAddGuard x b g = g { cge_guards = guard b x : cge_guards g }
  where 
    guard True    = F.eProp 
    guard False   = F.PNot . F.eProp


-- | A helper that returns the @RefType@ of variable @x@. Interstring cases:
--   
--   * Global variables (that can still be assigned) should not be strengthened
--     with single
--
--   * Class names (they might contain static fields)
--
--   * Local (non-assignable) variables (strengthened with singleton for base-types)
--
---------------------------------------------------------------------------------------
envFindTy :: (IsLocated x, F.Symbolic x, F.Expression x) => x -> CGEnv -> Maybe RefType 
---------------------------------------------------------------------------------------
envFindTy x g | isGlobalVar x g = E.envFindTy x $ cge_names g
              | otherwise       = fmap (`eSingleton` x) $ E.envFindTy x $ cge_names g


---------------------------------------------------------------------------------------
safeEnvFindTy :: (IsLocated x, F.Symbolic x, F.Expression x, PP x) 
              => x -> CGEnv -> CGM RefType 
---------------------------------------------------------------------------------------
safeEnvFindTy x g = case envFindTy x g of
                        Just t  -> return t
                        Nothing ->  cgError l $ bugEnvFindTy l x 
  where
    l = srcPos x


-- | `isGlobalVar x g` returns True if @x@ is a global variable.
---------------------------------------------------------------------------------------
isGlobalVar :: (IsLocated x, F.Symbolic x, F.Expression x) => x -> CGEnv -> Bool
---------------------------------------------------------------------------------------
isGlobalVar x = isJust . M.lookup (F.symbol x) . cge_globs


-- | `envRemSpec x g` removes:
--
--   * all bindings of @x@ from the name's environment, and
--
--   * all bindings of other global variables that are in scope from the set of
--     fixpoint bindings.
--
---------------------------------------------------------------------------------------
envRemSpec :: F.Symbolic x => x -> CGEnv -> CGEnv
---------------------------------------------------------------------------------------
envRemSpec x g 
  = g { cge_names = E.envDel x $ cge_names g
      , cge_fenv  = foldr F.deleteIBindEnv (cge_fenv g) $ concat $ M.elems $ cge_globs g }


---------------------------------------------------------------------------------------
envFindReturn :: CGEnv -> RefType 
---------------------------------------------------------------------------------------
envFindReturn = E.envFindReturn . cge_names


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
  = do ts <- mapM (freshTy "freshTyPhis")  τs
       g' <- envAdds (safeZip "freshTyPhis" xs ts) g
       _  <- mapM (wellFormed l g') ts
       return (g', ts)

---------------------------------------------------------------------------------------
freshTyPhisWhile :: (PP l, IsLocated l) => l -> CGEnv -> [Id l] -> [Type] -> CGM (CGEnv, [RefType])  
---------------------------------------------------------------------------------------
freshTyPhisWhile l g xs τs 
  = do ts <- mapM (freshTy "freshTyPhis")  τs
       g' <- envAdds (safeZip "freshTyPhis" xs ts) g
       _  <- mapM (wellFormed l g) ts
       return (g', ts)

-- | Fresh Object Type
---------------------------------------------------------------------------------------
freshTyObj :: (IsLocated l) => l -> CGEnv -> RefType -> CGM RefType 
---------------------------------------------------------------------------------------
freshTyObj l g t = freshTy "freshTyArr" t >>= wellFormed l g 


---------------------------------------------------------------------------------------
freshenCGEnvM :: CGEnv -> CGM CGEnv
---------------------------------------------------------------------------------------
freshenCGEnvM g  
  = do  names   <- E.envFromList  <$> mapM (freshenVarbindingM g) (E.envToList $ cge_names g)
        modules <- E.qenvFromList <$> mapM (freshenModuleDefM g) (E.qenvToList $ cge_mod g)
        return $ g { cge_names = names, cge_mod = modules } 
  where


freshenVarbindingM g (x,t) = (x,) <$> freshTyVar g (srcPos x) t

freshenModuleDefM g (a, m)  
  = do  vars     <- E.envFromList <$> mapM f (E.envToList $ m_variables m)
        types    <- E.envFromList <$> mapM h (E.envToList $ m_types m)        
        return (a,m { m_variables = vars, m_types = types })
  where
    f (x, (v, w, t)) =
      case w of
        WriteLocal -> return (x,(v,w,t))
        -- Freshen global variables and specs
        _          -> do  ft    <- freshTyVar g (srcPos x) t 
                          return   (x, (v, w, ft))

    h (x, ID c n αs hr es) = 
      case hr of
        Just (p,ps) -> do ps'   <- mapM (freshTyVar g (srcPos x)) ps 
                          es'   <- mapM (mapEltM $ freshTyVar g (srcPos x)) es 
                          return   (x, ID c n αs (Just (p,ps')) es')
        Nothing     -> do es'   <- mapM (mapEltM $ freshTyVar g (srcPos x)) es
                          return   (x, ID c n αs Nothing es')



---------------------------------------------------------------------------------------
-- | Adding Subtyping Constraints
---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------
subType :: (IsLocated l) => l -> CGEnv -> RefType -> RefType -> CGM ()
---------------------------------------------------------------------------------------
subType l g t1 t2 =
  do t1'   <- addInvariant t1  -- enhance LHS with invariants
     g'    <- envAdds [(symbolId l x, t) | (x, Just t) <- rNms t1' ++ rNms t2 ] g
     modify $ \st -> st {cs = c g' (t1', t2) : (cs st)}
  where
    c g     = uncurry $ Sub g (ci l)
    rNms t  = (\n -> (n, n `E.envFindTy` cge_names g)) <$> names t
    names   = foldReft rr []
    rr r xs = F.syms r ++ xs


-- FIXME: Restore this check !!!
-- ---------------------------------------------------------------------------------------
-- safeExtends :: SourceSpan -> CGEnv -> IfaceDef F.Reft -> CGM ()
-- ---------------------------------------------------------------------------------------
-- safeExtends l g (ID _ _ _ (Just (p, ts)) es) = zipWithM_ sub t1s t2s
--   where
--     sub t1 t2  = subType l g (zipType δ t1 t2) t2
--     (t1s, t2s) = unzip [ (t1,t2) | pe <- flatten True δ (findSymOrDie p δ, ts)
--                                  , ee <- es 
--                                  , sameBinder pe ee 
--                                  , let t1 = eltType ee
--                                  , let t2 = eltType pe ]
-- safeExtends _ _ _ (ID _ _ _ Nothing _) = return ()


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
  fresh = F.tempSymbol (F.symbol "nano") <$> fresh

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
  fresh                  = errorstar "fresh F.Reft"
  true    (F.Reft (v,_)) = return $ F.Reft (v, []) 
  refresh (F.Reft (_,_)) = curry F.Reft <$> freshVV <*> fresh
    where freshVV        = F.vv . Just  <$> fresh

instance Freshable F.SortedReft where
  fresh                  = errorstar "fresh F.Reft"
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
--
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
--
splitC (Sub g i (TAll α1 t1) (TAll α2 t2))
  | α1 == α2 
  = splitC $ Sub g i t1 t2
  | otherwise   
  = splitC $ Sub g i t1 t2' 
  where 
    θ   = fromList [(α2, tVar α1 :: RefType)]
    t2' = apply θ t2

-- | TVars
--
splitC (Sub g i t1@(TVar α1 _) t2@(TVar α2 _)) 
  | α1 == α2
  = bsplitC g i t1 t2
  | otherwise
  = cgError l $ bugBadSubtypes l t1 t2 where l = srcPos i

-- | Unions
--
splitC (Sub g i t1@(TApp TUn t1s _) t2@(TApp TUn t2s _))
  = (++) <$> bsplitC g i t1 t2 
         <*> concatMapM splitC (safeZipWith "splitc-3" (Sub g i) s1s s2s)
    where 
       s1s = L.sortBy (compare `on` toType) t1s
       s2s = L.sortBy (compare `on` toType) t2s

-- |Type references
--  
--  FIXME: restore co/contra-variance 
--
splitC (Sub g i t1@(TApp (TRef x1) (_:t1s) _) t2@(TApp (TRef x2) (μ2:t2s) _)) 
  | x1 /= x2 
  = cgError l $ bugBadSubtypes l t1 t2
  | isImmutable μ2
  = do  cs    <- bsplitC g i t1 t2
        -- cs'   <- splitWithVariance (varianceTDef $ findSymOrDie x1 δ) t1s t2s
        cs'   <- concatMapM splitC $ safeZipWith "split-4" (Sub g i) t1s t2s
        return $ cs ++ cs' 
  | otherwise 
  = do  cs    <- bsplitC g i t1 t2
        cs'   <- concatMapM splitC $ safeZipWith "splitc-4" (Sub g i) t1s t2s
        cs''  <- concatMapM splitC $ safeZipWith "splitc-5" (Sub g i) t2s t1s
        return $ cs ++ cs' ++ cs''
  where
--     splitWithVariance vs t1s t2s = concat <$> zipWith3M splitCov vs t1s t2s
--     splitCov True  t1 t2 = splitC (Sub g i t1 t2)
--     splitCov False t1 t2 = splitC (Sub g i t2 t1)
    l = srcPos i

-- -- FIXME: Add constraint for null
-- splitC (Sub _ _ (TApp (TRef _ _) _ _) (TApp TNull _ _)) 
--   = return []
-- 
-- splitC (Sub _ _ (TApp TNull _ _) (TApp (TRef _ _) _ _)) 
--   = return []

-- | Rest of TApp
--
splitC (Sub g i t1@(TApp c1 t1s _) t2@(TApp c2 t2s _))
  | c1 == c2
  = do  cs    <- bsplitC g i t1 t2
        cs'   <- concatMapM splitC ((safeZipWith "splitc-5") (Sub g i) t1s t2s)
        return $ cs ++ cs'
  | otherwise = cgError l $ bugBadSubtypes l t1 t2 where l = srcPos i

-- | These need to be here due to the lack of a folding operation
--
splitC (Sub g i t1@(TApp (TRef _) _ _) t2) = 
  case flattenType g t1 of
    Just t1' -> splitC (Sub g i t1' t2)
    Nothing  -> cgError l $ errorUnfoldType l t1 where l = srcPos i

splitC (Sub g i t1 t2@(TApp (TRef _) _ _)) = 
  case flattenType g t2 of
    Just t2' -> splitC (Sub g i t1 t2')
    Nothing  -> cgError l $ errorUnfoldType l t2 where l = srcPos i

-- | TCons
--
splitC (Sub g i t1@(TCons e1s μ1 _ ) t2@(TCons e2s μ2 _ ))
  = do  cs    <- bsplitC g i t1 t2
        cs'   <- splitEs g i μ1 μ2 e1s e2s
        return $ cs ++ cs'
  
splitC x@(Sub _ _ t1 t2)
  = cgError l $ bugBadSubtypes l t1 t2 where l = srcPos x


-- FIXME: 
--  * Add special cases: IndexSig ...
---------------------------------------------------------------------------------------
splitEs :: CGEnv -> Cinfo -> Mutability -> Mutability 
            -> [TypeMember F.Reft] -> [TypeMember F.Reft] -> CGM [FixSubC]

---------------------------------------------------------------------------------------
splitEs g i μ1 μ2 e1s e2s
  | length e1s' == length e2s'
  = concatMapM (uncurry $ splitE g i μ1 μ2) $ zip e1s' e2s'
  | otherwise
  = cgError l $ bugMalignedFields l e1s e2s 
  where
    l     = srcPos i
    e1s'  = L.sortBy (compare `on` F.symbol) (filter flt e1s)
    e2s'  = L.sortBy (compare `on` F.symbol) (filter flt e2s)
    flt x = nonStaticSig x && nonConstrElt x


splitE g i _  _ (CallSig t1) (CallSig t2) = splitC (Sub g i t1 t2)
splitE g i _  _ (ConsSig t1) (ConsSig t2) = splitC (Sub g i t1 t2)

splitE g i _ _   (IndexSig _ _ t1) (IndexSig _ _ t2) 
  = do  cs    <- splitC (Sub g i t1 t2)
        cs'   <- splitC (Sub g i t2 t1)
        return $ cs ++ cs'

splitE g i μ1 μ2 (FieldSig _ μf1 t1) (FieldSig _ μf2 t2)
  = splitWithMut g i μ1 μ2 (μf1,t1) (μf2,t2)

splitE g i μ1 μ2 (MethSig _ μf1 t1) (MethSig _ μf2 t2)
  = splitWithMut g i μ1 μ2 (μf1,t1) (μf2,t2)

splitE _ _ _ _ _ _ = return []


splitWithMut g i _ μ2 (_,t1) (μf2,t2)
  | isImmutable m2 
  = splitC (Sub g i t1 t2)
  | otherwise 
  = (++) <$> splitC (Sub g i t1 t2)
         <*> splitC (Sub g i t2 t1)
  where
    m2 = combMut μ2 μf2


-- splitMaybe g i (Just t1) (Just t2) = splitC (Sub g i t1 t2) 
-- splitMaybe _ _ _         _         = return []

---------------------------------------------------------------------------------------
bsplitC :: CGEnv -> a -> RefType -> RefType -> CGM [F.SubC a]
---------------------------------------------------------------------------------------
-- NOTE: removing addInvariant from RHS
bsplitC g ci t1 t2 = bsplitC' g ci <$> addInvariant t1 <*> return t2

bsplitC' g ci t1 t2
  | F.isFunctionSortedReft r1 && F.isNonTrivialSortedReft r2
  = F.subC (cge_fenv g) F.PTrue (r1 {F.sr_reft = fTop}) r2 Nothing [] ci
  | F.isNonTrivialSortedReft r2
  = F.subC (cge_fenv g) p r1 r2 Nothing [] ci
  | otherwise
  = []
  where
    p  = F.pAnd $ cge_guards g
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
  = [F.wfC (cge_fenv g) r' Nothing i] 
  | otherwise
  = []
  where r' = rTypeSortedReft t

envTyAdds l xts = envAdds [(symbolId l x, t) | B x t <- xts]

cgFunTys l f xs ft = 
  case funTys l f xs ft of 
    Left e  -> cgError l e 
    Right a -> return a


------------------------------------------------------------------------------
cgMethTys :: (PP a) => AnnTypeR -> a -> (Mutability, RefType)
                    -> CGM [(Int, Mutability, ([TVar], [RefType], RefType))]
------------------------------------------------------------------------------
cgMethTys l f (m,t) 
   = zip3 [0..] (repeat m) <$> mapM (methTys l f) (bkAnd t)

methTys l f ft0
  = case remThisBinding ft0 of
      Nothing         -> cgError l $ errorNonFunction (srcPos l) f ft0 
      Just (vs,bs,t)  -> return    $ (vs,b_type <$> bs,t)


--------------------------------------------------------------------------------
getSuperM :: IsLocated a => a -> RefType -> CGM RefType
--------------------------------------------------------------------------------
-- getSuperM l (TApp (TRef i) ts _) 
--   = do  z    <- findSymOrDieM i
--         case z of 
--           ID _ _ vs (Just (p,ps)) _ -> return  $ apply (fromList $ zip vs ts) 
--                                                $ TApp (TRef $ F.symbol p) ps fTop
--           ID _ _ _ Nothing _        -> cgError l $ errorSuper (srcPos l) 

getSuperM l _  = cgError l $ errorSuper $ srcPos l


--------------------------------------------------------------------------------
getSuperDefM :: IsLocated a => a -> RefType -> CGM (IfaceDef F.Reft)
--------------------------------------------------------------------------------
-- getSuperDefM l (TApp (TRef i) ts _) 
--   = do  z    <- findSymOrDieM i
--         case z of 
--           ID _ _ vs (Just (p,ps)) _ -> 
--             do ID c n ws pp ee  <- findSymOrDieM p
--                return            $ apply (fromList $ zip vs ts) 
--                                  $ apply (fromList $ zip ws ps)
--                                  $ ID c n [] pp ee
--           ID _ _ _ Nothing _ -> cgError l $ errorSuper (srcPos l) 

getSuperDefM l _  = cgError l $ errorSuper $ srcPos l



--------------------------------------------------------------------------------
-- | zipType wrapper

zipTypeM l g t1 t2 = 
  case zipType g t1 t2 of
    Just t  -> return t
    Nothing -> cgError l $ bugZipType l t1 t2

