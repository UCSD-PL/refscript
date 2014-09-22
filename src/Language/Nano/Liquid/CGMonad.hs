{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE DoAndIfThenElse           #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances      #-}

-- | Operations pertaining to Constraint Generation

module Language.Nano.Liquid.CGMonad (
    
  -- * Constraint Generation Monad
    CGM

  -- * Constraint Information
  , CGInfo (..)

  -- * Execute Action and Get FInfo
  , getCGInfo 

  -- * Throw Errors
  , cgError      

  -- * Fresh Templates for Unknown Refinement Types 
  , freshTyFun, freshTyVar, freshTyInst, freshTyPhis
  , freshTyPhisWhile, freshTyObj, freshenCGEnvM

  -- * Freshable
  , Freshable (..), refreshValueVar

  -- * Environment API
  , envAddFresh, envAdds, envAddReturn, envAddGuard, envPopGuard, envFindTy, envFindTyWithAsgn
  , safeEnvFindTy, safeEnvFindTyWithAsgn
  , envFindReturn, envPushContext
  , envGetContextCast, envGetContextTypArgs

  -- * Add Subtyping Constraints
  , subType, wellFormed -- , safeExtends
  
  -- * Add Type Annotations
  , addAnnot

  -- * Function Types
  , cgFunTys, cgMethTys, cgCtorTys

  -- * Zip type wrapper
  , zipTypeM


  ) where

import           Control.Applicative 
import           Control.Exception (throw)
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Except

import           Data.Generics.Schemes
import           Data.Generics.Aliases

import           Data.Maybe                     (catMaybes, maybeToList)
import           Data.Monoid                    (mempty)
import qualified Data.HashMap.Strict            as M
import qualified Data.List                      as L
import           Data.Function                  (on)
import           Text.PrettyPrint.HughesPJ
import qualified Data.Traversable                   as T 
import           Language.Nano.Types
import           Language.Nano.Errors
import           Language.Nano.Annots
import qualified Language.Nano.Env              as E
import           Language.Nano.Locations
import           Language.Nano.Names
import           Language.Nano.CmdLine
import           Language.Nano.Program
import           Language.Nano.Typecheck.Resolve
import qualified Language.Nano.SystemUtils      as S
import           Language.Nano.Typecheck.Types
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Typecheck.Sub
import           Language.Nano.Liquid.Environment
import           Language.Nano.Liquid.Types
import           Language.Nano.Liquid.Qualifiers


import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Errors

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
  = case runState (runExceptT act) $ initState cfg pgm of 
      (Left err, _) -> throw err
      (Right x, st) -> (x, st)  


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

type CGM     = ExceptT Error (State CGState)

type TConInv = M.HashMap TCon (Located RefType)
 

---------------------------------------------------------------------------------------
cgError     :: Error -> CGM b 
---------------------------------------------------------------------------------------
cgError err = throwE $ catMessage err "CG-ERROR\n"

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
envGetContextTypArgs :: Int -> CGEnv -> AnnTypeR -> [TVar] -> [RefType]
---------------------------------------------------------------------------------------
-- NOTE: If we do not need to instantiate any type parameter (i.e. length αs ==
-- 0), DO NOT attempt to compare that with the TypInst that might hide within
-- the expression, cause those type instantiations might serve anothor reason
-- (i.e. might be there for a separate instantiation).  
envGetContextTypArgs _ _ _ []        = []
envGetContextTypArgs n g a αs
  = case [i | TypInst m ξ' i <- ann_fact a, ξ' == cge_ctx g, n == m ] of 
      [i] | length i == length αs -> i 
      _                           -> die $ bugMissingTypeArgs $ srcPos a


---------------------------------------------------------------------------------------
envAddFresh :: IsLocated l => l -> (RefType, Assignability) -> CGEnv -> CGM (Id AnnTypeR, CGEnv) 
---------------------------------------------------------------------------------------
envAddFresh l (t,a) g 
  = do x  <- freshId $ srcPos l
       g' <- envAdds "envAddFresh" [(x,(t,a))] g
       addAnnot (srcPos l) x t
       return (x, g')
   
freshId l = Id (Ann l []) <$> fresh


-- | We do not add binders for WriteGlobal variables as they cannot appear in
--   refinements.
--
---------------------------------------------------------------------------------------
envAdds :: (PP [x], PP x, F.Symbolic x, IsLocated x) 
        => String -> [(x, (RefType, Assignability))] -> CGEnv -> CGM CGEnv
---------------------------------------------------------------------------------------
envAdds _ xts' g
  = do xtas     <- zip xs . (`zip` as) <$> mapM addInvariant ts'
       is       <- catMaybes <$> forM xtas addFixpointBind
       _        <- forM xtas            $  \(x,(t,_)) -> addAnnot (srcPos x) x t
       return    $ g { cge_names        = E.envAdds xtas       $ cge_names g
                     , cge_fenv         = F.insertsIBindEnv is $ cge_fenv  g }
    where
       (xs,ys)    = unzip xts'
       (ts',as)   = unzip ys
       -- adjust m (k,v) = M.adjust (v:) (F.symbol k) m

instance PP F.IBindEnv where
  pp e = F.toFix e 

-- ReturnVar definitely does not need to be in the bindings
---------------------------------------------------------------------------------------
addFixpointBind :: (F.Symbolic x) => (x, (RefType, Assignability)) -> CGM (Maybe F.BindId)
---------------------------------------------------------------------------------------
addFixpointBind (_, (_, ReturnVar)) = return Nothing 
addFixpointBind (x, (t, _))
  = do (i, bs') <- F.insertBindEnv s r . binds <$> get 
       modify    $ \st -> st { binds = bs' }
       return    $ Just i
  where
       (s,r)     = (F.symbol x, rTypeSortedReft t)


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


-- ---------------------------------------------------------------------------------------
-- scrapeQualifiers   :: RefType -> CGM RefType 
-- ---------------------------------------------------------------------------------------
-- scrapeQualifiers t  = do 
--     modify $ \st -> st { quals = qs ++ quals st }
--     return t 
--    where
--      qs             = concatMap (refTypeQualifiers γ0) $ [t]
--      γ0             = E.envEmpty :: F.SEnv F.Sort 
-- 
-- 
-- refTypeQualifiers γ0 t = efoldRType rTypeSort addQs γ0 [] t 
--   where addQs γ t qs   = (mkQuals γ t) ++ qs
-- 
-- mkQuals γ t      = [ mkQual γ v so pa | let (F.RR so (F.Reft (v, ras))) = rTypeSortedReft t 
--                                       , F.RConc p                    <- ras                 
--                                       , pa                         <- atoms p
--                    ]
-- 
-- mkQual γ v so p = F.Q (F.symbol "Auto") [(v, so)] (F.subst θ p) l0
--   where 
--     θ             = F.mkSubst [(x, F.eVar y)   | (x, y) <- xys]
--     xys           = zipWith (\x i -> (x, F.symbol ("~A" ++ show (i :: Int)))) xs [0..] 
--     xs            = L.delete v $ orderedFreeVars γ p
--     l0            = F.dummyPos "RSC.CGMonad.mkQual"
-- 
-- orderedFreeVars γ = L.nub . filter (`F.memberSEnv` γ) . F.syms 
-- 
-- atoms (F.PAnd ps)   = concatMap atoms ps
-- atoms p           = [p]


---------------------------------------------------------------------------------------
addAnnot       :: (F.Symbolic x) => SourceSpan -> x -> RefType -> CGM () 
---------------------------------------------------------------------------------------
addAnnot l x t = modify $ \st -> st {cg_ann = S.addAnnot l x t (cg_ann st)}

---------------------------------------------------------------------------------------
envAddReturn        :: (IsLocated f)  => f -> RefType -> CGEnv -> CGM CGEnv 
---------------------------------------------------------------------------------------
envAddReturn f t g  = return $ g { cge_names = E.envAddReturn f (t, ReturnVar) (cge_names g) } 

---------------------------------------------------------------------------------------
envAddGuard       :: (F.Symbolic x, IsLocated x) => x -> Bool -> CGEnv -> CGEnv  
---------------------------------------------------------------------------------------
envAddGuard x b g = g { cge_guards = guard b x : cge_guards g }
  where 
    guard True    = F.eProp 
    guard False   = F.PNot . F.eProp

---------------------------------------------------------------------------------------
envPopGuard       :: CGEnv -> CGEnv  
---------------------------------------------------------------------------------------
envPopGuard g = g { cge_guards = grdPop $ cge_guards g } 
  where
    grdPop (_:xs) = xs
    grdPop []     = []


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
envFindTy x g = fst <$> envFindTyWithAsgn x g


-- Only include the "singleton" refinement in the case where Assignability is
-- either ReadOnly of WriteLocal (SSAed)
---------------------------------------------------------------------------------------
envFindTyWithAsgn :: (IsLocated x, F.Symbolic x, F.Expression x) 
                  => x -> CGEnv -> Maybe (RefType, Assignability)
---------------------------------------------------------------------------------------
envFindTyWithAsgn x = (eSngl <$>) . findT x
  where
    eSngl (t, WriteGlobal) = (t, WriteGlobal)
    eSngl (t, a)           = (t `eSingleton` x, a)
    findT x g = case E.envFindTy x $ cge_names g of 
                  Just t   -> Just t
                  Nothing  -> case cge_parent g of 
                                Just g' -> findT x g'
                                Nothing -> Nothing



---------------------------------------------------------------------------------------
safeEnvFindTy :: (IsLocated x, F.Symbolic x, F.Expression x, PP x) 
              => x -> CGEnv -> CGM RefType 
---------------------------------------------------------------------------------------
safeEnvFindTy x g = case envFindTy x g of
                        Just t  -> return t
                        Nothing ->  cgError $ bugEnvFindTy l x 
  where
    l = srcPos x

---------------------------------------------------------------------------------------
safeEnvFindTyWithAsgn :: (IsLocated x, F.Symbolic x, F.Expression x, PP x) 
                      => x -> CGEnv -> CGM (RefType, Assignability)
---------------------------------------------------------------------------------------
safeEnvFindTyWithAsgn x g = case envFindTyWithAsgn x g of
                        Just t  -> return t
                        Nothing ->  cgError $ bugEnvFindTy l x 
  where
    l = srcPos x




---------------------------------------------------------------------------------------
envFindReturn :: CGEnv -> RefType 
---------------------------------------------------------------------------------------
envFindReturn = fst . E.envFindReturn . cge_names


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
       g' <- envAdds "freshTyPhis" (zip xs (zip ts (repeat WriteLocal))) g
       _  <- mapM (wellFormed l g') ts
       return (g', ts)

---------------------------------------------------------------------------------------
freshTyPhisWhile :: (PP l, IsLocated l) => l -> CGEnv -> [Id l] -> [Type] -> CGM (CGEnv, [RefType])  
---------------------------------------------------------------------------------------
freshTyPhisWhile l g xs τs 
  = do ts <- mapM (freshTy "freshTyPhis")  τs
       g' <- envAdds "freshTyPhisWhile" (zip xs (zip ts (repeat WriteLocal))) g
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


-- | @freshenVarbindingM@ 
--
--    XXX: only ReadOnly vars are freshened
--
freshenVarbindingM _ (x,(v@(TVar{}),a)) = return (x,(v,a))
freshenVarbindingM g (x,(t,ReadOnly)  ) = (\t -> (x,(t,ReadOnly))) <$> freshTyVar g (srcPos x) t
freshenVarbindingM _ (x,(t,a)         ) = return (x,(t,a))

freshenModuleDefM g (a, m)  
  = do  vars     <- E.envFromList <$> mapM f (E.envToList $ m_variables m)
        -- types    <- E.envFromList <$> mapM h (E.envToList $ m_types m)        
        return (a,m { m_variables = vars }) -- , m_types = types })
  where
    f (x, (v, w, t)) =
      case w of
        ReadOnly   -> do  ft    <- freshTyVar g (srcPos x) t 
                          return   (x, (v, w, ft))
        _          -> return (x,(v,w,t))


---------------------------------------------------------------------------------------
-- | Adding Subtyping Constraints
---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------
subType :: (IsLocated l) => l -> Error -> CGEnv -> RefType -> RefType -> CGM ()
---------------------------------------------------------------------------------------
subType l err g t1 t2 =
  do t1'    <- addInvariant t1  -- enhance LHS with invariants
     let xs  =    [(symbolId l x,(t,a)) | (x, Just (t,a)) <- rNms t1' ++ rNms t2 ]
              ++  [(symbolId l x,(t,a)) | (x,      (t,a)) <- E.envToList $ cge_names g ]
     g'     <- envAdds "subtype" xs g
     modify  $ \st -> st {cs = c g' (t1', t2) : (cs st)}
  where
    c g      = uncurry $ Sub g (ci err l)
    rNms t   = mapSnd (`envFindTyWithAsgn` g) . dup <$> names t
    dup a    = (a,a)
    names    = foldReft rr []
    rr r xs  = F.syms r ++ xs


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
wellFormed l g t
  = do modify $ \st -> st { ws = (W g (ci err l) t) : ws st }
       return t
    where
       err = errorWellFormed (srcPos l)

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

-- | Refreshing "∀ A . t" causes all occurences of A in t to be instantiated
--   with the same fresh k-var.
--
--------------------------------------------------------------------------------
refreshRefType :: RefType -> CGM RefType
--------------------------------------------------------------------------------
refreshRefType (TAll α t) 
  = do  rt    <- refresh t
        tα    <- refresh (fTop :: F.Reft)        
        return $ TAll α $ trans tα rt
  where
      trans r = everywhere $ mkT $ tx r
      tx r (TVar β _) | α == β = TVar β r      
      tx _ t                   = t
refreshRefType t = mapReftM refresh t


--------------------------------------------------------------------------------
refreshValueVar :: RefType -> CGM RefType
--------------------------------------------------------------------------------
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
splitC (Sub g i tf1@(TFun s1 xt1s t1 _) tf2@(TFun s2 xt2s t2 _))
  = do bcs       <- bsplitC g i tf1 tf2
       g'        <- envTyAdds "splitC" i xt2s g 
       cs        <- splitOC g i s1 s2
       cs'       <- concatMapM splitC $ safeZipWith "splitc-1" (Sub g' i) t2s t1s' 
       cs''      <- splitC $ Sub g' i (F.subst su t1) t2      
       return     $ bcs ++ cs ++ cs' ++ cs''
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
  = splitIncompatC l g i t1 t2 where l = srcPos i

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
splitC (Sub g i t1@(TApp (TRef x1) (m1:t1s) _) t2@(TApp (TRef x2) (m2:t2s) _)) 
  --
  -- * Incompatible mutabilities
  --
  | not (isSubtype g m1 m2) 
  = splitIncompatC l g i t1 t2
  --  
  -- * Both immutable, same name, non arrays: Co-variant subtyping
  --
  | x1 == x2 && isImmutable m2 && not (isArr t1) 
  = do  cs    <- bsplitC g i t1 t2
        cs'   <- concatMapM splitC $ safeZipWith "splitc-4" (Sub g i) t1s t2s
        return $ cs ++ cs'
  -- 
  -- * Non-immutable, same name: invariance
  --
  | x1 == x2 
  = do  cs    <- bsplitC g i t1 t2
        cs'   <- concatMapM splitC $ safeZipWith "splitc-5" (Sub g i) t1s t2s
        cs''  <- concatMapM splitC $ safeZipWith "splitc-6" (Sub g i) t2s t1s
        return $ cs ++ cs' ++ cs''
  | otherwise 
  = splitIncompatC l g i t1 t2

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
  | otherwise = splitIncompatC l g i t1 t2 where l = srcPos i

-- | These need to be here due to the lack of a folding operation
--
splitC (Sub g i t1@(TApp (TRef _) _ _) t2) = 
  case flattenType g t1 of
    Just t1' -> splitC (Sub g i t1' t2)
    Nothing  -> cgError $ errorUnfoldType l t1 where l = srcPos i

splitC (Sub g i t1 t2@(TApp (TRef _) _ _)) = 
  case flattenType g t2 of
    Just t2' -> splitC (Sub g i t1 t2')
    Nothing  -> cgError $ errorUnfoldType l t2 where l = srcPos i

-- | TCons
--
splitC (Sub g i t1@(TCons e1s μ1 _ ) t2@(TCons e2s μ2 _ ))
  = do  cs    <- bsplitC g i t1 t2
        cs'   <- splitEs g i μ1 μ2 e1s e2s
        return $ cs ++ cs'

splitC (Sub _ _ (TClass _) (TClass _)) = return []
splitC (Sub _ _ (TModule _) (TModule _)) = return []
  
splitC x@(Sub g i t1 t2)
  = splitIncompatC l g i t1 t2 where l = srcPos x

splitOC g i (Just t) (Just t') = splitC (Sub g i t t') 
splitOC _ _ _        _         = return []

-- splitIncompatC :: SourceSpan -> RefType -> RefType -> a
splitIncompatC _ g i t1 _ = bsplitC g i t1 (mkBot t1)
  
-- splitIncompatC l t1 t2 = cgError l $ bugBadSubtypes l t1 t2
    
mkBot   :: (F.Reftable r) => RType r -> RType r
mkBot t = setRTypeR t (F.bot r) where r = rTypeR t

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
  = cgError $ bugMalignedFields l e1s e2s 
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
splitW (W g i ft@(TFun s ts t _)) 
  = do let bws = bsplitW g ft i
       g'     <- envTyAdds "splitW" i ts g 
       ws     <- concatMapM splitW (W g' i <$> maybeToList s)
       ws'    <- concatMapM splitW [W g' i ti | B _ ti <- ts]
       ws''   <-            splitW (W g' i t)
       return  $ bws ++ ws ++ ws' ++ ws''

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

splitW (W _ _ (TClass _ ))
  = return []

splitW (W _ _ (TModule _ ))
  = return []

splitW (W _ _ t) = error $ render $ text "Not supported in splitW: " <+> pp t

bsplitW g t i 
  | F.isNonTrivialSortedReft r'
  = [F.wfC (cge_fenv g) r' Nothing i] 
  | otherwise
  = []
  where r' = rTypeSortedReft t

envTyAdds msg l xts = envAdds (msg ++  " - envTyAdds " ++ ppshow (srcPos l)) [(symbolId l x,(t,WriteLocal)) | B x t <- xts]


------------------------------------------------------------------------------
cgFunTys :: (F.Symbolic s, F.Reftable r, PP r, PP a) =>
  AnnSSA r -> a -> [s] -> RType r -> CGM [(Int, ([TVar], Maybe (RType r), [RType r], RType r))]
------------------------------------------------------------------------------
cgFunTys l f xs ft = either cgError return $ funTys l f xs ft 

------------------------------------------------------------------------------
cgMethTys :: (PP a) => AnnTypeR -> a -> (Mutability, RefType)
                    -> CGM [(Int, Mutability, ([TVar], Maybe RefType, [RefType], RefType))]
------------------------------------------------------------------------------
cgMethTys l f (m,t) 
   = zip3 [0..] (repeat m) <$> mapM (methTys l f) (bkAnd t)

methTys l f ft0
  = case remThisBinding ft0 of
      Nothing          -> cgError $ errorNonFunction (srcPos l) f ft0 
      Just (vs,s,bs,t) -> return  $ (vs,s,b_type <$> bs,t)

------------------------------------------------------------------------------
cgCtorTys :: (PP a) => AnnTypeR -> a -> RefType
                    -> CGM [(Int, ([TVar], Maybe RefType, [RefType], RefType))]
------------------------------------------------------------------------------
cgCtorTys l f t = zip [0..] <$> mapM (methTys l f) (bkAnd t)


--------------------------------------------------------------------------------
-- | zipType wrapper

zipTypeM l g t1 t2 = 
  case zipType g t1 t2 of
    Just t  -> return t
    Nothing -> cgError $ bugZipType (srcPos l) t1 t2

-- Local Variables:
-- flycheck-disabled-checkers: (haskell-liquid)
-- End:
