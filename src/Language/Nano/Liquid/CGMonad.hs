{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ViewPatterns              #-}
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
  , freshTyFun, freshenType, freshTyInst, freshTyPhis, freshTyPhis'
  , freshTyObj, freshenCGEnvM

  -- * Freshable
  , Freshable (..)

  -- * Environment API
  , envAddFresh, envAdds, envAdd
  , envAddReturn, envAddGuard, envPopGuard
  , envFindTy, envFindTyWithAsgn, envFindTyForAsgn
  , safeEnvFindTy, safeEnvFindTyWithAsgn, safeEnvFindTyNoSngl
  , envFindReturn, envPushContext
  , envGetContextCast, envGetContextTypArgs

  -- , reftCheck

  -- * Add Subtyping Constraints
  , subType, wellFormed -- , safeExtends
  
  -- * Add Type Annotations
  , addAnnot

  -- * Function Types
  , cgFunTys, cgMethTys, splitCtorTys

  -- * Zip type wrapper
  , zipTypeUpM, zipTypeDownM

  ) where

import           Control.Applicative 
import           Control.Exception (throw)
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Except

import           Data.Maybe                     (catMaybes, maybeToList)
import           Data.Monoid                    (mempty)
import qualified Data.HashMap.Strict            as HM
import qualified Data.Map.Strict                as M
import qualified Data.List                      as L
import           Data.Function                  (on)
import           Text.PrettyPrint.HughesPJ
import           Language.Nano.Types
import           Language.Nano.Errors
import           Language.Nano.Annots
import qualified Language.Nano.Env              as E
import           Language.Nano.Locations
import           Language.Nano.Environment
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

import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Errors

import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.PrettyPrint

import           Debug.Trace                        (trace)

-------------------------------------------------------------------------------
-- | Top level type returned after Constraint Generation
-------------------------------------------------------------------------------

data CGInfo = CGI { cgi_finfo :: F.FInfo Cinfo
                  , cgi_annot :: S.UAnnInfo RefType  
                  }

-- Dump the refinement subtyping constraints
instance PP CGInfo where
  pp (CGI finfo _) = cat (map pp (HM.elems $ F.cm finfo))

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
initState c p   = CGS F.emptyBindEnv [] [] 0 mempty invs c (max_id p)
  where 
    invs        = HM.fromList [(tc, t) | t@(Loc _ (TApp tc _ _)) <- invts p]


-------------------------------------------------------------------------------
cgStateCInfo :: NanoRefType -> (([F.SubC Cinfo], [F.WfC Cinfo]), CGState) -> CGInfo
-------------------------------------------------------------------------------
cgStateCInfo pgm ((fcs, fws), cg) = CGI (patchSymLits fi) (cg_ann cg)
  where 
    fi   = F.FI { F.cm    = HM.fromList $ F.addIds fcs  
                , F.ws    = fws
                , F.bs    = binds cg
                , F.gs    = measureEnv pgm
                , F.lits  = []
                , F.kuts  = F.ksEmpty
                , F.quals = pQuals pgm
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
    binds       :: F.BindEnv
  -- 
  -- ^ subtyping constraints
  --
  , cs          :: ![SubC]
  -- 
  -- ^ well-formedness constraints
  --
  , ws          :: ![WfC]               
  -- 
  -- ^ freshness counter
  --
  , cg_cnt      :: !Integer             
  -- 
  -- ^ recorded annotations
  --
  , cg_ann      :: S.UAnnInfo RefType   
  -- 
  -- ^ type constructor invariants
  --
  , invs        :: TConInv
  -- 
  -- ^ configuration options
  --
  , cg_opts     :: Config               
  -- 
  -- ^ AST Counter
  --
  , cg_ast_cnt  :: NodeId

  }

type CGM     = ExceptT Error (State CGState)

type TConInv = HM.HashMap TCon (Located RefType)
 

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
      cs  -> case L.find isDeadCast cs of 
               Just dc -> dc 
               Nothing -> die $ errorMultipleCasts (srcPos a) cs
  where
    isDeadCast CDead{} = True
    isDeadCast _       = False



---------------------------------------------------------------------------------------
envGetContextTypArgs :: Int -> CGEnv -> AnnTypeR -> [TVar] -> [RefType]
---------------------------------------------------------------------------------------
-- NOTE: If we do not need to instantiate any type parameter (i.e. length αs ==
-- 0), DO NOT attempt to compare that with the TypInst that might hide within
-- the expression, cause those type instantiations might serve anothor reason
-- (i.e. might be there for a separate instantiation).  
envGetContextTypArgs _ _ _ [] = []
envGetContextTypArgs n g a αs
  = case tys of
      [i] | length i == length αs -> i 
      _                           -> die $ bugMissingTypeArgs $ srcPos a
  where
    tys = [i | TypInst m ξ' i <- ann_fact a
             , ξ' == cge_ctx g
             , n == m ] 


---------------------------------------------------------------------------------------
envAddFresh :: IsLocated l 
            => l 
            -> (RefType, Assignability, Initialization) 
            -> CGEnv 
            -> CGM (Id AnnTypeR, CGEnv) 
---------------------------------------------------------------------------------------
envAddFresh l (t,a,i) g 
  = do x  <- freshId l
       g' <- envAdds "envAddFresh" [(x,(t,a,i))] g
       addAnnot l x t
       return (x, g')
   
freshId a = Id <$> freshenAnn a <*> fresh

freshenAnn :: IsLocated l => l -> CGM AnnTypeR
freshenAnn l
  = do n     <- cg_ast_cnt <$> get 
       modify $ \st -> st {cg_ast_cnt = 1 + n}
       return $ Ann n (srcPos l) []


envAdds = envAdds' True True



-- | No binders for WriteGlobal variables as they cannot appear in refinements.
--
--   doFields : add bindings for fields (True/False)
--
---------------------------------------------------------------------------------------
envAdds' :: (IsLocated x, F.Symbolic x, PP x, PP [x]) 
         => Bool
         -> Bool
         -> String 
         -> [(x, (RefType,Assignability,Initialization))] 
         -> CGEnv 
         -> CGM CGEnv
---------------------------------------------------------------------------------------
envAdds' _ _ _ [] g 
  = return g 
envAdds' doChecks doFields msg xts g
  = do  -- 1. Check for unbound symbols in refinement
        -- 
        --    Assuming that all @xts@ will be added to the env, we make 
        --    all binders available to be referenced by all types.
        -- 
        when doChecks   $ zipWithM_ (unboundSyms msg g xss) xs ts 

        -- 2. Add invariants
        --
        ts'            <- zipWithM inv ts is
        let xtas        = zip xs $ zip3 ts' as is

        -- 3. Add object field binders 
        --
        g'             <- foldM (addObjectFields doChecks doFields) g $ zip3 xs as ts'
          
        -- 4. Add fixpoint binds 
        --
        is             <- catMaybes <$> forM xtas addFixpointBind

        -- 5. Update environment @g@
        --
        return          $ g' { cge_names = E.envAdds xtas       $ cge_names g'
                             , cge_fenv  = F.insertsIBindEnv is $ cge_fenv  g' }
  where
    (xs,(ts,as,is))     = mapSnd unzip3 $ unzip xts
    xss                 = map F.symbol xs

    inv t Initialized   = addInvariant g t
    inv t _             = return t


unboundSyms m g ok x t  | not $ null uSyms
                        = cgError $ errorUnboundSyms (srcPos x) x t uSyms m
                        | otherwise 
                        = return ()
  where
    uSyms               =  efoldRType h f F.emptySEnv [] t
    
    h _                 = ()
    f γ t' s            = s ++ filter (not . isBound γ t') (F.syms $ noKVars $ rTypeReft t')
    isBound γ t' s      = s == x_sym 
                       || s == rTypeValueVar t'
                       || s `L.elem` ok
                       || s `F.memberSEnv` γ
                       || s `F.memberSEnv` cge_consts g
                       || s `envLikeMember` g 
                       -- TODO: make sure the referenced variable is READONLY
                       --       i.e. what reftCheck does
                       || s `L.elem` biExtra
    biExtra             = F.symbol <$> ["bvor", "bvand", "builtin_BINumArgs"]
    x_sym               = F.symbol x


-- reftCheck msg g ok bad x t  | Just its <- bkFuns t
--                             = mapM_ (reftCheckFun msg g ok bad x) its
--                             | otherwise 
--                             = reftCheck' msg g x t ok bad
-- 
-- 
-- reftCheckFun msg g ok bad x (_,s,bs,t)     
--                             = mapM_ (reftCheck msg g (ok `HS.union` ss) bad x) ts
--   where
--     (ss,ts)                 = (HS.fromList $ map b_sym bs, maybeToList s ++ map b_type bs ++ [t])
-- 
-- 
-- reftCheck' msg g x t ok bad | HS.null $ forbiddenSet `HS.difference` (x_sym `HS.insert` ok)
--                             = return ()
--                             | otherwise
--                             = cgError $ errorForbiddenSyms (srcPos x) t $ HS.toList forbiddenSet
--   where
--     x_sym                   = F.symbol x
-- 
--     allSyms                 = [ s | s <- foldReft rr [] t ]
-- 
--     forbiddenSyms           = [ s | s <- allSyms
--                                   , (_,a,_) <- maybeToList $ envFindTyWithAsgn s g
--                                   , a `elem` [ReturnVar,WriteGlobal] ]
--                            ++ [ s | s <- allSyms, s `HS.member` bad ]
--     forbiddenSet            = HS.fromList forbiddenSyms
-- 
--     rr (F.Reft (_, ras)) xs = concatMap ra ras ++ xs
--     ra (F.RConc p)          = F.syms p
--     ra (F.RKvar _ su)       = F.targetSubstSyms su


-- | `addObjectFieldsWithOK g (x,t)` when introducing an object @x@ in environment @g@
--   also introduce bindings for all its IMMUTABLE fields. 
---------------------------------------------------------------------------------------
addObjectFields :: (IsLocated x, F.Symbolic x, PP x, PP [x]) 
                      => Bool
                      -> Bool
                      -> CGEnv 
                      -> (x,Assignability,RefType) 
                      -> CGM CGEnv
---------------------------------------------------------------------------------------
addObjectFields chk False g _ = return g
addObjectFields chk True  g (x,a,t) 
              | a `elem` [WriteGlobal,ReturnVar] 
              = return g
              | otherwise
              = envAdds' chk False "addObjectFieldsWithOK" xts g
  where
    xts       = [(fd f, ty o tf) | FieldSig f o m tf <- ms, isImmutable m ]

    fd        = mkFieldB x 

    ty o tf   | optionalFieldType o 
              = (orUndef $ F.subst (substFieldSyms g x t) tf, a, Initialized)
              | otherwise
              = (F.subst (substFieldSyms g x t) tf, a, Initialized)

    ms        | Just (TCons m ms _) <- expandType g t = defMut m <$> M.elems ms
              | otherwise                              = []

    defMut m (FieldSig f o m0 t) 
              | isInheritedMutability m0      -- FIXME: this shouldn't need to happen here  
              = FieldSig f o m  t
              | otherwise                
              = FieldSig f o m0 t

    defMut _ f = f 


envAdd x t g = envAdds "envAdd" [(x,t)] g


instance PP F.IBindEnv where
  pp e = F.toFix e 


---------------------------------------------------------------------------------------
addFixpointBind :: (F.Symbolic x) 
                => (x, (RefType, Assignability, Initialization)) -> CGM (Maybe F.BindId)
---------------------------------------------------------------------------------------
-- No binding for globals: they shouldn't appear in refinements
addFixpointBind (_, (_, WriteGlobal,_)) = return Nothing    

-- No binding for return-val: it shouldn't appear in refinements
addFixpointBind (_, (_, ReturnVar,_)) = return Nothing 

addFixpointBind (x, (t, _, _))
  = do (i, bs') <- F.insertBindEnv s r . binds <$> get 
       modify    $ \st -> st { binds = bs' }
       return    $ Just i
  where
       (s,r)     = (F.symbol x, rTypeSortedReft t)


---------------------------------------------------------------------------------------
addInvariant :: CGEnv -> RefType -> CGM RefType
---------------------------------------------------------------------------------------
addInvariant g t             
  = do  extraInvariants <- extraInvs <$> cg_opts <$> get
        if extraInvariants then (hasProp . hierarchy . truthy . typeof t . invs) <$> get
                           else (          hierarchy . truthy . typeof t . invs) <$> get
  where
    -- | typeof 
    typeof t@(TApp tc _ o)  i = maybe t (strengthenOp t o . rTypeReft . val) $ HM.lookup tc i
    typeof t@(TRef _ _ _) _   = t `strengthen` F.Reft ((vv t,) [ F.RConc $ typeofExpr $ F.symbol "object" ])
    typeof   (TFun a b c _) _ = TFun a b c typeofReft
    typeof t                _ = t
    -- | Truthy
    truthy t                  | isTObj t
                              = t `strengthen` F.Reft ((vv t,) [ F.RConc $ F.eProp $ vv t ])
                              | otherwise          = t

    strengthenOp t o r        | L.elem r (ofRef o) = t
    strengthenOp t _ r        | otherwise          = t `strengthen` r
    typeofReft                = F.Reft $ (vv t,) [ F.RConc $ typeofExpr $ F.symbol "function"
                                                 , F.RConc $ F.eProp    $ vv t                ]
    typeofExpr s              = F.PAtom F.Eq (F.EApp (F.dummyLoc (F.symbol "ttag")) [F.eVar $ vv t]) 
                                             (F.expr $ F.symbolText s)

    ofRef (F.Reft (s, as))    = (F.Reft . (s,) . single) <$> as

    -- | { f: T } --> hasProperty("f", v)
    hasProp t                   = t `strengthen` keyReft (boundKeys g t) 

    keyReft                   = F.Reft . (vv t,) . (F.RConc . F.PBexp . hasPropExpr <$>) 
    hasPropExpr s             = F.EApp (F.dummyLoc (F.symbol "hasProperty")) 
                                [F.expr (F.symbolText s), F.eVar $ vv t]

    -- | extends class / interface
    hierarchy t@(TRef c _ _)  | isClassType g t 
                              = t `strengthen` rExtClass t (name <$> classAncestors g c)
                              | otherwise
                              = t `strengthen` rExtIface t (name <$> interfaceAncestors g c)
    hierarchy t               = t

    name (QN AK_ _ _ s)       = s

    rExtClass t cs            = F.Reft (vv t, refa t "extends_class"     <$> cs)
    rExtIface t cs            = F.Reft (vv t, refa t "extends_interface" <$> cs)

    refa t s c                = F.RConc $ F.PBexp $ F.EApp (sym s) 
                              $ [ F.expr $ rTypeValueVar t, F.expr $ F.symbolText c ]
    vv                        = rTypeValueVar
    sym s                     = F.dummyLoc $ F.symbol s


---------------------------------------------------------------------------------------
addAnnot       :: (IsLocated l, F.Symbolic x) => l -> x -> RefType -> CGM () 
---------------------------------------------------------------------------------------
addAnnot l x t = modify $ \st -> st {cg_ann = S.addAnnot (srcPos l) x t (cg_ann st)}

---------------------------------------------------------------------------------------
envAddReturn        :: (IsLocated f)  => f -> RefType -> CGEnv -> CGM CGEnv 
---------------------------------------------------------------------------------------
envAddReturn f t g  = return $ g { cge_names = E.envAddReturn f (t, ReturnVar, Initialized) (cge_names g) } 

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
envFindTy x g = fst3 <$> envFindTyWithAsgn x g

envFindTyNoSngl x g = fst3 <$> envFindTyWithAsgnNoSngl x g


-- Only include the "singleton" refinement in the case where Assignability is
-- either ReadOnly of WriteLocal (SSAed)
---------------------------------------------------------------------------------------
envFindTyWithAsgn :: (IsLocated x, F.Symbolic x, F.Expression x) 
                  => x -> CGEnv -> Maybe (RefType, Assignability, Initialization)
---------------------------------------------------------------------------------------
envFindTyWithAsgn x = (eSngl <$>) . findT x
  where
    eSngl (t, WriteGlobal,i) = adjustInit (t, WriteGlobal,i)
    eSngl (t, a,i)           = (t `eSingleton` x, a,i)
    findT x g = case E.envFindTy x $ cge_names g of 
                  Just t   -> Just t
                  Nothing  -> case cge_parent g of 
                                Just g' -> findT x g'
                                Nothing -> Nothing
    adjustInit s@(_, _, Initialized) = s
    adjustInit (t, a, _ ) = (orUndef t, a, Uninitialized)

---------------------------------------------------------------------------------------
envFindTyWithAsgnNoSngl :: (IsLocated x, F.Symbolic x, F.Expression x) 
                        => x -> CGEnv -> Maybe (RefType, Assignability, Initialization)
---------------------------------------------------------------------------------------
envFindTyWithAsgnNoSngl x = findT x
  where
    findT x g = case E.envFindTy x $ cge_names g of 
                  Just t   -> Just t
                  Nothing  -> case cge_parent g of 
                                Just g' -> findT x g'
                                Nothing -> Nothing

---------------------------------------------------------------------------------------
envFindTyForAsgn :: (IsLocated x, F.Symbolic x, F.Expression x) 
                 => x -> CGEnv -> Maybe (RefType, Assignability, Initialization)
---------------------------------------------------------------------------------------
envFindTyForAsgn x = (eSngl <$>) . findT x
  where
    eSngl (t, WriteGlobal,i) = (t, WriteGlobal,i)
    eSngl (t, a,i)           = (t `eSingleton` x, a,i)
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
safeEnvFindTyNoSngl :: (IsLocated x, F.Symbolic x, F.Expression x, PP x) 
                    => x -> CGEnv -> CGM RefType 
---------------------------------------------------------------------------------------
safeEnvFindTyNoSngl x g = case envFindTyNoSngl x g of
                            Just t  -> return t
                            Nothing ->  cgError $ bugEnvFindTy l x 
  where
    l = srcPos x

---------------------------------------------------------------------------------------
safeEnvFindTyWithAsgn :: (IsLocated x, F.Symbolic x, F.Expression x, PP x) 
                      => x -> CGEnv -> CGM (RefType, Assignability, Initialization)
---------------------------------------------------------------------------------------
safeEnvFindTyWithAsgn x g = case envFindTyWithAsgn x g of
                        Just t  -> return t
                        Nothing ->  cgError $ bugEnvFindTy l x 
  where
    l = srcPos x

---------------------------------------------------------------------------------------
envFindReturn :: CGEnv -> RefType 
---------------------------------------------------------------------------------------
envFindReturn = fst3 . E.envFindReturn . cge_names


---------------------------------------------------------------------------------------
-- | Fresh Templates
---------------------------------------------------------------------------------------

-- | Instantiate Fresh Type (at Function-site)
--
-- XXX: Removing addInvariant
--
--
---------------------------------------------------------------------------------------
freshTyFun :: (IsLocated l) => CGEnv -> l -> RefType -> CGM RefType 
---------------------------------------------------------------------------------------
freshTyFun g l t
  | not (isTFun t)     = return t
  | isTrivialRefType t = freshTy "freshTyFun" (toType t) >>= {- addInvariant g >>= -} wellFormed l g
  | otherwise          = return t


freshenType WriteGlobal g l t 
  | isTrivialRefType t = freshTy "freshenType-WG" (toType t) >>= wellFormed l g
  | otherwise          = return t

freshenType _ g l t 
  | not (isTFun t)     = return t   
  | isTrivialRefType t = freshTy "freshenType-RO" (toType t) >>= wellFormed l g
  | otherwise          = return t


-- | Instantiate Fresh Type (at Call-site)
freshTyInst l g αs τs tbody
  = do ts    <- mapM (freshTy "freshTyInst") τs
       _     <- mapM (wellFormed l g) ts
       return $ apply (fromList $ zip αs ts) tbody

-- | Instantiate Fresh Type (at Phi-site) 
---------------------------------------------------------------------------------------
freshTyPhis :: AnnTypeR -> CGEnv -> [Id AnnTypeR] -> [Type] -> CGM (CGEnv, [RefType])  
---------------------------------------------------------------------------------------
freshTyPhis l g xs τs 
  = do ts <- mapM (freshTy "freshTyPhis") τs
       g' <- envAdds "freshTyPhis" (zip xs ((,WriteLocal,Initialized) <$> ts)) g
       _  <- mapM (wellFormed l g') ts
       return (g', ts)


-- | Instantiate Fresh Type (at Phi-site) 
---------------------------------------------------------------------------------------
-- freshTyPhis' :: AnnTypeR -> CGEnv -> [Id AnnTypeR] 
--              -> [(Type, Assignability, Initialization)] -> CGM (CGEnv, [RefType])  
---------------------------------------------------------------------------------------
freshTyPhis' l g xs ts
  = do ts <- mapM (freshTy "freshTyPhis")  τs
       g' <- envAdds "freshTyPhis" (zip xs (zip3 ts as is)) g
       _  <- mapM (wellFormed l g') ts
       return (g', ts)
  where
    (τs,as,is) = unzip3 ts 


-- | Fresh Object Type
---------------------------------------------------------------------------------------
freshTyObj :: (IsLocated l) => l -> CGEnv -> RefType -> CGM RefType 
---------------------------------------------------------------------------------------
freshTyObj l g t = freshTy "freshTyArr" t >>= wellFormed l g 


---------------------------------------------------------------------------------------
freshenCGEnvM :: CGEnv -> CGM CGEnv
---------------------------------------------------------------------------------------
freshenCGEnvM g  
  = do  names   <- E.envFromList  <$> mapM (go g) (E.envToList  $ cge_names g)
        modules <- E.qenvFromList <$> mapM (freshenModuleDefM g) (E.qenvToList $ cge_mod g)
        return $ g { cge_names = names, cge_mod = modules } 
  where
    go _ (x, (v@(TVar{}),a,i)) = return (x,(v,a,i))
    go g (x, (t,ReadOnly,i)  ) = (x,) . (,ReadOnly,i) <$> freshenType ReadOnly g (srcPos x) t
    go _ (x, (t,a,i)         ) = return (x,(t,a,i))

freshenModuleDefM g (a, m)
  = do  vars     <- E.envFromList <$> mapM f (E.envToList $ m_variables m)
        types    <- E.envFromList <$> mapM h (E.envToList $ m_types m)        
        return (a,m { m_variables = vars, m_types = types })
  where
    f (x, (v,w,t,i)) =
      case w of
        ReadOnly   -> do  ft    <- freshenType ReadOnly g x t 
                          return   (x, (v, w, ft,i))
        _          -> return (x,(v,w,t,i))

    -- KVar class definitions only
    h (x, t) | t_class t == ClassKind 
             = do es <- M.fromList <$> mapM (freshElt x) (M.toList $ t_elts t)
                  return (x, t { t_elts = es })
             | otherwise
             = return (x,t)
    freshElt x (s,b) = (s,) <$> mapEltM (freshTyFun g x) b


---------------------------------------------------------------------------------------
-- | Adding Subtyping Constraints
---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------
subType :: AnnTypeR -> Error -> CGEnv -> RefType -> RefType -> CGM ()
---------------------------------------------------------------------------------------
subType l err g t1 t2 =
  do  t1'    <- addInvariant g t1  -- enhance LHS with invariants
      let xs  = [(symbolId l x,(t,a,i)) | (x, Just (t,a,i)) <- rNms t1' ++ rNms t2 ]
      let ys  = [(symbolId l x,(t,a,i)) | (x,      (t,a,i)) <- E.envToList $ cgeAllNames g ]
      ----  when (toType t1 /= toType t2) (errorstar (ppshow t1 ++ " VS " ++ ppshow t2))
      -- g'     <- envAdds "subtype" (trace (ppshow (srcPos l) ++ ppshow "\nLHS: " ++ ppshow t1 ++ "\nRHS: " ++ ppshow t2 ++ "\n") $ xs ++ ys) g
      g'     <- envAdds "subtype" (xs ++ ys) g
      modify  $ \st -> st {cs = c g' (t1', t2) : (cs st)}
  where
    c g      = uncurry $ Sub g (ci err l)
    rNms t   = mapSnd (`envFindTyWithAsgn` g) . dup <$> names t
    dup a    = (a,a)
    names    = foldReft rr []
    rr r xs  = F.syms r ++ xs


cgeAllNames g@(CGE { cge_parent = Just g' }) = cgeAllNames g' `E.envUnion` cge_names g 
cgeAllNames g@(CGE { cge_parent = Nothing }) = cge_names g


-- BUGGY !! -- 
-- BUGGY !! -- XXX: Removing addInvariant
-- BUGGY !! -- 
-- BUGGY !! ---------------------------------------------------------------------------------------
-- BUGGY !! subType :: AnnTypeR -> Error -> CGEnv -> RefType -> RefType -> CGM ()
-- BUGGY !! ---------------------------------------------------------------------------------------
-- BUGGY !! subType l err g t1 t2 =
-- BUGGY !!   do  -- t1'        <- addInvariant g t1  -- enhance LHS with invariants
-- BUGGY !!       t1'        <- return t1
-- BUGGY !!       g'         <- envAdds "subtype" (goT HM.empty [t1',t2] ++ 
-- BUGGY !!                                        goP HM.empty (cge_guards g)) g
-- BUGGY !!       modify      $ \st -> st { cs = Sub g' (ci err l) t1' t2 : cs st }
-- BUGGY !!   where
-- BUGGY !!     goP m         = goT m . catMaybes . map (`envFindTy` g) . concatMap F.syms
-- BUGGY !! 
-- BUGGY !!     goT m []      = [(symbolId l x,t) | (x,t) <- HM.toList m ]
-- BUGGY !!     goT m ts      = goT (m `HM.union` HM.fromList ps) ts'
-- BUGGY !!       where
-- BUGGY !!         (ps, ts') = unzip [ (p,t') | t              <- ts
-- BUGGY !!                                    , p@(x,(t',_,_)) <- foldT t 
-- BUGGY !!                                    , isNothing       $ HM.lookup x m ]
-- BUGGY !! 
-- BUGGY !!     foldT t       = [ (n,s) | n <- foldReft rr [] t
-- BUGGY !!                             , s <- maybeToList $ envFindTyWithAsgn n g ]
-- BUGGY !! 
-- BUGGY !!     rr r xs       = F.syms r ++ xs



-- FIXME: Restore this check !!!
-- ---------------------------------------------------------------------------------------
-- safeExtends :: SourceSpan -> CGEnv -> IfaceDef F.Reft -> CGM ()
-- ---------------------------------------------------------------------------------------
-- safeExtends l g (ID _ _ _ (Just (p, ts)) es) = zipWithM_ sub t1s t2s
--   where
--     sub t1 t2  = subType l g (zipType δ t1 t2) t2
--     (t1s, t2s) = unzip [ (t1,t2) | pe <- expand True δ (findSymOrDie p δ, ts)
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
  fresh = do modify $ \st -> st { cg_cnt = 1 + (cg_cnt st) }
             cg_cnt <$> get 

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
  refresh = mapReftM refresh
  true    = trueRefType 

trueRefType    :: RefType -> CGM RefType
trueRefType    = mapReftM true


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
       g'        <- envTyAdds "splitC" i (thisB ++ xt2s) g 
       cs        <- splitOC g i s1 s2
       cs'       <- concatMapM splitC $ zipWith (Sub g' i) t2s t1s' 
       cs''      <- splitC $ Sub g' i (F.subst su t1) t2      
       return     $ bcs ++ cs ++ cs' ++ cs''
    where  
       thisB      = B (F.symbol "this") <$> maybeToList s2
       t2s        = b_type <$> xt2s
       t1s'       = F.subst su (b_type <$> xt1s)
       su         = F.mkSubst $ zipWith bSub xt1s xt2s
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
splitC (Sub g i t1@(TApp TUn t1s r1) t2@(TApp TUn t2s _))
  | F.isFalse (F.simplify r1)
  = return []
  | otherwise
  = (++) <$> bsplitC g i t1 t2 
         <*> concatMapM splitC (safeZipWith "splitc-3" (Sub g i) s1s s2s)
    where 
       s1s = L.sortBy (compare `on` toType) t1s
       s2s = L.sortBy (compare `on` toType) t2s

-- |Type references
--  
--  FIXME: restore co/contra-variance 
--
splitC (Sub g i t1@(TRef x1 (m1:t1s) r1) t2@(TRef x2 (m2:t2s) r2))
  -- 
  -- * Trivial case (do not even descend) 
  --
  | F.isFalse (F.simplify r1)
  = return []
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
  | x1 == x2 && not (F.isFalse r2)
  = do  cs    <- bsplitC g i t1 t2
        cs'   <- concatMapM splitC $ safeZipWith "splitc-5" (Sub g i) t1s t2s
        cs''  <- concatMapM splitC $ safeZipWith "splitc-6" (Sub g i) t2s t1s
        return $ cs ++ cs' ++ cs''

  | x1 == x2
  = bsplitC g i t1 t2

  | otherwise 
  = splitIncompatC l g i t1 t2

  where
    l = srcPos i

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
splitC (Sub g i t1@(TRef _ _ _) t2) = 
  case expandType g t1 of
    Just t1' -> splitC (Sub g i t1' t2)
    Nothing  -> cgError $ errorUnfoldType l t1 where l = srcPos i

splitC (Sub g i t1 t2@(TRef _ _ _)) = 
  case expandType g t2 of
    Just t2' -> splitC (Sub g i t1 t2')
    Nothing  -> cgError $ errorUnfoldType l t2 where l = srcPos i

-- | TCons
--
splitC (Sub g i t1@(TCons _ e1s r1) t2@(TCons _ e2s _))
  | F.isFalse (F.simplify r1)
  = return []
  | otherwise
  = do  cs    <- bsplitC g i t1 t2
        cs'   <- splitEs g i e1s e2s
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

-- 
--  Gather the types of the LHS IMMUTABLE fields and add them to the
--  environment with the relevant field symbol as binder
--
--  FIXME: 
--
--  * Add special cases: IndexSig ...
--
---------------------------------------------------------------------------------------
splitEs :: CGEnv 
        -> Cinfo 
        -> TypeMembers F.Reft -> TypeMembers F.Reft 
        -> CGM [FixSubC]
---------------------------------------------------------------------------------------
splitEs g i e1s e2s 
  = do  g' <- envTyAdds "splitEs" i imms g
        concatMapM (uncurry $ splitE g' i) es 
  where
    es     = M.elems $ M.intersectionWith (,) e1s e2s
    imms   = [ B f t | (FieldSig f _ m t,_) <- es, isImmutable m ]



splitE g i (CallSig t1) (CallSig t2) = splitC (Sub g i t1 t2)
splitE g i (ConsSig t1) (ConsSig t2) = splitC (Sub g i t1 t2)

splitE g i (IndexSig _ _ t1) (IndexSig _ _ t2) 
  = do  cs    <- splitC (Sub g i t1 t2)
        cs'   <- splitC (Sub g i t2 t1)
        return $ cs ++ cs'

splitE g i (FieldSig _ _ μf1 t1) (FieldSig _ _ μf2 t2)
  = splitWithMut g i (μf1,t1) (μf2,t2)

splitE g i (MethSig _ t1) (MethSig _ t2)
  = splitC (Sub g i t1 t2)

splitE _ _ _ _ = return []


splitWithMut g i (_,t1) (μf2,t2)
  | isImmutable μf2
  = splitC (Sub g i t1 t2)
  | otherwise 
  = (++) <$> splitC (Sub g i t1 t2)
         <*> splitC (Sub g i t2 t1)


-- splitMaybe g i (Just t1) (Just t2) = splitC (Sub g i t1 t2) 
-- splitMaybe _ _ _         _         = return []

---------------------------------------------------------------------------------------
bsplitC :: CGEnv -> a -> RefType -> RefType -> CGM [F.SubC a]
---------------------------------------------------------------------------------------
-- NOTE: addInvariant nonly needed in LHS
bsplitC g ci t1 t2 = bsplitC' g ci <$> addInvariant g t1 <*> return t2

bsplitC' g ci t1 t2
  | F.isFunctionSortedReft r1 && F.isNonTrivialSortedReft r2
  -- = F.subC (cge_fenv g) F.PTrue (r1 {F.sr_reft = typeofReft t1}) r2 Nothing [] ci
  = F.subC (cge_fenv g) p (r1 {F.sr_reft = typeofReft t1}) r2 Nothing [] ci
  | F.isNonTrivialSortedReft r2
  = F.subC (cge_fenv g) p r1 r2 Nothing [] ci
  | otherwise
  = []
  where
    p              = F.pAnd $ cge_guards g
    (r1,r2)        = (rTypeSortedReft t1, rTypeSortedReft t2)
    typeofReft t   = F.Reft $ (vv t,) [F.RConc $ typeofExpr (F.symbol "function") t
                                      ,F.RConc $ F.eProp $ vv t ]
    typeofExpr s t = F.PAtom F.Eq (F.EApp (F.dummyLoc (F.symbol "ttag")) [F.eVar $ vv t]) 
                                  (F.expr $ F.symbolText s)
    vv             = rTypeValueVar

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

splitW (W g i t@(TRef _ ts _))
  =  do let ws = bsplitW g t i
        ws'   <- concatMapM splitW [W g i ti | ti <- ts]
        return $ ws ++ ws'

splitW (W g i (TAnd ts))
  = concatMapM splitW [W g i t | t <- ts]

splitW (W g i t@(TCons _ es _))
  = do let bws = bsplitW g t i
       -- FIXME: add field bindings in g?
       ws     <- concatMapM splitW [ W g i $ eltType e | e <- M.elems es ]
       return  $ bws ++ ws

splitW (W _ _ (TClass _ ))
  = return []

splitW (W _ _ (TModule _ ))
  = return []

splitW (W _ _ (TEnum _ ))
  = return []

splitW (W _ _ t) = error $ render $ text "Not supported in splitW: " <+> pp t

bsplitW g t i 
  | F.isNonTrivialSortedReft r'
  = [F.wfC (cge_fenv g) r' Nothing i] 
  | otherwise
  = []
  where r' = rTypeSortedReft t

-- 
-- TODO HEREHERE
--
-- Perhaps disable checks at envAdds ... 
envTyAdds msg l xts 
  = envAdds' False True (msg ++ " - envTyAdds " ++ ppshow (srcPos l)) 
      [(symbolId l x,(t,WriteLocal,Initialized)) | B x t <- xts]


------------------------------------------------------------------------------
cgFunTys :: (PPR r, F.Symbolic s, PP a) 
         => AnnSSA r -> a -> [s] -> RType r 
         -> CGM [(Int, ([TVar], Maybe (RType r), [RType r], RType r))]
------------------------------------------------------------------------------
cgFunTys l f xs ft = either cgError return $ funTys l f xs ft 

------------------------------------------------------------------------------
cgMethTys :: (PP a) 
          => AnnTypeR -> a -> (Mutability, RefType)
          -> CGM [(Int, Mutability, ([TVar], Maybe RefType, [RefType], RefType))]
------------------------------------------------------------------------------
cgMethTys l f (m,t) 
   = zip3 [0..] (repeat m) <$> mapM (methTys l f) (bkAnd t)

methTys l f ft0
  = case remThisBinding ft0 of
      Nothing          -> cgError $ errorNonFunction (srcPos l) f ft0 
      Just (vs,s,bs,t) -> return  $ (vs,s,b_type <$> bs,t)

------------------------------------------------------------------------------
splitCtorTys :: (PP a) => AnnTypeR -> a -> RefType
                    -> CGM [(Int, ([TVar], Maybe RefType, [RefType], RefType))]
------------------------------------------------------------------------------
splitCtorTys l f t = zip [0..] <$> mapM (methTys l f) (bkAnd t)


--------------------------------------------------------------------------------
-- | zipType wrapper
--
--   FIXME: What is the purpose of this substitution ???
-- 
-- 
-- 
zipTypeUpM l g x t1 t2 = 
  case zipType l g x t1 t2 of
    Just (f, (F.Reft (s,ras))) -> let su  = F.mkSubst [(s, F.expr x)] in 
                                  let rs  = F.simplify $ F.Reft (s, F.subst su ras) `F.meet` F.uexprReft x in
                                  return  $ f rs
                                  -- return $ ltracePP l ("\nUPCAST SUBST IN " ++ ppshow x ++ 
                                  --                      "\nT1      " ++ ppshow t1 ++ 
                                  --                      "\nT2      " ++ ppshow t2 ++ 
                                  --                      "\nSU      " ++ show (F.toFix su) ++ 
                                  --                      "\nIN      " ++ ppshow rs) $ f rs
    Nothing -> cgError $ bugZipType (srcPos l) t1 t2

zipTypeDownM l g x t1 t2 = 
  case zipType l g x t1 t2 of
    Just (f, r) -> return $ f r
--     Just (f, r@(F.Reft (s,ras))) -> let su  = F.mkSubst [(s, F.expr x)] in 
--                                     -- let rs  = F.simplify $ F.Reft (s, subK su <$> ras) `F.meet` F.uexprReft x in
--                                     return  $ f r
--                                     -- return $ ltracePP l ("\nDOWNCAST SUBST ON " ++ ppshow x ++
--                                     --                      "\nT1      " ++ ppshow t1 ++ 
--                                     --                      "\nT2      " ++ ppshow t2 ++ 
--                                     --                      "\nSU      " ++ show (F.toFix su) ++ 
--                                     --                      "\nIN      " ++ ppshow r ++ 
--                                     --                      "\nRESULT  " ++ ppshow rs) $ f rs
    Nothing     -> cgError $ bugZipType (srcPos l) t1 t2

-- Local Variables:
-- flycheck-disabled-checkers: (haskell-liquid)
-- End:
