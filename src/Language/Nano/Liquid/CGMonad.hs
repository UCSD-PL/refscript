{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE FlexibleInstances         #-}
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
  , getDefType

  , getDef
  , getPropTDefM

  -- * Throw Errors
  , cgError      

  -- * Fresh Templates for Unknown Refinement Types 
  , freshTyFun
  , freshTyVar
  , freshTyInst
  , freshTyPhis
  , freshTyPhisWhile
  , freshTyObj

  -- * Freshable
  , Freshable (..)

  -- * Environment API
  , envAddFresh
  , envAdds
  , envAddReturn
  , envAddGuard
  , envFindTy
  , envFindSpec
  , envFindAnnot
  , envToList
  , envFindReturn
  -- , envJoin
  , envPushContext
  , envGetContextCast
  , envGetContextTypArgs

  , addObjLitTyM
  , findTySymOrDieM
  , findTySymWithIdOrDieM

  -- * Add Subtyping Constraints
  , subTypeContainers

  -- RJ: all alignment should already be done in TC why again?
  -- , alignTsM
  {-, withAlignedM-}
  , wellFormed
  
  -- * Add Type Annotations
  , addAnnot


  -- * Function Types
  , cgFunTys

  -- * This
  , cgPeekThis
  , cgWithThis

  -- Zip types
  , zipType1, zipType2


  ) where

import           Data.Maybe                     (fromMaybe, listToMaybe, maybeToList)
import           Data.Monoid                    (mempty)
import           Data.Function                  (on)
import qualified Data.HashMap.Strict            as M
import qualified Data.List                      as L

-- import           Language.Fixpoint.PrettyPrint
import           Text.PrettyPrint.HughesPJ

import           Language.Nano.Types
import           Language.Nano.Errors
import qualified Language.Nano.Annots           as A
import qualified Language.Nano.Env              as E
import           Language.Nano.Typecheck.Types 
import           Language.Nano.Typecheck.Lookup
import           Language.Nano.Typecheck.Subst
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


type PPR r = (PP r, F.Reftable r)
type PPRSF r = (PPR r, Substitutable r (Fact r), Free (Fact r)) 

-------------------------------------------------------------------------------
getCGInfo :: Config -> NanoRefType -> CGM a -> CGInfo
-------------------------------------------------------------------------------
getCGInfo cfg pgm = clear . cgStateCInfo pgm . execute cfg pgm . (>> fixCWs)
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
initState c p = CGS F.emptyBindEnv (specs p) (defs p) [] [] 0 mempty invs c [this] 
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
                , F.quals = nanoQualifiers pgm 
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
        , cs       :: ![SubC]              -- ^ subtyping constraints
        , ws       :: ![WfC]               -- ^ well-formedness constraints
        , count    :: !Integer             -- ^ freshness counter
        , cg_ann   :: A.UAnnInfo RefType   -- ^ recorded annotations
        , invs     :: TConInv              -- ^ type constructor invariants
        , cg_opts  :: Config               -- ^ configuration options
        , cg_this  :: ![RefType]           -- ^ a stack holding types for 'this' 
        }

type CGM     = ErrorT Error (State CGState)

type TConInv = M.HashMap TCon (Located RefType)


-------------------------------------------------------------------------------
getDef  :: CGM (TDefEnv RefType)
-------------------------------------------------------------------------------
getDef = cg_defs <$> get

-------------------------------------------------------------------------------
setDef  :: TDefEnv RefType -> CGM ()
-------------------------------------------------------------------------------
setDef γ = modify $ \u -> u { cg_defs = γ } 


getPropTDefM l s t ts = do 
-- FIXME
  ε <- undefined -- getExts
  δ <- getDef 
  return $ getPropTDef l ε δ (F.symbol s) ts t



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
envGetContextCast :: CGEnv -> AnnTypeR -> Maybe (Cast RefType)
---------------------------------------------------------------------------------------
envGetContextCast g a 
  = case [c | TCast cx c <- ann_fact a, cx == cge_ctx g] of
      [ ] -> Nothing
      [c] -> Just c
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
envAddFresh :: (IsLocated l) => String -> l -> RefType -> CGEnv -> CGM (Id AnnTypeR, CGEnv) 
---------------------------------------------------------------------------------------
envAddFresh _ l t g 
  = do x  <- freshId loc
       g' <- envAdds [(x, t)] g
       return (x, g')
    where loc = srcPos l
   
freshId l = Id (Ann l []) <$> fresh


---------------------------------------------------------------------------------------
envAdds      :: (F.Symbolic x, IsLocated x) => [(x, RefType)] -> CGEnv -> CGM CGEnv
---------------------------------------------------------------------------------------
envAdds xts' g
  = do xts    <- zip xs  <$> mapM addInvariant ts
       is     <- forM xts $  addFixpointBind 
       _      <- forM xts $  \(x, t) -> addAnnot (srcPos x) x t
       return  $ g { renv = E.envAdds xts        (renv g) } 
                   { fenv = F.insertsIBindEnv is (fenv g) }
    where 
       (xs,ts) = unzip xts'


addFixpointBind :: (F.Symbolic x) => (x, RefType) -> CGM F.BindId
addFixpointBind (x, t) 
  = do let s     = F.symbol x
       let r     = rTypeSortedReft t
       (i, bs') <- F.insertBindEnv s r . binds <$> get 
       modify    $ \st -> st { binds = bs' }
       return    $ i

---------------------------------------------------------------------------------------
addInvariant   :: RefType -> CGM RefType
---------------------------------------------------------------------------------------
addInvariant t           = ((`tx` t) . invs) <$> get
  where 
    tx i t@(TApp tc _ o) = maybe t (\i -> strengthenOp t o $ rTypeReft $ val i) $ {- trace ("addInvariant on: " ++ ppshow o) $ -} M.lookup tc i
    tx _ t               = t 

    strengthenOp t o r   | L.elem r (ofRef o) = t
    strengthenOp t _ r   | otherwise          = strengthen t r

    ofRef (F.Reft (s, as)) = (F.Reft . (s,) . single) <$> as


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
                    
---------------------------------------------------------------------------------------
envFindTy     :: (IsLocated x, F.Symbolic x, F.Expression x) => x -> CGEnv -> RefType 
---------------------------------------------------------------------------------------
-- | A helper that returns the actual @RefType@ of the expression by
--     looking up the environment with the name, strengthening with
--     singleton for base-types.

envFindTy x g = (`eSingleton` x) $ fromMaybe err $ E.envFindTy x $ renv g
  where 
    err       = throw $ bugUnboundVariable (srcPos x) (F.symbol x)


---------------------------------------------------------------------------------------
envFindSpec     :: (IsLocated x, F.Symbolic x) => x -> CGEnv -> Maybe RefType 
---------------------------------------------------------------------------------------
envFindSpec x g = E.envFindTy x $ cge_spec g

envFindAnnot l x g = msum [tAnn, tEnv, annot] 
  where
    annot        = listToMaybe [ t | TAnnot t <- ann_fact l ]
    tAnn         = E.envFindTy x $ cge_spec g
    tEnv         = E.envFindTy x $ renv     g

---------------------------------------------------------------------------------------
envToList     ::  CGEnv -> [(Id SourceSpan, RefType)]
---------------------------------------------------------------------------------------
envToList g = E.envToList $ renv g


---------------------------------------------------------------------------------------
envFindReturn :: CGEnv -> RefType 
---------------------------------------------------------------------------------------
envFindReturn = E.envFindReturn . renv


-- | Monad versions of TDefEnv operations

updTDefEnv f = f <$> getDef >>= \(δ', a) -> setDef δ' >> return a

addObjLitTyM            = updTDefEnv . addObjLitTy
findTySymOrDieM i       = findTySymOrDie i <$> getDef
findTySymWithIdOrDieM i = findTySymWithIdOrDie i <$> getDef
findTyIdOrDieM :: TyID -> CGM (TDef RefType)
findTyIdOrDieM i        = findTyIdOrDie i <$> getDef


---------------------------------------------------------------------------------------
-- | Fresh Templates
---------------------------------------------------------------------------------------

-- | Instantiate Fresh Type (at Function-site)
---------------------------------------------------------------------------------------
freshTyFun :: (IsLocated l) => CGEnv -> l -> Id AnnTypeR -> RefType -> CGM RefType 
---------------------------------------------------------------------------------------
freshTyFun g l f t = freshTyFun' g l f t . kVarInst . cg_opts =<< get  

freshTyFun' g l _ t b
  | b && isTrivialRefType t = freshTy "freshTyFun" (toType t) >>= wellFormed l g
  | otherwise               = return t

freshTyVar g l t 
  | isTrivialRefType t = freshTy "freshTyVar" (toType t) >>= wellFormed l g
  | otherwise          = return t

-- | Instantiate Fresh Type (at Call-site)

freshTyInst l g αs τs tbody
  = do ts    <- mapM (freshTy "freshTyInst") τs
       _     <- mapM (wellFormed l g) ts
       let θ  = fromList $ zip αs ts
       return $ apply θ tbody

-- | Instantiate Fresh Type (at Phi-site) 
---------------------------------------------------------------------------------------
freshTyPhis :: (PP l, IsLocated l) => l -> CGEnv -> [Id l] -> [Type] -> CGM (CGEnv, [RefType])  
---------------------------------------------------------------------------------------
freshTyPhis l g xs τs 
  = do ts <- mapM    (freshTy "freshTyPhis")  τs
       g' <- envAdds (safeZip "freshTyPhis" xs ts) g
       _  <- mapM    (wellFormed l g') ts
       return (g', ts)

---------------------------------------------------------------------------------------
freshTyPhisWhile :: (PP l, IsLocated l) => l -> CGEnv -> [Id l] -> [Type] -> CGM (CGEnv, [RefType])  
---------------------------------------------------------------------------------------
freshTyPhisWhile l g xs τs 
  = do ts <- mapM    (freshTy "freshTyPhis")  τs
       g' <- envAdds (safeZip "freshTyPhis" xs ts) g
       _  <- mapM    (wellFormed l g) ts
       return (g', ts)

-- | Fresh Object Type
---------------------------------------------------------------------------------------
freshTyObj :: (IsLocated l) => l -> CGEnv -> RefType -> CGM RefType 
-- ---------------------------------------------------------------------------------------
freshTyObj l g t = freshTy "freshTyArr" t >>= wellFormed l g 

---------------------------------------------------------------------------------------
-- | Adding Subtyping Constraints
---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------
subType :: (IsLocated l) => l -> CGEnv -> RefType -> RefType -> CGM ()
---------------------------------------------------------------------------------------
subType l g t1 t2 =
  do t1'   <- addInvariant t1
     t2'   <- addInvariant t2
     -- TODO: Make this more efficient - only introduce new bindings
     g'    <- envAdds [(symbolId l x, t) | (x, Just t) <- rNms t1' ++ rNms t2' ] g
     -- FIXME: check for rawtype compatibility
     -- Using something like equivWUnionsM
     modify $ \st -> st {cs = c g' (t1', t2') : (cs st)}
  where
    c g     = uncurry $ Sub g (ci l)
    rNms t  = (\n -> (n, n `E.envFindTy` renv g)) <$> names t
    names   = foldReft rr []
    rr r xs = F.syms r ++ xs
    
    
-- A more verbose version
-- subType' msg l g t1 t2 = 
--   subType l g (trace (printf "SubType[%s]:\n\t%s\n\t%s" msg (ppshow t1) (ppshow t2)) t1) t2

-------------------------------------------------------------------------------
equivWUnions :: TDefEnv RefType -> RefType -> RefType -> Bool
-------------------------------------------------------------------------------
equivWUnions δ t1@(TApp TUn _ _) t2@(TApp TUn _ _) = 
  {-let msg = printf "In equivWUnions:\n%s - \n%s" (ppshow t1) (ppshow t2) in -}
  case unionParts t1 t2 of 
    (ts,[],[])  -> and $ uncurry (safeZipWith "equivWUnions" $ equivWUnions δ) (unzip ts)
    _           -> False
equivWUnions _ t t' = equiv t t'

equivWUnionsM t t' = getDef >>= \δ -> return (equivWUnions δ t t')


-- | Subtyping container contents: unions, objects. Includes top-level

-- `subTypeContainers'` breaks down container types (unions and objects) to their
-- sub-parts and recursively creates subtyping constraints for these parts. 
-- It returns simple subtyping for the rest of the cases (non-container types).
--
-- The top-level refinements of the container types strengthen the parts.
--
--      Γ |- {v:T1 | P1 ∧ P3}       <: { v:T1 | P1' ∧ P3'}
--      Γ |- {v:T2 | P2 ∧ P3}       <: { v:T2 | P2' ∧ P3'}
--      −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−   
--      Γ |- {v:T1/P1 + T2/P2 | P3} <: {v:T1/P1' + T2/P2' | P3'}
--
--
-- TODO: Will loop infinitely for cycles in type definitions
-------------------------------------------------------------------------------
subTypeContainers :: (IsLocated l) => String -> l -> CGEnv -> RefType -> RefType -> CGM ()
-------------------------------------------------------------------------------
subTypeContainers {- msg -} _ l g t1 t2 = subType l g t1 t2
    {-where -}
    {-  msg'                      = render $ text "subTypeContainers:" -}
    {-                                       $+$ text "  t1 =" <+> pp t1-}
    {-                                       $+$ text "  t2 =" <+> pp t2-}



---------------------------------------------------------------------------------------
-- | Adding Well-Formedness Constraints
---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------
wellFormed       :: (IsLocated l) => l -> CGEnv -> RefType -> CGM RefType  
---------------------------------------------------------------------------------------
wellFormed l g t = do modify $ \st -> st { ws = (W g (ci l) t) : ws st }
                      return t



---------------------------------------------------------------------------------------
-- | Generating Fresh Values 
---------------------------------------------------------------------------------------

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

freshTy :: RefTypable a => s -> a -> CGM RefType
freshTy _ τ = refresh $ rType τ

instance Freshable F.Refa where
  fresh = (`F.RKvar` mempty) <$> (F.intKvar <$> fresh)

instance Freshable [F.Refa] where
  fresh = single <$> fresh

instance Freshable F.Reft where
  fresh                  = errorstar "fresh Reft"
  true    (F.Reft (v,_)) = return $ F.Reft (v, []) 
  refresh (F.Reft (_,_)) = curry F.Reft <$> ({-tracePP "freshVV" <$> -}freshVV) <*> fresh
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

---------------------------------------------------------------------------------------
-- | Splitting Subtyping Constraints --------------------------------------------------
---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------
splitC :: SubC -> CGM [FixSubC]
---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------
-- | Function types
---------------------------------------------------------------------------------------
splitC (Sub g i tf1@(TFun xt1s t1 _) tf2@(TFun xt2s t2 _))
  = do bcs       <- bsplitC g i tf1 tf2
       g'        <- envTyAdds i xt2s g 
       cs        <- concatMapM splitC $ safeZipWith "splitC1" (Sub g' i) t2s t1s' 
       cs'       <- splitC $ Sub g' i (F.subst su t1) t2      
       return     $ bcs ++ cs ++ cs'
    where 
       t2s        = b_type <$> xt2s
       t1s'       = F.subst su (b_type <$> xt1s)
       su         = F.mkSubst $ safeZipWith "splitC2" bSub xt1s xt2s
       bSub b1 b2 = (b_sym b1, F.eVar $ b_sym b2)

---------------------------------------------------------------------------------------
-- | TAlls
---------------------------------------------------------------------------------------
splitC (Sub g i (TAll α1 t1) (TAll α2 t2))
  | α1 == α2 
  = splitC $ Sub g i t1 t2
  | otherwise   
  = splitC $ Sub g i t1 t2' 
  where 
    θ   = fromList [(α2, tVar α1 :: RefType)]
    t2' = apply θ t2

---------------------------------------------------------------------------------------
-- | TVars
---------------------------------------------------------------------------------------
splitC (Sub g i t1@(TVar α1 _) t2@(TVar α2 _)) 
  | α1 == α2
  = bsplitC g i t1 t2
  | otherwise
  = errorstar "UNEXPECTED CRASH in splitC"

---------------------------------------------------------------------------------------
-- | Unions:
-- We need to get the bsplitC for the top level refinements 
-- Nothing more should be added, the internal subtyping constraints have been
-- dealt with separately
---------------------------------------------------------------------------------------
splitC (Sub g i t1@(TApp TUn t1s r1) t2@(TApp TUn t2s r2)) =
  ifM (equivWUnionsM t1 t2) 
    (do  cs      <- bsplitC g i t1 t2
         -- constructor parameters are covariant
         let t1s' = (`strengthen` r1) <$> t1s
         let t2s' = (`strengthen` r2) <$> t2s
         cs'     <- concatMapM splitC $ safeZipWith "splitcTRef" (Sub g i) t1s' t2s'
         return   $ cs ++ cs'
    )
    (errorstar $ printf "Unequal unions in splitC: %s - %s" 
      (ppshow $ toType t1) (ppshow $ toType t2))

splitC (Sub _ _ t1@(TApp TUn _ _) t2) = 
  errorstar $ printf "Unions in splitC: %s - %s" (ppshow t1) (ppshow t2)
splitC (Sub _ _ t1 t2@(TApp TUn _ _)) = 
  errorstar $ printf "Unions in splitC: %s - %s" (ppshow t1) (ppshow t2)

---------------------------------------------------------------------------------------
-- |Type definitions
---------------------------------------------------------------------------------------
splitC (Sub g i t1@(TApp (TRef i1) t1s _) t2@(TApp (TRef i2) t2s _)) 
  | i1 == i2
  = do  cs    <- bsplitC g i t1 t2
        -- Invariant type parameters
        cs'   <- concatMapM splitC $ safeZipWith "splitcTRef" (Sub g i) t1s t2s
                                  ++ safeZipWith "splitcTRef" (Sub g i) t2s t1s
        return $ cs ++ cs' 
  | otherwise 
  = do  cs    <- bsplitC g i t1 t2        --XXX: Does bsplitC remain the same?
        d1@(TD _ v1s _ _) <- findTyIdOrDieM i1
        d2@(TD _ v2s _ _) <- findTyIdOrDieM i2
        e1s <- getDef >>= return . apply (fromList $ zip v1s t1s) . flatten d1
        e2s <- getDef >>= return . apply (fromList $ zip v2s t2s) . flatten d2
        cs' <- splitE g i e1s e2s
        return $ cs ++ cs'

splitC (Sub _ _ (TApp (TRef _) _ _) _)
  = errorstar "UNEXPECTED CRASH in splitC:TApp-TRef"
splitC (Sub _ _ _ (TApp (TRef _) _ _))
  = errorstar "UNEXPECTED CRASH in splitC:TApp-TRef"

---------------------------------------------------------------------------------------
-- | Rest of TApp
---------------------------------------------------------------------------------------
splitC (Sub g i t1@(TApp _ t1s _) t2@(TApp _ t2s _))
  = do cs    <- bsplitC g i t1 t2
       cs'   <- concatMapM splitC $ safeZipWith 
                                    (printf "splitC4: %s - %s" (ppshow t1) (ppshow t2)) 
                                    (Sub g i) t1s t2s
       return $ cs ++ cs'

---------------------------------------------------------------------------------------
-- | TArr
---------------------------------------------------------------------------------------
splitC (Sub g i t1@(TArr t1v _ ) t2@(TArr t2v _ ))
  = do cs    <- bsplitC g i t1 t2
       cs'   <- splitC (Sub g i t1v t2v) -- CO-VARIANCE 
       cs''  <- splitC (Sub g i t2v t1v) -- CONTRA-VARIANCE 
       return $ cs ++ cs' ++ cs''

splitC x 
  = cgError l $ bugBadSubtypes l x where l = srcPos x




---------------------------------------------------------------------------------------
splitE :: CGEnv -> Cinfo -> [TElt RefType] -> [TElt RefType] -> CGM [FixSubC]
---------------------------------------------------------------------------------------
splitE g i e1s e2s
    | length e1s == length e2s 
    = concatMapM splitC $ zipWith (Sub g i) t1s t2s
    | otherwise
    = cgError l $ bugMalignedFields l e1s e2s 
  where
    l   = srcPos i
    t1s = f_type <$> L.sortBy (compare `on` f_sym) e1s  
    t2s = f_type <$> L.sortBy (compare `on` f_sym) e2s




---------------------------------------------------------------------------------------
bsplitC :: CGEnv -> a -> RefType -> RefType -> CGM [F.SubC a]
---------------------------------------------------------------------------------------
bsplitC g ci t1 t2
  = bsplitC' g ci <$> addInvariant t1 <*> addInvariant t2

bsplitC' g ci t1 t2
  | F.isFunctionSortedReft r1 && F.isNonTrivialSortedReft r2
  = [F.subC (fenv g) F.PTrue (r1 {F.sr_reft = fTop}) r2 Nothing [] ci]
  | F.isNonTrivialSortedReft r2
  = [F.subC (fenv g) p r1 r2 Nothing [] ci]
  | otherwise
  = []
  where
    p  = F.pAnd $ guards g
    r1 = rTypeSortedReft t1
    r2 = rTypeSortedReft t2


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

splitW (W g i t@(TArr t' _))
  =  do let ws = bsplitW g t i
        ws'   <- splitW (W g i t')
        return $ ws ++ ws'

splitW (W g i (TAnd ts))
  = concatMapM splitW [W g i t | t <- ts]

splitW (W _ _ t) = error $ render $ text "Not supported in splitW: " <+> pp t

bsplitW g t i 
  | F.isNonTrivialSortedReft r'
  = [F.wfC (fenv g) r' Nothing i] 
  | otherwise
  = []
  where r' = rTypeSortedReft t

envTyAdds l xts = envAdds [(symbolId l x, t) | B x t <- xts]


---------------------------------------------------------------------------------------
-- | Replace all sorts with FInt


class ClearSorts a where
  clear :: a -> a
  clearM :: a -> CGM a 
  clearM = return . clear

instance ClearSorts F.BindEnv where
  clear = F.mapBindEnv (mapSnd clear)

instance (ClearSorts a, ClearSorts b) => ClearSorts (a,b) where
  clear (a,b) = (clear a, clear b)
                 
instance ClearSorts (F.SubC a) where
  clear (F.SubC e g l r i t ii) = F.SubC e g (clear l) (clear r) i t ii

instance ClearSorts a => ClearSorts [a] where
  clear xs = clear <$> xs

instance ClearSorts F.SortedReft where
  clear (F.RR s r) = F.RR (clear s) r

instance ClearSorts F.Sort where 
  clear F.FInt        = F.FInt
  clear F.FNum        = F.FInt
  clear (F.FObj _)    = F.FInt
  clear (F.FVar _)    = F.FInt
  clear (F.FFunc i s) = F.FFunc i $ clear <$> s
  clear (F.FApp _ _ ) = F.FInt -- F.FApp  c $ clear s

instance ClearSorts F.Symbol where
  clear = id

instance ClearSorts (F.WfC a) where
  clear (F.WfC e r i ii) = F.WfC e (clear r) i ii 

instance ClearSorts CGInfo where
  clear (CGI f a) = CGI (clear f) a

instance ClearSorts (F.FInfo a) where
  clear (F.FI cm ws bs gs lits kuts quals) =
    {-let msg = printf "\nGS: %s\n\n" (render $ F.toFix $ F.toListSEnv gs) in-}
    F.FI (M.map clear cm)
         (clear ws)
         (clear bs)
         -- XXX: Special treatment for Prop
         (F.mapSEnvWithKey clearProp {- $ trace msg -} gs)
         (clear lits)
         kuts
         quals

clearProp (sy, F.RR so re) 
  | F.symbolString sy == "Prop" 
  = (sy, F.RR (F.FFunc 2 [F.FInt, F.FApp F.boolFTyCon []]) re)
  | otherwise                   
  = (clear sy, clear $ F.RR so re)

cgFunTys l f xs ft = 
  case funTys l f xs ft of 
    Left e  -> cgError l e 
    Right a -> return a


-- | `this`

cgPeekThis = safeHead "get 'this'" <$> (cg_this <$> get)

cgPushThis t = modify $ \st -> st { cg_this = t : cg_this st } 

cgPopThis    = modify $ \st -> st { cg_this = tail $ cg_this st } 

cgWithThis t p = do { cgPushThis t; a <- p; cgPopThis; return a } 


-- | `zipType1` matches structurally equivalent parts of types @t1@ and @t2@:
-- * Keeping the structure of @t1@
-- * Applying f on the respective refinements 
-- * f is commutative
-- 
-- E.g. zipType1 (number + { boolean | p } ) { number | v > 0 } meet = 
--        { number | v > 0 } + { boolean | p } 
zipType1 f t1 t2 = zipType2 f t2 t1


-- | `zipType2` 
-- * walks through the equivalent parts of types @t1@ and @t2@
-- * applies function @f@ on the refinements of the equivalent parts 
-- * keeps the output as the resulting refinement
-- * preserves the shape of @t2@
--------------------------------------------------------------------------------
zipType2 :: (F.Reft -> F.Reft -> F.Reft) ->  RefType -> RefType -> CGM RefType
--------------------------------------------------------------------------------
zipType2 f (TApp TUn t1s r1) (TApp TUn t2s r2)  = do
  ts <- mapM (zipTypes f t1s) t2s 
  return $ TApp TUn ts $ f r1 r2

zipType2 f (TApp TUn ts _) t =  
  zipTypes f ts t

zipType2 f t (TApp TUn ts' r') = do
  ts <- mapM (zipTypes f [t]) ts'
  return $ TApp TUn ts r'                 -- The top-level refinement for t' should remain

zipType2 f (TApp (TRef i1) t1s r1) (TApp (TRef i2) t2s r2) 
  | i1 == i2  = do  ts <- zipWithM (zipType2 f) t1s t2s
                    return $ TApp (TRef i1) ts $ f r1 r2
  | otherwise = do  d1@(TD _ v1s _ _) <- findTyIdOrDieM i1
                    d2@(TD _ v2s _ _) <- findTyIdOrDieM i2
                    e1s <- apply (fromList $ zip v1s t1s) <$> flattenM d1
                    e2s <- apply (fromList $ zip v2s t2s) <$> flattenM d2 
                    let (e1s', e2s') = unzip [ (e1, e2) | e2 <- e2s, e1 <- maybeToList $ L.find (same e2) e1s]   
                    es <- zipWithM (zipElt2 f) e1s' e2s'
                    i  <- addObjLitTyM $ TD Nothing [] Nothing es
                    return $ TApp (TRef i) [] $ f r1 r2
  where
    same (TE s1 b1 _) (TE s2 b2 _) = s1 == s2 && b1 == b2
    
zipType2 f (TApp c [] r) (TApp c' [] r')    | c == c' = 
  return $ TApp c [] $ f r r'

zipType2 f (TVar v r) (TVar v' r') | v == v' = 
  return $ TVar v $ f r r'

zipType2 f (TFun x1s t1 r1) (TFun x2s t2 r2) = do
  xs <- zipWithM (zipBind2 f) x1s x2s 
  y  <- zipType2 f t1 t2
  return $ TFun xs y $ f r1 r2

zipType2 f (TArr t1 r1) (TArr t2 r2) = do
  t <- zipType2 f t1 t2
  return $ TArr t $ f r1 r2

zipType2 _ t1 t2 = 
  errorstar $ printf "BUG[zipType2]: mis-aligned types in:\n\t%s\nand\n\t%s" (ppshow t1) (ppshow t2)

zipTypes f ts t = 
  case filter (equiv t) ts of
    [  ] -> return t
    [t'] -> zipType2 f t' t
    _    -> errorstar "BUG[zipType]: multiple equivalent types" 
  

zipBind2 f (B s1 t1) (B s2 t2) 
  | s1 == s2 = B s1 <$> zipType2 f t1 t2 
zipBind2 _ _       _           
  = errorstar "BUG[zipBind2]: mis-matching binders"

zipElt2 f (TE s1 b1 t1) (TE s2 b2 t2) 
  | s1 == s2 && b1 == b2 = TE s1 b1 <$> zipType2 f t1 t2 
zipElt2 _ _       _                
  = errorstar "BUG[zipElt2]: mis-matching elements"

--------------------------------------------------------------------------------
flattenM :: TDef RefType -> CGM [TElt RefType]
--------------------------------------------------------------------------------
flattenM d = flatten d <$> getDef

