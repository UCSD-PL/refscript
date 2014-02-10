{-# LANGUAGE TypeSynonymInstances      #-}
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

  -- * Add Subtyping Constraints
  , subTypeContainers

  -- RJ: all alignment should already be done in TC why again?
  -- , alignTsM
  , withAlignedM
  , wellFormed

  , addInvariant
  
  -- * Add Type Annotations
  , addAnnot

  -- * Access container types
  -- , safeGetIdx
  -- , safeGetProp
  -- , indexType


  -- * Unfolding
  , unfoldSafeCG, unfoldFirstCG

  -- * Function Types
  , cgFunTys

  -- * This
  , cgPeekThis
  , cgWithThis

  ) where

import           Data.Maybe                     (fromMaybe, catMaybes, isJust, listToMaybe)
import           Data.Monoid                    (mempty)
import qualified Data.HashMap.Strict            as M

-- import           Language.Fixpoint.PrettyPrint
import           Text.PrettyPrint.HughesPJ

import           Language.Nano.Types
import           Language.Nano.Errors
import qualified Language.Nano.Annots           as A
import qualified Language.Nano.Env              as E
import           Language.Nano.Misc
import           Language.Nano.Typecheck.Types 
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Typecheck.Unfold
import           Language.Nano.Typecheck.Compare
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
import           Language.ECMAScript3.Parser.Type   (SourceSpan (..))
import           Language.ECMAScript3.PrettyPrint

import           Debug.Trace                        (trace)

-------------------------------------------------------------------------------
-- | Top level type returned after Constraint Generation ----------------------
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
initState c pgm = CGS F.emptyBindEnv (specs pgm) [] [] 0 mempty invs c [this] 
  where 
    invs        = M.fromList [(tc, t) | t@(Loc _ (TApp tc _ _)) <- invts pgm]  
    this        = tTop

getDefType f 
  = do m <- cg_defs <$> get
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

-- Only support indexing in arrays atm. Discharging array bounds checks makes
-- sense only for array types. 
-------------------------------------------------------------------------------
indexType :: RefType -> CGM RefType
-------------------------------------------------------------------------------
indexType (TArr t _) = return t
indexType t@(TApp TUn ts r) = do
    ts' <- mapM indexType ts
    return $ TApp TUn ts' fTop
indexType _          = errorstar "Unimplemented: indexing type other than array."



---------------------------------------------------------------------------------------
measureEnv   ::  Nano a (RType F.Reft) -> F.SEnv F.SortedReft
---------------------------------------------------------------------------------------
measureEnv   = fmap rTypeSortedReft . E.envSEnv . consts 

---------------------------------------------------------------------------------------
-- | Constraint Generation Monad ------------------------------------------------------
---------------------------------------------------------------------------------------

data CGState 
  = CGS { binds    :: F.BindEnv            -- ^ global list of fixpoint binders
        , cg_defs  :: !(E.Env RefType)     -- ^ type sigs for all defined functions
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

---------------------------------------------------------------------------------------
-- cgError :: (IsLocated l) => l -> String -> CGM a 
-- ---------------------------------------------------------------------------------------
-- cgError l msg = throwError $ printf "CG-ERROR at %s : %s" (ppshow $ srcPos l) msg

---------------------------------------------------------------------------------------
cgError     :: a -> Error -> CGM b 
---------------------------------------------------------------------------------------
cgError _ e = throwError e

---------------------------------------------------------------------------------------
-- | Environment API ------------------------------------------------------------------
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
    tx i t@(TApp tc _ _) = maybe t (strengthen t . rTypeReft . val) $ M.lookup tc i
    tx _ t               = t 


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


---------------------------------------------------------------------------------------
-- | Fresh Templates ------------------------------------------------------------------
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
-- | Adding Subtyping Constraints -----------------------------------------------------
---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------
subTypes :: (IsLocated x, F.Expression x, F.Symbolic x) 
         => AnnTypeR -> CGEnv -> [x] -> [RefType] -> CGM ()
---------------------------------------------------------------------------------------
subTypes l g xs ts      = zipWithM_ (subType l g)      [envFindTy x g | x <- xs] ts


subTypes' msg l g xs ts = zipWithM_ (subType' msg l g) [envFindTy x g | x <- xs] ts

-- | Subtyping
---------------------------------------------------------------------------------------
subType :: (IsLocated l) => l -> CGEnv -> RefType -> RefType -> CGM ()
---------------------------------------------------------------------------------------
subType l g t1 t2 =
  do tt1   <- addInvariant t1
     tt2   <- addInvariant t2
     -- TODO: Make this more efficient - only introduce new bindings
     g'    <- envAdds [(symbolId l x, t) | (x, Just t) <- relNames tt1 ++ relNames tt2 ] g
     s     <- checkTypes (tenv g) tt1 tt2
     modify $ \st -> st {cs = c g' s : (cs st)}
  where
    c g    = uncurry $ Sub g (ci l)
    checkTypes tds t1 t2
      | equivWUnions tds t1 t2 = return    $ (t1, t2)
    checkTypes _ _  t2
      | isTop t2               = return    $ (t1, t2)
    checkTypes  _    t1 t2
      | otherwise              = cgError l $ bugMalignedSubtype (srcPos l) t1 t2

    relNames t = (\n -> (n, n `E.envFindTy` renv g)) <$> names t

    names = foldReft rr []
    rr r xs = reftNames r ++ xs
    reftNames = F.syms
    
    
-- A more verbose version
subType' msg l g t1 t2 = 
  subType l g (trace (printf "SubType[%s]:\n\t%s\n\t%s" msg (ppshow t1) (ppshow t2)) t1) t2

-------------------------------------------------------------------------------
equivWUnions :: E.Env (TyDef RefType) -> RefType -> RefType -> Bool
-------------------------------------------------------------------------------
equivWUnions γ t1@(TApp TUn _ _) t2@(TApp TUn _ _) = 
  {-let msg = printf "In equivWUnions:\n%s - \n%s" (ppshow t1) (ppshow t2) in -}
  case unionPartsWithEq (equiv γ) t1 t2 of 
    (ts,[],[])  -> and $ uncurry (safeZipWith "equivWUnions" $ equivWUnions γ) (unzip ts)
    _           -> False
equivWUnions γ t t' = equiv γ t t'

equivWUnionsM γ t t' = return $ equivWUnions γ t t'

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
subTypeContainers msg l g t1 t2 = subType l g t1 t2
    where 
      msg'                      = render $ text "subTypeContainers:" 
                                           $+$ text "  t1 =" <+> pp t1
                                           $+$ text "  t2 =" <+> pp t2


-------------------------------------------------------------------------------
alignTsM :: CGEnv -> RefType -> RefType -> CGM (RefType, RefType)
-------------------------------------------------------------------------------
alignTsM g t t' = return $ alignTs (tenv g) t t'


-------------------------------------------------------------------------------
withAlignedM :: CGEnv -> (RefType -> RefType -> CGM a) -> RefType -> RefType -> CGM a
-------------------------------------------------------------------------------
withAlignedM g f t t' = alignTsM g t t' >>= uncurry f 


-- | Monadic unfolding
-------------------------------------------------------------------------------
unfoldFirstCG :: CGEnv -> RefType -> CGM RefType
-------------------------------------------------------------------------------
unfoldFirstCG g t = return $ unfoldFirst (tenv g) t


-------------------------------------------------------------------------------
unfoldSafeCG :: CGEnv -> RefType -> CGM RefType
-------------------------------------------------------------------------------
unfoldSafeCG g t  = return $ unfoldSafe (tenv g) t


---------------------------------------------------------------------------------------
-- | Adding Well-Formedness Constraints -----------------------------------------------
---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------
wellFormed       :: (IsLocated l) => l -> CGEnv -> RefType -> CGM RefType  
---------------------------------------------------------------------------------------
wellFormed l g t = do modify $ \st -> st { ws = (W g (ci l) t) : ws st }
                      return t



---------------------------------------------------------------------------------------
-- | Generating Fresh Values ----------------------------------------------------------
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
splitC' :: SubC -> CGM [FixSubC]
---------------------------------------------------------------------------------------
splitC c = splitC' c

---------------------------------------------------------------------------------------
-- | Function types
---------------------------------------------------------------------------------------
splitC' (Sub g i tf1@(TFun xt1s t1 _) tf2@(TFun xt2s t2 _))
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
splitC' (Sub g i (TAll α1 t1) (TAll α2 t2))
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
splitC' (Sub g i t1@(TVar α1 _) t2@(TVar α2 _)) 
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
splitC' (Sub g i t1@(TApp TUn t1s r1) t2@(TApp TUn t2s r2)) =
  ifM (equivWUnionsM (tenv g) t1 t2) 
    (do  cs      <- bsplitC g i t1 t2
         -- constructor parameters are covariant
         let t1s' = (`strengthen` r1) <$> t1s
         let t2s' = (`strengthen` r2) <$> t2s
         cs'     <- concatMapM splitC $ safeZipWith "splitcTDef" (Sub g i) t1s' t2s'
         return   $ cs ++ cs'
    )
    (errorstar $ printf "Unequal unions in splitC: %s - %s" (ppshow $ toType t1) (ppshow $ toType t2))

splitC' (Sub _ _ t1@(TApp TUn _ _) t2) = 
  errorstar $ printf "Unions in splitC: %s - %s" (ppshow t1) (ppshow t2)
splitC' (Sub _ _ t1 t2@(TApp TUn _ _)) = 
  errorstar $ printf "Unions in splitC: %s - %s" (ppshow t1) (ppshow t2)

---------------------------------------------------------------------------------------
-- |Type definitions
---------------------------------------------------------------------------------------
splitC' (Sub g i t1@(TApp d1@(TDef _) t1s _) t2@(TApp d2@(TDef _) t2s _)) | d1 == d2
  = do  cs    <- bsplitC g i t1 t2
        -- constructor parameters are covariant
        cs'   <- concatMapM splitC $ safeZipWith "splitcTDef" (Sub g i) t1s t2s
        return $ cs ++ cs' 

splitC' (Sub _ _ (TApp (TDef _) _ _) (TApp (TDef _) _ _))
  = errorstar "Unimplemented: Check type definition cycles"
  
splitC' (Sub g i t1@(TApp (TDef _) _ _ ) t2) = 
  unfoldSafeCG g t1 >>= \t1' -> splitC' $ Sub g i t1' t2

splitC' (Sub g i  t1 t2@(TApp (TDef _) _ _)) = 
  unfoldSafeCG g t2 >>= \t2' -> splitC' $ Sub g i t1 t2'

---------------------------------------------------------------------------------------
-- | Rest of TApp
---------------------------------------------------------------------------------------
splitC' (Sub g i t1@(TApp _ t1s _) t2@(TApp _ t2s _))
  = do cs    <- bsplitC g i t1 t2
       cs'   <- concatMapM splitC $ safeZipWith 
                                    (printf "splitC4: %s - %s" (ppshow t1) (ppshow t2)) 
                                    (Sub g i) t1s t2s
       return $ cs ++ cs'

---------------------------------------------------------------------------------------
-- | Objects
---------------------------------------------------------------------------------------
splitC' (Sub g i t1@(TObj _ _) t2@(TObj _ _ ))
  = do cs    <- bsplitC g i t1 t2
       -- RJ: not strengthening with top-level reft because not sure we need it...
       cs'   <- concatMapM splitC [Sub g i t1' t2' | (t1',t2') <- bkPaddedObject (srcPos i) t1 t2]
       return $ cs ++ cs' 

splitC' (Sub _ _ t1 t2@(TObj _ _ ))
  = error $ printf "splitC - should have been broken down earlier:\n%s <: %s" 
            (ppshow t1) (ppshow t2)

splitC' (Sub _ _ t1@(TObj _ _ ) t2)
  = error $ printf "splitC - should have been broken down earlier:\n%s <: %s" 
            (ppshow t1) (ppshow t2)


---------------------------------------------------------------------------------------
-- | TArr
---------------------------------------------------------------------------------------
splitC' (Sub g i t1@(TArr t1v _ ) t2@(TArr t2v _ ))
  = do cs    <- bsplitC g i t1 t2
       cs'   <- splitC' (Sub g i t1v t2v) -- CO-VARIANCE 
       cs''  <- splitC' (Sub g i t2v t1v) -- CONTRA-VARIANCE 
       return $ cs ++ cs' ++ cs''

splitC' x 
  = cgError l $ bugBadSubtypes l x where l = srcPos x

---------------------------------------------------------------------------------------
-- bsplitC :: (F.Reftable r) => CGEnv -> a -> RType r -> RType r -> CGM [F.SubC a]
---------------------------------------------------------------------------------------
bsplitC g ci t1 t2
  = bsplitC' g ci <$> addInvariant t1 <*> addInvariant t2
  -- = do t1'   <- tracePP "addInv1: " <$> addInvariant t1
  --      t2'   <- tracePP "addInv2: " <$> addInvariant t2
  --      return $ bsplitC' g ci t1' t2'

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
-- | Splitting Well-Formedness Constraints --------------------------------------------
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

splitW (W g i t@(TObj ts _ ))
  = do  g'    <- envTyAdds i ts g
        let bs = bsplitW g t i
        ws    <- concatMapM splitW [W g' i ti | B _ ti <- ts]
        return $ bs ++ ws

splitW (W g i (TAnd ts))
  = concatMapM splitW [W g i t | t <- ts]

splitW (W _ _ t) = error $ render $ text "Not supported in splitW: " <+> pp t

bsplitW g t i 
  | F.isNonTrivialSortedReft r'
  = [F.wfC (fenv g) r' Nothing i] 
  | otherwise
  = []
  where r' = rTypeSortedReft t

-- mkSortedReft tce = F.RR . rTypeSort tce

-- refTypeId ::  (F.Reftable r, IsLocated l) => l -> RType r -> Id l
-- refTypeId l = symbolId l . F.symbol -- rTypeValueVar 

envTyAdds l xts = envAdds [(symbolId l x, t) | B x t <- xts]

-------------------------------------------------------------------------------------------

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

