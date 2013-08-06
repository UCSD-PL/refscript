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

  -- * Get Defined Types
  , getTDefs

  -- * Throw Errors
  , cgError      

  -- * Fresh Templates for Unknown Refinement Types 
  , freshTyFun
  , freshTyInst
  , freshTyPhis

  -- * Freshable
  , Freshable (..)

  -- * Environment API
  , envAddFresh
  , envAdds
  , envAddReturn
  , envAddGuard
  , envFindTy
  , envFindReturn
  , envJoin

  -- * Add Subtyping Constraints
  , subTypes
  , subType

  , addInvariant
  
  -- * Add Type Annotations
  , addAnnot
  ) where

import           Data.Maybe             (fromMaybe)
import           Data.Monoid            (mempty) -- hiding ((<>))            
import qualified Data.List               as L
import qualified Data.HashMap.Strict     as M

-- import           Language.Fixpoint.PrettyPrint
import           Text.PrettyPrint.HughesPJ

import           Language.Nano.Types
import           Language.Nano.Errors
import qualified Language.Nano.Annots           as A
import qualified Language.Nano.Env              as E
import           Language.Nano.Misc
import           Language.Nano.Typecheck.Types 
import           Language.Nano.Typecheck.Subst
import           Language.Nano.Typecheck.Compare
import           Language.Nano.Liquid.Types


import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Misc
import           Control.Applicative 

import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Error
import           Text.Printf 

import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Parser        (SourceSpan (..))
import           Language.ECMAScript3.PrettyPrint

import qualified Debug.Trace                    as T

-------------------------------------------------------------------------------
-- | Top level type returned after Constraint Generation ----------------------
-------------------------------------------------------------------------------

data CGInfo = CGI { cgi_finfo :: F.FInfo Cinfo
                  , cgi_annot :: A.AnnInfo RefType  
                  }

-- Dump the refinement subtyping constraints
instance PP CGInfo where
  pp (CGI finfo _) = cat (map pp (M.elems $ F.cm finfo))

instance PP (F.SubC c) where
  pp s = pp (F.lhsCs s) <+> text " <: " <+> pp (F.rhsCs s)



-------------------------------------------------------------------------------
getCGInfo        :: Config -> NanoRefType -> CGM a -> CGInfo  
-------------------------------------------------------------------------------
getCGInfo cfg pgm = cgStateCInfo pgm . execute cfg pgm . (>> fixCWs)
  where 
    fixCWs       = (,) <$> fixCs <*> fixWs
    fixCs        = concatMapM splitC . cs =<< get 
    fixWs        = concatMapM splitW . ws =<< get

execute :: Config -> Nano AnnType RefType -> CGM a -> (a, CGState)
execute cfg pgm act
  = case runState (runErrorT act) $ initState cfg pgm of 
      (Left err, _) -> errorstar err
      (Right x, st) -> (x, st)  

initState       :: Config -> Nano AnnType RefType -> CGState
initState c pgm = CGS F.emptyBindEnv (defs pgm) (tDefs pgm) [] [] 0 mempty pgm invs c 
  where 
    invs        = M.fromList [(tc, t) | t@(Loc _ (TApp tc _ _)) <- invts pgm]  

getDefType f 
  = do m <- cg_defs <$> get
       maybe err return $ E.envFindTy f m 
    where 
       err = cgError l $ errorMissingSpec l f
       l   = srcPos f

-- cgStateFInfo :: Nano a1 (RType F.Reft)-> (([F.SubC Cinfo], [F.WfC Cinfo]), CGState) -> CGInfo
cgStateCInfo pgm ((fcs, fws), cg) = CGI fi (cg_ann cg)
  where 
    fi   = F.FI { F.cm    = M.fromList $ F.addIds fcs  
                , F.ws    = fws
                , F.bs    = binds cg
                , F.gs    = measureEnv pgm 
                , F.lits  = []
                , F.kuts  = F.ksEmpty
                , F.quals = quals pgm 
                }

---------------------------------------------------------------------------------------
getTDefs :: CGM (E.Env RefType)
---------------------------------------------------------------------------------------
getTDefs  = cg_tdefs <$> get


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
        , cg_tdefs :: !(E.Env RefType)     -- ^ type definitions
        , cs       :: ![SubC]              -- ^ subtyping constraints
        , ws       :: ![WfC]               -- ^ well-formedness constraints
        , count    :: !Integer             -- ^ freshness counter
        , cg_ann   :: A.AnnInfo RefType    -- ^ recorded annotations
        , pgm      :: Nano AnnType RefType -- ^ the program
        , invs     :: TConInv              -- ^ type constructor invariants
        , cg_opts  :: Config               -- ^ configuration options
        }

type CGM     = ErrorT String (State CGState)

type TConInv = M.HashMap TCon (Located RefType)

---------------------------------------------------------------------------------------
cgError :: (IsLocated l) => l -> String -> CGM a 
---------------------------------------------------------------------------------------
cgError l msg = throwError $ printf "CG-ERROR at %s : %s" (ppshow $ srcPos l) msg


---------------------------------------------------------------------------------------
-- | Environment API ------------------------------------------------------------------
---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------
envAddFresh :: (IsLocated l) => l -> RefType -> CGEnv -> CGM (Id l, CGEnv) 
---------------------------------------------------------------------------------------
envAddFresh l t g 
  = do x  <- {- tracePP ("envAddFresh " ++ ppshow t) <$> -} freshId l
       g' <- envAdds [(x, t)] g
       return (x, g')

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
    err       = errorstar $ bugUnboundVariable (srcPos x) (F.symbol x)

---------------------------------------------------------------------------------------
envFindReturn :: CGEnv -> RefType 
---------------------------------------------------------------------------------------
envFindReturn = E.envFindReturn . renv


----------------------------------------------------------------------------------
envJoin :: AnnType -> CGEnv -> Maybe CGEnv -> Maybe CGEnv -> CGM (Maybe CGEnv)
----------------------------------------------------------------------------------
envJoin _ _ Nothing x           = return x
envJoin _ _ x Nothing           = return x
envJoin l g (Just g1) (Just g2) = Just <$> envJoin' l g g1 g2 

----------------------------------------------------------------------------------
envJoin' :: AnnType -> CGEnv -> CGEnv -> CGEnv -> CGM CGEnv
----------------------------------------------------------------------------------

-- HINT: 1. use @envFindTy@ to get types for the phi-var x in environments g1 AND g2
--       2. use @freshTyPhis@ to generate fresh types (and an extended environment with 
--          the fresh-type bindings) for all the phi-vars using the unrefined types 
--          from step 1.
--       3. generate subtyping constraints between the types from step 1 and the fresh types
--       4. return the extended environment.

envJoin' l g g1 g2
  = do  {- td      <- E.envMap toType <$> cg_tdefs <$> get -}
        let xs   = [x | PhiVar x <- ann_fact l] 
            t1s  = (`envFindTy` g1) <$> xs 
            t2s  = (`envFindTy` g2) <$> xs
        when (length t1s /= length t2s) $ cgError l (bugBadPhi l t1s t2s)

        γ       <- getTDefs
        let t4   = zipWith (compareTs γ) t1s t2s


        (g',ts) <- freshTyPhis (srcPos l) g xs $ toType <$> fst4 <$> t4
        -- To facilitate the sort check t1s and t2s need to change to their
        -- equivalents that have the same sort with the joined types (ts) (with
        -- the added False's to make the types equivalent
        envAdds (zip xs $ snd4 <$> t4) g1 
        envAdds (zip xs $ thd4 <$> t4) g2
        subTypes l g1 xs (tracePP "After JOIN" ts)
        subTypes l g2 xs ts
        return g'


---------------------------------------------------------------------------------------
-- | Fresh Templates ------------------------------------------------------------------
---------------------------------------------------------------------------------------

-- | Instantiate Fresh Type (at Function-site)
---------------------------------------------------------------------------------------
freshTyFun :: (IsLocated l) => CGEnv -> l -> Id AnnType -> RefType -> CGM RefType 
---------------------------------------------------------------------------------------
freshTyFun g l f t = freshTyFun' g l f t . kVarInst . cg_opts =<< get  

freshTyFun' g l _ t b
  | b && isTrivialRefType t = freshTy "freshTyFun" (toType t) >>= wellFormed l g 
  | otherwise               = return t

-- | Instantiate Fresh Type (at Call-site)
---------------------------------------------------------------------------------------
-- freshTyInst :: (IsLocated l) => l -> CGEnv -> [TVar] -> [Type] -> RefType -> CGM RefType 
freshTyInst :: AnnType -> CGEnv -> [TVar] -> [Type] -> RefType -> CGM RefType 
---------------------------------------------------------------------------------------
freshTyInst l g αs τs tbody
  = do ts    <- mapM (freshTy "freshTyInst") τs
       _     <- mapM (wellFormed l g) ts
       let θ  = fromList $ zip αs ts
       return $  {- tracePP msg $ -} apply θ tbody
    {-where-}
    {-   msg = printf "freshTyInst αs=%s τs=%s: " (ppshow αs) (ppshow τs)-}

-- | Instantiate Fresh Type (at Phi-site) 
---------------------------------------------------------------------------------------
freshTyPhis :: (PP l, IsLocated l) => l -> CGEnv -> [Id l] -> [Type] -> CGM (CGEnv, [RefType])  
---------------------------------------------------------------------------------------
freshTyPhis l g xs τs 
  = do ts <- mapM    (freshTy "freshTyPhis")  ({-tracePP "From" -} τs)
       g' <- envAdds (safeZip "freshTyPhis" xs ts) g
       _  <- mapM    (wellFormed l g') ts
       return (g', ts)


---------------------------------------------------------------------------------------
-- | Adding Subtyping Constraints -----------------------------------------------------
---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------
subTypes :: (IsLocated x, F.Expression x, F.Symbolic x) 
         => AnnType -> CGEnv -> [x] -> [RefType] -> CGM ()
---------------------------------------------------------------------------------------
subTypes l g xs ts = zipWithM_ (subType l g) [envFindTy x g | x <- xs] ts


---------------------------------------------------------------------------------------
subType :: AnnType -> CGEnv -> RefType -> RefType -> CGM ()
---------------------------------------------------------------------------------------
subType l g t1 t2 =
  do tt1   <- addInvariant t1
     tt2   <- addInvariant t2
     -- Removing bkTypes from here:
     -- With the addition of upcasts the type sorts
     -- should be aligned and compatible so just do a check here
     let s  = checkTypes tt1 tt2
     modify $ \st -> st {cs = c s : (cs st)}
  where
    c      = uncurry $ Sub g (ci l)
    checkTypes t1 t2 | toType t1 == toType t2 = (t1,t2)
    checkTypes t1 t2 | otherwise              = 
      errorstar (printf "CGMonad: checkTypes not aligned: \n%s\nwith\n%s"
                (ppshow t1) (ppshow t2))


---------------------------------------------------------------------------------------
noUnion :: (F.Reftable r) => RType r -> Bool
---------------------------------------------------------------------------------------
noUnion (TApp TUn _ _)  = False
noUnion (TApp _  rs _)  = and $ map noUnion rs
noUnion (TFun bs rt _)  = and $ map noUnion $ rt : (map b_type bs)
noUnion (TObj bs    _)  = and $ map noUnion $ map b_type bs
noUnion (TBd  _      )  = error "noUnion: cannot have TBodies here"
noUnion (TAll _ t    )  = noUnion t
noUnion _               = True

unionCheck t | noUnion t = t 
unionCheck t | otherwise = error $ printf "%s found. Cannot have unions." $ ppshow t


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

freshId   :: (IsLocated l) => l -> CGM (Id l)
freshId l = Id l <$> fresh

freshTy     :: (Show a) => a -> Type -> CGM RefType 
freshTy _ τ = refresh $ rType τ

instance Freshable F.Refa where
  fresh = (`F.RKvar` F.emptySubst) <$> (F.intKvar <$> fresh)

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
splitC = splitC'

-- RJ splitC c = bkTypesC c >>= concatMapM splitC'
-- RJ 
-- RJ bkTypesC c@(Sub g i t1 t2)
-- RJ   = do t1'   <- addInvariant t1
-- RJ        t2'   <- addInvariant t2
-- RJ        s     <- bkTypesM (t1, t2)
-- RJ        return   [Sub g i t1' t2' | (t1', t2') <- s] 

---------------------------------------------------------------------------------------
-- | Function types
---------------------------------------------------------------------------------------
splitC' (Sub g i tf1@(TFun xt1s t1 _) tf2@(TFun xt2s t2 _))
  = do let bcs    = bsplitC g i tf1 tf2
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
  = return $ bsplitC g i t1 t2
  | otherwise
  = errorstar "UNEXPECTED CRASH in splitC"

---------------------------------------------------------------------------------------
-- | Unions:
-- We need to get the bsplitC for the top level refinements 
-- Nothing more should be added, the internal subtyping constrains have been
-- dealt with separately
---------------------------------------------------------------------------------------
splitC' (Sub g i t1@(TApp TUn _ _) t2@(TApp TUn _ _)) 
  | toType t1 == toType t2 = return    $ bsplitC g i t1 t2
  | otherwise              = errorstar $ printf "Unions in splitC: %s - %s" (ppshow t1) (ppshow t2)

splitC' (Sub _ _ t1@(TApp TUn _ _) t2) = 
  errorstar $ printf "Unions in splitC: %s - %s" (ppshow t1) (ppshow t2)
splitC' (Sub _ _ t1 t2@(TApp TUn _ _)) = 
  errorstar $ printf "Unions in splitC: %s - %s" (ppshow t1) (ppshow t2)

---------------------------------------------------------------------------------------
-- | Type definitions
---------------------------------------------------------------------------------------
splitC' (Sub g i t1@(TApp d1@(TDef _) t1s _) t2@(TApp d2@(TDef _) t2s _)) | d1 == d2
  = do  let cs = bsplitC g i t1 t2
        -- constructor parameters are covariant
        cs'   <- concatMapM splitC $ safeZipWith "splitcTDef" (Sub g i) t1s t2s
        return $ cs ++ cs' 

splitC' (Sub _ _ (TApp (TDef _) _ _) (TApp (TDef _) _ _))
  = errorstar "Unimplemented: Check type definition cycles"
  
splitC' (Sub g i t1@(TApp (TDef _) _ _ ) t2)  
  = errorstar $ printf "splitC - should have been broken down earlier:\n%s <: %s" 
            (ppshow t1) (ppshow t2)

splitC' (Sub g i t1 t2@(TApp (TDef _) _ _ ))
  = errorstar $ printf "splitC - should have been broken down earlier:\n%s <: %s" 
            (ppshow t1) (ppshow t2)

---------------------------------------------------------------------------------------
-- | Rest of TApp
---------------------------------------------------------------------------------------
splitC' (Sub g i t1@(TApp _ t1s _) t2@(TApp _ t2s _))
  = do let cs = bsplitC g i t1 t2
       cs'   <- concatMapM splitC $ safeZipWith 
                                    (printf "splitC4: %s - %s" (ppshow t1) (ppshow t2)) 
                                    (Sub g i) t1s t2s
       return $ cs ++ cs'

---------------------------------------------------------------------------------------
-- | Objects
-- Only empty objects (top-level) should reach this far                               
---------------------------------------------------------------------------------------
splitC' (Sub g i tf1@(TObj [] _ ) tf2@(TObj [] _ ))
  = return $ bsplitC g i tf1 tf2

splitC' (Sub g i t1 t2@(TObj _ _ ))
  = error $ printf "splitC - should have been broken down earlier:\n%s <: %s" 
            (ppshow t1) (ppshow t2)

splitC' (Sub g i t1@(TObj _ _ ) t2)
  = error $ printf "splitC - should have been broken down earlier:\n%s <: %s" 
            (ppshow t1) (ppshow t2)


splitC' x 
  = cgError (srcPos x) $ bugBadSubtypes x 


bsplitC g ci t1 t2
  | F.isFunctionSortedReft r1 && F.isNonTrivialSortedReft r2
  = [F.subC (fenv g) F.PTrue (r1 {F.sr_reft = F.top}) r2 Nothing [] ci]
  | F.isNonTrivialSortedReft r2
  = [F.subC (fenv g) p r1 r2 Nothing [] ci]
  | otherwise
  = {- tracePP "bsplitC trivial" -} []
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
splitW (W g i (TFun ts t _)) 
  = do let bws = bsplitW g t i
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

splitW (W g i (TObj ts _ ))
  = do  g' <- envTyAdds i ts g
        concatMapM splitW [W g' i ti | B _ ti <- ts]

splitW (W _ _ _ ) = error "Not supported in splitW"

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

