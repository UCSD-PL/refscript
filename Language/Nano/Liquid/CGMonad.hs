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

  -- * Get binding
  , getBindingM

  -- * Throw Errors
  , cgError      

  -- * Fresh Templates for Unknown Refinement Types 
  , freshTyFun
  , freshTyInst
  , freshTyPhis
  , freshTyCast

  -- * Freshable
  , Freshable (..)

  -- * Environment API
  , envAddFresh
  , envAdds
  , envAddReturn
  , envAddGuard
  , envFindTy
  , envToList
  , envFindReturn
  , envJoin

  -- * Add Subtyping Constraints
  , subTypes
  , subType
  , subTypeContainers
  , subTypesContainers

  , addInvariant
  
  -- * Add Type Annotations
  , addAnnot

  -- * Unfolding
  , unfoldSafeCG, unfoldFirstCG

  -- * Environment sort check
  , fixBase, fixEnv
  , fixUpcast
  ) where

import           Data.Maybe                     (fromMaybe, maybeToList, catMaybes, isJust)
import qualified Data.List                      as L
import           Data.Monoid                    (mempty)
import qualified Data.HashMap.Strict            as M
import qualified Data.HashSet                   as S
import qualified Data.Graph                     as G
import qualified Data.Tree                      as T

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

-- import           Debug.Trace                        (trace)

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
getCGInfo :: Config -> Nano AnnTypeR RefType -> CGM a -> CGInfo
-------------------------------------------------------------------------------
getCGInfo cfg pgm = cgStateCInfo pgm . execute cfg pgm . (>> fixCWs)
  where 
    fixCWs       = (,) <$> fixCs <*> fixWs
    fixCs        = concatMapM splitC . cs =<< get 
    fixWs        = concatMapM splitW . ws =<< get

execute :: Config -> Nano AnnTypeR RefType -> CGM a -> (a, CGState)
execute cfg pgm act
  = case runState (runErrorT act) $ initState cfg pgm of 
      (Left err, _) -> errorstar err
      (Right x, st) -> (x, st)  

initState       :: Config -> Nano AnnTypeR RefType -> CGState
initState c pgm = CGS F.emptyBindEnv (defs pgm) (tDefs pgm) [] [] 0 mempty invs glbs c 
  where 
    invs        = M.fromList [(tc, t) | t@(Loc _ (TApp tc _ _)) <- invts pgm]  
    glbs        = S.fromList [s       |   (Loc _ s)             <- globs pgm]  

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

getBindingM i t 
  = do  td <- cg_tdefs <$> get
        return $ getBinding td i t 


-- | Get binding from object type
---------------------------------------------------------------------------------
getBinding :: (PP r, F.Reftable r) => E.Env (RType r) -> Id a -> RType r -> Either String (RType r)
---------------------------------------------------------------------------------
getBinding _ i (TObj bs _ ) = 
  case L.find (\s -> F.symbol i == b_sym s) bs of
    Just b      -> Right $ b_type b
    _           -> Left  $ errorObjectBinding
getBinding defs i t@(TApp (TDef _) _ _) = 
  case unfoldMaybe defs t of
    Right t'    -> getBinding defs i t'
    Left  s     -> Left $ s ++ "\nand\n" ++ errorObjectTAccess t
getBinding _ _ t = Left $ errorObjectTAccess t



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
        , invs     :: TConInv              -- ^ type constructor invariants
        , glbs     :: TGlobs               -- ^ predicate symbols that can be lifted to supertypes
        , cg_opts  :: Config               -- ^ configuration options
        }

type CGM     = ErrorT String (State CGState)

type TConInv = M.HashMap TCon (Located RefType)

type TGlobs  = S.HashSet F.Symbol

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
    -- Falsy values:
    -- ∙ false
    -- ∙ 0 (zero)
    -- ∙ "" (empty string)
    -- ∙ null
    -- ∙ undefined
    -- ∙ NaN (Not a number)

    -- guard True  v = F.eProp v
    -- guard False v = F.pOr [ F.PNot (F.eProp v) 
    --                         
    --                      ] 
    --   where
    --     vEqX x    = F.PAtom F.Eq (F.eVar v) x
                           
                    
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
envFindTy'     :: (F.Symbolic x, F.Expression x) => x -> CGEnv -> RefType 
---------------------------------------------------------------------------------------
envFindTy' x g = (`eSingleton` x) $ fromMaybe err $ E.envFindTy x $ renv g
  where 
    err       = errorstar $ bugUnboundVariable dummySpan (F.symbol x)


---------------------------------------------------------------------------------------
envToList     ::  CGEnv -> [(Id SourceSpan, RefType)]
---------------------------------------------------------------------------------------
envToList g = E.envToList $ renv g


---------------------------------------------------------------------------------------
envFindReturn :: CGEnv -> RefType 
---------------------------------------------------------------------------------------
envFindReturn = E.envFindReturn . renv


----------------------------------------------------------------------------------
envJoin :: AnnTypeR -> CGEnv -> Maybe CGEnv -> Maybe CGEnv -> CGM (Maybe CGEnv)
----------------------------------------------------------------------------------
envJoin _ _ Nothing x           = return x
envJoin _ _ x Nothing           = return x
envJoin l g (Just g1) (Just g2) = Just <$> envJoin' l g g1 g2 

----------------------------------------------------------------------------------
envJoin' :: AnnTypeR -> CGEnv -> CGEnv -> CGEnv -> CGM CGEnv
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
        subTypes l g1 xs ts
        subTypes l g2 xs ts
        return g'


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

-- | Instantiate Fresh Type (at Call-site)

---------------------------------------------------------------------------------------
-- freshTyInst :: (IsLocated l) => l -> CGEnv -> [TVar] -> [Type] -> RefType -> CGM RefType 
-- freshTyInst :: AnnTypeR -> CGEnv -> [TVar] -> [Type] -> RefType -> CGM RefType 
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

-- | Instantiate Fresh Type (at Cast-site)
---------------------------------------------------------------------------------------
freshTyCast :: (PP l, IsLocated l) => l -> CGEnv -> Id l -> RefType -> CGM (CGEnv, RefType)  
---------------------------------------------------------------------------------------
freshTyCast l g x τ
  = do t  <- freshTy "freshTyCast" τ
       g' <- envAdds [(x, t)] g
       _  <- wellFormed l g t
       return (g', t)



---------------------------------------------------------------------------------------
-- | Adding Subtyping Constraints -----------------------------------------------------
---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------
subTypes :: (IsLocated x, F.Expression x, F.Symbolic x) 
         => AnnTypeR -> CGEnv -> [x] -> [RefType] -> CGM ()
---------------------------------------------------------------------------------------
subTypes l g xs ts = zipWithM_ (subType l g) [envFindTy x g | x <- xs] ts


---------------------------------------------------------------------------------------
subType :: AnnTypeR -> CGEnv -> RefType -> RefType -> CGM ()
---------------------------------------------------------------------------------------
subType l g t1 t2 =
  do tt1   <- addInvariant {- $ tracePP "Liquid:subtype t1" -} t1
     tt2   <- addInvariant {- $ tracePP "Liquid:subtype t2" -} t2
-- XXX: Are the invariants added for types nested in containers (i.e. unions and
-- objects)? Probably not, so this process should be done after the splitC.
     tdefs <- getTDefs
     let s  = checkTypes tdefs 
              ({- trace ("subTypes: " ++ ppshow tt1 ++ " - " ++ ppshow tt2 ++ "\n")-} tt1) tt2
     modify $ \st -> st {cs = c s : (cs st)}
  where
    c      = uncurry $ Sub g (ci l)
    -- Sort check 
    checkTypes tdefs t1 t2 | equivWUnions tdefs t1 t2 = (t1,t2)
    checkTypes  _ t1 t2    | otherwise                   =
      errorstar (printf "[%s]\nCGMonad: checkTypes not aligned: \n%s\nwith\n%s"
                (ppshow $ ann l) (ppshow $ toType t1) (ppshow $ toType t2))

-------------------------------------------------------------------------------
equivWUnions :: E.Env RefType -> RefType -> RefType -> Bool
-------------------------------------------------------------------------------
equivWUnions γ t1@(TApp TUn _ _) t2@(TApp TUn _ _) = 
  case unionPartsWithEq (equiv γ) t1 t2 of 
    (ts,[],[])  -> and $ uncurry (safeZipWith "equivWUnions" $ equivWUnions γ) (unzip ts)
    _           -> False
equivWUnions γ t t' = equiv γ t t'

equivWUnionsM t t' = getTDefs >>= \γ -> return $ equivWUnions γ t t'


-- | Subtyping container contents: unions, objects. Includes top-level

-- Need to be padded and so on...
-------------------------------------------------------------------------------
subTypeContainers :: AnnTypeR -> CGEnv -> RefType -> RefType -> CGM ()
-------------------------------------------------------------------------------
-- XXX: Will loop infinitely for cycles in type definitions
subTypeContainers l g (TApp d@(TDef _) ts _) (TApp d'@(TDef _) ts' _) | d == d' = 
  mapM_ (uncurry $ subTypeContainers l g) $ zip ts ts'

subTypeContainers l g t1 t2@(TApp (TDef _) _ _ ) = 
  unfoldSafeCG t2 >>= \t2' -> subTypeContainers l g t1 t2'

subTypeContainers l g t1@(TApp (TDef _) _ _ ) t2 = 
  unfoldSafeCG t1 >>= \t1' -> subTypeContainers l g t1' t2

subTypeContainers l g t1@(TApp TUn _ _) t2@(TApp TUn _ _) = 
  do  γ <- getTDefs
      case unionParts γ t1 t2 of
        (ts, [], []) -> mapM_ (uncurry $ subTypeContainers l g) ts
        _            -> errorstar "subTypeContainers: Unions non-matchable"
      subType l g t1 t2   -- top-level

subTypeContainers l g t1@(TObj _ _) t2@(TObj _ _) =
  -- TODO: might need to match like the union case
  do  mapM_ (uncurry $ subTypeContainers l g) $ bkPaddedObject t1 t2
      subType l g t1 t2   -- top-level

subTypeContainers l g t1 t2 = subType l g t1 t2


---------------------------------------------------------------------------------------
subTypesContainers :: (IsLocated x, F.Expression x, F.Symbolic x) 
         => AnnTypeR -> CGEnv -> [x] -> [RefType] -> CGM ()
---------------------------------------------------------------------------------------
subTypesContainers l g xs ts = zipWithM_ (subTypeContainers l g) [envFindTy x g | x <- xs] ts

 

-- | Monadic unfolding
-------------------------------------------------------------------------------
unfoldFirstCG :: RefType -> CGM RefType
-------------------------------------------------------------------------------
unfoldFirstCG t = getTDefs >>= \γ -> return $ unfoldFirst γ t


-------------------------------------------------------------------------------
unfoldSafeCG :: RefType -> CGM RefType
-------------------------------------------------------------------------------
unfoldSafeCG   t = getTDefs >>= \γ -> return $ unfoldSafe γ t


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

-- freshTy     :: (Show a) => a -> Type -> CGM RefType 
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
splitC c@(Sub g i t t') 
  | envSortCheck g 
  = splitC' c
  | otherwise      
  = do  g' <- envSortSanitize g 
        splitC' (tracePP  "SANITIZED" $ Sub g' i t t')

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
splitC' (Sub g i t1@(TApp TUn _ _) t2@(TApp TUn _ _)) =
  ifM (equivWUnionsM t1 t2) 
    (return    $ bsplitC g i t1 t2) 
    (errorstar $ printf "Unequal unions in splitC: %s - %s" (ppshow $ toType t1) (ppshow $ toType t2))

splitC' (Sub _ _ t1@(TApp TUn _ _) t2) = 
  errorstar $ printf "Unions in splitC: %s - %s" (ppshow t1) (ppshow t2)
splitC' (Sub _ _ t1 t2@(TApp TUn _ _)) = 
  errorstar $ printf "Unions in splitC: %s - %s" (ppshow t1) (ppshow t2)

---------------------------------------------------------------------------------------
-- |Type definitions
---------------------------------------------------------------------------------------
splitC' (Sub g i t1@(TApp d1@(TDef _) t1s _) t2@(TApp d2@(TDef _) t2s _)) | d1 == d2
  = do  let cs = bsplitC g i t1 t2
        -- constructor parameters are covariant
        cs'   <- concatMapM splitC $ safeZipWith "splitcTDef" (Sub g i) t1s t2s
        return $ cs ++ cs' 

splitC' (Sub _ _ (TApp (TDef _) _ _) (TApp (TDef _) _ _))
  = errorstar "Unimplemented: Check type definition cycles"
  
splitC' (Sub g i t1@(TApp (TDef _) _ _ ) t2) = 
  unfoldSafeCG t1 >>= \t1' -> splitC' $ Sub g i t1' t2

splitC' (Sub g i  t1 t2@(TApp (TDef _) _ _)) = 
  unfoldSafeCG t2 >>= \t2' -> splitC' $ Sub g i t1 t2'

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
-- Just the top-level constraint will be included here
---------------------------------------------------------------------------------------
splitC' (Sub g i t1@(TObj _ _ ) t2@(TObj _ _ ))
  = return $ bsplitC g i t1 t2

splitC' (Sub _ _ t1 t2@(TObj _ _ ))
  = error $ printf "splitC - should have been broken down earlier:\n%s <: %s" 
            (ppshow t1) (ppshow t2)

splitC' (Sub _ _ t1@(TObj _ _ ) t2)
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





-- fixBase converts:
--                         -----tE-----
-- g, x :: { v: U | r } |- { v: B | p }
--              ^
--            Union
--
-- into:
--
-- g, x :: { v: B | r } |- { v: B | p ∧ (v = x) }
-- --------g'----------    ----------tE'---------

---------------------------------------------------------------------------------------------
fixBase :: (F.Symbolic x, F.Expression x) => CGEnv -> x -> RefType -> CGM (CGEnv, RefType)
---------------------------------------------------------------------------------------------
fixBase g x t =
  do  let t'   = eSingleton t x
      {-let msg  = printf "TE: %s\nWill fix:%s\n"-}
      {-             (ppshow t') (ppshow $ findCCs (F.symbol x) g)-}
      g'      <- fixEnv g (F.symbol $ {- trace msg -} x) t
      return   $ (g', t')

fixBaseG g x t = fst <$> fixBase g x t 

---------------------------------------------------------------------------------------------
fixEnv :: F.Symbolic a => CGEnv -> a -> RType F.Reft -> CGM CGEnv
---------------------------------------------------------------------------------------------
fixEnv g start base = foldM fixX g xs
  where xs          = findCCs (F.symbol start) g
        fixX g x    = envAdds [(x, toT x)] g 
        toT  x      = base `strengthen` rTypeReft (envFindTy' x g)


-- `fixUpcast` compares/patches types @b@ and @u@ and returns their "comparible"
-- version. It also tries to propagate as much of the refinements of the base
-- type to the top-level (union) type.
--
-- b = { v: B | p } -- upcast --> b = { v: U | p' }
-- 
-- where
--
-- u  = { v: U | q }
-- p' = global(p)
-- B    is part of U

---------------------------------------------------------------------------------------------
fixUpcast :: RefType -> RefType -> CGM (RefType, RefType)
---------------------------------------------------------------------------------------------
fixUpcast b u =
  do  γ                   <- cg_tdefs <$> get
      let (_,b', u',_)     = compareTs γ b u
          -- Substitute the v variable
          vv               = rTypeValueVar b
          vv'              = rTypeValueVar b'
          fx v | v == vv   = vv'
          fx v | otherwise = v
          {-msg              = printf "VV(b) = %s, VV(b') = %s\nGlobPreds" (ppshow vv) (ppshow vv')-}
      gs                  <- glbs     <$> get
      maybe (return (b', u')) 
            (\p -> return (F.substa fx $ b' `strengthen` p, u'))
            ({-traceShow msg <$> -} glbPreds gs b)
  where
    glbPreds gs = glbReft gs . rTypeReft


glbReft g (F.Reft (v, rs)) = glb g rs >>= \rs' -> return $ F.Reft (v, rs')

-- Specifies the parts of a refinement that can be propagated to the top-level
-- union refinement. Needed for proving properties when upcasting. 
class Global a where 
  glb :: TGlobs -> a -> Maybe a

instance (Show a, Global a) => Global [a] where
  glb = glbAny

instance Global F.Refa where
  glb g (F.RConc p   ) = F.RConc <$> glb g p
  glb _ (F.RKvar _ _ )       = Nothing

instance Global F.Pred where
   glb _  F.PTrue            = return  $  F.PTrue
   glb _  F.PFalse           = return  $  F.PFalse
   glb g (F.PAnd xs)         = F.PAnd   <$> glb g xs
   glb g (F.POr xs)          = F.POr    <$> glbAll g xs
   glb g (F.PNot x)          = F.PNot   <$> glb g x
   glb g (F.PImp x y)        = liftM2 F.PImp (glb g x) (glb g y)
   glb g (F.PIff x y)        = liftM2 F.PIff (glb g x) (glb g y)
   glb g (F.PBexp e)         = F.PBexp  <$> glb g e
   glb g (F.PAtom b e1 e2)   = liftM2 (F.PAtom b) (glb g e1) (glb g e2)
   glb _  _                  = Nothing

instance Global F.Expr where
  glb _ (F.ESym sc)      = return $ F.ESym sc 
  glb _ (F.ECon cst)     = return $ F.ECon cst
  glb g (F.EVar s)       = F.EVar <$> glb g s
  glb _ (F.ELit _ _ )    = Nothing
  glb g (F.EApp s es)    = glb g s >>= \s' -> return $ F.EApp s' es
  glb g (F.EBin b e1 e2) = liftM2 (F.EBin b) (glb g e1) (glb g e2)
  glb g (F.EIte p e1 e2) = liftM3 F.EIte (glb g p) (glb g e1) (glb g e2)
  glb g (F.ECst e srt)   = glb g e >>= return . (`F.ECst` srt)
  glb _  F.EBot          = return $ F.EBot
          
glbAll = glbList all
glbAny = glbList any

glbList what g xs | what isJust ms = Just $ catMaybes ms
                  | otherwise     = Nothing
                    where ms = map (glb g) xs

instance Global F.Symbol where
  glb g s | s `S.member` g = return s   -- only allow the designated vars
          | otherwise      = Nothing 



---------------------------------------------------------------------------------------------
envSortCheck :: CGEnv -> Bool
---------------------------------------------------------------------------------------------
envSortCheck g = and $ map (check . sorts) $ mkCCs g 
  where
    check elts = length (L.nub elts) < 2
    sorts xs   = rTypeSort . (`envFindTy'` g) <$> xs

-- -- DEBUG
-- envSortCheck g = and $ map (\l -> traceShow "check them" $ check (sorts l)) $ mkCCs g 
--   where
--     check elts = length (L.nub elts) < 2
--     sorts xs   = traceShow "envSortCheck:sorts" $ rTypeSort . (`envFindTy'` g) <$> (tracePP "xs" xs)


---------------------------------------------------------------------------------------------
envSortSanitize :: CGEnv -> CGM CGEnv
---------------------------------------------------------------------------------------------
envSortSanitize g = foldM fixGroup g xs 
  where
    xs         = mkCCs g
    fixGroup g xs | check $ sorts xs = return g
                  | otherwise        = freshBase >>= \b -> foldM (\g x -> fixBaseG g x b) g xs
 
    freshBase  = freshId dummySpan >>= \i -> return $ ofType $ TApp (TDef i) [] ()

    check elts = length (L.nub elts) < 2
    sorts xs   = rTypeSort . (`envFindTy'` g) <$> xs

  
---------------------------------------------------------------------------------------------
findCCs :: F.Symbol -> CGEnv -> [F.Symbol]
---------------------------------------------------------------------------------------------
findCCs x g = concat $ maybeToList $ L.find (x `elem`) $ mkCCs g 

-- Connected componets in the symbols in the graph
-- The CCs need to have the same sort!
---------------------------------------------------------------------------------------------
mkCCs :: CGEnv -> [[F.Symbol]]
---------------------------------------------------------------------------------------------
mkCCs g   = (fst3 . vs <$>) <$> T.flatten <$> G.components gr
  where (gr, vs, _) = mkGraph g

-- Make a graph:
-- ∙ Vertices: the symbols in the environment
-- ∙ Edges   : same sort constraint ("v = x")
---------------------------------------------------------------------------------------------
mkGraph :: CGEnv -> (G.Graph, G.Vertex -> (F.Symbol, F.Symbol, [F.Symbol]), F.Symbol -> Maybe G.Vertex)
---------------------------------------------------------------------------------------------
mkGraph g = G.graphFromEdges $ f <$> envToList g
  where
    f (id, t) = (F.symbol id, F.symbol id, veqx t)

-- XXX: might need to generalize this, e.g. x < y, x = y, etc.   
---------------------------------------------------------------------------------------------
veqx :: F.Reftable r => RType r -> [F.Symbol]
---------------------------------------------------------------------------------------------
veqx t      = L.nub [ x | F.RConc (F.PAtom F.Eq (F.EVar s) (F.EVar x)) <- refas, s == vv ]
  where vv               = rTypeValueVar t
        F.Reft (_,refas) = rTypeReft t




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

