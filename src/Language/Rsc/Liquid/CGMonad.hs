{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DoAndIfThenElse           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

-- | Operations pertaining to Constraint Generation

module Language.Rsc.Liquid.CGMonad (

  -- * Constraint Generation Monad
    CGM
  , cgTickAST
  , getCgBinds
  , getCgRevBinds
  , setCgBinds
  , setCgRevBinds
  , getCgOpts
  , getCgInvs
  , getCons
  , getWFCons

  , execute

  -- * Constraint Information
  , CGInfo (..)
  , cgStateCInfo


  -- * Throw Errors
  , cgError

  -- * Freshable
  , Freshable (..)

  , resolveTypeM

  , envAddGuard, envPopGuard

  -- * Add Subtyping Constraints
  , subType, wellFormed -- , safeExtends

  -- * Add Type Annotations
  , addAnnot

  -- * Function Types
  , cgFunTys, substNoCapture

  , unqualifyThis

  ) where

import           Control.Arrow                   ((***))
import           Control.Exception               (throw)
import           Control.Monad.State
import           Control.Monad.Trans.Except
import qualified Data.HashMap.Strict             as HM
import qualified Data.List                       as L
import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Types         as F
import           Language.Fixpoint.Types.Errors
import           Language.Fixpoint.Types.Names   (symbolString)
import           Language.Fixpoint.Types.Visitor (SymConsts (..))
import qualified Language.Fixpoint.Types.Visitor as V
import           Language.Rsc.Annotations
import           Language.Rsc.CmdLine
import           Language.Rsc.Constraints
import           Language.Rsc.Core.Env
import           Language.Rsc.Environment
import           Language.Rsc.Errors
import           Language.Rsc.Liquid.Qualifiers
import           Language.Rsc.Liquid.Refinements
import           Language.Rsc.Locations
import           Language.Rsc.Names
import           Language.Rsc.Pretty
import           Language.Rsc.Program
import           Language.Rsc.Symbols
import qualified Language.Rsc.SystemUtils        as S
import           Language.Rsc.Transformations
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
import           Language.Rsc.TypeUtilities
import           Text.PrettyPrint.HughesPJ


--------------------------------------------------------------------------------
-- | Top level type returned after Constraint Generation
--------------------------------------------------------------------------------

data CGInfo = CGI { cgi_finfo :: F.FInfo Cinfo
                  , cgi_annot :: S.UAnnInfo RefType
                  }

-- Dump the refinement subtyping constraints
instance PP CGInfo where
  pp (CGI finfo _) = cat (map pp (HM.elems $ F.cm finfo))


--------------------------------------------------------------------------------
-- | Constraint Generation Monad
--------------------------------------------------------------------------------

data CGState = CGS {
  --
  -- ^ global list of fixpoint binders
  --
    cg_binds     :: F.BindEnv

  --
  -- ^ reverse mapping of fixpoint symbols
  --   (to ensure unique bindings)
  --
  , cg_rev_binds :: F.SEnv F.BindId
  --
  -- ^ subtyping constraints
  --
  , cg_cs        :: ![SubC]
  --
  -- ^ well-formedness constraints
  --
  , cg_ws        :: ![WfC]
  --
  -- ^ freshness counter
  --
  , cg_cnt       :: !Integer
  --
  -- ^ recorded annotations
  --
  , cg_ann       :: S.UAnnInfo RefType
  --
  -- ^ type constructor invariants
  --
  , cg_invs      :: TConInv
  --
  -- ^ configuration options
  --
  , cg_opts      :: Config
  --
  -- ^ AST Counter
  --
  , cg_ast_cnt   :: NodeId

  }

-- | Aliases
--
type CGM     = ExceptT Error (State CGState)
type TConInv = HM.HashMap TPrim (Located RefType)


cgTickAST = do
  n     <- cg_ast_cnt <$> get
  modify $ \st -> st {cg_ast_cnt = 1 + n}
  return $ n

getCgBinds    = cg_binds     <$> get
getCgRevBinds = cg_rev_binds <$> get
setCgBinds    b = modify $ \st -> st { cg_binds = b }
setCgRevBinds b = modify $ \st -> st { cg_rev_binds = b }
getCgOpts     = cg_opts <$> get
getCgInvs     = cg_invs <$> get
getCons       = cg_cs   <$> get
getWFCons     = cg_ws   <$> get


--------------------------------------------------------------------------------
execute :: Config -> RefScript -> CGM a -> (a, CGState)
--------------------------------------------------------------------------------
execute cfg pgm act
  = case runState (runExceptT act) $ initState cfg pgm of
      (Left e, _)   -> throw e
      (Right x, st) -> (x, st)

--------------------------------------------------------------------------------
initState :: Config -> RefScript -> CGState
--------------------------------------------------------------------------------
initState c p = CGS F.emptyBindEnv F.emptySEnv [] [] 0 mempty invars c (maxId p)
  where
    invars    = HM.fromList [(pr, t) | t@(Loc _ (TPrim pr _)) <- invts p]

--------------------------------------------------------------------------------
cgStateCInfo :: FilePath -> RefScript -> (([F.SubC Cinfo], [F.WfC Cinfo]), CGState) -> CGInfo
--------------------------------------------------------------------------------
cgStateCInfo f pgm ((fcs, fws), cg) = CGI finfo (cg_ann cg)
  where
    quals    = pQuals pgm ++ scrapeQuals pgm
    finfo    = F.fi fcs fws bs lits mempty quals mempty f False
    bs       = cg_binds cg
    lits     = lits1 `mappend` lits2
    lits1    = F.sr_sort <$> measureEnv pgm
    lits2    = cgLits bs fcs

cgLits :: F.BindEnv -> [F.SubC a] -> F.SEnv F.Sort
cgLits bs cs = F.fromListSEnv cts
  where
    cts      = [ (F.symbol c, F.strSort) | c <- csLits ++ bsLits ]
    csLits   = concatMap symConsts cs
    bsLits   = symConsts bs


-- | Get binding from object type

--------------------------------------------------------------------------------
measureEnv :: RefScript -> F.SEnv F.SortedReft
--------------------------------------------------------------------------------
measureEnv = fmap rTypeSortedReft . envSEnv . consts

--------------------------------------------------------------------------------
cgError :: Error -> CGM b
--------------------------------------------------------------------------------
cgError = throwE

--------------------------------------------------------------------------------
-- | Environment API
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
addAnnot       :: (IsLocated l, F.Symbolic x) => l -> x -> RefType -> CGM ()
--------------------------------------------------------------------------------
addAnnot l x t = modify $ \st -> st {cg_ann = S.addAnnot (srcPos l) x t (cg_ann st)}

--------------------------------------------------------------------------------
envAddGuard       :: (F.Symbolic x, IsLocated x) => x -> Bool -> CGEnv -> CGEnv
--------------------------------------------------------------------------------
envAddGuard x b g = g { cge_guards = guard b x : cge_guards g }
  where
    guard True    = F.eProp
    guard False   = F.PNot . F.eProp

--------------------------------------------------------------------------------
envPopGuard       :: CGEnv -> CGEnv
--------------------------------------------------------------------------------
envPopGuard g = g { cge_guards = grdPop $ cge_guards g }
  where
    grdPop (_:xs) = xs
    grdPop []     = []


instance F.Expression a => F.Expression (Located a) where
  expr (Loc _ a) = F.expr a

instance F.Expression TVar where
  expr (TV a _) = F.expr a


--------------------------------------------------------------------------------
-- | Adding Subtyping Constraints
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
subType :: AnnLq -> Maybe Error -> CGEnv -> RefType -> RefType -> CGM ()
--------------------------------------------------------------------------------
subType l (Just err) g t1 t2
  = modify $ \st -> st { cg_cs = Sub g (ci err l) t1 t2 : cg_cs st }

subType l Nothing g t1 t2
  = subType l (Just (mkErr l msg)) g t1 t2
  where
    msg = text "Liquid Type Error" $+$
          text "LHS" $+$ nest 2 (pp t1) $+$
          text "RHS" $+$ nest 2 (pp t2)


-- TODO: KVar subst
--
instance F.Subable a => F.Subable (Env a) where
  substa f = envFromList . map ((***) (F.substa f . F.symbol) (F.substa f)) . envToList
  substf f = envFromList . map ((***) (F.substf f . F.symbol) (F.substf f)) . envToList
  subst su = envFromList . map ((***) (F.subst su . F.symbol) (F.subst su)) . envToList
  syms x   = concat [ F.syms (F.symbol x) ++ F.syms t | (x, t) <- envToList x ]

instance (PP r, F.Reftable r, F.Subable r) => F.Subable (SymInfo r) where
  substa f (SI x l a t) = SI x l a $ F.substa f t
  substf f (SI x l a t) = SI x l a $ F.substf f t
  subst su (SI x l a t) = SI x l a $ F.subst su t
  syms     (SI _ _ _ t) = F.syms t


-- errorLiquid l g t1 t2         = mkErr k $ printf "Liquid Type Error" where k = srcPos l


-- TODO: Restore this check !!!
-- --------------------------------------------------------------------------------
-- safeExtends :: SrcSpan -> CGEnv -> IfaceDef F.Reft -> CGM ()
-- --------------------------------------------------------------------------------
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
  = do modify $ \st -> st { cg_ws = (W g (ci err l) t) : cg_ws st }
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
  fresh = F.tempSymbol (F.symbol "rsc") <$> fresh

instance Freshable String where
  fresh = symbolString <$> fresh

-- OLD CODE -- instance Freshable F.Refa where
-- OLD CODE --   fresh = F.Refa . (`F.PKVar` mempty) . F.intKvar <$> fresh
-- OLD CODE --
-- OLD CODE -- instance Freshable [F.Refa] where
-- OLD CODE --   fresh = single <$> fresh

instance Freshable F.Expr where
  fresh  = kv <$> fresh
    where
      kv = (`F.PKVar` mempty) . F.intKvar

instance Freshable F.Reft where
  fresh                  = errorstar "fresh F.Reft"
  true    (F.Reft (v,_)) = return $ F.Reft (v, mempty)
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
cgFunTys :: (IsLocated l, F.Symbolic b, PP x, PP [b])
         => l -> x -> [b] -> RefType -> CGM [IOverloadSig F.Reft]
--------------------------------------------------------------------------------
cgFunTys l f xs ft   | Just ts <- bkFuns ft
                     = zip [0..] <$> mapM fTy (concatMap expandOpts ts)
                     | otherwise
                     = cgError $ errorNonFunction (srcPos l) f ft
  where
    fTy (αs, yts, t) | Just yts' <- padUndefineds xs yts
                     = uncurry (αs,,) <$> substNoCapture xs (yts', t)
                     | otherwise
                     = cgError $ errorArgMismatch (srcPos l) f ft (length yts) (length xs)

-- | `substNoCapture xs (yts,t)` substitutes formal parameters in `yts` with
--   actual parameters passed as bindings in `xs`.
--
--   Avoids capture of function binders by existing value variables
--------------------------------------------------------------------------------
substNoCapture :: F.Symbolic a => [a] -> ([Bind F.Reft], RefType) -> CGM ([Bind F.Reft], RefType)
--------------------------------------------------------------------------------
substNoCapture xs (yts, rt)
  | length yts /= length xs
  = error "substNoCapture - length test failed"
  | otherwise
  = (,) <$> mapM (onT (mapReftM ff)) yts <*> mapReftM ff rt
  where
    -- If the symbols we are about to introduce are already captured by some vv:
    -- (1) create a fresh vv., and
    -- (2) proceed with the substitution after replacing with the new vv.
    ff r@(F.Reft (v,ras)) | v `L.elem` xss
                          = do v' <- freshVV
                               return $ F.subst su $ F.Reft . (v',)
                                      $ F.subst (F.mkSubst [(v, F.expr v')]) ras
                          | otherwise
                          = return $ F.subst su r
    freshVV               = F.vv . Just <$> fresh
    xss                   = F.symbol <$> xs
    su                    = F.mkSubst $ safeZipWith "substNoCapture" fSub yts xs
    fSub                  = curry $ (***) b_sym F.eVar
    onT f (B s o t)       = B s o <$> f t


-- | Substitute occurences of `offset(this, "f")` in type @t'@, with 'f'
--------------------------------------------------------------------------------
unqualifyThis :: RefType -> RefType
--------------------------------------------------------------------------------
unqualifyThis = emapReft (\_ -> V.trans vis () ()) []
  where
    vis      :: V.Visitor () ()
    vis       = V.defaultVisitor { V.txExpr = tExpr }
    tExpr _ (F.splitEApp -> (F.EVar o , [F.EVar th, F.ESym (F.SL c)]))
              | o == offsetSym, th == thisSym = F.eVar c
    tExpr _ e = e



-- Local Variables:
-- flycheck-disabled-checkers: (haskell-liquid)
-- End:
