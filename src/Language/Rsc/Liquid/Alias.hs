{-# LANGUAGE FlexibleContexts #-}

module Language.Rsc.Liquid.Alias (expandAliases) where

import           Control.Applicative          ((<$>))
import           Control.Monad
import           Control.Monad.State
import           Data.Generics
import           Data.Maybe
import           Language.Fixpoint.Types.Errors
import qualified Language.Fixpoint.Types      as F
import qualified Language.Fixpoint.Types.Visitor    as V
import           Language.Rsc.Annotations
import           Language.Rsc.Core.Env
import           Language.Rsc.Errors
import           Language.Rsc.Liquid.Types    ()
import           Language.Rsc.Locations
import           Language.Rsc.Names
import           Language.Rsc.Program
import qualified Language.Rsc.Typecheck.Subst as S
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types

import           Language.Rsc.Pretty

-- pe'
-- te' [using pe']
-- tx :: RefType -> RefType p(using pe' and te')
-- lift above to Annot r

expandAliases   :: RelRefScript -> RelRefScript
expandAliases p =  expandCodePred pe'
                $  expandCodeTAlias te'
                $  expandPred pe'
               <$> expandRefType te'
               <$> p'
  where
    p'          = p { pAlias = pe' } {tAlias = te'}
    pe'         = expandPAliasEnv $ pAlias p
    te'         = expandTAliasEnv $ tAlias p

expandCodeTAlias :: TAliasEnv (RTypeQ RK F.Reft) -> RelRefScript -> RelRefScript
expandCodeTAlias te p@(Rsc { code = Src stmts }) = p { code = Src $ (patch <$>) <$> stmts }
  where
    patch (FA i ss f) = FA i ss (expandRefType te <$> f)

expandCodePred :: PAliasEnv -> RelRefScript -> RelRefScript
expandCodePred te p@(Rsc { code = Src stmts }) = p { code = Src $ (patch <$>) <$> stmts }
  where
    patch (FA i ss f) = FA i ss (expandPred te <$> f)


------------------------------------------------------------------------------
-- | One-shot expansion for @PAlias@ -----------------------------------------
------------------------------------------------------------------------------

expandPAliasEnv :: PAliasEnv -> PAliasEnv
expandPAliasEnv pe = solve pe support expandPAlias
  where
    support        = filter (`envMem` pe) . getPApps . al_body

-- OLD syb getPApps       :: F.Pred -> [F.Symbol]
-- OLD syb getPApps p     = everything (++) ([] `mkQ` fromP) p
-- OLD syb   where
-- OLD syb     fromP (F.PBexp (F.EApp f _))
-- OLD syb                = [F.val f]
-- OLD syb     fromP _    = []

getPApps :: F.Pred -> [F.Symbol]
getPApps = V.fold pAppVis () []
  where
    pAppVis = V.defaultVisitor { V.accPred = pA }
    pA _ (F.PBexp (F.EApp f _)) = [F.val f]
    pA _ _                      = []

expandPAlias      :: PAliasEnv -> PAlias -> PAlias
expandPAlias pe a = a { al_body = expandPred pe $ al_body a }

expandPred :: Data a => PAliasEnv -> a -> a
expandPred pe = everywhere $ mkT tx
  where
    tx p@(F.PBexp (F.EApp f es))
              = maybe p (applyPAlias p f es) $ envFindTy f pe
    tx p      = p

applyPAlias p f es a
  | ne == nx  = F.subst su $ al_body a
  | otherwise = die $ errorBadPAlias (srcPos f) p nx ne
  where
    su        = F.mkSubst $ zip xs es
    xs        = al_syvars a
    nx        = length xs
    ne        = length es


------------------------------------------------------------------------------
-- | One-shot expansion for @TAlias@ -----------------------------------------
------------------------------------------------------------------------------

expandTAliasEnv    :: TAliasEnv (RTypeQ RK F.Reft) ->  TAliasEnv (RTypeQ RK F.Reft)
expandTAliasEnv te = solve te support expandTAlias
  where
    support        = filter (`envMem` te) . getTApps . al_body

getTApps    :: RTypeQ RK F.Reft -> [F.Symbol]
getTApps    = everything (++) ([] `mkQ` fromT)
  where
    fromT   :: RTypeQ RK F.Reft -> [F.Symbol]
    fromT (TRef (Gen (QN (QP RK_ _ []) c) _) _) = [c]
    fromT _ = [ ]

expandTAlias  :: TAliasEnv (RTypeQ RK F.Reft) ->  TAlias (RTypeQ RK F.Reft) -> TAlias (RTypeQ RK F.Reft)
expandTAlias te a = a {al_body = expandRefType te $ al_body a}

-- expandRefType :: TAliasEnv RefType -> RefType -> RefType
-- expandRefType = expandRefType'

expandRefType :: Data a => TAliasEnv (RTypeQ RK F.Reft) -> a -> a
expandRefType te = everywhere $ mkT tx
  where
    tx t@(TRef (Gen (QN (QP RK_ l []) c) ts) r) = maybe t (applyTAlias l t c ts r) $ envFindTy c te
    tx t = t

applyTAlias l t _ ts_ r a
  | (nt, ne) == (nα, nx) = {- tracePP "applyTAlias" $ -} F.subst su (S.apply θ $ al_body a) `strengthen` r
  | otherwise = die $ errorBadTAlias l t nt ne nα nx
  where
    xs        = al_syvars a
    αs        = al_tyvars a
    nx        = length xs
    nα        = length αs
    ne        = length es
    nt        = length ts
    (ts, es)  = splitTsEs l t nα nx ts_
    su        = F.mkSubst  $ zip xs es
    θ         = S.fromList $ zip αs ts

splitTsEs l t na nx ts_
  | na + nx /= n = die $ errorTAliasNumArgs l t na nx n
  | otherwise    = (ts, rTypeExp l t <$> tes)
  where
    n            = length ts_
    (ts, tes)    = splitAt na ts_

rTypeExp _ _ (TExp e) = e
rTypeExp _ _(TRef (Gen r []) _) = F.expr $ F.symbol r
rTypeExp l t a = die $ errorTAliasMismatch l t a

-----------------------------------------------------------------------------
-- | A Generic Solver for Expanding Definitions -----------------------------
-----------------------------------------------------------------------------

solve :: (IsLocated a)
      => Env a              -- ^ Input definitions
      -> (a -> [F.Symbol])  -- ^ Dependencies (each Symbol is in `defs`)
      -> (Env a -> a -> a)  -- ^ Expansion function
      -> Env a              -- ^ Output "closed" definitions

solve defs deps exF = ex_solved $ execState act st0
  where
    st0             = ExS defs mempty -- envEmpty
    xs              = [x `at` d | (x, d) <- envToList defs]
    act             = forM_ xs $ solveM deps exF []


solveM deps exp stk x
  | x `elem` stk    = die $ errorCyclicDefs (srcPos x) x stk
  | otherwise       = do xr <- getResult x
                         case xr of
                           Just d' -> return (x, d')
                           Nothing -> do d      <- getDefinition x
                                         let ys  = [ y `at` d | y <- deps d]
                                         yds'   <- mapM (solveM deps exp (x:stk)) ys
                                         setResult x $ exp (envFromList yds') d

type ExM a     = State (ExState a)

data ExState a = ExS { ex_defs   :: Env a
                     , ex_solved :: Env a
                     }

-- getDefinition   :: F.Symbol -> ExM a a
getDefinition x = (fromMaybe (die $ bugUnknownAlias (srcPos x) x) . envFindTy (val x) . ex_defs) <$> get

-- getResult     :: F.Symbol -> ExM a (Maybe a)
getResult x   = (envFindTy (val x) . ex_solved) <$> get

setResult     :: (IsLocated a) => Located F.Symbol -> a -> ExM a (Located F.Symbol, a)
setResult x d = do modify $ \st -> st { ex_solved = envAdd x d (ex_solved st) }
                   return (x, d)

at x d        = Loc (srcPos d) (F.symbol x)
