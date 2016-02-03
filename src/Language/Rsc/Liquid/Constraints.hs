{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

module Language.Rsc.Liquid.Constraints (

  -- * Execute Action and Get FInfo
    getCGInfo

  -- * Type Ops
  , narrowType

  ) where

import           Control.Monad
import qualified Data.List                       as L
import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Types         as F
import           Language.Rsc.ClassHierarchy
import           Language.Rsc.CmdLine
import           Language.Rsc.Constraints
import           Language.Rsc.Environment
import           Language.Rsc.Errors
import           Language.Rsc.Liquid.CGMonad
import           Language.Rsc.Liquid.Environment
import           Language.Rsc.Liquid.Types
import           Language.Rsc.Locations
import           Language.Rsc.Misc               (concatMapM)
import           Language.Rsc.Pretty
import           Language.Rsc.Program
import           Language.Rsc.Typecheck.Sub
import           Language.Rsc.Typecheck.Subst
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
import           Text.PrettyPrint.HughesPJ

-------------------------------------------------------------------------------
getCGInfo :: Config -> FilePath -> RefScript -> CGM a -> CGInfo
-------------------------------------------------------------------------------
getCGInfo cfg f p = cgStateCInfo f p . execute cfg p . (>> fixCWs)
  where
    fixCWs        = (,) <$> fixCs <*> fixWs
    fixCs         = getCons   >>= concatMapM splitC
    fixWs         = getWFCons >>= concatMapM splitW

--------------------------------------------------------------------------------
splitC :: SubC -> CGM [FixSubC]
--------------------------------------------------------------------------------

-- | S-Var-R
--
splitC (Sub g i t1@(TVar α1 _) t2@(TVar α2 _))
  | α1 == α2
  = bsplitC g i t1 t2
  | otherwise
  = splitIncompatC g i t1

-- | S-Var-L
--
splitC (Sub g i t1@(TVar _ _) t2)
  | Just t1' <- envFindBoundOpt g t1
  = splitC (Sub g i t1' t2)

-- | S-Mut (Ignore if the target type is mutability related)
--
splitC (Sub g i t1 t2)
  | mutRelated t2
  = return []
  | Just t2' <- envFindBoundOpt g t2, mutRelated t2'
  = return []
  | mutRelated t1
  = cgError $ bugSplitC i t1 t2

-- | S-Fun
--
splitC (Sub g i tf1@(TFun xt1s t1 _) tf2@(TFun xt2s t2 _))
  = do bcs       <- bsplitC g i tf1 tf2
       g'        <- envTyAdds "splitC" i xt2s g
       cs        <- concatMapM splitC $ zipWith (Sub g' i) t2s t1s'
       cs'       <- splitC $ Sub g' i (F.subst su t1) t2
       return     $ bcs ++ cs ++ cs'
    where
       t2s        = b_type <$> xt2s
       t1s'       = F.subst su (b_type <$> xt1s)
       su         = F.mkSubst $ zipWith bSub xt1s xt2s
       bSub b1 b2 = (b_sym b1, F.eVar $ b_sym b2)

-- | S-And-L
--
splitC (Sub _ _ TAnd{} _)
  = error "TAnd not supported in splitC"

-- | S-And-R
--
splitC (Sub _ _ _ TAnd{})
  = error "TAnd not supported in splitC"

-- | S-All
--
splitC (Sub g i (TAll α1 t1) (TAll α2 t2))
  | α1 == α2
  = splitC $ Sub g i t1 t2
  | otherwise
  = splitC $ Sub g i t1 t2'
  where
    θ   = fromList [(btvToTV α2, btVar α1 :: RefType)]
    t2' = apply θ t2

-- | S-Union-L
--
splitC (Sub g c t1@(TOr t1s r) t2)
  = do  m1      <- bsplitC g c t1 t2
        ms      <- concatMapM (\t -> splitC (Sub g c (t `strengthen` r) t2)) t1s
        return   $ m1 ++ ms

-- | S-Union-R
--
splitC (Sub g c s t@(TOr ts _))
  = do  m0      <- bsplitC g c s t
        mss     <- mapM (splitC . Sub g c s) ts
        ts      <- pure (map (sameTag g s) ts)
        case L.find fst (zip ts mss) of
          Just (_, ms) -> return (m0 ++ ms)
          Nothing      -> splitIncompatC g c s


-- splitC (Sub g c s t@(TOr ts _))
--   = do  m0        <- bsplitC g c s t
--         (mi, g')  <- foldM step ([], g) (init ts)
--         ml        <- splitC $ Sub g' c s (last ts)
--         return     $ m0 ++ mi ++ ml
--   where
--     step (ms, g) t2 =
--       do  bk    <- refresh tBool
--           _     <- wellFormed c g bk
--           pk    <- pure   $ F.reftPred (rTypeReft bk)
--           g'    <- pure   $ g { cge_guards =        pk : cge_guards g }
--           g''   <- pure   $ g { cge_guards = F.PNot pk : cge_guards g }
--           ms'   <- splitC $ Sub g' c s t2
--           return $ (ms ++ ms', g'')

-- | S-Ref
--
--    TODO: Find variance for type arguments
--
splitC (Sub g i t1@(TRef n1@(Gen x1 (_ :t1s)) r1)
                t2@(TRef    (Gen x2 (m2:t2s)) _ ))

  -- Trivial case (do not descend)
  --
  | F.isFalse (F.simplify r1)
  = return []

  -- Same name
  --
  | x1 == x2
  = if isSubtype g m2 tMU then
      do  cs    <- bsplitC g i t1 t2
          cs'   <- concatMapM splitC $ safeZipWith "splitc-5" (Sub g i) t1s t2s
          cs''  <- concatMapM splitC $ safeZipWith "splitc-6" (Sub g i) t2s t1s
          return $ cs ++ cs' ++ cs''
    else
      do  cs    <- bsplitC g i t1 t2
          cs'   <- concatMapM splitC $ safeZipWith "splitc-4" (Sub g i) t1s t2s
          return $ cs ++ cs'

  -- Upcast
  --
  | Just n1'@(Gen x1' _) <- weaken (envCHA g) n1 x2
  , x1' == x2 -- sanity check
  = splitC (Sub g i (TRef n1' r1) t2)

  | otherwise
  = splitIncompatC g i t1

-- | S-Prim
--
splitC (Sub g i t1@(TPrim c1 _) t2@(TPrim c2 r2))
  | isTTop t2 = bsplitC g i t1 (rTop t1 `strengthen` r2)
  | isTAny t2 = bsplitC g i t1 (rTop t1 `strengthen` r2)
  | c1 == c2  = bsplitC g i t1 t2
  | otherwise = splitIncompatC g i t1

-- | S-Obj
--
splitC (Sub g i@(Ci _ l) t1@(TObj ms1 r1) t2@(TObj ms2 _))
  | F.isFalse (F.simplify r1)
  = return []
  | otherwise
  = do  cs     <- bsplitC g i t1 t2
        (x,g') <- cgEnvAddFresh "" l t1 g
        cs'    <- splitTM g' (F.symbol x) i ms1 (ltracePP i ("obj-" ++ ppshow ms1) ms2)
        return $ cs ++ cs'

splitC (Sub g i t1 t2)
  | all maybeTObj [t1, t2]
  = case (expandType NonCoercive (envCHA g) t1, expandType NonCoercive (envCHA g) t2) of
      (Just t1', Just t2') -> splitC (Sub g i t1' t2')
      _ -> cgError $ errorUnfoldTypes l t1 t2 where l = srcPos i

splitC (Sub g i t1 _) = splitIncompatC g i t1

--------------------------------------------------------------------------------
splitIncompatC :: CGEnv -> a -> RefType -> CGM [F.SubC a]
--------------------------------------------------------------------------------
splitIncompatC g i t = bsplitC g i t (mkBot t)

--------------------------------------------------------------------------------
mkBot :: (F.Reftable r) => RType r -> RType r
--------------------------------------------------------------------------------
mkBot t = t `strengthen` F.bot (rTypeR t)

-- | Substitute occurences of `this` in the parts of the object members
--   Do not substiute static members and constructors.
--------------------------------------------------------------------------------
splitTM :: CGEnv -> F.Symbol -> Cinfo
        -> (TypeMembers F.Reft) -> (TypeMembers F.Reft) -> CGM [FixSubC]
--------------------------------------------------------------------------------
splitTM g x c (TM p1 sp1 c1 k1 s1 n1) (TM p2 sp2 c2 k2 s2 n2)
  = concatMapM (splitElt g c) (tracePP "splitTM" $ ms ++ sms) +++
    concatMapM splitT (cs ++ ks ++ ss ++ ns)
  where
    (+++) = liftM2 (++)
    ms  = substThis x $ F.toListSEnv $ F.intersectWithSEnv (,) p1 p2
    sms =               F.toListSEnv $ F.intersectWithSEnv (,) sp1 sp2
    cs  = substThis x [ (t1,t2) | Just t1 <- [c1], Just t2 <- [c2] ]
    ks  =             [ (t1,t2) | Just t1 <- [k1], Just t2 <- [k2] ]
    ss  = substThis x [ (t1,t2) | Just t1 <- [s1], Just t2 <- [s2] ]
    ns  = substThis x [ (t1,t2) | Just t1 <- [n1], Just t2 <- [n2] ]
    splitT (t1,t2) = splitC (Sub g c t1 t2)

--------------------------------------------------------------------------------
splitElt :: CGEnv -> Cinfo -> (t, (TypeMember F.Reft, TypeMember F.Reft)) -> CGM [FixSubC]
--------------------------------------------------------------------------------
splitElt g i (_, (FI _ _ m1 t1, FI _ _ _ t2))
  -- Co-variant subtying for immutable members
  --
  | isSubtype g m1 tIM
  = splitC (Sub g i t1 t2)

  -- Co-variant subtyping for unique members (unaliased)
  --
  | isSubtypeWithUq g m1 tUQ
  = splitC (Sub g i (ltracePP i "Unique splitC" t1) t2)

  | otherwise
  = (++) <$> splitC (Sub g i t1 t2) <*> splitC (Sub g i t2 t1)

splitElt _ i (_, (m, m'))
  = cgError $ unsupportedSplitElt (srcPos i) m m'


subCTag :: [Int]
subCTag = [1]

conjoinPred :: F.Expr -> F.SortedReft -> F.SortedReft
conjoinPred p r    = r {F.sr_reft = F.Reft (v, F.pAnd [pr, p]) }
  where
    F.Reft (v, pr) = F.sr_reft r


--------------------------------------------------------------------------------
bsplitC :: CGEnv -> a -> RefType -> RefType -> CGM [F.SubC a]
--------------------------------------------------------------------------------
-- NOTE: addInvariant only needed in LHS
bsplitC g ci t1 t2 = bsplitC' g ci <$> addInvariant g t1 <*> return t2

bsplitC' :: CGEnv -> a -> RefType -> RefType -> [F.SubC a]
bsplitC' g ci t1 t2
  | F.isFunctionSortedReft r1 && F.isNonTrivial r2
  = F.subC bs (conjoinPred p $ r1 {F.sr_reft = typeofReft t1}) r2 Nothing subCTag ci
  | F.isNonTrivial r2
  = F.subC bs (conjoinPred p r1) r2 Nothing subCTag ci
  | otherwise
  = []
  where
    bs             = cge_fenv g
    p              = F.pAnd $ cge_guards g
    (r1, r2)       = (rTypeSortedReft t1, rTypeSortedReft t2)
    typeofReft t   = F.reft (vv t) $ F.pAnd [ typeofExpr (F.symbol "function") t
                                            , F.eProp $ vv t ]
    typeofExpr s t = F.PAtom F.Eq (F.mkEApp (F.dummyLoc (F.symbol "ttag")) [F.eVar $ vv t])
                                  (F.expr $ F.symbolText s)
    vv             = rTypeValueVar


--------------------------------------------------------------------------------
-- | Splitting Well-Formedness Constraints
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
splitW :: WfC -> CGM [FixWfC]
--------------------------------------------------------------------------------
splitW (W g i ft@(TFun ts t _))
  = do let bws = bsplitW g ft i
       g'    <- envTyAdds "splitW" i ts g
       ws    <- concatMapM splitW [W g' i ti | B _ ti <- ts]
       ws'   <-            splitW (W g' i t)
       return  $ bws ++ ws ++ ws'

splitW (W g i (TAll _ t))
  = splitW (W g i t)

splitW (W g i t@(TVar _ _))
  = return $ bsplitW g t i

splitW (W g i t@(TPrim _ _))
  =  return $ bsplitW g t i

splitW (W g i t@(TRef (Gen _ ts) _))
  =  do let ws = bsplitW g t i
        ws'   <- concatMapM splitW [W g i ti | ti <- ts]
        return $ ws ++ ws'

splitW (W g i (TAnd (map snd -> ts)))
  = concatMapM splitW [W g i t | t <- ts]

splitW (W g i t@(TOr ts _))
  = do let ws = bsplitW g t i
       ws'   <- concatMapM splitW [W g i t | t <- ts]
       return $ ws ++ ws'

splitW (W g i t@(TObj ms _))
  = do let bws = bsplitW g t i
       -- TODO: add field bindings in g?
       ws     <- concatMapM splitW [ W g i t' | t' <- typesOfTM ms ]
       return  $ bws ++ ws

splitW (W _ _ (TClass _ ))
  = return []

splitW (W _ _ (TMod _ ))
  = return []

splitW (W _ _ t)
  = error $ render $ text "Not supported in splitW: " <+> pp t

bsplitW g t i
  | F.isNonTrivial r' = F.wfC bs r' i
  | otherwise         = []
  where
    r' = rTypeSortedReft t
    bs = cge_fenv g



-- | Runtime tag checks
--------------------------------------------------------------------------------
-- sameTag :: CGEnv -> RefType -> RefType -> Bool
--------------------------------------------------------------------------------
sameTag _ (TPrim c1 _) (TPrim c2 _) = c1 == c2
sameTag _ (TVar  v1 _) (TVar  v2 _) = v1 == v2
sameTag g t1@TRef{}    t2@TRef{}    = isSubtype g t1 t2 || isSubtype g t2 t1
sameTag _ TObj{}       TObj{}       = True
sameTag _ TObj{}       TRef{}       = True
sameTag _ TRef{}       TObj{}       = True
sameTag _ TFun{}       TFun{}       = True
sameTag _ _            _            = False


-------------------------------------------------------------------------------
narrowType :: CGEnv -> RefType-> Type -> RefType
-------------------------------------------------------------------------------
narrowType g (TOr t1s r) t2
  = tOrR (L.filter (\t1 -> isSubtype g t1 (ofType t2)) t1s) r

narrowType _ t1 _ = t1
