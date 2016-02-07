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


addHist (Ci e l h) s = Ci e l (s:h)

--------------------------------------------------------------------------------
splitC :: SubC -> CGM [FixSubC]
--------------------------------------------------------------------------------

-- | S-Bot-R
--   Use this to keep the deadcast messages cleaner
--
splitC (Sub g i t1 (TPrim TBot _))
  | not (isTBot t1) = splitIncompatC g i t1

-- | S-Var-R
--
splitC s@(Sub g i t1@(TVar α1 _) t2@(TVar α2 _))
  | α1 == α2  = bsplitC g i' t1 t2
  | otherwise = splitIncompatC g i' t1
  where
    i' = addHist i s

-- | S-Var-L
--
splitC s@(Sub g i t1@(TVar _ _) t2)
  | Just t1' <- envFindBoundOpt g t1
  = splitC (Sub g (addHist i s) t1' t2)

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
splitC s@(Sub g i tf1@(TFun xt1s t1 _) tf2@(TFun xt2s t2 _))
  = do bcs       <- bsplitC g i' tf1 tf2
       g'        <- envTyAdds "splitC" i xt2s g
       cs        <- concatMapM splitC $ zipWith (Sub g' i') t2s t1s'
       cs'       <- splitC $ Sub g' i' (F.subst su t1) t2
       return     $ bcs ++ cs ++ cs'
    where
       t2s        = b_type <$> xt2s
       t1s'       = F.subst su (b_type <$> xt1s)
       su         = F.mkSubst $ zipWith bSub xt1s xt2s
       bSub b1 b2 = (b_sym b1, F.eVar $ b_sym b2)
       i'         = addHist i s

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
splitC s@(Sub g i (TAll α1 t1) (TAll α2 t2))
  | α1 == α2
  = splitC $ Sub g i' t1 t2
  | otherwise
  = splitC $ Sub g i' t1 t2'
  where
    θ   = fromList [(btvToTV α2, btVar α1 :: RefType)]
    t2' = apply θ t2
    i'  = addHist i s

-- | S-Union-L
--
splitC s@(Sub g i t1@(TOr t1s r) t2)
  = do  m1      <- bsplitC g i t1 t2
        ms      <- concatMapM (\t -> splitC (Sub g i' (t `strengthen` r) t2)) t1s
        return   $ m1 ++ ms
  where
    i' = addHist i s

-- | S-Union-R
--
splitC s@(Sub g i t1 t2@(TOr ts _))
  = do  m0      <- bsplitC g i' t1 t2
        mss     <- mapM (splitC . Sub g i' t1) ts
        ts      <- pure (map (sameTag g t1) ts)
        case L.find fst (zip ts mss) of
          Just (_, ms) -> return (m0 ++ ms)
          Nothing      -> splitIncompatC g i' t1
  where
    i' = addHist i s


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
splitC s@(Sub g i t1@(TRef n1@(Gen x1 (_ :t1s)) r1)
                  t2@(TRef    (Gen x2 (m2:t2s)) _ ))

  -- Trivial case (do not descend)
  --
  | F.isFalse (F.simplify r1)
  = return []

  -- Same name
  --
  | x1 == x2
  = if isSubtype g m2 tMU then
      do  cs    <- bsplitC g i' t1 t2
          cs'   <- concatMapM splitC $ safeZipWith "splitc-5" (Sub g i') t1s t2s
          cs''  <- concatMapM splitC $ safeZipWith "splitc-6" (Sub g i') t2s t1s
          return $ cs ++ cs' ++ cs''
    else
      do  cs    <- bsplitC g i' t1 t2
          cs'   <- concatMapM splitC $ safeZipWith "splitc-4" (Sub g i') t1s t2s
          return $ cs ++ cs'

  -- Upcast
  --
  | Just n1'@(Gen x1' _) <- weaken (envCHA g) n1 x2
  , x1' == x2 -- sanity check
  = splitC (Sub g i (TRef n1' r1) t2)

  -- XXX: Is this possible to be caught later?
  | otherwise
  = splitIncompatC g i' t1

  where
    i' = addHist i s

-- | S-Prim
--
splitC s@(Sub g i t1@(TPrim c1 _) t2@(TPrim c2 r2))
  | isTTop t2 = bsplitC g i' t1 (rTop t1 `strengthen` r2)
  | isTAny t2 = bsplitC g i' t1 (rTop t1 `strengthen` r2)
  | c1 == c2  = bsplitC g i' t1 t2
  | otherwise = splitIncompatC g i' t1
  where
    i' = addHist i s

-- | S-Obj
--
splitC s@(Sub g i t1@(TObj m1 ms1 r1) t2@(TObj _ ms2 _))
  | F.isFalse (F.simplify r1)
  = return []
  | otherwise
  = do  cs     <- bsplitC g i' t1 t2
        cs'    <- splitTM g i' m1 ms1 r1 ms2
        return $ cs ++ cs'
  where
    i' = addHist i s

splitC s@(Sub g i t1 t2)
  | all maybeTObj [t1, t2]
  = case (expandType econf (envCHA g) t1, expandType econf (envCHA g) t2) of
      (Just t1', Just t2') -> splitC (Sub g i' t1' t2')
      (Nothing , _       ) -> cgError $ errorUnfoldType l t1
      (_       , Nothing ) -> cgError $ errorUnfoldType l t2
  where
    l     = srcPos i
    i'    = addHist i s
    econf = EConf False False

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
splitTM
  :: CGEnv -> Cinfo -> Mutability -> RefTypeMembers -> F.Reft
  -> RefTypeMembers -> CGM [FixSubC]
--------------------------------------------------------------------------------
splitTM g i m1_ (TM p1 sp1 c1 k1 s1 n1) r1 (TM p2 sp2 c2 k2 s2 n2)
  = do
        -- Add a binding for the LHS, but replace the Unique mutabilities
        -- with the mutabilities that appear in the RHS
        --
        (x, g') <- cgEnvAddFresh "splitTM" i t1' g
        ps'     <- pure $ substThis x ps
        cs1     <- concatMapM (splitElt g' i) ps'
        cs2     <- concatMapM (splitElt g  i) sps
        -- Call signature
        cs      <- pure $ substThis x [ (t1,t2) | Just t1 <- [c1], Just t2 <- [c2] ]
        -- Constructor signature
        ks      <- pure $             [ (t1,t2) | Just t1 <- [k1], Just t2 <- [k2] ]
        cs3     <- concatMapM splitT (cs ++ ks)
        -- Indexers
        s1'     <- pure $ substThis x <$> s1
        s2'     <- pure $ substThis x <$> s2
        n1'     <- pure $ substThis x <$> n1
        n2'     <- pure $ substThis x <$> n2
        cs4     <- splitSIdxs g i p1 s1' s2'
        cs5     <- splitNIdxs g i    n1' n2'

        return     (cs1 ++ cs2 ++ cs3 ++ cs4 ++ cs5)
  where
    ps  = F.toListSEnv (F.intersectWithSEnv (,) p1  p2)
    sps = F.toListSEnv (F.intersectWithSEnv (,) sp1 sp2)
    t1' = TObj m1_ (tmsFromList (map fixMut ps)) r1

    splitT (a,b) = splitC (Sub g i a b)

    -- Carry over the RHS's mutability (to avoid having unique when including
    -- the object type in the environment). The goal is to get the `Immutable`s
    -- from the RHS.
    fixMut (_, (FI s1 o m1 t, FI _ _ m2 _)) | isSubtypeWithUq g m1 tUQ
                                            = FI s1 o m2 t
    fixMut (_, (f1          , _          )) = f1

type IndInfo = Maybe (Mutability, RefType)

--------------------------------------------------------------------------------
splitSIdxs :: CGEnv -> Cinfo -> F.SEnv (TypeMember F.Reft)
           -> IndInfo -> IndInfo -> CGM [FixSubC]
--------------------------------------------------------------------------------
-- LHS should have empty props if there is a string indexer present
splitSIdxs g i _ (Just (m1, t1)) (Just (m2, t2)) =
    splitElt g i (f, (FI f Opt m1 t1, FI f Opt m2 t2))
  where
    f = F.symbol "String indexer"

-- LHS should not have an indexer type here
splitSIdxs g i ps1 _ (Just (m2, t2)) =
    concatMapM doMem fps
  where
    doMem (f, p) = splitElt g i (f, (p, FI f Opt m2 t2))
    fps          = [(f, p) | (_, p@(FI f _ _ _)) <- F.toListSEnv ps1 ]

splitSIdxs _ _ _ _ Nothing = return []

--------------------------------------------------------------------------------
splitNIdxs :: CGEnv -> Cinfo -> IndInfo -> IndInfo -> CGM [FixSubC]
--------------------------------------------------------------------------------
-- LHS should have empty props if there is a string indexer present
splitNIdxs g i (Just (m1, t1)) (Just (m2, t2)) =
    splitElt g i (f, (FI f Opt m1 t1, FI f Opt m2 t2))
  where
    f = F.symbol "Numeric indexer"

splitNIdxs _ _ _ Nothing = return []
splitNIdxs g i Nothing _ = splitIncompatC g i tBool


--------------------------------------------------------------------------------
splitElt
  :: CGEnv -> Cinfo -> (t, (TypeMember F.Reft, TypeMember F.Reft)) -> CGM [FixSubC]
--------------------------------------------------------------------------------
splitElt g i (_, (FI _ _ m1 t1, FI _ _ _ t2))
  -- Co- & Contra-variant subtying for mutable members
  --
  | isSubtype g m1 tMU
  = (++) <$> splitC (Sub g i t1 t2) <*> splitC (Sub g i t2 t1)

  -- Co-variant subtyping otherwise
  | otherwise
  = splitC (Sub g i t1 t2)

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

--------------------------------------------------------------------------------
bsplitC' :: CGEnv -> a -> RefType -> RefType -> [F.SubC a]
--------------------------------------------------------------------------------
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

splitW (W g i t@(TObj _ ms _))
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
