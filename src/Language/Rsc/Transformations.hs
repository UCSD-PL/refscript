{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Rsc.Transformations (
    convertTVar
  , convertTVars
  , replaceDotRef
  , replaceAbsolute
  , fixFunBinders

  ) where

import           Control.Applicative          hiding (empty)
import           Control.Exception            (throw)
import           Data.Default
import           Data.Generics
import qualified Data.HashSet                 as HS
import qualified Data.IntMap.Strict           as I
import           Data.Maybe                   (fromMaybe)
import           Data.Maybe                   (listToMaybe)
import           Data.Monoid                  hiding ((<>))
import           Data.Text                    (pack, splitOn)
import           Language.Fixpoint.Names      (symSepName)
import qualified Language.Fixpoint.Types      as F
import qualified Language.Fixpoint.Visitor    as FV
import           Language.Rsc.Annots
import           Language.Rsc.AST
import           Language.Rsc.Core.Env
import           Language.Rsc.Errors
import           Language.Rsc.Locations
import           Language.Rsc.Misc
import           Language.Rsc.Names
import           Language.Rsc.Pretty
import           Language.Rsc.Program
import           Language.Rsc.Traversals
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
import           Language.Rsc.Visitor


--------------------------------------------------------------------------------
-- | Convert bound TRefs to TVars
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
convertTVars :: (PP r, F.Reftable r) => BareRelRsc r -> BareRelRsc r
--------------------------------------------------------------------------------
convertTVars = visitRsc convertTvarVisitor []

--------------------------------------------------------------------------------
convertTVar    :: (PP r, F.Reftable r, Transformable t) => [TVar] -> t q r -> t q r
--------------------------------------------------------------------------------
convertTVar as = trans tx as []
  where
    tx αs _ t@(TRef (Gen c []) r) | Just α <- mkTvar αs c = TVar α r
    tx _  _ t = t

mkTvar :: (IsLocated a, F.Symbolic a) => [TVar] -> a -> Maybe TVar
mkTvar αs r = listToMaybe [ α { tv_loc = srcPos r }  | α <- αs, F.symbol α == F.symbol r]

--------------------------------------------------------------------------------
convertTvarVisitor :: (PP r, F.Reftable r) => Visitor () [TVar] (AnnRel r)
--------------------------------------------------------------------------------
convertTvarVisitor = defaultVisitor {
    ctxStmt = ctxStmtTvar
  , ctxCElt = ctxCEltTvar
  , txStmt  = transFmap (const . convertTVar)
  , txExpr  = transFmap (const . convertTVar)
  , txCElt  = transFmap (const . convertTVar)
  }

ctxStmtTvar as s = go s ++ as
  where
    go :: Statement (AnnRel r)  -> [TVar]
    go s@(FunctionStmt {}) = grab s
    go s@(InterfaceStmt {})    = grab s
    go s@(ClassStmt {})    = grab s
    go s@(ModuleStmt {})   = grab s
    go _                   = []

    grab :: Statement (FAnnQ q r) -> [TVar]
    grab = concatMap factTVars . fFact . getAnnotation

ctxCEltTvar as s = go s ++ as
  where
    go :: ClassElt (AnnRel r)  -> [TVar]
    go s@Constructor{}    = grab s
    go s@MemberMethDecl{} = grab s
    go _                  = []

    grab :: ClassElt (FAnnQ q r) -> [TVar]
    grab = concatMap factTVars . fFact . getAnnotation

----------------------------------------------------------------------------------
factTVars :: FactQ q r -> [TVar]
----------------------------------------------------------------------------------
factTVars = go
  where
    tvars t | Just ts <- bkFuns t
            = HS.toList $ foldUnions
            $ map HS.fromList [ btvToTV <$> t | (t, _, _) <- ts ]
            | otherwise
            = []

    foldUnions (α:αs) = foldl HS.intersection α αs
    foldUnions _      = HS.empty

    go (VarAnn _ _ (Just t)) = tvars t
    go (SigAnn _ t)          = tvars t
    go (FieldAnn (FI _ _ t)) = tvars t
    go (MethAnn (MI _ _ t))  = tvars t
    go (CtorAnn t)           = tvars t
    go (ClassAnn _ sig)      = btvToTV <$> b_args (sigTRef sig)
    go (InterfaceAnn d)      = btvToTV <$> b_args (sigTRef (typeSig d))
    go _                     = []


--------------------------------------------------------------------------------
-- | Replace all relatively qualified names/paths with absolute ones.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
replaceAbsolute :: (PPR r, Data r, Typeable r) => BareRelRsc r -> BareRsc r
--------------------------------------------------------------------------------
replaceAbsolute pgm@(Rsc { code = Src ss }) = pgm { code = Src $ (tr <$>) <$> ss }
  where
    (ns, ps)        = accumNamesAndPaths ss
    tr l            = ntrans (safeAbsName l) (safeAbsPath l) l
    safeAbsName l a = case absAct (absoluteName ns) l a of
                        Just a' -> a'
                        -- If it's a type alias, don't throw error
                        Nothing | isAlias a -> toAbsoluteName a
                                | otherwise -> throw $ errorUnboundName (srcPos l) a
    safeAbsPath l a = fromMaybe (throw $ errorUnboundPath (srcPos l) a)
                                (absAct (absolutePath ps) l a)

    isAlias (QN (QP RK_ _ []) s) = envMem s $ tAlias pgm
    isAlias (QN _ _) = False

    absAct f l a    = I.lookup (fId l) mm >>= (`f` a)
    mm              = snd $ visitStmts vs (QP AK_ def []) ss
    vs              = defaultVisitor { ctxStmt = cStmt }
                                     { accStmt = acc   }
                                     { accExpr = acc   }
                                     { accCElt = acc   }
                                     { accVDec = acc   }
    cStmt (QP AK_ l p) (ModuleStmt _ x _)
                    = QP AK_ l $ p ++ [F.symbol x]
    cStmt q _       = q
    acc c s         = I.singleton (fId a) c where a = getAnnotation s


--------------------------------------------------------------------------------
-- | Replace `a.b.c...z` with `offset(offset(...(offset(a),"b"),"c"),...,"z")`
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
replaceDotRef :: RefScript -> RefScript
--------------------------------------------------------------------------------
replaceDotRef p@(Rsc { code = Src fs, tAlias = ta, pAlias = pa, invts = is })
    = p { code         = Src $ tf       <##>  fs
        , tAlias       = trans tt [] [] <###> ta
        , pAlias       =       tt [] [] <##>  pa
        , invts        = trans tt [] [] <##>  is
        }
  where
    tf (FA l a facts) = FA l a $ trans tt [] [] <$> facts
    tt _ _            = fmap $ FV.trans vs () ()

    vs                = FV.defaultVisitor { FV.txExpr = tx }
    tx _ (F.EVar s)   | (x:y:zs) <- pack "." `splitOn` pack (F.symbolString s)
                      = foldl offset (F.eVar x) (y:zs)
    tx _ e            = e
    offset k v        = F.EApp offsetLocSym [F.expr k, F.expr v]


--
-- XXX: Treat this at lookup
--
-- --------------------------------------------------------------------------------
-- -- | Replace `TRef x _ _` where `x` is a name for an enumeration with `number`
-- --------------------------------------------------------------------------------
--
-- --------------------------------------------------------------------------------
-- fixEnums :: PPR r => QEnv (ModuleDef r) -> BareRsc r -> (QEnv (ModuleDef r), BareRsc r)
-- --------------------------------------------------------------------------------
-- fixEnums m p@(Rsc { code = Src ss }) = (m',p')
--   where
--     p'    = p { code = Src $ (trans f [] [] <$>) <$> ss }
--     m'    = fixEnumsInModule m `qenvMap` m
--     f _ _ = fixEnumInType m
--
-- fixEnumInType :: F.Reftable r => QEnv (ModuleDef r) -> RType r -> RType r
-- fixEnumInType ms (TRef (Gen (QN p x) []) r)
--   | Just m <- qenvFindTy p ms
--   , Just e <- envFindTy x $ m_enums m
--   = if isBvEnum e then tBV32 `strengthen` r
--                   else tNum  `strengthen` r
-- fixEnumInType _ t = t
--
-- fixEnumsInModule :: F.Reftable r => QEnv (ModuleDef r) -> ModuleDef r -> ModuleDef r
-- fixEnumsInModule m = trans (const $ const $ fixEnumInType m) [] []
--

--------------------------------------------------------------------------------
-- | Add a '#' at the end of every function binder (to avoid capture)
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- fixFunBinders :: PPR r => BareRsc r -> BareRsc r
-- fixFunBinders :: RefScript -> RefScript
--------------------------------------------------------------------------------
fixFunBinders p@(Rsc { code = Src ss }) = p'
  where
    p'    = p { code = Src $ (trans f [] [] <$>) <$> ss }
    f _ _ = fixFunBindersInType

fixFunBindersInType t | Just is <- bkFuns t = mkAnd $ map (mkFun . f) is
                      | otherwise           = t
  where
    f (vs, yts, t) = (vs, ssb yts, sub t)
      where
        ks  = [ y | B y _ <- yts ]
        ks' = (F.eVar . (`mappend` F.symbol [symSepName])) <$> ks
        sub :: F.Subable a => a -> a
        sub = F.subst $ F.mkSubst $ zip ks ks'
        ssb bs = [ B (sub s) (sub t) | B s t <- bs ]

-- fixFunBindersInModule m@(ModuleDef { m_variables = mv, m_types = mt })
--                 = m { m_variables = mv', m_types = mt' }
--   where
--    mv'          = envMap f mv
--    f (VI a i t) = VI a i $ fixFunBindersInType t
--    mt'          = envMap (trans g [] []) mt
--    g _ _        = fixFunBindersInType
