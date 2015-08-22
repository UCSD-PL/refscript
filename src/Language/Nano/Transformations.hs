{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Nano.Transformations (
    convertTVar
  , convertTVars
  , replaceDotRef
  , replaceAbsolute
  , fixEnums
  , fixFunBinders

  ) where

import           Control.Applicative           hiding (empty)
import           Control.Exception             (throw)
import           Data.Default
import           Data.Generics
import qualified Data.HashSet                  as HS
import qualified Data.IntMap.Strict            as I
import           Data.Maybe                    (listToMaybe)
import           Data.Monoid                   hiding ((<>))
import           Data.Text                     (pack, splitOn)
import           Language.Fixpoint.Names       (symSepName)
import qualified Language.Fixpoint.Types       as F
import qualified Language.Fixpoint.Visitor     as FV
import           Language.Nano.Annots
import           Language.Nano.AST
import           Language.Nano.Core.Env
import           Language.Nano.Errors
import           Language.Nano.Locations
import           Language.Nano.Misc
import           Language.Nano.Names
import           Language.Nano.Pretty
import           Language.Nano.Program
import           Language.Nano.Traversals
import           Language.Nano.Typecheck.Types
import           Language.Nano.Types
import           Language.Nano.Visitor


-------------------------------------------------------------------------------
-- | Convert bound TRefs to TVars
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
convertTVars :: F.Reftable r => BareRelRsc r -> BareRelRsc r
-------------------------------------------------------------------------------
convertTVars a = visitNano convertTvarVisitor [] a

----------------------------------------------------------------------------------
convertTVar    :: (F.Reftable r, Transformable t) => [TVar] -> t q r -> t q r
----------------------------------------------------------------------------------
convertTVar as = trans tx as []
  where
    tx αs _ (TRef (Gen c []) r) | Just α <- mkTvar αs c = TVar α r
    tx _  _ t = t

mkTvar :: (IsLocated a, F.Symbolic a) => [TVar] -> a -> Maybe TVar
mkTvar αs r = listToMaybe [ α { tv_loc = srcPos r }  | α <- αs, F.symbol α == F.symbol r]

----------------------------------------------------------------------------------
convertTvarVisitor :: (F.Reftable r) => Visitor () [TVar] (AnnRel r)
----------------------------------------------------------------------------------
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
    go s@(FuncAmbDecl {})  = grab s
    go s@(FuncOverload {}) = grab s
    go s@(IfaceStmt {})    = grab s
    go s@(ClassStmt {})    = grab s
    go s@(ModuleStmt {})   = grab s
    go _                   = []

    grab :: Statement (FAnnQ q r) -> [TVar]
    grab = concatMap factTVars . fFact . getAnnotation

ctxCEltTvar as s = go s ++ as
  where
    go :: ClassElt (AnnRel r)  -> [TVar]
    go s@Constructor{}     = grab s
    go s@MemberMethDef{}   = grab s
    go _                   = []

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

    go (VarAnn _ (Just t))     = tvars t
    go (FuncAnn t)             = tvars t
    go (FieldAnn _ (FI _ _ t)) = tvars t
    go (MethAnn _ (MI _ _ t))  = tvars t
    go (ConsAnn t)             = tvars t
    go (ClassAnn (TS _ bs _))  = btvToTV <$> b_args bs
    go (InterfaceAnn (TD (TS _ bs _) _))
                               = btvToTV <$> b_args bs
    go _                       = []


---------------------------------------------------------------------------------------
-- | Replace all relatively qualified names/paths with absolute ones.
---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------
replaceAbsolute :: (PPR r, Data r, Typeable r) => BareRelRsc r -> BareRsc r
---------------------------------------------------------------------------------------
replaceAbsolute pgm@(Rsc { code = Src ss }) = pgm { code = Src $ (tr <$>) <$> ss }
  where
    (ns, ps)        = accumNamesAndPaths ss
    tr l            = ntrans (safeAbsName l) (safeAbsPath l) l
    safeAbsName l a = case absAct (absoluteName ns) l a of
                        Just a' -> a'
                        -- If it's a type alias, don't throw error
                        Nothing | isAlias a -> toAbsoluteName a
                                | otherwise -> throw $ errorUnboundName (srcPos l) a
    safeAbsPath l a = case absAct (absolutePath ps) l a of
                        Just a' -> a'
                        Nothing -> throw $ errorUnboundPath (srcPos l) a

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


---------------------------------------------------------------------------------------
-- | Replace `a.b.c...z` with `offset(offset(...(offset(a),"b"),"c"),...,"z")`
---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------
replaceDotRef :: RefScript -> RefScript
---------------------------------------------------------------------------------------
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


---------------------------------------------------------------------------------------
-- | Replace `TRef x _ _` where `x` is a name for an enumeration with `number`
---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------
fixEnums :: PPR r => QEnv (ModuleDef r) -> BareRsc r -> (QEnv (ModuleDef r), BareRsc r)
---------------------------------------------------------------------------------------
fixEnums m p@(Rsc { code = Src ss }) = (m',p')
  where
    p'    = p { code = Src $ (trans f [] [] <$>) <$> ss }
    m'    = fixEnumsInModule m `qenvMap` m
    f _ _ = fixEnumInType m

fixEnumInType :: F.Reftable r => QEnv (ModuleDef r) -> RType r -> RType r
fixEnumInType ms (TRef (Gen (QN p x) []) r)
  | Just m <- qenvFindTy p ms
  , Just e <- envFindTy x $ m_enums m
  = if isBvEnum e then tBV32 `strengthen` r
                  else tNum  `strengthen` r
fixEnumInType _ t = t

fixEnumsInModule :: F.Reftable r => QEnv (ModuleDef r) -> ModuleDef r -> ModuleDef r
fixEnumsInModule m = trans (const $ const $ fixEnumInType m) [] []


---------------------------------------------------------------------------------------
-- | Add a '#' at the end of every function binder (to avoid capture)
---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------
fixFunBinders :: PPR r => QEnv (ModuleDef r) -> BareRsc r -> (QEnv (ModuleDef r), BareRsc r)
---------------------------------------------------------------------------------------
fixFunBinders m p@(Rsc { code = Src ss }) = (m', p')
  where
    p'    = p { code = Src $ (trans f [] [] <$>) <$> ss }
    m'    = qenvMap fixFunBindersInModule m
    f _ _ = fixFunBindersInType

fixFunBindersInType t | Just is <- bkFuns t = mkAnd $ map (mkFun . f) is
                      | otherwise           = t
  where
    f (vs, yts, t)    = (vs, ssb yts, sub t)
      where
        ks            = [ y | B y _ <- yts ]
        ks'           = (F.eVar . (`mappend` F.symbol [symSepName])) <$> ks
        sub :: F.Subable a => a -> a
        sub          = F.subst $ F.mkSubst $ zip ks ks'
        ssb bs        = [ B (sub s) (sub t) | B s t <- bs ]

fixFunBindersInModule m@(ModuleDef { m_variables = mv, m_types = mt })
                = m { m_variables = mv', m_types = mt' }
  where
   mv'          = envMap f mv
   f (VI a i t) = VI a i $ fixFunBindersInType t
   mt'          = envMap (trans g [] []) mt
   g _ _        = fixFunBindersInType

