{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Rsc.Liquid.Qualifiers (scrapeQuals) where

import           Data.List                 (delete, nub)
import           Data.Maybe                (fromMaybe)
import           Language.Fixpoint.Types   hiding (quals)
import           Language.Rsc.Annotations
import           Language.Rsc.AST
import           Language.Rsc.Core.Env
import           Language.Rsc.Errors
import           Language.Rsc.Liquid.Types
import           Language.Rsc.Locations
import           Language.Rsc.Names
import           Language.Rsc.Pretty
import           Language.Rsc.SystemUtils
import           Language.Rsc.Traversals
import qualified Language.Rsc.Types        as T
import           Language.Rsc.Visitor
import           Text.PrettyPrint.HughesPJ

qualifiers xts = concatMap (refTypeQualifiers γ0) xts
  where
     γ0        = envSEnv $ envMap rTypeSort $ envFromList xts

refTypeQualifiers γ0 (l, t)
                  = efoldRType rTypeSort addQs γ0 [] t
  where
    addQs γ t qs  = mkQuals l γ t ++ qs

mkQuals l γ t     = [ mkQual l γ v so pa | let (RR so (Reft (v, ra))) = rTypeSortedReft t
                                         , pa                        <- conjuncts ra
                                         , noThis (syms pa) ]
  where
    noThis        = all (not . isPrefixOfSym thisSym)

mkQual l γ v so p = Q (symbol "Auto") ((v, so) : yts) (subst θ p) l0
  where
    yts           = [(y, lookupSort l x γ) | (x, y) <- xys ]
    θ             = mkSubst [(x, eVar y)   | (x, y) <- xys]
    xys           = zipWith (\x i -> (x, symbol ("~A" ++ show i))) xs [0..]
    xs            = delete v $ orderedFreeVars γ p
    l0            = sourcePos l

lookupSort l  x γ = fromMaybe errorMsg $ lookupSEnv x γ
  where
    errorMsg      = die $ bug (srcPos l) $ "Unbound variable " ++ show x ++ " in specification for " ++ show (unId l)

orderedFreeVars γ = nub . filter (`memberSEnv` γ) . syms

-- | Extracts all qualifiers from a RefScript program
---------------------------------------------------------------------------------
scrapeQuals :: [Qualifier] -> [Statement (AnnRel Reft)] -> [Qualifier]
---------------------------------------------------------------------------------
scrapeQuals qs = (qs ++) . qualifiers . mkUq . foldStmts tbv [] . filter nonLibFile
  where
    tbv = defaultVisitor { accStmt = gos, accCElt = goe }

    gos _ (FunctionStmt l f _ _) = [(f, t) | SigAnn _ t <- fFact l]
    gos _ (VarDeclStmt _ vds)    = [(x, t) | VarDecl l x _ <- vds
                                           , VarAnn _ _ (Just t) <- fFact l]
    gos _ _                      = []

    goe _ (Constructor l _ _)        = [(x, t) | CtorAnn  t <- fFact l, let x = Id l "ctor" ]
    goe _ (MemberVarDecl l _ x _)    = [(x, t) | MemberAnn (T.FI _ _ _ t) <- fFact l ]
    goe _ (MemberMethDecl l _ x _ _) = [(x, t) | MemberAnn (T.MI _ _ mts) <- fFact l, (_, t) <- mts ]

nonLibFile :: IsLocated a => Statement a -> Bool
nonLibFile = not . isDeclarationFile -- not . isSuffixOf ".d.ts" . srcSpanFile

mkUq = zipWith tx ([0..] :: [Int])
  where
    tx i (Id l s, t) = (Id l $ s ++ "_" ++ show i, t)

instance {-# OVERLAPPING #-} PP [Qualifier] where
  pp = vcat . map toFix
  -- pp qs = vcat $ map (\q -> pp (q_name q) <+> colon <+> pp (q_body q)) qs

