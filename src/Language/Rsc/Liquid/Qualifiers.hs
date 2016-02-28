{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Rsc.Liquid.Qualifiers (scrapeQuals) where

import           Data.List                       (delete, nub)
import           Data.Maybe                      (fromMaybe)
import           Language.Fixpoint.Types         hiding (quals)
import           Language.Rsc.Annotations
import           Language.Rsc.AST
import           Language.Rsc.Core.Env
import           Language.Rsc.Errors
import           Language.Rsc.Liquid.Refinements
import           Language.Rsc.Locations
import           Language.Rsc.Names
import           Language.Rsc.Pretty
import           Language.Rsc.Program
import           Language.Rsc.SystemUtils
import           Language.Rsc.Traversals
import qualified Language.Rsc.Types              as T
import           Language.Rsc.Visitor
import           Text.PrettyPrint.HughesPJ



-- | Extracts all qualifiers from a RefScript program
--
--   Excludes qualifier declarations (qualif Q(...): ...)
--
--   XXX: No need to do `mkUq` here, since we're dropping common bindings later.
--
---------------------------------------------------------------------------------
scrapeQuals :: RefScript -> [Qualifier]
---------------------------------------------------------------------------------
scrapeQuals (Rsc { code = Src ss }) =
    qualifiers $ {- mkUq . -} foldStmts tbv [] [] $ filter nonLibFile ss
  where
    tbv = defaultVisitor { accStmt = gos, accCElt = goe, ctxStmt = ctx }

    gos c (FunctionStmt l x _ _) = [(qualify c x, t) | SigAnn _ _ t          <- fFact l]
    gos c (VarDeclStmt _ vds)    = [(qualify c x, t) | VarDecl l x _         <- vds
                                                     , VarAnn _ _ _ (Just t) <- fFact l]
    gos _ _                      = []

    goe c (Constructor l _ _)        = [(qualify c x, t) | CtorAnn  t <- fFact l, let x = Id l "ctor" ]
    goe c (MemberVarDecl l _ x _)    = [(qualify c x, t) | MemberAnn (T.FI _ _ _ t) <- fFact l ]
    goe c (MemberMethDecl l _ x _ _) = [(qualify c x, t) | MemberAnn (T.MI _ _ mts) <- fFact l, (_, t) <- mts ]

    -- XXX: Use this perhaps to make the bindings unique
    qualify _ (Id a x) = Id a x -- (mconcat c ++ x)

    ctx c (ModuleStmt _ m _ ) = symbolString (symbol m) : c
    ctx c (ClassStmt  _ m _ ) = symbolString (symbol m) : c
    ctx c _                   = c

nonLibFile :: IsLocated a => Statement a -> Bool
nonLibFile = not . isDeclarationFile -- not . isSuffixOf ".d.ts" . srcSpanFile

-- mkUq = zipWith tx ([0..] :: [Int])
--   where
--     tx i (Id l s, t) = (Id l $ s ++ "_" ++ show i, t)



-- XXX: Will drop multiple bindings tp the same name
--      To fix, replace envFromList' with something else
--
qualifiers xts    = concatMap (refTypeQualifiers γ0) xts
  where
     γ0           = envSEnv $ envMap rTypeSort $ envFromList' xts

refTypeQualifiers γ0 (l, t)
                  = efoldRType rTypeSort addQs γ0 [] t
  where
    addQs γ t qs  = mkQuals l γ t ++ qs

mkQuals l γ t     = [ mkQual l γ v so pa | pa <- conjuncts ra, noThis (syms pa) ]
  where
    RR so (Reft (v, ra)) = rTypeSortedReft t
    noThis        = all (not . isPrefixOfSym thisSym)

mkQual l γ v so p = Q (symbol "Auto") ((v, so) : yts) (subst θ p) l0
  where
    yts           = [(y, lookupSort l x γ) | (x, y) <- xys ]
    θ             = mkSubst [(x, eVar y)   | (x, y) <- xys]
    xys           = zipWith (\x i -> (x, symbol ("~A" ++ show i))) xs [0..]
    xs            = delete v $ orderedFreeVars γ p
    l0            = sourcePos l

-- XXX: The error won't be triggered cause we've dropped multiple bidings
--      at `qualifiers`.
--
lookupSort l  x γ = fromMaybe errorMsg $ lookupSEnv x γ
  where
    errorMsg      = die $ bug (srcPos l) $ "Unbound variable " ++ show x ++ " in specification for " ++ show (unId l)

orderedFreeVars γ = nub . filter (`memberSEnv` γ) . syms


instance {-# OVERLAPPING #-} PP [Qualifier] where
  pp = vcat . map toFix
  -- pp qs = vcat $ map (\q -> pp (q_name q) <+> colon <+> pp (q_body q)) qs

instance PP Qualifier where
  pp = toFix

