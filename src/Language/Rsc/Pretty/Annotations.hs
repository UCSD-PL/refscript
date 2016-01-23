{-# LANGUAGE FlexibleInstances #-}

module Language.Rsc.Pretty.Annotations where

import           Control.Applicative        ((<$>))
import qualified Data.IntMap.Strict         as I
import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Types    as F
import           Language.Rsc.Annotations
import           Language.Rsc.Pretty.Common
import           Language.Rsc.Pretty.Errors ()
import           Language.Rsc.Pretty.Types  ()
import           Language.Rsc.Types
import           Prelude                    hiding (maybe)
import           Text.PrettyPrint.HughesPJ

-- instance PP SubTRes where
--   pp EqT        = text "="
--   pp (SubErr _) = text "dead"
--   pp SubT       = text "UP"
--   pp SupT       = text "DN"

instance (F.Reftable r, PP r) => PP (Fact r) where
  pp (PhiVar x)             = text "phi"          <+> pp x
  pp (PhiVarTy x)           = text "phi-ty"       <+> pp x
  pp (PhiVarTC x)           = text "phi-tc"       <+> pp x
  pp (PhiPost _)            = text "phi-post"
  pp (TypInst i ξ ts)       = text "inst"         <+> pp i <+> pp ξ <+> pp ts
  pp (Overload ξ i)         = text "overload"     <+> pp ξ <+> pp i
  pp (EltOverload ξ m t)    = text "elt_overload" <+> pp ξ <+> pp m <+> pp t
  pp (TypeCast ξ t)         = text "cast"         <+> pp ξ <+> pp t
  pp (DeadCast ξ _)         = text "deadcast"     <+> pp ξ
  pp (VarAnn _ _ t)         = text "Var Ann"      <+> pp t
  pp (CtorAnn c)            = text "Ctor Ann"     <+> pp c
  pp (UserCast c)           = text "Cast Ann"     <+> pp c
  pp (SigAnn _ t)           = text "Func Ann"     <+> pp t
  pp (MemberAnn (FI _ _ t)) = text "Field Ann"    <+> pp t
  pp (MemberAnn (MI _ mts)) = text "Method Ann"   <+> pp mts
  pp (InterfaceAnn _)       = text "UNIMPLEMENTED:pp:InterfaceAnn"
  pp (ClassAnn _ _)         = text "UNIMPLEMENTED:pp:ClassAnn"
  pp (ModuleAnn l s)        = text "module"       <+> pp s
  pp (EnumAnn s)            = text "enum"         <+> pp s
  pp (BypassUnique)         = text "BypassUnique"

instance (F.Reftable r, PP r) => PP (AnnR r) where
  pp (FA _ s fs) = vcat $ map pp fs

instance (F.Reftable r, PP r) => PP (AnnInfo r) where
  pp             = vcat . (ppB <$>) . I.toList
    where
      ppB (x, t) = pp x <+> dcolon <+> pp t

instance (PP a, PP b) => PP (Annot b a) where
  pp (Ann _ x ys) = text "Annot: " <+> pp x <+> pp ys

instance PP SyntaxKind where
  pp FuncDeclKind     = text "FuncDeclKind"
  pp MethDeclKind     = text "MethDeclKind"
  pp FieldDeclKind    = text "FieldDeclKind"
  pp CtorDeclKind     = text "CtorDeclKind"
  pp VarDeclKind      = text "VarDeclKind"
  pp ClassDeclKind    = text "ClassDeclKind"
  pp ModuleDeclKind   = text "ModuleDeclKind"
  pp EnumDeclKind     = text "EnumDeclKind"

