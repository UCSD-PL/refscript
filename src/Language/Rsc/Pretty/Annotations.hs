{-# LANGUAGE FlexibleInstances #-}

module Language.Rsc.Pretty.Annotations where

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

instance (F.Reftable r, PP r) => PP (FactQ q r) where
  pp (PhiVar x)          = text "phi"          <+> pp x
  pp (PhiLoop xs)        = text "PHI-Loop"     $+$ pp xs
  pp (PhiLoopTC xs)      = text "PHI-Loop-TC"  $+$ pp xs
  pp (TypInst i ξ ts)    = text "TypInst"      <+> pp i <+> pp ξ <+> pp ts
  pp (Overload ξ f i)    = text "overload"     <+> pp ξ <+> pp f <+> pp i
  pp (EltOverload ξ m t) = text "elt_overload" <+> pp ξ <+> pp m <+> pp t
  pp (TypeCast ξ t)      = text "cast"         <+> pp ξ <+> pp t
  pp (DeadCast ξ es)     = text "DEADCAST"     <+> pp ξ $+$ nest 2 (vcat (map pp es))
  pp (VarAnn x _ _ t)    = text "Var Ann"      <+> pp x <+> dcolon <+> pp t
  pp (CtorAnn c)         = text "Ctor Ann"     <+> pp c
  pp (UserCast c)        = text "Cast Ann"     <+> pp c
  pp (SigAnn x _ t)      = text "Func Ann"     <+> pp x <+> dcolon <+> pp t
  pp (MemberAnn f@FI{})  = text "Field Ann"    <+> pp (f_ty f)
  pp (MemberAnn m@MI{})  = text "Method Ann"   <+> pp (m_ty m)
  pp (InterfaceAnn _)    = text "UNIMPLEMENTED:pp:InterfaceAnn"
  pp (ClassAnn{})        = text "UNIMPLEMENTED:pp:ClassAnn"
  pp (ClassInvAnn{})     = text "UNIMPLEMENTED:pp:ClassInvAnn"
  pp (ModuleAnn _ s)     = text "module"       <+> pp s
  pp (EnumAnn s)         = text "enum"         <+> pp s
  pp (BypassUnique)      = text "BypassUnique"

instance (F.Reftable r, PP r) => PP (AnnR r) where
  pp (FA _ _ fs) = vcat $ map pp fs

instance (F.Reftable r, PP r) => PP (AnnInfo r) where
  pp             = vcat . (ppB <$>) . I.toList
    where
      ppB (x, t) = pp x <+> dcolon <+> pp t

instance (PP a, PP b) => PP (Annot b a) where
  pp (Ann _ x ys) = text "Annot: " <+> pp x <+> pp ys

