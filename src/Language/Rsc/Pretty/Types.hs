{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveFoldable            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE IncoherentInstances       #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

module Language.Rsc.Pretty.Types (

) where

import           Control.Applicative               ((<$>))
import           Data.Graph.Inductive.Graph        hiding (empty)
import           Data.Graph.Inductive.PatriciaTree
import qualified Data.HashMap.Strict               as HM
import qualified Data.Map.Strict                   as M
import           Language.Fixpoint.Misc            (intersperse)
import qualified Language.Fixpoint.Types           as F
import           Language.Rsc.AST
import           Language.Rsc.Pretty.Common
import           Language.Rsc.Pretty.Syntax
import           Language.Rsc.Program
import           Language.Rsc.Typecheck.Subst
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
import           Text.PrettyPrint.HughesPJ

angles p = char '<' <> p <> char '>'
ppHMap p = map (p . snd) . M.toList

instance PP Bool where
  pp True   = text "True"
  pp False  = text "False"

instance PP () where
  pp _ = text ""

instance PP a => PP (Maybe a) where
  pp = maybe (text "Nothing") pp

instance PP Char where
  pp = char

instance (F.Reftable r, PP r) => PP (RTypeQ q r) where
  pp (TPrim c r)     = F.ppTy r $ pp c
  pp (TVar α r)      = F.ppTy r $ pp α
  pp (TOr ts r)      = F.ppTy r $ ppArgs id (text " +") ts
  pp (TAnd ts)       = vcat [text "/\\" <+> pp t | t <- ts]
  pp (TRef t r)      = F.ppTy r (pp t)
  pp (TObj _ ms r)   = F.ppTy r $ braces $ pp ms
  pp (TClass t)      = text "class" <+> pp t
  pp (TMod t)        = text "module" <+> pp t
  pp t@(TAll _ _)    = ppArgs angles comma αs <> pp t' where (αs, t') = bkAll t
  pp (TFun xts t _)  = ppArgs parens comma xts <+> text "=>" <+> pp t
  pp (TExp e)        = pprint e

instance (F.Reftable r, PP r) => PP (TypeMembersQ q r) where
  pp (TM ms sms cs cts sidx nidx)
    = ppMem  ms  $+$
      ppSMem sms $+$
      ppCall cs   $+$
      ppCtor cts  $+$
      ppSIdx sidx $+$
      ppNIdx nidx

ppMem  = sep . map (\(x, f) ->                 ppElt x f <> semi) . F.toListSEnv
ppSMem = sep . map (\(x, f) -> pp "static" <+> ppElt x f <> semi) . F.toListSEnv

ppCall optT | Just t <- optT = pp t              <> semi | otherwise = empty
ppCtor optT | Just t <- optT = pp "new" <+> pp t <> semi | otherwise = empty

ppSIdx (Just t) = pp "[x: string]:" <+> pp t <> semi
ppSIdx _        = empty
ppNIdx (Just t) = pp "[x: number]:" <+> pp t <> semi
ppNIdx _        = empty

instance PPR r => PP (TypeMemberQ q r) where
  pp (FI o _ t) = pp o <> colon <+> pp t
  pp (MI o mts) = pp o <> vcat (map (\(m,t) -> brackets (pp m) <> pp t) mts)

ppElt x (FI o m t) = pp m <+> pp x <> pp o <> colon <+> pp t
ppElt x (MI o mts) = vcat (map (\(m,t) -> text "@" <> pp m <+> pp x <> pp o <> pp t) mts)

instance PP FieldAsgn where
  pp Assignable = pp "@Assignable"
  pp Final      = pp "@Final"
  pp _          = empty

instance PP Optionality where
  pp Opt = text "?"
  pp Req = empty

instance (F.Reftable r, PP r) => PP (TGenQ q r) where
  pp (Gen x []) = pp x
  pp (Gen x ts) = pp x <> ppArgs angles comma ts

instance (F.Reftable r, PP r) => PP (BTGenQ q r) where
  pp (BGen x []) = pp x
  pp (BGen x ts) = pp x <> ppArgs angles comma ts

instance PP TVar where
  pp = pprint . F.symbol

instance (F.Reftable r, PP r) => PP (BTVarQ q r) where
  pp (BTV v _ (Just t)) = pprint v <+> text "<:" <+> pp t
  pp (BTV v _ _       ) = pprint v

instance PP TPrim where
  pp TString     = text "string"
  pp (TStrLit s) = text "\"" <> text s <> text "\""
  pp TNumber     = text "number"
  pp TBoolean    = text "boolean"
  pp TBV32       = text "bitvector32"
  pp TVoid       = text "void"
  pp TUndefined  = text "undefined"
  pp TNull       = text "null"
  pp TBot        = text "_|_"
  pp TTop        = text " T "
  pp TAny        = text "any"
  pp TFPBool     = text "_bool_"

instance (PP r, F.Reftable r) => PP (BindQ q r) where
  pp (B x t) = pp x <> colon <> pp t

instance (PP s, PP t) => PP (M.Map s t) where
  pp m = vcat $ pp <$> M.toList m

instance PP Locality where
  pp Exported     = text "Exported"
  pp Local        = text "Local"

instance PP Assignability where
  pp Ambient      = text "Ambient"
  pp RdOnly       = text "ReadOnly"
  pp WriteLocal   = text "WriteLocal"
  pp ForeignLocal = text "ForeignLocal"
  pp WriteGlobal  = text "WriteGlobal"
  pp ReturnVar    = text "ReturnVar"
  pp _            = text "ErrorAssignability"

instance (PP r, F.Reftable r) => PP (TypeDeclQ q r) where
  pp (TD s m) = pp s <+> lbrace $+$ nest 4 (pp m) $+$ rbrace

instance (PP r, F.Reftable r) => PP (TypeSigQ q r) where
  pp (TS k n h) = pp k <+> pp n <+> ppHeritage h

instance PP TypeDeclKind where
  pp InterfaceTDK = text "interface"
  pp ClassTDK     = text "class"

ppHeritage (es,is) = ppExtends es <+> ppImplements is

ppExtends []    = text ""
ppExtends (n:_) = text "extends" <+> pp n

ppImplements [] = text ""
ppImplements ts = text "implements" <+> intersperse comma (pp <$> ts)

mutSym (TRef (F.symbol -> s) _)
  | s == F.symbol "Mutable"       = Just "_MU_"
  | s == F.symbol "UniqueMutable" = Just "_UM_"
  | s == F.symbol "Immutable"     = Just "_IM_"
  | s == F.symbol "ReadOnly"      = Just "_RO_"
  | s == F.symbol "AssignsFields" = Just "_AF"

mutSym _ = Nothing

ppMut t@TVar{} = pp t
ppMut t        | Just s <- mutSym t = pp s
               | otherwise          = pp "_??_"

instance PP EnumDef where
  pp (EnumDef n m) = pp n <+> braces (pp m)

instance (F.Reftable r, PP r) => PP (VarInfo r) where
  pp (VI _ a _ t) = brackets (pp a) <+> pp t

instance (PP r, F.Reftable r) => PP (ModuleDef r) where
  pp (ModuleDef vars tys enums path) =
          pp (take 80 (repeat '-'))
      $+$ text "module" <+> pp path
      $+$ pp (take 80 (repeat '-'))
      $+$ text "Variables"    $+$ nest 4 (pp vars)
      $+$ text "Types"        $+$ nest 4 (pp tys)
      $+$ text "Enums"        $+$ nest 4 (pp enums)

instance PP IContext where
  pp (IC x) = text "Context: " <+> pp x

instance PP Initialization where
  pp Initialized   = text "init"
  pp Uninitialized = text "non-init"

instance (PP a, PP s, PP t) => PP (Alias a s t) where
  pp (Alias n _ _ body) = text "alias" <+> pp n <+> text "=" <+> pp body

instance (PP r, F.Reftable r) => PP (Rsc a r) where
  pp pgm@(Rsc {code = (Src s) }) =  inComments extras
                                $+$ text "\n// CODE"
                                $+$ pp s
    where
      extras =  text "\nCONSTANTS"          $+$ nest 4 (pp (consts pgm))
            $+$ text "\nPREDICATE ALIASES"  $+$ nest 4 (pp (pAlias pgm))
            $+$ text "\nTYPE ALIASES"       $+$ nest 4 (pp (tAlias pgm))
            -- $+$ text "\nQUALIFIERS"         $+$ nest 4 (vcat (F.toFix <$> take 3 (pQuals pgm)))
            $+$ text "\nQUALIFIERS"         $+$ nest 4 (vcat (F.toFix <$> pQuals pgm))
            $+$ text "..."
            $+$ text "\nINVARIANTS"         $+$ nest 4 (vcat (pp <$> invts pgm))

instance (F.Reftable r, PP r) => PP (RSubst r) where
  pp (Su m) | HM.null m      = text "empty"
            | HM.size m < 10 = intersperse comma $ (ppBind <$>) $ HM.toList m
            | otherwise      = vcat $ (ppBind <$>) $ HM.toList m

ppBind (x, t) = pp x <+> text ":=" <+> pp t


-- | PP Fixpoint

instance PP (F.SortedReft) where
  pp (F.RR _ b) = pp b

instance PP F.Reft where
  pp = pprint

instance PP (F.SubC c) where
  pp s = text "TODO PP F.SubC" -- pp (F.lhsCs s) <+> text " <: " <+> pp (F.rhsCs s)

