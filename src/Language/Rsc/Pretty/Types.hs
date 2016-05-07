{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE IncoherentInstances       #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}

module Language.Rsc.Pretty.Types (

) where

import qualified Data.HashMap.Strict          as HM
import qualified Data.Map.Strict              as M
import           Language.Fixpoint.Misc       (intersperse)
import qualified Language.Fixpoint.Types      as F
import           Language.Rsc.Pretty.Common
import           Language.Rsc.Program
import           Language.Rsc.Typecheck.Subst
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types
import           Text.PrettyPrint.HughesPJ

angles p = langle <> p <> rangle
langle   = char '<'
rangle   = char '>'

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
  pp (TOr [] _)      = pp (TPrim TBot ())
  pp (TOr (t:ts) r)  = F.ppTy r $ sep $ pp t : map ((text "+" <+>) . pp) ts
  pp (TAnd ts)       = vcat [text "/\\" <+> pp t | t <- ts]
  pp t@TRef{}        | mutRelated t = ppMut t
  pp (TRef t r)      = F.ppTy r (pp t)
  pp (TObj m ms r)   = parens (pp (toType m)) <+> ppBody ms r
    where
      ppBody ms r    = F.ppTy r (hsep [lbrace, nest 2 (pp ms), rbrace])
  pp (TClass t)      = text "class" <+> pp t
  pp (TMod t)        = text "module" <+> pp t
  pp t@(TAll _ _)    = ppArgs angles comma αs <> pp t' where (αs, t') = bkAll t
  pp (TFun xts t _)  = ppArgs parens comma xts <+> text "=>" <+> pp t
  pp (TExp e)        = pprint e


ppMut t@TRef{} | isUQ t = pp "UQ"
               | isIM t = pp "IM"
               | isAF t = pp "AF"
               | isRO t = pp "RO"
               | isMU t = pp "MU"
ppMut (TVar v _ )       = pp v
ppMut _                 = pp "MUT???"


instance (F.Reftable r, PP r) => PP (TypeMembersQ q r) where
  pp (TM ms sms cs cts sidx nidx)
    = ppMem  ms  $+$
      ppSMem sms $+$
      ppCall cs   $+$
      ppCtor cts  $+$
      ppSIdx sidx $+$
      ppNIdx nidx

ppMem  = sep . map (\(_, f) ->                 pp f <> semi) . F.toListSEnv
ppSMem = sep . map (\(_, f) -> pp "static" <+> pp f <> semi) . F.toListSEnv

ppCall optT | Just t <- optT = pp t              <> semi | otherwise = empty
ppCtor optT | Just t <- optT = pp "new" <+> pp t <> semi | otherwise = empty

ppSIdx (Just t) = pp "[x: string]:" <+> pp t <> semi
ppSIdx _        = empty
ppNIdx (Just t) = pp "[x: number]:" <+> pp t <> semi
ppNIdx _        = empty

instance PPR r => PP (TypeMemberQ q r) where
  pp (FI s o m t) = parens (pp (toType m)) <+> pp s <> pp o <> colon <+> pp t
  pp (MI s o mts) = vcat (map (\(m,t) -> char '@' <> pp (toType m) <+> pp s <> pp o <> pp t) mts)

instance PP Optionality where
  pp Opt = text "?"
  pp Req = empty

instance (F.Reftable r, PP r) => PP (TGenQ q r) where
  pp (Gen x [])     = pp x
  pp (Gen x (m:ts)) = pp x <> angles (intersperse comma (ppMut m : map pp ts))

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
  pp TReal       = text "real"
  pp TBoolean    = text "boolean"
  pp TBV32       = text "bitvector32"
  pp TVoid       = text "void"
  pp TUndefined  = text "undefined"
  pp TNull       = text "null"
  pp TBot        = text "_|_"
  pp TTop        = text "Top"
  pp TAny        = text "any"
  pp TFPBool     = text "_bool_"

instance (PP r, F.Reftable r) => PP (BindQ q r) where
  pp (B x o t) = pp x <> pp o <> colon <+> pp t

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

instance (PP r, F.Reftable r) => PP (TypeDeclQ q r) where
  pp (TD s p m) = pp s $+$ text "pred" <+> pp p $+$
                  lbrace $+$ nest 4 (pp m) $+$ rbrace

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

instance PP EnumDef where
  pp (EnumDef n m) = pp n <+> braces (pp m)

instance PP IContext where
  pp (IC x) = text "Context: " <+> pp x

instance (PP a, PP s, PP t) => PP (Alias a s t) where
  pp (Alias n αs xs body) = text "alias" <+> pp n <> withnull angles comma αs <>
                            withnull brackets comma xs <+> text "=" <+> pp body
    where
      withnull _ _ [] = empty
      withnull s p xs = s $ intersperse p (map pp xs)

instance (PP r, F.Reftable r) => PP (Rsc a r) where
  pp pgm@(Rsc {code = (Src s) }) =  extras
                                $+$ text "\n// CODE"
                                $+$ pp s
    where
      extras =  -- text "\nCONSTANTS"          $+$ nest 4 (pp (consts pgm)) $+$
                text "\nPREDICATE ALIASES"  $+$ nest 4 (pp (pAlias pgm))
            $+$ text "\nTYPE ALIASES"       $+$ nest 4 (pp (tAlias pgm))
            -- $+$ text "\nQUALIFIERS"         $+$ nest 4 (vcat (F.toFix <$> take 3 (pQuals pgm)))
            -- $+$ text "\nQUALIFIERS"         $+$ nest 4 (vcat (F.toFix <$> pQuals pgm))
            -- $+$ text "..."
            $+$ text "\nINVARIANTS"         $+$ nest 4 (vcat (pp <$> invts pgm))

instance (F.Reftable r, PP r) => PP (RSubst r) where
  pp (Su m) | HM.null m      = text "empty"
            | HM.size m < 10 = intersperse comma $ (ppBind <$>) $ HM.toList m
            | otherwise      = vcat $ (ppBind <$>) $ HM.toList m

ppBind (x, t) = pp x <+> text ":=" <+> pp t


-- | PP Fixpoint

instance PP F.Sort where
  pp = pprint

instance PP (F.SortedReft) where
  pp (F.RR s b) = braces (pp s <+> text "|" <+> pp b)

instance PP F.Reft where
  pp = pprint

instance PP (F.SubC c) where
  pp s =  parens (pp (F.slhs s)) <+> text " => " <+> pp (F.srhs s)

