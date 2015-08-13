
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE DeriveFoldable            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE IncoherentInstances       #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.Nano.Pretty.Types (

) where

import           Control.Applicative            ((<$>))

import           Language.Nano.AST
import           Language.Nano.Pretty.Common
import           Language.Nano.Pretty.Syntax
import           Language.Nano.Program
import           Language.Nano.Types
import           Language.Nano.Typecheck.Types

import           Language.Fixpoint.Misc         (intersperse)
import qualified Language.Fixpoint.Types        as F
import           Text.PrettyPrint.HughesPJ 
import qualified Data.Map.Strict                as M
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree

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
  pp (TVar α r)      = F.ppTy r $ (text "#" <> pp α)
  pp (TOr ts)        = ppArgs id (text " +") ts
  pp (TAnd ts)       = vcat [text "/\\" <+> pp t | t <- ts]
  pp (TRef t r)      = F.ppTy r $ pp t
  pp (TObj ms r)     = F.ppTy r $ braces $ pp ms
  pp (TType k t)     = pp k <+> pp t
  pp (TMod t)        = text "module" <+> pp t
  pp t@(TAll _ _)    = ppArgs angles comma αs <> text "." <+> pp t' where (αs, t') = bkAll t
  pp (TFun xts t _)  = ppArgs parens comma xts <+> text "=>" <+> pp t  
  pp (TExp e)        = pprint e 

instance PP NamedTypeKind where
  pp ClassK          = text "class"
  pp EnumK           = text "enum"

instance (F.Reftable r, PP r) => PP (TypeMembersQ q r) where
  pp (TM fs ms sfs sms cs cts sidx nidx) = ppProp fs  <+> ppMeth ms  <+> 
                                           ppProp sfs <+> ppMeth sms <+> 
                                           ppCall cs  <+> ppCtor cts <+>
                                           ppIdx sidx <+> ppIdx nidx

ppProp  = undefined
ppMeth  = undefined
ppCall  = undefined
ppCtor  = undefined
ppIdx   = undefined

instance (F.Reftable r, PP r) => PP (TGenQ q r) where
  pp (Gen x ts) = pp x <> ppArgs angles comma ts

instance (F.Reftable r, PP r) => PP (BTGenQ q r) where
  pp (BGen x ts) = pp x <> ppArgs angles comma ts

instance PP TVar where 
  pp = pprint . F.symbol

instance (F.Reftable r, PP r) => PP (BTVarQ q r) where 
  pp (BTV v t _) = pprint v <+> text "<:" <+> pp t

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

instance (PP r, F.Reftable r) => PP (BindQ q r) where 
  pp (B x t) = pp x <> colon <> pp t 

instance (PP s, PP t) => PP (M.Map s t) where
  pp m = vcat $ pp <$> M.toList m

instance PP Assignability where
  pp Ambient      = text "Ambient"
  pp WriteLocal   = text "WriteLocal"
  pp ForeignLocal = text "ForeignLocal"
  pp WriteGlobal  = text "WriteGlobal"
  pp ReturnVar    = text "ReturnVar"

instance (PP r, F.Reftable r) => PP (TypeDeclQ q r) where
  pp (TD s m) = pp s <+> lbrace $+$ nest 2 (pp m) $+$ rbrace

instance (PP r, F.Reftable r) => PP (TypeSigQ q r) where
  pp (TS k n h) = pp k <+> pp n <+> ppHeritage h

instance PP TypeDeclKind where
  pp InterfaceKind  = text "interface"
  pp ClassKind      = text "class"

ppHeritage (es,is) = ppExtends es <+> ppImplements is

ppExtends Nothing  = text ""
ppExtends (Just n) = text "extends" <+> pp n

ppImplements [] = text ""
ppImplements ts = text "implements" <+> intersperse comma (pp <$> ts)

mutSym (TRef n _) | s == F.symbol "Mutable"       = Just "_MU_"
                  | s == F.symbol "UniqueMutable" = Just "_UM_"
                  | s == F.symbol "Immutable"     = Just "_IM_"
                  | s == F.symbol "ReadOnly"      = Just "_RO_"
                  | s == F.symbol "AssignsFields" = Just "_AF"
  where s = F.symbol n
mutSym _ = Nothing

ppMut t@TVar{} = pp t
ppMut t        | Just s <- mutSym t = pp s
               | otherwise          = pp "_??_"

instance PP EnumDef where
  pp (EnumDef n m) = pp n <+> braces (pp m)

instance (F.Reftable r, PP r) => PP (VarInfo r) where 
  pp (VI _ _ t) = pp t
 
instance (PP r, F.Reftable r) => PP (ModuleDef r) where
  pp (ModuleDef vars tys enums path) =  
          text "==================="
      $+$ text "module" <+> pp path 
      $+$ text "==================="
      $+$ text "Variables" 
      $+$ text "----------"
      $+$ braces (pp vars)
      $+$ text "-----"
      $+$ text "Types" 
      $+$ text "-----"
      $+$ pp tys
      $+$ text "-----"
      $+$ text "Enums" 
      $+$ text "-----"
      $+$ pp enums

instance PP IContext where
  pp (IC x) = text "Context: " <+> pp x

instance PP Initialization where
  pp Initialized   = text "init"
  pp Uninitialized = text "non-init"

instance (PP a, PP s, PP t) => PP (Alias a s t) where
  pp (Alias n _ _ body) = text "alias" <+> pp n <+> text "=" <+> pp body

instance (F.Reftable r, PP r) => PP (ClassHierarchy r) where
  pp (ClassHierarchy g _)   =  text "***********************************************"
                           $+$ text "Class Hierarchy"
                           $+$ text "***********************************************"
                           $+$ vcat (ppEdge <$> edges g)
    where
      ppEdge (a,b)          =  ppNode a <+> text "->" <+> ppNode b
      ppNode                =  pp . lab' . context g


instance (PP r, F.Reftable r) => PP (Nano a r) where
  pp pgm@(Nano {code = (Src s) })
    =   text "\n******************* Code **********************"
    $+$ pp s
    $+$ text "\n******************* Constants *****************"
    $+$ pp (consts pgm)
    $+$ text "\n******************* Predicate Aliases *********"
    $+$ pp (pAlias pgm)
    $+$ text "\n******************* Type Aliases **************"
    $+$ pp (tAlias pgm)
    $+$ text "\n******************* Qualifiers ****************"
    $+$ vcat (F.toFix <$> take 3 (pQuals pgm))
    $+$ text "..."
    $+$ text "\n******************* Invariants ****************"
    $+$ vcat (pp <$> invts pgm)
    $+$ text "\n***********************************************\n"

