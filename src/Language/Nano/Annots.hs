
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE OverlappingInstances   #-}

module Language.Nano.Annots (

  -- * SSA 
    SsaInfo(..), Var

  -- * Annotations
  , NodeId, Annot (..), UFact, FactQ (..), Fact, phiVarsAnnot

  -- * Casts
  , CastQ(..), Cast, SubTRes(..), castType

  -- * Aliases for annotated Source 
  , AnnQ, AnnR, AnnRel, AnnBare, UAnnBare, AnnSSA , UAnnSSA
  , AnnType, UAnnType, AnnInfo, UAnnInfo

  -- Options
  , RscOption (..)
                                  
) where

import           Control.Applicative            hiding (empty)
import           Data.Default
import           Data.Maybe                     (maybeToList)
import           Data.Monoid
import qualified Data.Map.Strict                as M
import qualified Data.IntMap.Strict             as I
import           Data.Generics                   
import           Text.PrettyPrint.HughesPJ 

import           Language.Nano.Types
import           Language.Nano.Env
import           Language.Nano.Locations
import           Language.Nano.Names
import           Language.Nano.Typecheck.Types

import           Language.Nano.Syntax 
import           Language.Nano.Syntax.Annotations
import           Language.Nano.Syntax.PrettyPrint

import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Types        as F


-----------------------------------------------------------------------------
-- | Casts 
-----------------------------------------------------------------------------

data CastQ q r = CNo                                            -- .
               | CDead { err :: [Error]   , tgt :: RTypeQ q r } -- |dead code|
               | CUp   { org :: RTypeQ q r, tgt :: RTypeQ q r } -- <t1 UP t2>
               | CDn   { org :: RTypeQ q r, tgt :: RTypeQ q r } -- <t1 DN t2>
               deriving (Data, Typeable, Functor)

type Cast  = CastQ AK   -- Version with absolute types

castType CNo = tNull
castType c   = tgt c


data SubTRes = EqT              -- .
             | SubErr  [Error]   -- |dead code|
             | SubT              -- <UP>
             | SupT              -- <DN>
             deriving (Eq, Ord, Show, Data, Typeable)

instance Monoid SubTRes where
 mempty                          = EqT
 mappend (SubErr e1) (SubErr e2) = SubErr $ e1 ++ e2
 mappend _           (SubErr e2) = SubErr e2
 mappend (SubErr e1) _           = SubErr e1
 mappend SubT        SupT        = SubErr []
 mappend SupT        SubT        = SubErr []
 mappend SupT        _           = SupT
 mappend _           SupT        = SupT
 mappend SubT        _           = SubT
 mappend _           SubT        = SubT
 mappend EqT        EqT          = EqT

instance (PP r, F.Reftable r) => PP (Cast r) where
  pp CNo         = text "No cast"
  pp (CDead e t) = text "Dead code:" <+> pp e <+> text "::" <+> pp t
  pp (CUp t1 t2) = text "<" <+> pp t1 <+> text "UP" <+> pp t2 <+> text ">"
  pp (CDn t1 t2) = text "<" <+> pp t1 <+> text "DN" <+> pp t2 <+> text ">"

instance PP SubTRes where
  pp EqT        = text "="
  pp (SubErr _) = text "dead"
  pp SubT       = text "UP"
  pp SupT       = text "DN"


-----------------------------------------------------------------------------
-- | Facts
-----------------------------------------------------------------------------

data FactQ q r
  -- SSA
  = PhiVar      ![Var r]
  | PhiVarTC    !(Var r)
  | PhiVarTy    !(Var r, RTypeQ q r)
  | PhiPost     ![(Var r, Var r, Var r)]

  -- Unification
  | TypInst     Int !IContext ![RTypeQ q r]

  -- Overloading
  | EltOverload !IContext  !(MethodInfoQ q r)
  | Overload    !IContext  !(RTypeQ q r)

  -- Type annotations
  | VarAnn      !(Assignability, Maybe (RTypeQ q r))
  | AmbVarAnn   !(RTypeQ q r)

  -- Class member annotations
  | FieldAnn    !(FieldInfoQ q r)
  | MethAnn     !(MethodInfoQ q r) 
  | ConsAnn     !(RTypeQ q r)
    
  | UserCast    !(RTypeQ q r)
  | FuncAnn     !(RTypeQ q r)
  | TCast       !IContext  !(CastQ q r)

  -- Named type annotation
  | ClassAnn     !(TypeSigQ q r)
  | InterfaceAnn !(TypeDeclQ q r)

  | ExportedElt
  | ReadOnlyVar
  | ModuleAnn   !(F.Symbol)
  | EnumAnn     !(F.Symbol)

  -- Auxiliary
  | BypassUnique
    deriving (Data, Typeable)

type Fact      = FactQ AK
type UFact     = Fact ()

type NodeId    = Int

data Annot b a = Ann { ann_id   :: NodeId
                     , ann      ::  a
                     , ann_fact :: [b] } deriving (Show, Data, Typeable)

type AnnQ q  r = Annot (FactQ q r) SrcSpan
type AnnR    r = AnnQ AK r                      -- absolute paths,  
type AnnRel  r = AnnQ RK r                      -- relative paths, NO facts, parsed versioin
type AnnBare r = AnnR r                         -- absolute paths, NO facts
type AnnSSA  r = AnnR r                         -- absolute paths, Phi facts
type AnnType r = AnnR r                         -- absolute paths, Phi + t. annot. + Cast facts
type AnnInfo r = I.IntMap [Fact r] 

type UAnnBare  = AnnBare () 
type UAnnSSA   = AnnSSA  ()
type UAnnType  = AnnType ()
type UAnnInfo  = AnnInfo ()


newtype SsaInfo r = SI (Var r) deriving (Ord, Typeable, Data)
type    Var     r = Id (AnnSSA r)

instance PP (SsaInfo r) where
  pp (SI i) =  pp $ fmap (const ()) i

instance Eq (SsaInfo r) where
  SI i1 == SI i2 =  i1 == i2 


instance HasAnnotation (Annot b) where 
  getAnnotation = ann 

instance Default a => Default (Annot b a) where
  def = Ann def def []

instance Ord (AnnSSA  r) where 
  compare (Ann i1 s1 _) (Ann i2 s2 _) = compare (i1,s1) (i2,s2)

instance Eq (Annot a SrcSpan) where 
  (Ann i1 s1 _) == (Ann i2 s2 _) = (i1,s1) == (i2,s2)


instance (F.Reftable r, PP r) => PP (Fact r) where
  pp (PhiVar x)                 = text "phi"             <+> pp x
  pp (PhiVarTy x)               = text "phi-ty"          <+> pp x
  pp (PhiVarTC x)               = text "phi-tc"          <+> pp x
  pp (PhiPost _)                = text "phi-post"
  pp (TypInst i ξ ts)           = text "inst"            <+> pp i <+> pp ξ <+> pp ts
  pp (Overload ξ i)             = text "overload"        <+> pp ξ <+> pp i
  pp (EltOverload ξ (MI _ _ t)) = text "elt_overload"    <+> pp ξ <+> pp t
  pp (TCast  ξ c)               = text "cast"            <+> pp ξ <+> pp c
  pp (VarAnn (_,t))             = text "Var Ann"         <+> pp t
  pp (AmbVarAnn t)              = text "Amb Var Ann"     <+> pp t
  pp (ConsAnn c)                = text "Ctor Ann"        <+> pp c
  pp (UserCast c)               = text "Cast Ann"        <+> pp c
  pp (ExportedElt)              = text "Exported"
  pp (ReadOnlyVar)              = text "ReadOnlyVar"
  pp (FuncAnn t)                = text "Func Ann"        <+> pp t
  pp (FieldAnn (FI _ _ t))      = text "Field Ann"       <+> pp t
  pp (MethAnn (MI _ _ t))       = text "Method Ann"      <+> pp t
  pp (InterfaceAnn _)           = text "UNIMPLEMENTED:pp:InterfaceAnn"
  pp (ClassAnn _)               = text "UNIMPLEMENTED:pp:ClassAnn"
  pp (ModuleAnn s)              = text "module"          <+> pp s
  pp (EnumAnn s)                = text "enum"            <+> pp s
  pp (BypassUnique)             = text "BypassUnique"

instance (F.Reftable r, PP r) => PP (AnnInfo r) where
  pp             = vcat . (ppB <$>) . I.toList 
    where 
      ppB (x, t) = pp x <+> dcolon <+> pp t

instance (PP a, PP b) => PP (Annot b a) where
  pp (Ann _ x ys) = text "Annot: " <+> pp x <+> pp ys

phiVarsAnnot l = concat [xs | PhiVar xs <- ann_fact l]


-----------------------------------------------------------------------------
-- | RSC Options
-----------------------------------------------------------------------------

data RscOption = RealOption
    deriving (Eq, Show, Data, Typeable)

