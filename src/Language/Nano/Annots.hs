
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
  , CastQ(..), Cast, CastDirection(..), castDirection, noCast, upCast, dnCast, ddCast, castType

  -- * Aliases for annotated Source 
  , AnnQ, AnnR, AnnRel, AnnBare, UAnnBare, AnnSSA , UAnnSSA
  , AnnType, UAnnType, AnnInfo, UAnnInfo

  -- * Deconstructing Facts
  , factRTypes
                                  
) where

import           Control.Applicative            hiding (empty)
import           Data.Default
import           Data.Monoid
import qualified Data.Map.Strict                as M
import qualified Data.IntMap.Strict             as I
import           Data.Generics                   
import           Text.PrettyPrint.HughesPJ 

import           Language.Nano.Types
import           Language.Nano.Locations
import           Language.Nano.Names
import           Language.Nano.Typecheck.Types

import           Language.ECMAScript3.Syntax 
import           Language.ECMAScript3.Syntax.Annotations
import           Language.ECMAScript3.PrettyPrint

import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Types        as F


-----------------------------------------------------------------------------
-- | Casts 
-----------------------------------------------------------------------------

data CastQ q r = CNo                                            -- .
               | CDead { err :: Error     , tgt :: RTypeQ q r } -- |dead code|
               | CUp   { org :: RTypeQ q r, tgt :: RTypeQ q r } -- <t1 UP t2>
               | CDn   { org :: RTypeQ q r, tgt :: RTypeQ q r } -- <t1 DN t2>
               deriving (Eq, Show, Data, Typeable, Functor)

type CastR = CastQ RK   -- Version with relative types
type Cast  = CastQ AK   -- Version with absolute types

castType CNo = tNull
castType c   = tgt c


data CastDirection  = CDNo    -- .
                    | CDDead  -- |dead code|
                    | CDUp    -- <UP>
                    | CDDn    -- <DN>
             deriving (Eq, Ord, Show, Data, Typeable)

instance Monoid CastDirection where
 mempty             = CDNo
 mappend CDDead _   = CDDead
 mappend _ CDDead   = CDDead

 mappend CDUp CDDn  = CDDead
 mappend CDDn CDUp  = CDDead

 mappend CDDn _     = CDDn
 mappend _    CDDn  = CDDn

 mappend CDUp _     = CDUp
 mappend _    CDUp  = CDUp

 mappend CDNo CDNo  = CDNo


instance (PP r, F.Reftable r) => PP (Cast r) where
  pp CNo         = text "No cast"
  pp (CDead e t) = text "Dead code:" <+> pp e <+> text "::" <+> pp t
  pp (CUp t1 t2) = text "<" <+> pp t1 <+> text "UP" <+> pp t2 <+> text ">"
  pp (CDn t1 t2) = text "<" <+> pp t1 <+> text "DN" <+> pp t2 <+> text ">"

instance PP CastDirection where
  pp CDNo   = text "="
  pp CDDead = text "dead"
  pp CDUp   = text "UP"
  pp CDDn   = text "DN"

noCast = (`elem` [CDNo])
upCast = (`elem` [CDNo, CDUp])
dnCast = (`elem` [CDNo, CDDn])
ddCast = (`elem` [CDDead])

-- upCast = (`elem` [CDNo, CDUp])
-- dnCast = (`elem` [CDNo, CDDn])
-- ddCast = (`elem` [CDDead])

castDirection (CNo  {}) = CDNo
castDirection (CDead{}) = CDDead
castDirection (CUp  {}) = CDUp
castDirection (CDn  {}) = CDDn

data FactQ q r
  -- SSA
  = PhiVar      ![Var r]
  | PhiVarTy    ![(Var r, RTypeQ q ())]
  -- Unification
  | TypInst     Int !IContext ![RTypeQ q r]
  -- Overloading
  | EltOverload !IContext  !(TypeMemberQ q r)
  | Overload    !IContext  !(RTypeQ q r)
  -- Type annotations
  | VarAnn      !(RTypeQ q r)
  | AmbVarAnn   !(RTypeQ q r)
  -- Class member annotations
  | FieldAnn    !(TypeMemberQ q r)
  | MethAnn     !(TypeMemberQ q r) 
  | StatAnn     !(TypeMemberQ q r) 
  | ConsAnn     !(TypeMemberQ q r)
    
  | UserCast    !(RTypeQ q r)
  | FuncAnn     !(RTypeQ q r)
  | TCast       !IContext  !(CastQ q r)
  -- Named type annotation
  | IfaceAnn    !(IfaceDefQ q r)
  | ClassAnn    !(ClassSigQ q r)
  | ExportedElt
  | ModuleAnn   !(F.Symbol)
  | EnumAnn     !(F.Symbol)
    deriving (Eq, Show, Data, Typeable)

type FactR     = FactQ RK
type Fact      = FactQ AK
type UFact     = Fact ()

type NodeId    = Int

data Annot b a = Ann { ann_id   :: NodeId
                     , ann      ::  a
                     , ann_fact :: [b] } deriving (Show, Data, Typeable)

type AnnQ q  r = Annot (FactQ q r) SourceSpan
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

instance Show r => Show (SsaInfo r) where
  show (SI v) = show v

instance PP (SsaInfo r) where
  pp (SI i) =  pp $ fmap (const ()) i

instance Eq (SsaInfo r) where
  SI i1 == SI i2 =  i1 == i2 


instance HasAnnotation (Annot b) where 
  getAnnotation = ann 

instance Default a => Default (Annot b a) where
  def = Ann def def []

instance Default SourceSpan where
  def = srcPos dummySpan
  
instance Ord (AnnSSA  r) where 
  compare (Ann i1 s1 _) (Ann i2 s2 _) = compare (i1,s1) (i2,s2)

instance Eq (Annot a SourceSpan) where 
  (Ann i1 s1 _) == (Ann i2 s2 _) = (i1,s1) == (i2,s2)

instance IsLocated (Annot a SourceSpan) where 
  srcPos = ann

instance (F.Reftable r, PP r) => PP (Fact r) where
  pp (PhiVar x)       = text "phi"                    <+> pp x
  pp (PhiVarTy x)     = text "phi-ty"                 <+> pp x
  pp (TypInst i ξ ts) = text "inst"                   <+> pp i <+> pp ξ <+> pp ts 
  pp (Overload ξ i)   = text "overload"               <+> pp ξ <+> pp i
  pp (EltOverload ξ i)= text "elt_overload"           <+> pp ξ <+> pp i
  pp (TCast  ξ c)     = text "cast"                   <+> pp ξ <+> pp c
  pp (VarAnn t)       = text "Var Annotation"         <+> pp t
  pp (AmbVarAnn t)    = text "Amb Var Annotation"     <+> pp t
  pp (ConsAnn c)      = text "Constructor Annotation" <+> pp c
  pp (UserCast c)     = text "Cast Annotation"        <+> pp c
  pp (ExportedElt)    = text "Exported"
  pp (FuncAnn t)      = text "Func Annotation"        <+> pp t
  pp (FieldAnn f)     = text "Field Annotation"       <+> pp f
  pp (MethAnn m)      = text "Method Annotation"      <+> pp m
  pp (StatAnn s)      = text "Static Annotation"      <+> pp s
  pp (ClassAnn _)     = text "UNIMPLEMENTED:pp:ClassAnn"
  pp (IfaceAnn _)     = text "UNIMPLEMENTED:pp:IfaceAnn"
  pp (ModuleAnn s)    = text "module"                 <+> pp s
  pp (EnumAnn s)      = text "enum"                   <+> pp s

instance (F.Reftable r, PP r) => PP (AnnInfo r) where
  pp             = vcat . (ppB <$>) . I.toList 
    where 
      ppB (x, t) = pp x <+> dcolon <+> pp t

instance (PP a, PP b) => PP (Annot b a) where
  pp (Ann _ x ys) = text "Annot: " <+> pp x <+> pp ys

phiVarsAnnot l = concat [xs | PhiVar xs <- ann_fact l]

factRTypes :: (Show r) => Fact r -> [RType r]
factRTypes = go
  where
    go (TypInst _ _ ts)   = ts
    go (EltOverload _ m)  = [f_type m]
    go (Overload _ t)     = [t] 
    go (VarAnn t)         = [t]
    go (AmbVarAnn t)      = [t]
    go (UserCast t)       = [t]
    go (FuncAnn t)        = [t]
    go (FieldAnn m)       = [f_type m]
    go (MethAnn m)        = [f_type m]
    go (StatAnn m)        = [f_type m]
    go (ConsAnn m)        = [f_type m]
    go (IfaceAnn ifd)     = f_type . snd <$> M.toList (t_elts ifd)
    go (ClassAnn (_,e,i)) = concatMap snd e ++ concatMap snd i
    go f                  = error ("factRTypes: TODO :" ++ show f)

