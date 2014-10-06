
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE OverlappingInstances      #-}




module Language.Nano.Annots (

  -- * SSA 
    SsaInfo(..), Var

  -- * Annotations
  , NodeId, Annot (..), UFact, Fact (..), phiVarsAnnot

  -- * Casts
  , Cast(..), CastDirection(..), castDirection, noCast, upCast, dnCast, ddCast

  -- * Aliases for annotated Source 
  , AnnR, AnnBare, UAnnBare, AnnSSA , UAnnSSA
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
import           Language.Nano.Typecheck.Types()

import           Language.ECMAScript3.Syntax 
import           Language.ECMAScript3.Syntax.Annotations
import           Language.ECMAScript3.PrettyPrint

import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Types        as F


-----------------------------------------------------------------------------
-- | Casts 
-----------------------------------------------------------------------------

data Cast r  = CNo                                      -- .
             | CDead { err :: Error  , tgt :: RType r } -- |dead code|
             | CUp   { org :: RType r, tgt :: RType r } -- <t1 UP t2>
             | CDn   { org :: RType r, tgt :: RType r } -- <t1 DN t2>
             deriving (Eq, Show, Data, Typeable, Functor)


data CastDirection   = CDNo    -- .
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

data Fact r
  -- SSA
  = PhiVar      ![Var r]
  | PhiVarTy    ![(Var r, Type)]
  -- Unification
  | TypInst     Int !IContext ![RType r]
  -- Overloading
  | EltOverload !IContext  !(TypeMember r)
  | Overload    !IContext  !(RType r)
  -- Type annotations
  | VarAnn      !(RType r)
  -- Class member annotations
  | FieldAnn    !(TypeMember r)
  | MethAnn     !(TypeMember r) 
  | StatAnn     !(TypeMember r) 
  | ConsAnn     !(TypeMember r)
    
  | UserCast    !(RType r)
  | FuncAnn     !(RType r)
  | TCast       !IContext  !(Cast r)
  -- Named type annotation
  | IfaceAnn    !(IfaceDef r)
  | ClassAnn    !([TVar], Maybe (RelName, [RType r]))
  | ExporedModElt
  | ModuleAnn   !(F.Symbol)
    deriving (Eq, Show, Data, Typeable)

type UFact     = Fact ()

type NodeId    = Int

data Annot b a = Ann { ann_id   :: NodeId
                     , ann      ::  a
                     , ann_fact :: [b] } deriving (Show, Data, Typeable)

type AnnR r    = Annot (Fact r) SourceSpan
type AnnBare r = AnnR r -- NO facts
type AnnSSA  r = AnnR r -- Phi facts
type AnnType r = AnnR r -- Phi + t. annot. + Cast facts
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
  pp (ConsAnn c)      = text "Constructor Annotation" <+> pp c
  pp (UserCast c)     = text "Cast Annotation"        <+> pp c
  pp (ExporedModElt)  = text "Exported"
  pp (FuncAnn t)      = text "Func Annotation"        <+> pp t
  pp (FieldAnn f)     = text "Field Annotation"       <+> pp f
  pp (MethAnn m)      = text "Method Annotation"      <+> pp m
  pp (StatAnn s)      = text "Static Annotation"      <+> pp s
  pp (ClassAnn _)     = text "UNIMPLEMENTED:pp:ClassAnn"
  pp (IfaceAnn _)     = text "UNIMPLEMENTED:pp:IfaceAnn"
  pp (ModuleAnn s)    = text "module"                 <+> pp s

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
    go (UserCast t)       = [t]
    go (FuncAnn t)        = [t]
    go (FieldAnn m)       = [f_type m]
    go (MethAnn m)        = [f_type m]
    go (StatAnn m)        = [f_type m]
    go (ConsAnn m)        = [f_type m]
    go (IfaceAnn ifd)     = f_type . snd <$> M.toList (t_elts ifd)
    go (ClassAnn (_, c))  = maybe [] snd c
    go f                  = error ("factRTypes: TODO :" ++ show f)

