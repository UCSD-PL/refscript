
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE OverlappingInstances      #-}

module Language.Nano.Annots (

  -- * Annotations
    Annot (..), UFact, Fact (..), phiVarsAnnot

  -- * Casts
  , Cast(..), CastDirection(..), noCast, upCast, dnCast, ddCast

  -- * Aliases for annotated Source 
  , AnnBare, UAnnBare, AnnSSA , UAnnSSA
  , AnnType, UAnnType, AnnInfo, UAnnInfo

) where

import           Control.Applicative            hiding (empty)
import           Data.Default
import           Data.Function                  (on)
import           Data.Generics                   
import qualified Data.HashMap.Strict            as M
import           Text.PrettyPrint.HughesPJ 

import           Language.Nano.Types
import           Language.Nano.Locations
import           Language.Nano.Names
import           Language.Nano.Typecheck.Types

import           Language.ECMAScript3.Syntax 
import           Language.ECMAScript3.Syntax.Annotations
import           Language.ECMAScript3.PrettyPrint

import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Types        as F




-----------------------------------------------------------------------------
-- | Casts 
-----------------------------------------------------------------------------
data Cast r  = CNo                                      -- .
             | CDead {                 tgt :: RType r } -- |dead code|
             | CUp   { org :: RType r, tgt :: RType r } -- <t1 UP t2>
             | CDn   { org :: RType r, tgt :: RType r } -- <t1 DN t2>
             deriving (Eq, Ord, Show, Data, Typeable, Functor)

data CastDirection   = CDNo    -- .
                     | CDDead  -- |dead code|
                     | CDUp    -- <UP>
                     | CDDn    -- <DN>
             deriving (Eq, Ord, Show, Data, Typeable)


instance (PP r, F.Reftable r) => PP (Cast r) where
  pp CNo         = text "No cast"
  pp (CDead _)   = text "Dead code"
  pp (CUp t1 t2) = text "<" <+> pp t1 <+> text "UP" <+> pp t2 <+> text ">"
  pp (CDn t1 t2) = text "<" <+> pp t1 <+> text "DN" <+> pp t2 <+> text ">"

instance PP CastDirection where
  pp CDNo   = text "="
  pp CDDead = text "dead"
  pp CDUp   = text "UP"
  pp CDDn   = text "DN"

noCast CDNo = True
noCast _   = False

upCast CDDead = False
upCast CDNo   = True
upCast CDUp   = True
upCast CDDn   = False

dnCast CDDead = False
dnCast CDNo   = True
dnCast CDUp   = False
dnCast CDDn   = True

ddCast CDDead = True
ddCast _      = False



data Fact r
  -- SSA
  = PhiVar      ![(Id SourceSpan)]
  -- Unification
  | TypInst     !IContext ![RType r]
  -- Overloading
  | EltOverload !IContext  !(TypeMember r)
  | Overload    !IContext  !(RType r)
  -- Type annotations
  | VarAnn      !(RType r)
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
    deriving (Eq, Show, Data, Typeable, Functor)

type UFact = Fact ()

data Annot b a = Ann { ann :: a, ann_fact :: [b] } deriving (Show, Data, Typeable)
type AnnBare r = Annot (Fact r) SourceSpan -- NO facts
type AnnSSA  r = Annot (Fact r) SourceSpan -- Phi facts
type AnnType r = Annot (Fact r) SourceSpan -- Phi + t. annot. + Cast facts
type AnnInfo r = M.HashMap SourceSpan [Fact r] 

type UAnnBare = AnnBare () 
type UAnnSSA  = AnnSSA  ()
type UAnnType = AnnType ()
type UAnnInfo = AnnInfo ()


instance HasAnnotation (Annot b) where 
  getAnnotation = ann 

instance Default a => Default (Annot b a) where
  def = Ann def []

instance Default SourceSpan where
  def = srcPos dummySpan
  

instance Ord (AnnSSA  r) where 
  compare (Ann s1 _) (Ann s2 _) = compare s1 s2

-- XXX: This shouldn't have to be that hard...
instance Ord (Fact r) where
  compare (PhiVar i1       ) (PhiVar i2       )   = compare i1 i2
  compare (TypInst c1 t1   ) (TypInst c2 t2   )   = compare (c1,toType <$> t1) (c2,toType <$> t2)
  compare (EltOverload c1 t1) (EltOverload c2 t2) = compare (c1, const () <$> t1) (c2, const () <$> t2)
  compare (Overload c1 t1  ) (Overload c2 t2  )   = compare (c1, toType t1) (c2, toType t2)
  compare (TCast c1 _      ) (TCast c2 _      )   = compare c1 c2
  compare (VarAnn t1       ) (VarAnn t2       )   = on compare toType t1 t2
  compare (FieldAnn f1     ) (FieldAnn f2     )   = on compare (fmap $ const ()) f1 f2
  compare (MethAnn m1      ) (MethAnn m2      )   = on compare (fmap $ const ()) m1 m2
  compare (StatAnn s1      ) (StatAnn s2      )   = on compare (fmap $ const ()) s1 s2
  compare (ConsAnn c1      ) (ConsAnn c2      )   = on compare (fmap $ const ()) c1 c2
  compare (UserCast c1     ) (UserCast c2     )   = on compare (fmap $ const ()) c1 c2
  compare (FuncAnn t1      ) (FuncAnn t2      )   = on compare (fmap $ const ()) t1 t2
  compare (ClassAnn (_,m1) ) (ClassAnn (_,m2) )   = on compare (fst <$>) m1 m2
  compare (IfaceAnn d1     ) (IfaceAnn d2     )   = compare (fmap (const ()) d1) (fmap (const ()) d2) 
  compare (ExporedModElt   ) (ExporedModElt   )   = EQ
  compare (ModuleAnn s1    ) (ModuleAnn s2    )   = compare s1 s2
  compare f1 f2                                   = on compare factToNum f1 f2

factToNum (PhiVar _        ) = 0
factToNum (TypInst _ _     ) = 1
factToNum (EltOverload _ _ ) = 2
factToNum (Overload _  _   ) = 3
factToNum (TCast _ _       ) = 4
factToNum (VarAnn _        ) = 6
factToNum (FieldAnn _      ) = 7
factToNum (MethAnn _       ) = 8
factToNum (StatAnn _       ) = 9
factToNum (ConsAnn _       ) = 10
factToNum (UserCast _      ) = 11
factToNum (FuncAnn _       ) = 12
factToNum (ClassAnn _      ) = 13
factToNum (IfaceAnn _      ) = 14
factToNum (ExporedModElt   ) = 15
factToNum (ModuleAnn _     ) = 16


instance Eq (Annot a SourceSpan) where 
  (Ann s1 _) == (Ann s2 _) = s1 == s2

instance IsLocated (Annot a SourceSpan) where 
  srcPos = ann

instance (F.Reftable r, PP r) => PP (Fact r) where
  pp (PhiVar x)       = text "phi"                    <+> pp x
  pp (TypInst ξ ts)   = text "inst"                   <+> pp ξ <+> pp ts 
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
  pp             = vcat . (ppB <$>) . M.toList 
    where 
      ppB (x, t) = pp x <+> dcolon <+> pp t

instance (PP a, PP b) => PP (Annot b a) where
  pp (Ann x ys) = text "Annot: " <+> pp x <+> pp ys

phiVarsAnnot l = concat [xs | PhiVar xs <- ann_fact l]

