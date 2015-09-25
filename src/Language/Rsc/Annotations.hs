{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}

module Language.Rsc.Annotations (

  -- * SSA
    SsaInfo(..), Var

  , NodeId
  , UFact, FactQ (..), Fact, phiVarsAnnot

  , SyntaxKind(..)

  , scrapeVarDecl

  -- * Casts
  , CastQ(..), Cast, SubTRes(..), castType

  -- * Annotations & aliases
  , Annot (..)
  , FAnnQ (..)
  , AnnR, AnnRel, AnnBare, AnnSSA, AnnTc, AnnLq

  , UAnnBare, UAnnSSA, UAnnTc, AnnInfo, UAnnInfo

) where

import           Data.Default
import           Data.Generics
import qualified Data.IntMap.Strict           as I
import           Data.Monoid
import           Language.Fixpoint.Errors
import qualified Language.Fixpoint.Types      as F
import           Language.Rsc.AST.Annotations
import           Language.Rsc.AST.Syntax
import           Language.Rsc.Locations
import           Language.Rsc.Names
import           Language.Rsc.Typecheck.Types
import           Language.Rsc.Types


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
 mappend EqT         EqT         = EqT


-----------------------------------------------------------------------------
-- | Facts
-----------------------------------------------------------------------------

data FactQ q r
  -- ** ANALYSIS **
  -- SSA
  = PhiVar        [Var r]
  | PhiVarTC      (Var r)
  | PhiVarTy      (Var r, RTypeQ q r)
  | PhiPost       [(Var r, Var r, Var r)]

  -- Unification
  | TypInst       Int IContext [RTypeQ q r]

  -- Typechecking
  | TCast         IContext (CastQ q r)

  -- Overloading
  | EltOverload   IContext (MethodInfoQ q r)
  | Overload      IContext Int  -- Overload index

  -- ** SPECIFICATIONS **

  -- Type annotations
  | SigAnn        Locality (RTypeQ q r)
  | VarAnn        Locality Assignability (Maybe (RTypeQ q r))

  -- Class member annotations
  | FieldAnn      (FieldInfoQ q r)
  | MethAnn       (MethodInfoQ q r)
  | CtorAnn       (RTypeQ q r)

  -- User specifier casts
  | UserCast      (RTypeQ q r)

  -- Named type annotation
  | ClassAnn      Locality (TypeSigQ q r)
  | InterfaceAnn  (TypeDeclQ q r)

  | ModuleAnn     (F.Symbol)
  | EnumAnn       (F.Symbol)

  -- Auxiliary
  | BypassUnique
    deriving (Data, Typeable)

type Fact      = FactQ AK
type UFact     = Fact ()

type NodeId    = Int

data Annot b a = Ann { ann_id   :: NodeId
                     , ann      ::  a
                     , ann_fact :: [b]
                     } deriving (Show, Data, Typeable)

instance Annotated (Annot b) where
  getAnnotation = ann

instance Default a => Default (Annot b a) where
  def = Ann def def []

instance Eq (Annot a SrcSpan) where
  (Ann i1 s1 _) == (Ann i2 s2 _) = (i1,s1) == (i2,s2)


data FAnnQ q r = FA  { fId   :: NodeId
                     , fSrc  :: SrcSpan
                     , fFact :: [FactQ q r]
                     } deriving (Data, Typeable)

instance IsLocated (FAnnQ q r) where
  srcPos (FA _ s _) = s

instance Default (FAnnQ q r) where
  def = FA def def []

instance Eq (FAnnQ q r) where
  (FA i1 s1 _) == (FA i2 s2 _) = (i1,s1) == (i2,s2)


type AnnR   r  = FAnnQ AK r                      -- absolute paths,
type AnnRel r  = FAnnQ RK r                      -- relative paths, NO facts, parsed versioin
type AnnBare r = AnnR r                         -- absolute paths, NO facts
type AnnSSA  r = AnnR r                         -- absolute paths, Phi facts
type AnnTc   r = AnnR r                         -- absolute paths, Phi + t. annot. + Cast facts
type AnnLq     = AnnR F.Reft

type AnnInfo r = I.IntMap [Fact r]

type UAnnBare  = AnnBare ()
type UAnnSSA   = AnnSSA  ()
type UAnnTc    = AnnTc ()
type UAnnInfo  = AnnInfo ()


newtype SsaInfo r = SI (Var r) deriving (Typeable, Data)

instance Eq (SsaInfo r) where
  SI i1 == SI i2 = i1 == i2

type Var r = Id (AnnSSA r)


phiVarsAnnot l = concat [xs | PhiVar xs <- fFact l]


-- | scrapeVarDecl: Scrape a variable declaration for annotations
----------------------------------------------------------------------------------
scrapeVarDecl :: VarDecl (AnnSSA r) -> [(Locality, SyntaxKind, Assignability, Maybe (RType r))]
----------------------------------------------------------------------------------
scrapeVarDecl (VarDecl l _ _)
  = [ (loc, VarDeclKind, a, t)                | VarAnn loc a t      <- fFact l ]
 ++ [ (Local, FieldDeclKind, Ambient, Just t) | FieldAnn (FI _ _ t) <- fFact l ]
 -- PV: Assignability & Locality values are default for the field case

