{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE FlexibleInstances  #-}

module Language.Rsc.Annotations (

  -- * SSA
    Var

  , NodeId
  , UFact, FactQ (..), Fact, phiVarsAnnot

  , SyntaxKind(..)

  , scrapeVarDecl

  -- * Annotations & aliases
  , Annot (..)
  , FAnnQ (..)
  , AnnR, AnnRel, AnnBare, AnnSSA, AnnTc, AnnLq

  , UAnnBare, UAnnSSA, UAnnTc, AnnInfo, UAnnInfo

  -- * Uniqueness
  , enableUnique, isUniqueEnabled

) where

import           Data.Default
import           Data.Generics
import qualified Data.IntMap.Strict             as I
import qualified Language.Fixpoint.Types        as F
import           Language.Fixpoint.Types.Errors
import           Language.Rsc.AST.Annotations
import           Language.Rsc.AST.Syntax
import           Language.Rsc.Core.Env
import           Language.Rsc.Locations
import           Language.Rsc.Names
import           Language.Rsc.Types


-----------------------------------------------------------------------------
-- | Facts
-----------------------------------------------------------------------------

data FactQ q r
  -- ** ANALYSIS **
  -- SSA
  = PhiVar        (Var r)            -- Φ-var (SSA version) at end of if-stmt
  | PhiLoop       (Var r, Var r)     -- Φ-var (SSA version) before loop and at
                                     -- end of loop body
                                     --
  | PhiLoopTC     (Var r, Var r, Type)

  -- | PhiVarTC      (Var r)
  -- | PhiVarTy      (Var r, RTypeQ q r)
  -- | PhiLoop       !(Env (Var r, Var r))
  -- | PhiPost       [(Var r, Var r, Var r)]

  -- Unification
  | TypInst       Int IContext [RTypeQ q r]

  -- Typechecking
  | DeadCast      IContext [Error]
  | TypeCast      IContext Type

  -- Overloading
  | EltOverload   IContext (MutabilityQ q r) (RTypeQ q r)
  | Overload      IContext F.Symbol Int  -- (Overload index) (Func Name) (Node id)

  -- ** SPECIFICATIONS **

  -- Type annotations
  | SigAnn        Locality (RTypeQ q r)
  | VarAnn        Locality Assignability (Maybe (RTypeQ q r))

  -- Class member annotations
  | MemberAnn     (TypeMemberQ q r)
  | CtorAnn       (RTypeQ q r)

  -- User specifier casts
  | UserCast      (RTypeQ q r)

  -- Named type annotation
  | ClassAnn      Locality (TypeSigQ q r)
  | InterfaceAnn  (TypeDeclQ q r)

  | ModuleAnn     Locality (QP q)
  | EnumAnn       F.Symbol

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

instance {-# OVERLAPPING #-} IsLocated (FAnnQ q r) where
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

type Var r = Id (AnnSSA r)

phiVarsAnnot l = [ x | PhiVar x <- fFact l]


-- | scrapeVarDecl: Scrape a variable declaration for annotations
----------------------------------------------------------------------------------
scrapeVarDecl :: VarDecl (AnnSSA r) -> [(Locality, SyntaxKind, Assignability, Maybe (RType r))]
----------------------------------------------------------------------------------
scrapeVarDecl (VarDecl l _ _)
  = [ (loc, VarDeclKind, a, t)                | VarAnn loc a t         <- fFact l ]
 ++ [ (Local, FieldDeclKind, Ambient, Just t) | MemberAnn (FI _ _ _ t) <- fFact l ]
 -- PV: Assignability & Locality values are default for the field case


--------------------------------------------------------------------------------
enableUnique :: Functor f => f (FAnnQ q r) -> f (FAnnQ q r)
--------------------------------------------------------------------------------
enableUnique e   = fmap (\a -> a { fFact = BypassUnique : fFact a }) e

--------------------------------------------------------------------------------
isUniqueEnabled :: Annotated a => a (FAnnQ q r) -> Bool
--------------------------------------------------------------------------------
isUniqueEnabled e = any isBypassUnique $ fFact $ getAnnotation e
  where
    isBypassUnique BypassUnique = True
    isBypassUnique _            = False


