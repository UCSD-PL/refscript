
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveFunctor          #-}

module Language.Nano.Types where

import           Control.Applicative                ((<$>))
import           Data.Hashable
import           Data.Default
import           Data.Monoid
import           Data.Function                      (on)
import qualified Data.Map.Strict                 as M
import           Data.Typeable                      (Typeable)
import           Data.Generics                      (Data)
import           Data.List                          ((\\))
import           Data.Traversable            hiding (sequence, mapM)
import           Data.Foldable                      (Foldable())
import           Language.Nano.Syntax
import           Language.Nano.Syntax.PrettyPrint          (PP (..))

import qualified Language.Fixpoint.Types         as F

import           Language.Fixpoint.Misc
import           Language.Nano.Env
import           Language.Nano.Names
import           Language.Nano.Locations
import           Text.PrettyPrint.HughesPJ


---------------------------------------------------------------------------------
-- | RefScript Types
---------------------------------------------------------------------------------


-- | Type parameter
data TParam q r = TP  { tp_sym    :: F.Symbol               -- Parameter symbol
                      , tp_constr :: RTypeQ q r             -- Constraint
                      , tp_loc    :: SrcSpan 
                      } deriving (Data, Typeable, Functor, Foldable, Traversable)

data TIntrinsic = TAny | TString | TNumber | TBoolean | TVoid | TUndefined | TNull deriving (Eq, Show, Data, Typeable)


-- | (Raw) Refined Types
data RTypeQ q r = Intrinsic         TIntrinsic              -- Intrinsic
                                    r                       -- Refinement

                | StringLitType     String                  -- String literal type

                | TypeParam         (TParam q r) r

                | UnionType         [RTypeQ q r]            -- Union members

                | ITypeWithBase     [RTypeQ q r]            -- Type parameters
                                    [ObjectTypeQ q r]       -- Base types
                                    r 

                | TypeReference     (GenericTypeQ q r) r 

                | ITypeWithMembers  (TypeMembersQ q r) r

                {- Internal -}

                | TExp              F.Expr

                {- TODO: Fill in with more cases -}

  deriving (Data, Typeable, Functor, Foldable, Traversable)


type GenericTypeQ q r = (QN q, [RTypeQ q r])
type ObjectTypeQ  q r = RTypeQ q r
type TypeParamQ   q r = RTypeQ q r

data ArrowSigQ    q r = ArrSig [TypeParamQ q r] [BindQ q r] (RTypeQ q r) r  deriving (Data, Typeable, Functor, Foldable, Traversable)
data BindQ        q r = B { b_sym  :: F.Symbol, b_type :: !(RTypeQ  q r) } deriving (Eq, Data, Typeable, Functor, Foldable, Traversable)

type FieldSigQ    q r = (F.Symbol, OptionalityQ q, MutabilityQ q, RTypeQ q r)
type CallSigQ     q r = (F.Symbol, ArrowSigQ q r)
type CtorSigQ     q r = ArrowSigQ q r

type TypeMembersQ q r = ( [FieldSigQ q r]       -- properties
                        , [CallSigQ  q r]       -- call signatures
                        , [CtorSigQ  q r]       -- contructor signatures
                        , Maybe (RTypeQ q r)    -- string indexer
                        , Maybe (RTypeQ q r)    -- numeric indexer
                        ) 



-- | 
type RType            = RTypeQ AK
type Type             = RType ()
type Bind             = BindQ AK
type TypeMembers r    = TypeMembersQ AK r


---------------------------------------------------------------------------------
-- | Type (Class, Interface) definitions
---------------------------------------------------------------------------------

data TypeDeclQ q r = ClassDecl     F.Symbol                       -- Name
                                   [TParam q r]                   -- Type parameters
                                   (Maybe (GenericTypeQ q r))     -- Extends
                                   [GenericTypeQ q r]             -- Implements
                                   (TypeMembersQ q r)             -- Body

                   | InterfaceDecl F.Symbol                       -- Name
                                   [TParam q r]                   -- Type parameters
                                   [GenericTypeQ q r]             -- Extends
                                   (TypeMembersQ q r)             -- Body

  deriving (Data, Typeable, Foldable, Traversable, Functor)

data Modifier = Public | Private | Abstract | Final | Static | Local | Exported 
  deriving (Data, Typeable)


---------------------------------------------------------------------------------
-- | Enumeration definition
---------------------------------------------------------------------------------

data EnumDef = EnumDef {
      e_name       :: F.Symbol
    -- ^ Contents: Symbols -> Expr (expected IntLit or HexLit)
    , e_mapping    :: Env (Expression ())

} deriving (Data, Typeable)


------------------------------------------------------------------------------------------
-- | Module Body
------------------------------------------------------------------------------------------
--
--  As per TypeScript spec par. 10.2:
--
--  Each module body has a declaration space for local variables (including
--  functions, modules, class constructor functions, and enum objects), a
--  declaration space for local named types (classes, interfaces, and enums),
--  and a declaration space for local namespaces (containers of named types).
--  Every declaration (whether local or exported) in a module contributes to
--  one or more of these declaration spaces.
--
--  PV: the last case has not been included
--
data ModuleDefQ q r = ModuleDef {
  --
  -- ^ Contents of a module (local and exported)
  --
  --   * Interfaces are _not_ included here (because thery don't appear as
  --   bindings in the language)
  --
    m_variables   :: Env ([Modifier], Assignability, RTypeQ q r, Initialization)
  --
  -- ^ Types
  --
  , m_types       :: Env (TypeDeclQ q r)
  --
  -- ^ Enumerations
  --
  , m_enums       :: Env EnumDef
  --
  -- ^ Absolute path of definition
  --
  , m_path        :: AbsPath
  }
  deriving (Data, Typeable, Functor)

type ModuleDef = ModuleDefQ AK

instance Monoid (ModuleDefQ q r) where
  mempty = ModuleDef mempty mempty mempty def
  ModuleDef v t e p `mappend` ModuleDef v' t' e' _ = ModuleDef (v `mappend` v')
                                                               (t `mappend` t')
                                                               (e `mappend` e')
                                                               p


------------------------------------------------------------------------------------------
-- | Assignability
------------------------------------------------------------------------------------------

data Assignability
  --
  -- ^ Import,  cannot be modified
  -- ^ Contains: FunctionStmts, Measures, Classes, Modules.
  -- ^ Can appear in refinements
  --
  = ReadOnly
  --
  -- ^ Like ReadOnly but for function declarations with no body
  -- ^ Can appear in refinements.
  --
  | ImportDecl
  --
  -- ^ written in local-scope, can be SSA-ed
  -- ^ Can appear in refinements
  --
  | WriteLocal
  --
  -- ^ Local but not in current scope.
  -- ^ CANNOT appear in refinements, cannot be read at SSA
  --
  | ForeignLocal
  --
  -- ^ Written in non-local-scope, cannot do SSA
  -- ^ CANNOT appear in refinements
  --
  | WriteGlobal
  --
  -- ^ Used to denote return variable
  -- ^ CANNOT appear in refinements
  --
  | ReturnVar
  deriving (Show, Eq, Data, Typeable)


---------------------------------------------------------------------------------
-- | Mutability
---------------------------------------------------------------------------------

type MutabilityQ q = RTypeQ q ()
type Mutability    = Type


---------------------------------------------------------------------------------
-- | Optional Argument
---------------------------------------------------------------------------------

type OptionalityQ q = RTypeQ q ()
type Optionality    = Type


---------------------------------------------------------------------------------
-- | Initialization
---------------------------------------------------------------------------------

data Initialization = Initialized | Uninitialized
  deriving (Show, Eq, Data, Typeable)

instance Monoid Initialization where
  mempty                                = Uninitialized
  _             `mappend` Uninitialized = Uninitialized
  Uninitialized `mappend` _             = Uninitialized
  Initialized   `mappend` Initialized   = Initialized


---------------------------------------------------------------------------------
-- | Instances
---------------------------------------------------------------------------------


instance Eq q => Eq (TParam q r) where
  TP s1 c1 _ == TP s2 c2 _ = s1 == s2 && c1 == c2

instance IsLocated (TParam q r) where
  srcPos = tp_loc

instance Hashable (TParam q r) where
  hashWithSalt i α = hashWithSalt i $ tp_sym α


instance F.Symbolic (TParam q r) where
  symbol = tp_sym

instance F.Symbolic a => F.Symbolic (Located a) where
  symbol = F.symbol . val


-- Ignoring refinements in equality check
instance Eq q => Eq (RTypeQ  q r) where
  _ == _ = False

-- USE CAREFULLY !!!
instance Eq q => Ord (RTypeQ q r) where
  compare = undefined -- compare `on` rTypeCode


-----------------------------------------------------------------------
-- | Operator Types
-----------------------------------------------------------------------

data BuiltinOp = BIUndefined
               | BIBracketRef
               | BIBracketAssign
               | BIArrayLit
               | BIObjectLit
               | BINumArgs
               | BITruthy
               | BISetProp
               | BICondExpr
               | BICastExpr
               | BISuper
               | BISuperVar
               | BIForInKeys
               | BICtorExit
               | BICtor
               | BIThis
                 deriving (Eq, Ord, Show)

instance PP BuiltinOp where
  pp = text . show


-----------------------------------------------------------------------------
-- | IContext keeps track of context of intersection-type cases
-----------------------------------------------------------------------------

-- | Keeps track of intersection-type context, to allow casts to be guarded by
--   context. Otherwise, the "dead-casts" for one case must be proven in another
--   case which is impossible. See tests/liquid/pos/misc/negate-05.js
--   A context IC [i_1,...,i_n] denotes the case where we use the conjunct i_k
--   from the kth function in lexical scope order (ignoring functions thatz have
--   a single conjunct.)

class CallSite a where
  siteIndex :: a -> Int

instance CallSite Int where
  siteIndex i = i

newtype IContext = IC [Int]
                   deriving (Eq, Ord, Show, Data, Typeable)

emptyContext         :: IContext
emptyContext         = IC []

pushContext          :: (CallSite a) => a -> IContext -> IContext
pushContext s (IC c) = IC ((siteIndex s) : c)


instance PP Int where
  pp = int

ppArgs p sep l = p $ intersperse sep $ map pp l

instance PP a => PP [a] where
  pp = ppArgs brackets comma

instance PP IContext where
  pp (IC x) = text "Context: " <+> pp x

instance PP Initialization where
  pp Initialized   = text "init"
  pp Uninitialized = text "non-init"


-----------------------------------------------------------------------
-- Type and Predicate Aliases
-----------------------------------------------------------------------

data Alias a s t = Alias {
    al_name   :: Id SrcSpan     -- ^ alias name
  , al_tyvars :: ![a]           -- ^ type  parameters
  , al_syvars :: ![s]           -- ^ value parameters
  , al_body   :: !t             -- ^ alias body
  } deriving (Eq, Ord, Show, Functor, Data, Typeable)

-- type TAlias t    = Alias TParam F.Symbol t
-- type PAlias      = Alias ()   F.Symbol F.Pred
-- type TAliasEnv t = Env (TAlias t)
-- type PAliasEnv   = Env PAlias

instance IsLocated (Alias a s t) where
  srcPos = srcPos . al_name

instance (PP a, PP s, PP t) => PP (Alias a s t) where
  pp (Alias n _ _ body) = text "alias" <+> pp n <+> text "=" <+> pp body

---------------------------------------------------------------------------------
---------------------------------------------------------------------------------

-- Local Variables:
-- flycheck-disabled-checkers: (haskell-liquid)
-- End:
