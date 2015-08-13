
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

import qualified Language.Fixpoint.Types         as F

import           Language.Fixpoint.Misc
import           Language.Nano.Env
import           Language.Nano.AST
import           Language.Nano.Names
import           Language.Nano.Locations
import           Text.PrettyPrint.HughesPJ


---------------------------------------------------------------------------------
-- | RefScript Types
---------------------------------------------------------------------------------


-- | Type parameter
data TVar         = TV { tv_sym    :: F.Symbol                -- Parameter symbol
                       , tv_loc    :: SrcSpan 
                       } 
                    deriving (Data, Typeable)

data BTVarQ q r   = BTV { btv_sym    :: F.Symbol              -- Parameter symbol
                        , btv_loc    :: SrcSpan 
                        , btv_constr :: Maybe (RTypeQ q r)    -- Constraint
                        } 
                    deriving (Data, Typeable, Functor, Foldable, Traversable)


data TPrim        = TString | TStrLit String | TNumber | TBoolean | TBV32 | TVoid | TUndefined | TNull 
                  {- Internal -}
                  | TTop | TBot 
                  deriving (Eq, Show, Data, Typeable)


-- | Refined Types
data RTypeQ q r = 
  -- 
  -- Primitive
  --
    TPrim TPrim r
  -- 
  -- Type parameter
  --
  | TVar TVar r
  --
  -- Union
  --
  | TOr [RTypeQ q r]
  -- 
  -- Intersection
  --
  | TAnd [RTypeQ q r]                      
  -- 
  -- Type Reference
  --
  | TRef (TGenQ q r) r 
  -- 
  -- Object
  --
  | TObj (TypeMembersQ q r) r
  -- 
  -- Class / Enum
  --
  | TType NamedTypeKind (TGenQ q r)
  -- 
  -- Namespace
  --
  | TMod AbsPath
  --
  -- Forall [A <: T] . S
  -- 
  | TAll  (BTVarQ q r) (RTypeQ q r)         
  -- 
  -- Function
  --
  | TFun  [BindQ q r] (RTypeQ q r) r
  -- 
  -- /// Internal ///
  --
  | TExp  F.Expr
  deriving (Data, Typeable, Functor, Foldable, Traversable)

data NamedTypeKind    = EnumK | ClassK
                        deriving (Eq, Data, Typeable)

data TGenQ q r        = Gen { g_name :: QN q
                            , g_args :: [RTypeQ q r] 
                            }
                        deriving (Data, Typeable, Functor, Foldable, Traversable)

data BTGenQ q r       = BGen (QN q) [BTVarQ q r] 
                        deriving (Data, Typeable, Functor, Foldable, Traversable)

data BindQ q r        = B { b_sym  :: F.Symbol
                          , b_type :: RTypeQ  q r 
                          } 
                        deriving (Data, Typeable, Functor, Foldable, Traversable)

data FunArgs t        = FA (Maybe t) [t]
                        deriving (Functor, Traversable, Foldable)

data TypeMembersQ q r = TM { tm_prop  :: F.SEnv (FieldInfoQ q r)    -- Properties
                           , tm_meth  :: F.SEnv (MethodInfoQ q r)   -- Method signatures
                           , tm_sprop :: F.SEnv (FieldInfoQ q r)    -- Static Properties
                           , tm_smeth :: F.SEnv (MethodInfoQ q r)   -- Static Method signatures 
                           , tm_call  :: Maybe (RTypeQ q r)         -- Call signatures
                           , tm_ctor  :: Maybe (RTypeQ q r)         -- Contructor signatures
                           , tm_sidx  :: Maybe (RTypeQ q r)         -- String indexer
                           , tm_nidx  :: Maybe (RTypeQ q r)         -- Numeric indexer
                           }
                        deriving (Data, Typeable, Functor, Foldable, Traversable)

data FieldInfoQ q r   = FI Optionality                          -- Optional
                           (RTypeQ q r)                         -- Mutability                           
                           (RTypeQ q r)                         -- Type
                        deriving (Data, Typeable, Functor, Foldable, Traversable)

data MethodInfoQ q r  = MI Optionality                          -- Optional
                           MutabilityMod                        -- Mutability                           
                           (RTypeQ q r)                         -- Type
                        deriving (Data, Typeable, Functor, Foldable, Traversable)

data MutabilityMod    = Mutable
                      | Immutable
                      | ReadOnly
                      | AssignsFields
                        deriving (Eq, Data, Typeable)

type Mutability r     = RType r

data TypeSigQ q r     = TS TypeDeclKind (BTGenQ q r) (HeritageQ q r)
                        deriving (Data, Typeable, Foldable, Traversable, Functor)

data TypeDeclQ q r    = TD (TypeSigQ q r) (TypeMembersQ q r)
                        deriving (Data, Typeable, Foldable, Traversable, Functor)

type HeritageQ q r    = (Maybe (TGenQ q r), [TGenQ q r])

data TypeDeclKind     = InterfaceKind | ClassKind
                        deriving (Eq, Data, Typeable)

data StaticKind       = StaticMember | InstanceMember
                        deriving (Eq, Ord, Show, Data, Typeable)

data Optionality      = Opt | Req deriving (Eq, Ord, Show, Data, Typeable)

---------------------------------------------------------------------------------
-- | Enumeration definition
---------------------------------------------------------------------------------

data EnumDef = EnumDef {
      e_name       :: F.Symbol
    -- ^ Contents: Symbols -> Expr (expected IntLit or HexLit)
    , e_mapping    :: Env (Expression ())

} deriving (Data, Typeable)


-- | Aliases
--
type RType r          = RTypeQ AK r
type Bind r           = BindQ AK r
type TGen r           = TGenQ AK r
type BTGen r          = BTGenQ AK r
type TypeMembers r    = TypeMembersQ AK r
type BTVar r          = BTVarQ AK r

type TypeDecl r       = TypeDeclQ AK r
type TypeSig r        = TypeSigQ AK r
type FieldInfo r      = FieldInfoQ AK r
type MethodInfo r     = MethodInfoQ AK r
type VarInfo r        = VarInfoQ AK r 

type Type             = RType ()

type OverloadSig r    = ([BTVar r], [RType r], RType r)
type IOverloadSig r   = (Int, OverloadSig r)


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
  -- Contents of a module 
  --
  -- 1. XXX: local/exported info not included atm
  --
  -- 2. Interfaces are _not_ included here (because thery don't 
  --    appear as bindings in the language)
  --
    m_variables   :: Env (VarInfoQ q r) 
  --
  -- Types definitions
  --
  , m_types       :: Env (TypeDeclQ q r)
  --
  -- Enumerations definitions
  --
  , m_enums       :: Env EnumDef
  --
  -- Absolute path of module
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

data Assignability =
  --
  -- Import, cannot be modified, appears in refinements
  --
    Ambient
  --
  -- Written in local-scope, SSA-ed, appears in refinements
  --
  | WriteLocal
  --
  -- Declared in outer scope, CANNOT appear in refinements, NOT SSA-ed
  --
  | ForeignLocal
  --
  -- Written in non-local-scope, CANNOT appear in refinements, NOT SSA-ed
  --
  | WriteGlobal
  --
  -- Return variable, CANNOT appear in refinements
  --
  | ReturnVar
  deriving (Show, Eq, Data, Typeable)


---------------------------------------------------------------------------------
-- | Initialization
---------------------------------------------------------------------------------

data Initialization = 
  -- 
  -- Variable initialized
  --
    Initialized 
  -- 
  -- Variable uninitialized (undefined)
  --
  | Uninitialized
  deriving (Show, Eq, Data, Typeable)


---------------------------------------------------------------------------------
-- | Variable information
---------------------------------------------------------------------------------

data VarInfoQ q r = VI { v_asgn :: Assignability
                       , v_init :: Initialization
                       , v_type :: RTypeQ q r 
                       }
                       deriving (Data, Typeable, Functor)


---------------------------------------------------------------------------------
-- | Instances
---------------------------------------------------------------------------------

instance Eq TVar where
  TV s1 _ == TV s2 _ = s1 == s2

instance IsLocated TVar where
  srcPos = tv_loc

instance IsLocated (BTVarQ q r) where
  srcPos = btv_loc

instance Hashable TVar where
  hashWithSalt i α = hashWithSalt i $ tv_sym α


-- | Symbolic

instance F.Symbolic TVar where
  symbol = tv_sym

instance F.Symbolic (BTVarQ q r) where
  symbol = btv_sym

instance F.Symbolic a => F.Symbolic (Located a) where
  symbol = F.symbol . val

instance F.Symbolic (TGenQ q r) where
  symbol (Gen n _) = F.symbol n

instance F.Symbolic (BTGenQ q r) where
  symbol (BGen n _) = F.symbol n

instance F.Symbolic NamedTypeKind where
  symbol EnumK = F.symbol "enum"
  symbol ClassK = F.symbol "class"


-- | Monoid

instance Monoid (TypeMembers r) where  
  mempty = TM mempty mempty mempty mempty Nothing Nothing Nothing Nothing
  TM f1 m1 sf1 sm1 c1 ct1 s1 n1 `mappend` TM f2 m2 sf2 sm2 c2 ct2 s2 n2 
    = TM (f1  `mappend` f2)  (m1  `mappend` m2) 
         (sf1 `mappend` sf2) (sm1 `mappend` sm2) 
         (c1 `orElse` c2) (ct1 `orElse` ct2) 
         (s1 `orElse` s2) (n1 `orElse` n2) 
    where
      Just x  `orElse` _ = Just x
      Nothing `orElse` y = y

instance Monoid Initialization where
  mempty                              = Uninitialized
  Initialized `mappend` Initialized   = Initialized
  _           `mappend` _             = Uninitialized


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

newtype IContext = IC [Int] deriving (Eq, Ord, Show, Data, Typeable)

emptyContext :: IContext
emptyContext = IC []

pushContext :: (CallSite a) => a -> IContext -> IContext
pushContext s (IC c) = IC ((siteIndex s) : c)


-----------------------------------------------------------------------
-- Type and Predicate Aliases
-----------------------------------------------------------------------

data Alias a s t = Alias {
    al_name   :: Id SrcSpan     -- ^ alias name
  , al_tyvars :: ![a]           -- ^ type  parameters
  , al_syvars :: ![s]           -- ^ value parameters
  , al_body   :: !t             -- ^ alias body
  } deriving (Eq, Ord, Show, Functor, Data, Typeable)

type TAlias t    = Alias TVar F.Symbol t
type PAlias      = Alias ()   F.Symbol F.Pred
type TAliasEnv t = Env (TAlias t)
type PAliasEnv   = Env PAlias

instance IsLocated (Alias a s t) where
  srcPos = srcPos . al_name

-- Local Variables:
-- flycheck-disabled-checkers: (haskell-liquid)
-- End:
