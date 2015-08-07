
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
data TVar             = TV { tv_sym    :: F.Symbol                -- Parameter symbol
                           , tv_loc    :: SrcSpan 
                           } 
                        deriving (Data, Typeable)

data BTVarQ q r       = BTV { btv_sym    :: F.Symbol              -- Parameter symbol
                            , btv_constr :: Maybe (RTypeQ q r)    -- Constraint
                            , btv_loc    :: SrcSpan 
                            } 
                        deriving (Data, Typeable, Functor, Foldable, Traversable)


data TPrim            = TString | TStrLit String | TNumber | TBoolean | TBV32 | TVoid | TUndefined | TNull 
                      {- Internal -}
                      | TTop | TBot 
                        deriving (Eq, Show, Data, Typeable)


-- | Refined Types
data RTypeQ q r       = TPrim TPrim r                           -- Primitive
                      | TVar  TVar r                     -- Type parameter
                      | TOr   [RTypeQ q r]                      -- Union type
                      | TAnd  [RTypeQ q r]                      -- Intersection type
                      | TRef  (TGenQ q r) r                     -- Type Reference
                      | TObj  (TypeMembersQ q r) r              -- Object type
                      | TType [TGenQ q r]                       -- Interface or class Type
                      | TAll  (BTVarQ q r) (RTypeQ q r)         -- Forall [A <: T] . S
                      | TFun  [BindQ q r] (RTypeQ q r) r        -- Function type
                      {- Internal -}
                      | TExp  F.Expr
                        deriving (Data, Typeable, Functor, Foldable, Traversable)

data TGenQ q r        = Gen (QN q) [RTypeQ q r] deriving (Data, Typeable, Functor, Foldable, Traversable)

data BTGenQ q r       = BGen (QN q) [BTVarQ q r] deriving (Data, Typeable, Functor, Foldable, Traversable)

data BindQ q r        = B { b_sym  :: F.Symbol
                          , b_type :: RTypeQ  q r 
                          } 
                        deriving (Eq, Data, Typeable, Functor, Foldable, Traversable)

data TypeMembersQ q r = TM (F.SEnv (FieldInfoQ q r))            -- Properties
                           (F.SEnv (MethodInfoQ q r))           -- Method signatures
                           (Maybe (RTypeQ q r))                 -- Call signatures
                           (Maybe (RTypeQ q r))                 -- Contructor signatures
                           (Maybe (RTypeQ q r))                 -- String indexer
                           (Maybe (RTypeQ q r))                 -- Numeric indexer
                        deriving (Data, Typeable, Functor, Foldable, Traversable)

data FieldInfoQ q r   = FI [MemberMod]                          -- Modifiers
                           (RTypeQ q r)                         -- Mutability                           
                           (RTypeQ q r)                         -- Type
                        deriving (Data, Typeable, Functor, Foldable, Traversable)

data MethodInfoQ q r  = MI [MemberMod]                          -- Modifiers
                           (RTypeQ q r)                         -- Mutability                           
                           (RTypeQ q r)                         -- Type
                        deriving (Data, Typeable, Functor, Foldable, Traversable)

data MemberMod        = {- Sharing -} 
                        Private 
                        {- Optional -}
                      | Optional
                        {- Static -}
                      | Static
                        deriving (Eq, Data, Typeable)

data TypeDeclQ q r    = TD  TypeDeclKind              -- Class or interface
                            (BTGenQ q r)              -- Name & bounded type parameters
                            (HeritageQ q r)           -- Extends
                            (TypeMembersQ q r)        -- Body
                        deriving (Data, Typeable, Foldable, Traversable, Functor)

type HeritageQ q r    = (Maybe (TGenQ q r), [TGenQ q r])

data TypeDeclKind     = InterfaceKind | ClassKind
                        deriving (Data, Typeable)


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
type FieldInfo r      = FieldInfoQ AK r
type MethodInfo r     = MethodInfoQ AK r

type Type             = RType ()

type Mutability       = Type


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
  -- ^ Contents of a module 
  --
  --   XXX: local/exported info not included atm
  --
  --   * Interfaces are _not_ included here (because thery don't appear as
  --   bindings in the language)
  --
    m_variables   :: Env (VarInfo q r) 
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
  -- ^ Import, cannot be modified
  -- ^ Contains: FunctionStmts, Measures, Classes, Modules.
  -- ^ Can appear in refinements
  --
  = Ambient
--   --
--   -- ^ Like ReadOnly but for function declarations with no body
--   -- ^ Can appear in refinements.
--   --
--   | ImportDecl
  --
  -- ^ written in local-scope, can be SSA-ed
  -- ^ Can appear in refinements
  --
  | WriteLocal
  --
  -- ^ Declared in uouter scope
  -- ^ CANNOT appear in refinements, NOT SSA-ed
  --
  | ForeignLocal
  --
  -- ^ Written in non-local-scope
  -- ^ CANNOT appear in refinements, NOT SSA-ed
  --
  | WriteGlobal
--   --
--   -- ^ Used to denote return variable
--   -- ^ CANNOT appear in refinements
--   --
  | ReturnVar
  deriving (Show, Eq, Data, Typeable)



data VarInfo q r = VI { 
    v_asgn :: Assignability
  , v_init :: Initialization
  , v_type :: RTypeQ q r 
  } deriving (Data, Typeable, Functor)


---------------------------------------------------------------------------------
-- | Initialization
---------------------------------------------------------------------------------

data Initialization = Initialized | Uninitialized
  deriving (Show, Eq, Data, Typeable)

instance Monoid Initialization where
  mempty                              = Uninitialized
  Initialized `mappend` Initialized   = Initialized
  _           `mappend` _             = Uninitialized


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


-- Ignoring refinements in equality check
instance Eq q => Eq (RTypeQ  q r) where
  _ == _ = False

-- USE CAREFULLY !!!
instance Eq q => Ord (RTypeQ q r) where
  compare = undefined -- compare `on` rTypeCode

instance Default SrcSpan where
  def = srcPos dummySpan 

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

type TAlias t    = Alias TVar F.Symbol t
type PAlias      = Alias ()   F.Symbol F.Pred
type TAliasEnv t = Env (TAlias t)
type PAliasEnv   = Env PAlias

instance IsLocated (Alias a s t) where
  srcPos = srcPos . al_name

instance (PP a, PP s, PP t) => PP (Alias a s t) where
  pp (Alias n _ _ body) = text "alias" <+> pp n <+> text "=" <+> pp body

---------------------------------------------------------------------------------
---------------------------------------------------------------------------------

-- Local Variables:
-- flycheck-disabled-checkers: (haskell-liquid)
-- End:
