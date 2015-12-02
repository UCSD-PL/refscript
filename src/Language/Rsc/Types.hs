{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Rsc.Types where

import           Data.Default
import           Data.Foldable           (Foldable ())
import           Data.Generics           (Data)
import           Data.Hashable
import           Data.Monoid
import           Data.Traversable        hiding (mapM, sequence)
import           Data.Typeable           (Typeable)
import qualified Language.Fixpoint.Types as F
import           Language.Rsc.AST.Syntax
import           Language.Rsc.Core.Env
import           Language.Rsc.Locations
import           Language.Rsc.Names


--------------------------------------------------------------------------------
-- | RefScript Types
--------------------------------------------------------------------------------


-- | Type parameter
data TVar         = TV { tv_sym :: F.Symbol                -- Parameter symbol
                       , tv_loc :: SrcSpan
                       }
                    deriving (Data, Typeable)

data BTVarQ q r   = BTV { btv_sym    :: F.Symbol              -- Parameter symbol
                        , btv_loc    :: SrcSpan
                        , btv_constr :: Maybe (RTypeQ q r)    -- Constraint
                        }
                    deriving (Data, Typeable, Functor, Foldable, Traversable)


data TPrim        = TString | TStrLit String | TNumber | TBoolean | TBV32 | TVoid | TUndefined | TNull | TAny
                  {- Internal -}
                  | TTop | TBot | TFPBool
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
  | TAnd [(Int, RTypeQ q r)]
  --
  -- Type Reference
  --
  | TRef (TGenQ q r) r
  --
  -- Object
  --
  --  The mutability modifier is meant to be internal (default: Immutable)
  --
  | TObj (MutabilityQ q r) (TypeMembersQ q r) r
  --
  -- Class / Enum
  --
  | TClass (BTGenQ q r)
  --
  -- Namespace
  --
  | TMod (QP q)
  --
  -- Forall [A <: T] . S
  --
  | TAll (BTVarQ q r) (RTypeQ q r)
  --
  -- Function
  --
  | TFun [BindQ q r] (RTypeQ q r) r
  --
  -- /// Internal ///
  --
  | TExp  F.Expr
  deriving (Data, Typeable, Functor, Foldable, Traversable)

data TGenQ q r        = Gen { g_name :: QN q
                            , g_args :: [RTypeQ q r]
                            }
                        deriving (Data, Typeable, Functor, Foldable, Traversable)

data BTGenQ q r       = BGen { b_name :: QN q
                             , b_args :: [BTVarQ q r]
                             }
                        deriving (Data, Typeable, Functor, Foldable, Traversable)

data BindQ q r        = B { b_sym  :: F.Symbol
                          , b_type :: RTypeQ q r
                          }
                        deriving (Data, Typeable, Functor, Foldable, Traversable)

data TypeMembersQ q r = TM { i_mems  :: F.SEnv (TypeMemberQ q r)      -- Instance Members
                           , s_mems  :: F.SEnv (TypeMemberQ q r)      -- Static members
                           , tm_call :: Maybe (RTypeQ q r)            -- Call signatures
                           , tm_ctor :: Maybe (RTypeQ q r)            -- Contructor signatures
                           , tm_sidx :: Maybe (RTypeQ q r)            -- String indexer
                           , tm_nidx :: Maybe (RTypeQ q r)            -- Numeric indexer
                           }
                        deriving (Data, Typeable, Functor, Foldable, Traversable)

data TypeMemberQ q r  = FI                                            -- Field Members
                           { f_opt :: Optionality                     -- Optional
                           , f_asg :: FieldAsgn                       -- Assignability
                           , f_ty  :: RTypeQ q r                      -- Type
                           }
                      | MI                                            -- Method Members
                           { m_opt :: Optionality                     -- Optional
                           , m_ty  :: [(MutabilityQ q r, RTypeQ q r)] -- [(Mutability, Type)]
                           }
                        deriving (Data, Typeable, Functor, Foldable, Traversable)

data FieldAsgn        = Assignable | Final | Inherited
                        deriving (Data, Typeable, Eq)

type MutabilityQ q r  = RTypeQ q r
type Mutability r     = MutabilityQ AK r

data TypeSigQ q r     = TS { sigKind  :: TypeDeclKind
                           , sigTRef  :: BTGenQ q r
                           , sigHerit :: HeritageQ q r
                           }
                        deriving (Data, Typeable, Foldable, Traversable, Functor)

data TypeDeclQ q r    = TD { typeSig  :: TypeSigQ q r
                           , typeBody :: TypeMembersQ q r
                           }
                        deriving (Data, Typeable, Foldable, Traversable, Functor)

type HeritageQ q r    = ([TGenQ q r], [TGenQ q r])

data TypeDeclKind     = InterfaceTDK | ClassTDK
                        deriving (Eq, Data, Typeable)

data StaticKind       = StaticK | InstanceK
                        deriving (Eq, Ord, Show, Data, Typeable)

data Optionality      = Opt | Req
                        deriving (Eq, Ord, Show, Data, Typeable)


instance Monoid Optionality where
  mempty = Req
  mappend Opt _ = Opt
  mappend _ Opt = Opt
  mappend _ _   = Req

--------------------------------------------------------------------------------
-- | Enumeration definition
--------------------------------------------------------------------------------

data EnumDef = EnumDef {
      e_name    :: F.Symbol
    -- ^ Contents: Symbols -> Expr (expected IntLit or HexLit)
    , e_mapping :: Env (Expression ())

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
type TypeMember r     = TypeMemberQ AK r
type VarInfo r        = VarInfoQ AK r

type Type             = RType ()

type OverloadSig r    = ([BTVar r], [Bind r], RType r)
type IOverloadSig r   = (IntCallSite, OverloadSig r)


--------------------------------------------------------------------------------
-- | Module Body
--------------------------------------------------------------------------------
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
  -- | Variables (Interfaces excluded, as they don't appear as language bindings)
  --
    m_variables :: Env (VarInfoQ q r)
  --
  -- | Types definitions
  --
  , m_types     :: Env (TypeDeclQ q r)
  --
  -- | Enumerations definitions
  --
  , m_enums     :: Env EnumDef
  --
  -- | Absolute path of module
  --
  , m_path      :: AbsPath
  }
  deriving (Data, Typeable, Functor)

type ModuleDef = ModuleDefQ AK

instance Monoid (ModuleDefQ q r) where
  mempty = ModuleDef mempty mempty mempty def
  ModuleDef v t e p `mappend` ModuleDef v' t' e' _ = ModuleDef (v `mappend` v')
                                                               (t `mappend` t')
                                                               (e `mappend` e')
                                                               p


--------------------------------------------------------------------------------
-- | Assignability
--------------------------------------------------------------------------------
{-
   _________________________________________________________________________
  |               |              |                |           |             |
  |     Kind      |    Comment   | In refinements |   SSA-ed  | Initialized |
  |_______________|______________|________________|___________|_____________|
  |               |              |                |           |             |
  |    RdOnly     |     RO       |        Y       |     N     |             |
  |               |              |                |           |             |
  |    Ambient    |   Ambient    |        Y       |     N     |      Y      |
  |               |              |                |           |             |
  |  WriteLocal   | Local scope  |        N       |     Y     |             |
  |               |              |                |           |             |
  | ForeignLocal  | Outer scope  |        N       |     N     |             |
  |               |              |                |           |             |
  |  WriteGlobal  | WR anywhere  |        N       |     N     |             |
  |               |              |                |           |             |
  |   ReturnVar   |  return var  |        _       |     N     |             |
  |_______________|______________|________________|___________|_____________|

-}

data Assignability = RdOnly | Ambient | WriteLocal | ForeignLocal | WriteGlobal | ReturnVar
                   | {- internal -} ErrorAssignability
                     deriving (Show, Eq, Data, Typeable)

instance Monoid Assignability where
  mempty = ErrorAssignability
  mappend a1 a2 | a1 == a2  = a1
                | otherwise = ErrorAssignability


--------------------------------------------------------------------------------
-- | Initialization
--------------------------------------------------------------------------------

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


--------------------------------------------------------------------------------
-- | Locality
--------------------------------------------------------------------------------

data Locality =
  --
  -- Member is exported
  --
    Exported
  --
  -- Local
  --
  | Local
  deriving (Show, Eq, Data, Typeable)


--------------------------------------------------------------------------------
-- | Variable information
--------------------------------------------------------------------------------

data VarInfoQ q r = VI { v_loc  :: Locality
                       , v_asgn :: Assignability
                       , v_init :: Initialization
                       , v_type :: RTypeQ q r
                       }
                       deriving (Data, Typeable, Functor)


--------------------------------------------------------------------------------
-- | Instances
--------------------------------------------------------------------------------

instance Eq TVar where
  TV s1 _ == TV s2 _ = s1 == s2

instance IsLocated TVar where
  srcPos = tv_loc

instance IsLocated (BTVarQ q r) where
  srcPos = btv_loc

instance Hashable TVar where
  hashWithSalt i α = hashWithSalt i $ tv_sym α

instance Hashable TPrim where
  hashWithSalt i TString     = i `hashWithSalt` (0 :: Int)
  hashWithSalt i (TStrLit s) = i `hashWithSalt` (1 :: Int) `hashWithSalt` s
  hashWithSalt i TNumber     = i `hashWithSalt` (2 :: Int)
  hashWithSalt i TBoolean    = i `hashWithSalt` (3 :: Int)
  hashWithSalt i TBV32       = i `hashWithSalt` (4 :: Int)
  hashWithSalt i TVoid       = i `hashWithSalt` (5 :: Int)
  hashWithSalt i TUndefined  = i `hashWithSalt` (6 :: Int)
  hashWithSalt i TNull       = i `hashWithSalt` (7 :: Int)
  hashWithSalt i TTop        = i `hashWithSalt` (8 :: Int)
  hashWithSalt i TBot        = i `hashWithSalt` (9 :: Int)
  hashWithSalt i TFPBool     = i `hashWithSalt` (10 :: Int)
  hashWithSalt i TAny        = i `hashWithSalt` (11 :: Int)

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

instance F.Symbolic (TypeSigQ q r) where
  symbol (TS _ n _) = F.symbol n


-- | Monoid

instance Monoid (TypeMembersQ q r) where
  mempty = TM mempty mempty Nothing Nothing Nothing Nothing
  TM m1 sm1 c1 ct1 s1 n1 `mappend` TM m2 sm2 c2 ct2 s2 n2
    = TM (m1  `mappend` m2)
         (sm1 `mappend` sm2)
         (c1 `orElse` c2) (ct1 `orElse` ct2)
         (s1 `orElse` s2) (n1 `orElse` n2)
    where
      Just x  `orElse` _ = Just x
      Nothing `orElse` y = y

instance Monoid Initialization where
  mempty                              = Uninitialized
  Initialized `mappend` Initialized   = Initialized
  _           `mappend` _             = Uninitialized


-- instance Monoid (MethodInfo r) where
--   mempty                                = MI Req []
--   MI r t `mappend` MI r' t' | r == r'   = MI r   (t ++ t')
--                             | otherwise = MI Req (t ++ t')
--

--------------------------------------------------------------------------------
-- | IContext keeps track of context of intersection-type cases
--------------------------------------------------------------------------------

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
pushContext s (IC c) = IC (siteIndex s : c)

type IntCallSite = Int


--------------------------------------------------------------------------------
-- Type and Predicate Aliases
--------------------------------------------------------------------------------

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

instance {-# OVERLAPPING #-} IsLocated (Alias a s t) where
  srcPos = srcPos . al_name

-- Local Variables:
-- flycheck-disabled-checkers: (haskell-liquid)
-- End:
