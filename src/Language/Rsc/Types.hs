{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Rsc.Types where

import           Data.Generics           (Data)
import           Data.Hashable
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
data TVar         = TV { tv_sym :: F.Symbol                   -- Parameter symbol
                       , tv_loc :: SrcSpan
                       }
                    deriving (Data, Typeable)

data BTVarQ q r   = BTV { btv_sym    :: F.Symbol              -- Parameter symbol
                        , btv_loc    :: SrcSpan
                        , btv_constr :: Maybe (RTypeQ q r)    -- Constraint
                        }
                    deriving (Data, Typeable, Functor, Foldable, Traversable)


data TPrim
  = TString
  | TStrLit String
  | TNumber
  | TReal
  | TBoolean
  | TBV32
  | TVoid
  | TUndefined
  | TNull
  | TAny
  {- Internal -}
  | TTop
  | TBot
  | TFPBool
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
  | TOr [RTypeQ q r] r
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
  | TObj (MutabilityQ q r) (TypeMembersQ q r) r
  --
  -- class C
  --
  | TClass (BTGenQ q r)
  --
  -- Namespace
  --
  | TMod (QP q)
  --
  -- ∀A[<:T].S
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
                          , b_opt  :: Optionality
                          , b_type :: RTypeQ q r
                          }
                        deriving (Data, Typeable, Functor, Foldable, Traversable)

data TypeMembersQ q r = TM { i_mems  :: F.SEnv (TypeMemberQ q r)      -- Instance Members
                           , s_mems  :: F.SEnv (TypeMemberQ q r)      -- Static members
                           , tm_call :: Maybe (RTypeQ q r)            -- Call signatures
                           , tm_ctor :: Maybe (RTypeQ q r)            -- Contructor signatures
                           , tm_sidx :: Maybe ( MutabilityQ q r       -- String indexer
                                              , RTypeQ q r)
                           , tm_nidx :: Maybe ( MutabilityQ q r       -- Numeric indexer
                                              , RTypeQ q r)
                           }
                        deriving (Data, Typeable, Functor, Foldable, Traversable)

data TypeMemberQ q r  = FI                                            -- Field Members
                           { f_name :: F.Symbol                         -- Name
                           , f_opt  :: Optionality                      -- Optional
                           , f_asg  :: MutabilityQ q r                  -- Assignability
                           , f_ty   :: RTypeQ q r                       -- Type
                           }
                      | MI                                            -- Method Members
                           { m_name :: F.Symbol                          -- Name
                           , m_opt  :: Optionality                       -- Optional
                           , m_ty   :: [(MutabilityQ q r, RTypeQ q r)]   -- [(Mutability, Type)]
                           }
                        deriving (Data, Typeable, Functor, Foldable, Traversable)

instance F.Symbolic (TypeMemberQ q r) where
  symbol (FI n _ _ _ ) = n
  symbol (MI n _ _   ) = n


type MutabilityQ q r  = RTypeQ q r
type MutabilityR r    = MutabilityQ AK r
type Mutability       = MutabilityR F.Reft

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
type RefTypeMembers   = TypeMembers F.Reft
type BTVar r          = BTVarQ AK r

type TypeDecl r       = TypeDeclQ AK r
type TypeSig r        = TypeSigQ AK r
type TypeMember r     = TypeMemberQ AK r

type Type             = RType ()

-- XXX: Binds here are all Req
--
type OverloadSig r    = ([BTVar r], [Bind r], RType r)
type IOverloadSig r   = (IntCallSite, OverloadSig r)


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
                     deriving (Show, Eq, Data, Typeable)




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
  --
  -- Unknown status
  --
  | InitUnknown
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
  hashWithSalt i TReal       = i `hashWithSalt` (3 :: Int)
  hashWithSalt i TBoolean    = i `hashWithSalt` (4 :: Int)
  hashWithSalt i TBV32       = i `hashWithSalt` (5 :: Int)
  hashWithSalt i TVoid       = i `hashWithSalt` (6 :: Int)
  hashWithSalt i TUndefined  = i `hashWithSalt` (7 :: Int)
  hashWithSalt i TNull       = i `hashWithSalt` (8 :: Int)
  hashWithSalt i TTop        = i `hashWithSalt` (9 :: Int)
  hashWithSalt i TBot        = i `hashWithSalt` (10 :: Int)
  hashWithSalt i TFPBool     = i `hashWithSalt` (11 :: Int)
  hashWithSalt i TAny        = i `hashWithSalt` (12 :: Int)

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

-- | If an elements occurs in both maps, the mapping from the first
--   will be the mapping in the result.
--
--   This requires that the type yielding the second TypeMembers correctly
--   extends the one yielding the first one.
--
instance Monoid (TypeMembersQ q r) where
  mempty = TM mempty mempty Nothing Nothing Nothing Nothing
  mappend (TM ps1 sps1 c1 k1 s1 n1) (TM ps2 sps2 c2 k2 s2 n2)
    = TM (ps1  `mappend` ps2) (sps1 `mappend` sps2)
         (maybe c2 Just c1)   (maybe k2 Just k1)
         (maybe s2 Just s1)   (maybe n2 Just n1)

instance Monoid Initialization where
  mempty                                = InitUnknown
  Initialized   `mappend` Initialized   = Initialized
  Uninitialized `mappend` Uninitialized = Uninitialized
  _             `mappend` _             = InitUnknown


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
  siteIndex :: a -> IntCallSite

instance CallSite Int where
  siteIndex i = i

newtype IContext = IC [IntCallSite] deriving (Eq, Ord, Show, Data, Typeable)

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
type PAlias      = Alias ()   F.Symbol F.Expr
type TAliasEnv t = Env (TAlias t)
type PAliasEnv   = Env PAlias

instance {-# OVERLAPPING #-} IsLocated (Alias a s t) where
  srcPos = srcPos . al_name

-- Local Variables:
-- flycheck-disabled-checkers: (haskell-liquid)
-- End:
