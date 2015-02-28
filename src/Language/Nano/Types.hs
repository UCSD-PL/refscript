
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
import           Language.ECMAScript3.Syntax 
import           Language.ECMAScript3.PrettyPrint   (PP (..))

import qualified Language.Fixpoint.Types         as F

import           Language.Fixpoint.Misc
import           Language.Nano.Env
import           Language.Nano.Names
import           Language.Nano.Locations
import           Text.PrettyPrint.HughesPJ


---------------------------------------------------------------------------------
-- | RefScript Types
---------------------------------------------------------------------------------


-- | Type Variables
data TVar = TV { 

    tv_sym :: F.Symbol

  , tv_loc :: SourceSpan 

  } deriving (Show, Data, Typeable)


-- | Type Constructors
data TCon
  = TInt                -- ^ number
  | TBV32               -- ^ bitvector
  | TBool               -- ^ boolean
  | TString             -- ^ string
  | TVoid               -- ^ void
  | TTop                -- ^ top
  | TUn                 -- ^ union
  | TNull               -- ^ null
  | TUndef              -- ^ undefined
  | TFPBool             -- ^ liquid 'bool'
    deriving (Ord, Show, Data, Typeable)

-- | (Raw) Refined Types 
data RTypeQ q r =
  -- 
  -- ^ C T1,...,Tn
  --
    TApp TCon [RTypeQ  q r] r
  -- 
  -- ^ A
  --
  | TVar TVar r               
  -- 
  -- ^ ([Tthis], xi:T1, .., xn:Tn) => T
  --
  | TFun (Maybe (RTypeQ q r)) [BindQ q r] (RTypeQ q r) r
  -- 
  -- ^ {f1:T1,..,fn:Tn} 
  --
  | TCons (MutabilityQ q) (TypeMembersQ q r) r
  -- 
  -- ^ forall A. T
  --
  | TAll TVar (RTypeQ q r)
  -- 
  -- ^ /\ (T1..) => T1' ... /\ (Tn..) => Tn'
  --
  | TAnd [RTypeQ q r]                                   
  --
  -- ^ Type Reference
  --
  | TRef (QN q) [RTypeQ  q r] r
  -- 
  -- ^ typeof A.B.C (class)
  -- 
  | TClass (QN q)
  -- 
  -- ^ typeof L.M.N (module)
  --
  | TModule (QP q)
  -- 
  -- ^ enumeration L.M.N 
  -- 
  | TEnum (QN q)
  -- 
  -- ^ Self type (with Mutability)
  --
  | TSelf (RTypeQ q r)
  -- 
  -- ^ "Expression" parameters for type-aliases: never appear in real/expanded RType
  --
  | TExp F.Expr

    deriving (Show, Data, Typeable)

deriving instance Functor (RTypeQ q) 
deriving instance Traversable (RTypeQ q)
deriving instance Foldable (RTypeQ q)


-- 
-- The main typechecking RType uses absolute paths
--
type RType = RTypeQ AK
 

type TypeMembersQ q r = M.Map (F.Symbol, StaticKind) (TypeMemberQ q r)

type TypeMembers r = M.Map (F.Symbol, StaticKind) (TypeMember r)

data StaticKind = 
    StaticMember 
  | InstanceMember 
  deriving (Eq, Ord, Show, Data, Typeable)


-- | Standard Types
type Type    = RType ()


-- | Type binder
data BindQ  q r = B { 

  -- 
  -- ^ Binding's symbol
  --
    b_sym  :: F.Symbol                              
  --
  -- ^ Field type
  --
  , b_type :: !(RTypeQ  q r)

  } deriving (Eq, Show, Data, Typeable)

deriving instance Functor (BindQ q) 
deriving instance Traversable (BindQ q)
deriving instance Foldable (BindQ q)

type Bind = BindQ AK


data FuncInputs t = FI { 
    fi_self :: Maybe t
  , fi_args :: [t] 
  } 
  deriving (Functor, Traversable, Foldable)


---------------------------------------------------------------------------------
-- | Interface definitions 
---------------------------------------------------------------------------------

data IfaceKind = ClassKind | InterfaceKind
  deriving (Eq, Show, Data, Typeable)

data IfaceDefQ q r = ID {
  -- 
  -- ^ The full name of the type 
  -- 
    t_name  :: QN q 
  -- 
  -- ^ Kind
  --
  , t_class :: IfaceKind
  -- 
  -- ^ Type variables
  --
  , t_args  :: ![TVar]                             
  -- 
  -- ^ Heritage
  --
  , t_base  :: !(HeritageQ q r)
  -- 
  -- ^ List of data type elts 
  --
  , t_elts  :: !(TypeMembersQ q r)
  } 
  deriving (Eq, Show, Data, Typeable)

deriving instance Functor (IfaceDefQ q) 
deriving instance Traversable (IfaceDefQ q)
deriving instance Foldable (IfaceDefQ q)

type HeritageQ q r      = ([TypeReferenceQ q r], [TypeReferenceQ q r])
type Heritage r         = HeritageQ AK r

type ClassSigQ q r      = ([TVar], [TypeReferenceQ q r], [TypeReferenceQ q r])

type TypeReferenceQ q r = (QN q, [RTypeQ q r])
type SIfaceDefQ q r     = (IfaceDefQ q r, [RTypeQ q r])

-- Full names
type IfaceDef r         = IfaceDefQ AK r
type SIfaceDef r        = SIfaceDefQ AK r
type TypeReference r    = TypeReferenceQ AK r

data IndexKind          = StringIndex | NumericIndex
  deriving (Eq, Show, Data, Typeable)



data TypeMemberQ  q r
  -- 
  -- ^ Call signature
  --
  = CallSig   { f_type :: RTypeQ q r }
  -- 
  -- ^ Constructor signature
  --
  | ConsSig   { f_type :: RTypeQ q r }
  -- 
  -- ^ Index signature
  --
  | IndexSig  { f_sym  :: F.Symbol
              , f_key  :: IndexKind
              , f_type :: RTypeQ q r }
  -- 
  -- ^ Field signature
  --
  | FieldSig  { f_sym  :: F.Symbol                      -- ^ Name  
              , f_opt  :: (OptionalityQ q)              -- ^ Optional
              , f_mut  :: (MutabilityQ q)               -- ^ Mutability
              , f_type :: RTypeQ  q r }                 -- ^ Property type (could be function)
  -- 
  -- ^ Method signature
  --
  | MethSig   { f_sym  :: F.Symbol                      -- ^ Name  
              , f_type :: RTypeQ  q r }                 -- ^ Method type (also encodes mutability
                                                        --   through the 'this' binding

  deriving (Show, Data, Typeable)

deriving instance Functor (TypeMemberQ q) 
deriving instance Traversable (TypeMemberQ q)
deriving instance Foldable (TypeMemberQ q)

type TypeMember = TypeMemberQ AK

data Visibility 

  = Local 

  | Exported
  deriving (Show, Eq, Data, Typeable)


---------------------------------------------------------------------------------
-- | Enumeration definition
---------------------------------------------------------------------------------

data EnumDef = EnumDef {
    -- 
    -- ^ Name
    --
      e_name       :: F.Symbol
    -- 
    -- ^ Contents: Symbols -> Expr (expected IntLit or HexLit)
    --
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
    m_variables   :: Env (Visibility, Assignability, RTypeQ q r, Initialization)
  --
  -- ^ Types
  --
  , m_types       :: Env (IfaceDefQ q r)
  -- 
  -- ^ Enumerations
  --
  , m_enums       :: Env EnumDef
  -- 
  -- ^ Absolute path of definition
  --
  , m_path        :: AbsPath
  }
  deriving (Data, Typeable)

deriving instance Functor (ModuleDefQ q) 

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


instance Eq TVar where 
  a == b = tv_sym a == tv_sym b

instance IsLocated TVar where 
  srcPos = tv_loc

instance IsLocated (TypeMember r) where
  srcPos _ = srcPos dummySpan

instance Hashable TVar where 
  hashWithSalt i α = hashWithSalt i $ tv_sym α 

instance Hashable StaticKind where
  hashWithSalt _ StaticMember   = 0
  hashWithSalt _ InstanceMember = 1


instance F.Symbolic TVar where
  symbol = tv_sym 

instance F.Symbolic a => F.Symbolic (Located a) where 
  symbol = F.symbol . val


instance Eq TCon where
  TInt     == TInt     = True   
  TBV32    == TBV32    = True   
  TBool    == TBool    = True           
  TString  == TString  = True
  TVoid    == TVoid    = True         
  TTop     == TTop     = True
  TUn      == TUn      = True
  TNull    == TNull    = True
  TUndef   == TUndef   = True
  TFPBool  == TFPBool  = True
  _        == _        = False
 

-- Ignoring refinements in equality check
instance Eq q => Eq (RTypeQ  q r) where
  TApp TUn t1 _   == TApp TUn t2 _   = (null $ t1 \\ t2) && (null $ t2 \\ t1)
  TApp c1 t1s _   == TApp c2 t2s _   = (c1, t1s) == (c2, t2s)
  TVar v1 _       == TVar v2 _       = v1        == v2
  TFun s1 b1 t1 _ == TFun s2 b2 t2 _ = (s1, b_type <$> b1, t1)  == (s2, b_type <$> b2, t2)
  TAll v1 t1      == TAll v2 t2      = (v1,t1)  == (v2,t2)   -- Very strict Eq here
  TAnd t1s        == TAnd t2s        = t1s == t2s
  TCons e1s m1 _  == TCons e2s m2 _  = (e1s,m1) == (e2s,m2)
  TRef x1 t1s _   == TRef x2 t2s _   = (x1,t1s) == (x2,t2s)
  TClass c1       == TClass c2       = c1 == c2
  TModule m1      == TModule m2      = m1 == m2
  TEnum e1        == TEnum e2        = e1 == e2
  TSelf m1        == TSelf m2        = m1 == m2
  _               == _               = False


instance Eq q => Eq (TypeMemberQ q r) where 
  CallSig t1           == CallSig t2           = t1 == t2
  ConsSig t1           == ConsSig t2           = t1 == t2
  IndexSig _ b1 t1     == IndexSig _ b2 t2     = (b1,t1) == (b2,t2)
  FieldSig f1 o1 m1 t1 == FieldSig f2 o2 m2 t2 = (f1,o1,m1,t1) == (f2,o2,m2,t2)
  MethSig  f1 t1       == MethSig f2 t2        = (f1,t1) == (f2,t2)
  _                    == _                    = False
 

-- USE CAREFULLY !!!
instance Eq q => Ord (RTypeQ q r) where
  compare = compare `on` rTypeCode

rTypeCode (TVar _ _)     = 0
-- rTypeCode (TFun _ _ _ _) = 1
rTypeCode (TCons _ _ _ ) = 2
rTypeCode (TAll _ _ )    = 3
rTypeCode (TAnd _ )      = 4
rTypeCode (TRef _ _ _)   = 5
rTypeCode (TClass _ )    = 6
rTypeCode (TExp _ )      = 7
rTypeCode (TModule _)    = 8
rTypeCode (TEnum _)      = 9
rTypeCode (TSelf _)      = 10
rTypeCode (TApp c _ _)   = 11 + tconCode c
rTypeCode _              = errorstar "Types.rTypeCode"

tconCode TInt            = 0
tconCode TBool           = 1
tconCode TString         = 3
tconCode TVoid           = 4
tconCode TTop            = 5
tconCode TUn             = 6
tconCode TNull           = 7
tconCode TUndef          = 8
tconCode TFPBool         = 9
tconCode TBV32           = 10


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
    al_name   :: Id SourceSpan  -- ^ alias name
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

