{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverlappingInstances   #-}

module Language.Nano.Types where

import           Control.Applicative                ((<$>))
import           Data.Hashable
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

  } deriving (Show, Ord, Data, Typeable)


-- | Type Constructors
data TCon
  = TInt                -- ^ number
  | TBool               -- ^ boolean
  | TString             -- ^ string
  | TVoid               -- ^ void
  | TTop                -- ^ top
  | TRef RelName        -- ^ A.B.C (class)
  | TUn                 -- ^ union
  | TNull               -- ^ null
  | TUndef              -- ^ undefined
  | TFPBool             -- ^ liquid 'bool'
    deriving (Ord, Show, Data, Typeable)

-- | (Raw) Refined Types 
data RType r =
  -- 
  -- ^ C T1,...,Tn
  --
    TApp TCon [RType r] r
  -- 
  -- ^ A
  --
  | TVar TVar r               
  -- 
  -- ^ ([Tthis], xi:T1, .., xn:Tn) => T
  --
  | TFun (Maybe (RType r)) [Bind r] (RType r) r
  -- 
  -- ^ {f1:T1,..,fn:Tn} 
  --
  | TCons [TypeMember r] Mutability r
  -- 
  -- ^ forall A. T
  --
  | TAll TVar (RType r)                   
  -- 
  -- ^ /\ (T1..) => T1' ... /\ (Tn..) => Tn'
  --
  | TAnd [RType r]                                   
  -- 
  -- ^ typeof A.B.C (class)
  --
  | TClass RelName
  -- 
  -- ^ typeof L.M.N (module)
  | TModule RelPath
  -- 
  -- ^ "Expression" parameters for type-aliases: never appear in real/expanded RType
  --
  | TExp F.Expr
    deriving (Ord, Show, Functor, Data, Typeable, Traversable, Foldable)


-- | Standard Types
type Type    = RType ()


-- | Type binder
data Bind r = B { 

  -- 
  -- ^ Binding's symbol
  --
    b_sym  :: F.Symbol                              
  --
  -- ^ Field type
  --
  , b_type :: !(RType r)                            

  } deriving (Eq, Ord, Show, Functor, Data, Typeable, Traversable, Foldable)


data FuncInputs t = FI { fi_self :: Maybe t, fi_args :: [t] } deriving (Functor, Traversable, Foldable)


---------------------------------------------------------------------------------
-- | Interface definitions 
---------------------------------------------------------------------------------

data IfaceKind = ClassKind | InterfaceKind
  deriving (Eq, Ord, Show, Data, Typeable)

data IfaceDef r = ID { 
  -- 
  -- ^ Kind
  --
    t_class :: IfaceKind
  -- 
  -- ^ Name
  --
  , t_name  :: !(Id SourceSpan)                    
  -- 
  -- ^ Type variables
  --
  , t_args  :: ![TVar]                             
  -- 
  -- ^ Heritage
  --
  , t_proto :: !(Heritage r)
  -- 
  -- ^ List of data type elts 
  --
  , t_elts  :: ![TypeMember r]
  } 
  deriving (Eq, Ord, Show, Functor, Data, Typeable, Traversable, Foldable)


type Heritage r  = Maybe (RelName, [RType r])

type SIfaceDef r = (IfaceDef r, [RType r])

data IndexKind   = StringIndex | NumericIndex
  deriving (Eq, Ord, Show, Data, Typeable)


data TypeMember r 
  -- 
  -- ^ Call signature
  --
  = CallSig   { f_type :: RType r }                     
  -- 
  -- ^ Constructor signature
  --
  | ConsSig   { f_type :: RType r }
  -- 
  -- ^ Index signature
  --
  | IndexSig  { f_sym  :: F.Symbol
              , f_key  :: IndexKind
              , f_type :: RType r }                     
  -- 
  -- ^ Field signature
  --
  | FieldSig  { f_sym  :: F.Symbol                      -- ^ Name  
              , f_mut  :: Mutability                    -- ^ Mutability
              , f_type :: RType r }                     -- ^ Property type (could be function)
  -- 
  -- ^ Method signature
  --
  | MethSig   { f_sym  :: F.Symbol                      -- ^ Name  
              , f_mut  :: Mutability                    -- ^ Mutability
              , f_type :: RType r }                     -- ^ Method type
  -- 
  -- ^ Static field signature
  --
  | StatSig   { f_sym  :: F.Symbol                      -- ^ Name  
              , f_mut  :: Mutability                    -- ^ Mutability
              , f_type :: RType r }                     -- ^ Property type (could be function)

  deriving (Ord, Show, Functor, Data, Typeable, Traversable, Foldable)


data Visibility 

  = Local 

  | Exported
  deriving (Eq, Data, Typeable)


------------------------------------------------------------------------------------------
-- | Assignability 
------------------------------------------------------------------------------------------

data Assignability 
  -- 
  -- ^ import,  cannot be modified, can appear in refinements
  -- ^ contains: FunctionStmts, Measures, Classes, Modules.
  --
  = ReadOnly    
  -- 
  -- ^ Like ReadOnly but for function declarations with no body
  --
  | ImportDecl
  -- 
  -- ^ written in local-scope, can be SSA-ed, can appear in refinements
  --
  | WriteLocal  
  -- 
  -- ^ written in non-local-scope, cannot do SSA, cannot appear in refinements
  --
  | WriteGlobal 
  -- 
  -- SPECIAL VALUES
  -- 
  -- ^ Used to denote return variable
  -- 
  | ReturnVar
  -- 
  -- ^ Used to denote 'this' variable
  -- 
  | ThisVar
  deriving (Eq, Data, Typeable)



-- | Module Body 
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
data ModuleDef r = ModuleDef {
  -- 
  -- ^ Contents of a module (local and exported)
  --   
  --   * Interfaces are _not_ included here (because thery don't appear as
  --   bindings in the language)
  --
    m_variables   :: Env (Visibility, Assignability, RType r)
  --
  -- ^ Types
  --
  , m_types       :: Env (IfaceDef r)
  -- 
  -- ^ Absolute path of definition
  --
  , m_path        :: AbsPath
  }
  deriving (Functor, Data, Typeable)



---------------------------------------------------------------------------------
-- | Mutability 
---------------------------------------------------------------------------------

type Mutability = Type 


---------------------------------------------------------------------------------
-- | Eq Instances
---------------------------------------------------------------------------------


instance Eq TVar where 
  a == b = tv_sym a == tv_sym b

instance IsLocated TVar where 
  srcPos = tv_loc

instance Hashable TVar where 
  hashWithSalt i α = hashWithSalt i $ tv_sym α 

instance F.Symbolic TVar where
  symbol = tv_sym 

instance F.Symbolic a => F.Symbolic (Located a) where 
  symbol = F.symbol . val


instance Eq TCon where
  TInt     == TInt     = True   
  TBool    == TBool    = True           
  TString  == TString  = True
  TVoid    == TVoid    = True         
  TTop     == TTop     = True
  TRef x1  == TRef x2  = x1 == x2
  TUn      == TUn      = True
  TNull    == TNull    = True
  TUndef   == TUndef   = True
  TFPBool  == TFPBool  = True
  _        == _        = False
 

-- Ignoring refinements in equality check
instance Eq (RType r) where
  TApp TUn t1 _   == TApp TUn t2 _   = (null $ t1 \\ t2) && (null $ t2 \\ t1)
  TApp c1 t1s _   == TApp c2 t2s _   = (c1, t1s) == (c2, t2s)
  TVar v1 _       == TVar v2 _       = v1        == v2
  TFun s1 b1 t1 _ == TFun s2 b2 t2 _ = (s1, b_type <$> b1, t1)  == (s2, b_type <$> b2, t2)
  TAll v1 t1      == TAll v2 t2      = (v1,t1)  == (v2,t2)   -- Very strict Eq here
  TAnd t1s        == TAnd t2s        = t1s == t2s
  TCons e1s m1 _  == TCons e2s m2 _  = (e1s,m1) == (e2s,m2)
  TClass c1       == TClass c2       = c1 == c2
  TModule m1      == TModule m2      = m1 == m2
  _               == _               = False


instance Eq (TypeMember r) where 
  CallSig t1        == CallSig t2        = t1 == t2
  ConsSig t1        == ConsSig t2        = t1 == t2
  IndexSig _ b1 t1  == IndexSig _ b2 t2  = (b1,t1) == (b2,t2)
  FieldSig f1 m1 t1 == FieldSig f2 m2 t2 = (f1,m1,t1) == (f2,m2,t2)
  MethSig  f1 m1 t1 == MethSig f2 m2 t2  = (f1,m1,t1) == (f2,m2,t2)
  StatSig f1 m1 t1  == StatSig f2 m2 t2  = (f1,m1,t1) == (f2,m2,t2)
  _                 == _                 = False
 

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
               | BIForInKeys
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


