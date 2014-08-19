{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

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
  -- ^ (xi:T1,..,xn:Tn) => T
  --
  | TFun [Bind r] (RType r) r
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
  --
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


---------------------------------------------------------------------------------
-- | Interfacce definitions 
---------------------------------------------------------------------------------

data IfaceDef r = ID { 
  -- 
  -- ^ Class (True) or interface (False)
  --
    t_class :: Bool                                
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
              , f_key  :: Bool                         -- True = string
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


-- | A module's content (local and exported API)
--
data ModuleMember r 
  -- 
  -- ^ Class / Interface type
  --
  = ModType   { m_sym  :: Id SourceSpan
              , m_vis  :: Visibility
              , m_def  :: IfaceDef r }
  -- 
  -- ^ Variable / Function binding
  --
  | ModVar    { m_sym  :: Id SourceSpan                 
              , m_vis  :: Visibility
              , m_type :: RType r }                    
  -- 
  -- ^ Module -- use this to find the module definition
  --
  | ModModule { m_sym  :: Id SourceSpan
              , m_vis  :: Visibility }

  deriving (Functor)


data Visibility 

  = Local 

  | Exported


data ModuleDef r = ModuleDef {
  -- 
  -- ^ Contents of a module (local and exported)
  --
    m_contents    :: Env (ModuleMember r)
  -- 
  -- ^ Absolute path of definition
  --
  , m_path        :: AbsPath
  }
  deriving (Functor)



---------------------------------------------------------------------------------
-- | Mutability 
---------------------------------------------------------------------------------

type Mutability = Type 


data CommonTypes r = CommonTypes {
    t_ReadOnly       :: RType r
  , t_Immutable      :: RType r
  , t_Mutable        :: RType r
  , t_AnyMutability  :: RType r
  , t_InheritedMut   :: RType r
  }



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
  TApp TUn t1 _  == TApp TUn t2 _  = (null $ t1 \\ t2) && (null $ t2 \\ t1)
  TApp c1 t1s _  == TApp c2 t2s _  = (c1, t1s) == (c2, t2s)
  TVar v1 _      == TVar v2 _      = v1        == v2
  TFun b1 t1 _   == TFun b2 t2 _   = (b_type <$> b1, t1)  == (b_type <$> b2, t2)
  TAll v1 t1     == TAll v2 t2     = (v1,t1)  == (v2,t2)   -- Very strict Eq here
  TAnd t1s       == TAnd t2s       = t1s == t2s
  TCons e1s m1 _ == TCons e2s m2 _ = (e1s,m1) == (e2s,m2)
  _              == _              = False


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
               | BINumArgs
               | BITruthy
               | BISetProp
               | BICondExpr
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
--   from the kth function in lexical scope order (ignoring functions that have
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
  pp        = int

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


-- Local Variables:
-- flycheck-disabled-checkers: (haskell-liquid)
-- End:






