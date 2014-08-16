{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Language.Nano.Types (

  -- * Configuration Options
    Config (..)

  -- * (Refinement) Types
  , RType (..), Bind (..)

  -- * Regular Types
  , Heritage, Type, IfaceDef (..), SIfaceDef, TVar (..), TCon (..), TypeMember (..)
  , ModuleExports, ModuleMember (..)
  , Mutability, CommonTypes (..)

  -- * Some Operators on Pred
  , pAnd, pOr

  -- * Error message
  , convertError

  -- * Builtin Operators
  , BuiltinOp (..) 

  -- * Contexts
  , CallSite (..), IContext, emptyContext, pushContext
 
  -- * Aliases
  , Alias (..), TAlias, PAlias, PAliasEnv, TAliasEnv

  ) where

import           Control.Applicative                ((<$>))
import           Data.Hashable
import           Data.Typeable                      (Typeable)
import           Data.Generics                      (Data)   
import           Data.Monoid                        (Monoid (..))
import           Data.Maybe                         (catMaybes)
import           Data.List                          (stripPrefix, (\\))
import           Data.Traversable            hiding (sequence, mapM) 
import           Data.Foldable                      (Foldable()) 
import           Language.ECMAScript3.Syntax 
import           Language.ECMAScript3.Syntax.Annotations
import           Language.ECMAScript3.PrettyPrint   (PP (..))
import           Language.ECMAScript3.Parser.Type   (SourceSpan (..))

import qualified Language.Fixpoint.Types         as F

import           Language.Fixpoint.Errors
import           Language.Fixpoint.PrettyPrint
import           Language.Fixpoint.Misc
import           Language.Nano.Errors
import           Language.Nano.Env
import           Language.Nano.Locations
import           Text.PrettyPrint.HughesPJ
import           Text.Printf                        (printf)

---------------------------------------------------------------------
-- | Command Line Configuration Options
---------------------------------------------------------------------

data Config 
  = TC     { files       :: [FilePath]     -- ^ source files to check
           , incdirs     :: [FilePath]     -- ^ path to directory for include specs
           , noFailCasts :: Bool           -- ^ fail typecheck when casts are inserted
           }
  | Liquid { files       :: [FilePath]     -- ^ source files to check
           , incdirs     :: [FilePath]     -- ^ path to directory for include specs
           , kVarInst    :: Bool           -- ^ instantiate function types with k-vars
           }
  deriving (Data, Typeable, Show, Eq)




------------------------------------------------------------------
-- | Converting `ECMAScript3` values into `Fixpoint` values, 
--   i.e. *language* level entities into *logic* level entities.
------------------------------------------------------------------

instance F.Symbolic (LValue a) where
  symbol (LVar _ x) = F.symbol x
  symbol lv         = convertError "F.Symbol" lv

instance F.Symbolic (Prop a) where 
  symbol (PropId _ id) = F.symbol id
  symbol p             = error $ printf "Symbol of property %s not supported yet" (ppshow p)

instance F.Expression (Id a) where
  expr = F.eVar

instance F.Expression (LValue a) where
  expr = F.eVar

instance F.Expression (Expression a) where
  expr (IntLit _ i)                 = F.expr i
  expr (VarRef _ x)                 = F.expr x
  expr (InfixExpr _ o e1 e2)        = F.EBin (bop o) (F.expr e1) (F.expr e2)
  expr (PrefixExpr _ PrefixMinus e) = F.EBin F.Minus (F.expr (0 :: Int)) (F.expr e)  
  expr e                            = convertError "F.Expr" e

instance F.Predicate  (Expression a) where 
  prop (BoolLit _ True)            = F.PTrue
  prop (BoolLit _ False)           = F.PFalse
  prop (PrefixExpr _ PrefixLNot e) = F.PNot (F.prop e)
  prop e@(InfixExpr _ _ _ _ )      = eProp e
  prop e                           = convertError "F.Pred" e  

convertError tgt e  = errortext $ msg <+> pp e
  where 
    msg             = text $ "Cannot convert to: " ++ tgt


------------------------------------------------------------------
eProp :: Expression a -> F.Pred
------------------------------------------------------------------

eProp (InfixExpr _ OpLT   e1 e2)       = F.PAtom F.Lt (F.expr e1) (F.expr e2) 
eProp (InfixExpr _ OpLEq  e1 e2)       = F.PAtom F.Le (F.expr e1) (F.expr e2) 
eProp (InfixExpr _ OpGT   e1 e2)       = F.PAtom F.Gt (F.expr e1) (F.expr e2)  
eProp (InfixExpr _ OpGEq  e1 e2)       = F.PAtom F.Ge (F.expr e1) (F.expr e2)  
eProp (InfixExpr _ OpEq   e1 e2)       = F.PAtom F.Eq (F.expr e1) (F.expr e2) 
-- XXX @==@ and @===@ are translated the same. This should not make a difference
-- as long as same type operands are used.
eProp (InfixExpr _ OpStrictEq   e1 e2) = F.PAtom F.Eq (F.expr e1) (F.expr e2) 
eProp (InfixExpr _ OpNEq  e1 e2)       = F.PAtom F.Ne (F.expr e1) (F.expr e2) 
eProp (InfixExpr _ OpLAnd e1 e2)       = pAnd (F.prop e1) (F.prop e2) 
eProp (InfixExpr _ OpLOr  e1 e2)       = pOr  (F.prop e1) (F.prop e2)
eProp e                                = convertError "InfixExpr -> F.Prop" e

------------------------------------------------------------------
bop       :: InfixOp -> F.Bop
------------------------------------------------------------------

bop OpSub = F.Minus 
bop OpAdd = F.Plus
bop OpMul = F.Times
bop OpDiv = F.Div
bop OpMod = F.Mod
bop o     = convertError "F.Bop" o

------------------------------------------------------------------
pAnd p q  = F.pAnd [p, q] 
pOr  p q  = F.pOr  [p, q]





---------------------------------------------------------------------------------
-- | RefScript Types
---------------------------------------------------------------------------------


-- | Type Variables
data TVar 
  = TV { tv_sym :: F.Symbol
       , tv_loc :: SourceSpan 
       }
    deriving (Show, Ord, Data, Typeable)


-- | Type Constructors
data TCon
  = TInt                                                -- ^ number
  | TBool                                               -- ^ boolean
  | TString                                             -- ^ string
  | TVoid                                               -- ^ void
  | TTop                                                -- ^ top
  | TRef QName                                          -- ^ A.B.C (class)
  | TUn                                                 -- ^ union
  | TNull                                               -- ^ null
  | TUndef                                              -- ^ undefined

  | TFPBool                                             -- ^ liquid 'bool'
    deriving (Ord, Show, Data, Typeable)

-- | (Raw) Refined Types 
data RType r  
  = TApp    TCon            [RType r]   r               -- ^ C T1,...,Tn
  | TVar    TVar                        r               -- ^ A
  | TFun    [Bind r]        (RType r)   r               -- ^ (xi:T1,..,xn:Tn) => T
  | TCons   [TypeMember r]  Mutability  r               -- ^ {f1:T1,..,fn:Tn} 
  | TAll    TVar            (RType r)                   -- ^ forall A. T
  | TAnd    [RType r]                                   -- ^ /\ (T1..) => T1' ...
                                                        -- ^ /\ (Tn..) => Tn'

  | TClass  QName                                       -- ^ typeof A.B.C (class)
  | TModule NameSpacePath                               -- ^ typeof L.M.N (module)
                                                        -- ^ names are relative to current
                                                        -- ^ environment

  | TExp F.Expr                                         -- ^ "Expression" parameters for type-aliases: 
                                                        -- ^ never appear in real/expanded RType
    deriving (Ord, Show, Functor, Data, Typeable, Traversable, Foldable)


-- | Standard Types
type Type    = RType ()


-- | Type binder
data Bind r
  = B { b_sym  :: F.Symbol                              -- ^ Binding's symbol
      , b_type :: !(RType r)                            -- ^ Field type
      } 
    deriving (Eq, Ord, Show, Functor, Data, Typeable, Traversable, Foldable)


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


type Heritage r  = Maybe (QName, [RType r])


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


-- | A module s exported API
--
data ModuleMember r 
  -- 
  -- ^ Exported class
  --
  = ModClass  { m_sym  :: Id SourceSpan                 
              , m_def  :: IfaceDef r }
  -- 
  -- ^ Exported binding
  --
  | ModVar    { m_sym  :: Id SourceSpan                 
              , m_type :: RType r }                    
  -- 
  -- ^ Exported module -- Use this to find the module definition
  --
  | ModModule { m_sym  :: Id SourceSpan }               

  deriving (Functor)

type ModuleExports r = [ModuleMember r] 


---------------------------------------------------------------------------------
-- | Mutability (TODO: move to new file - perhaps new folder called Mutability)
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






