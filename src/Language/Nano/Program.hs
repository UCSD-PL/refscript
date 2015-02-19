
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE OverlappingInstances   #-}

module Language.Nano.Program (

  -- * Programs
    Nano (..)
  , UNanoBare, UNanoSSA, NanoBareR, NanoBareRelR, NanoSSAR, NanoRefType
  , NanoTypeR, UNanoType, ExprSSAR, StmtSSAR
  , Source (..)

  -- * Nano Definition
  , IsNano (..), checkTopStmt

  -- * Nano Transformations
  , flattenStmt

  -- * SSA Ids 
  , mkNextId, isNextId, mkSSAId , mkKeysId, mkKeysIdxId, mkCtorStr, mkCtorId


  -- * Types
  , SyntaxKind(..)
  , MemberKind(..)
  , VarInfo 

  -- * CHA
  , ClassHierarchy (..)

  ) where

import           Control.Applicative     hiding (empty)
import           Control.Exception              (throw)
import           Data.Monoid             hiding ((<>))            
import           Data.Default
import           Data.List                      (stripPrefix)
import qualified Data.HashMap.Strict              as HM
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree
import qualified Data.HashSet                   as H
import           Data.Generics                   
import qualified Data.IntMap                    as I
import           Text.PrettyPrint.HughesPJ 

import           Language.Nano.Annots
import           Language.Nano.Env
import           Language.Nano.Errors
import           Language.Nano.Locations
import           Language.Nano.Names
import           Language.Nano.Types

import           Language.ECMAScript3.Syntax 
import           Language.ECMAScript3.PrettyPrint

import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Types        as F

-- import           Debug.Trace                        hiding (traceShow)


---------------------------------------------------------------------------------
-- | Nano Program 
---------------------------------------------------------------------------------
--
-- consts, aliases, invariants refer to absolute names 
-- (hence the use of RType r)

data Nano a r = Nano { 
  -- 
  -- ^ Code to check
  --
    code      :: !(Source a)
  -- 
  -- ^ Measure Signatures
  --
  , consts    :: !(Env (RType r))
  -- 
  -- ^ Type aliases
  --
  , tAlias    :: !(TAliasEnv (RTypeQ RK r))    
  -- 
  -- ^ Predicate aliases
  --
  , pAlias    :: !(PAliasEnv)
  -- 
  -- ^ Qualifiers
  --
  , pQuals    :: ![F.Qualifier]            
  -- 
  -- ^ Type Invariants
  --
  , invts     :: ![Located (RType r)]
  -- 
  -- ^ Maximum id 
  --
  , max_id    :: NodeId
  -- 
  -- ^ All fully qualified names
  --
  , fullNames :: H.HashSet AbsName
  --
  -- ^ All fully qualified namespaces
  --
  , fullPaths :: H.HashSet AbsPath
  -- 
  -- ^ Modules
  --
  , pModules  :: QEnv (ModuleDef r)
  -- 
  -- ^ CHA
  --
  , pCHA      :: ClassHierarchy r

  --
  -- ^ Options
  --
  , pOptions  :: [String]

  } deriving (Functor) -- , Data, Typeable)


newtype Source a = Src [Statement a]
  deriving (Data, Typeable)



type NanoBareRelR r = Nano  (AnnRel  r) r       -- ^ After parse (relative names)
type NanoBareR r    = Nano  (AnnBare r) r       -- ^ After Parse
type NanoSSAR r     = Nano  (AnnSSA  r) r       -- ^ After SSA  
type NanoTypeR r    = Nano  (AnnType r) r       -- ^ After TC
type NanoRefType    = NanoTypeR F.Reft          -- ^ After Liquid

type ExprSSAR r     = Expression (AnnSSA r)
type StmtSSAR r     = Statement  (AnnSSA r)

type UNanoBare      = NanoBareR ()
type UNanoSSA       = NanoSSAR  ()
type UNanoType      = NanoTypeR ()


data ClassHierarchy r = ClassHierarchy { 
  
    c_graph       :: Gr (IfaceDefQ AK r) ()

  , c_nodesToKeys :: HM.HashMap AbsName Int
  
  }

instance Default (ClassHierarchy r) where
  def = ClassHierarchy (mkGraph [] []) HM.empty

instance Functor ClassHierarchy where
  fmap f (ClassHierarchy g n) = ClassHierarchy (nmap (fmap f) g) n


---------------------------------------------------------------------------
-- | Instances
---------------------------------------------------------------------------

instance Monoid (Source a) where
  mempty                    = Src []
  mappend (Src s1) (Src s2) = Src $ s1 ++ s2

instance Functor Source where 
  fmap f (Src zs) = Src (map (fmap f) zs)

instance (PP r, F.Reftable r) => PP (Nano a r) where
  pp pgm@(Nano {code = (Src s) }) 
    =   text "\n******************* Code **********************"
    $+$ pp s
    $+$ text "\n******************* Constants *****************"
    $+$ pp (consts pgm) 
    $+$ text "\n******************* Predicate Aliases *********"
    $+$ pp (pAlias pgm)
    $+$ text "\n******************* Type Aliases **************"
    $+$ pp (tAlias pgm)
    $+$ text "\n******************* Qualifiers ****************"
    $+$ vcat (F.toFix <$> (take 3 $ pQuals pgm))
    $+$ text "..."
    $+$ text "\n******************* Invariants ****************"
    $+$ vcat (pp <$> (invts pgm))
    $+$ text "\n***********************************************\n"

instance PP t => PP (I.IntMap t) where
  pp m = vcat (pp <$> I.toList m)

instance PP t => PP (F.SEnv t) where
  pp m = vcat $ pp <$> F.toListSEnv m

instance PP (ClassHierarchy r) where
  pp (ClassHierarchy g _)   =  text "***********************************************"
                           $+$ text "Class Hierarchy" 
                           $+$ text "***********************************************"
                           $+$ vcat (ppEdge <$> edges g)
    where
      ppEdge (a,b)          =  ppNode a <+> text "->" <+> ppNode b
      ppNode                =  ppSig . lab' . context g
      ppSig (ID n _ vs _ _) =  pp n <> optPpArgs vs
      angles p              =  char '<' <> p <> char '>'
      optPpArgs []          =  text ""
      optPpArgs vs          =  ppArgs angles comma vs
  

---------------------------------------------------------------------
-- | Wrappers around `Language.ECMAScript3.Syntax` ------------------
---------------------------------------------------------------------

-- | `IsNano` is a predicate that describes the **syntactic subset** 
--   of ECMAScript3 that comprises `Nano`.

class IsNano a where 
  isNano :: a -> Bool


instance IsNano InfixOp where
  isNano OpLT         = True --  @<@
  isNano OpLEq        = True --  @<=@
  isNano OpGT         = True --  @>@
  isNano OpGEq        = True --  @>=@
  -- isNano OpEq         = True --  @==@
  isNano OpStrictEq   = True --  @===@
  isNano OpNEq        = True --  @!=@
  isNano OpStrictNEq  = True --  @!==@

  isNano OpLAnd       = True --  @&&@
  isNano OpLOr        = True --  @||@

  isNano OpSub        = True --  @-@
  isNano OpAdd        = True --  @+@
  isNano OpMul        = True --  @*@
  isNano OpDiv        = True --  @/@
  isNano OpMod        = True --  @%@
  isNano OpInstanceof = True --  @instanceof@
  isNano OpIn         = True --  @in@
  isNano OpBOr        = True --  @|@
  isNano OpBXor       = True --  @^@
  isNano OpBAnd       = True --  @&@

  isNano OpLShift     = True --  @<<@
  isNano OpSpRShift   = True --  @>>@
  isNano OpZfRShift   = True --  @>>>@
  isNano e            = errortext (text "Not Nano InfixOp!" <+> pp e)

instance IsNano (LValue a) where 
  isNano (LVar _ _)        = True
  isNano (LDot _ e _)      = isNano e
  isNano (LBracket _ e e') = isNano e && isNano e'

instance IsNano (VarDecl a) where
  isNano (VarDecl _ _ (Just e)) = isNano e
  isNano (VarDecl _ _ Nothing)  = True

instance IsNano (Expression a) where 
  isNano (BoolLit _ _)           = True
  isNano (IntLit _ _)            = True
  isNano (HexLit _ _)            = True
  isNano (NullLit _ )            = True
  isNano (ArrayLit _ es)         = all isNano es
  isNano (StringLit _ _)         = True
  isNano (CondExpr _ e1 e2 e3)   = all isNano [e1,e2,e3]
  isNano (VarRef _ _)            = True
  isNano (InfixExpr _ o e1 e2)   = isNano o && isNano e1 && isNano e2
  isNano (PrefixExpr _ o e)      = isNano o && isNano e
  isNano (CallExpr _ e es)       = all isNano (e:es)
  isNano (ObjectLit _ bs)        = all isNano $ snd <$> bs
  isNano (DotRef _ e _)          = isNano e
  isNano (BracketRef _ e1 e2)    = isNano e1 && isNano e2
  isNano (AssignExpr _ _ l e)    = isNano e && isNano l && isNano e
  isNano (UnaryAssignExpr _ _ l) = isNano l
  isNano (ThisRef _)             = True 
  isNano (SuperRef _)            = True 
  isNano (FuncExpr _ _ _ s)      = isNano s
  isNano (NewExpr _ e es)        = isNano e && all isNano es
  isNano (Cast _ e)              = isNano e
  isNano e                       = errortext (text "Not Nano Expression!" <+> pp e)
  -- isNano _                     = False

instance IsNano AssignOp where
  isNano OpAssign     = True
  isNano OpAssignAdd  = True
  isNano OpAssignSub  = True
  isNano OpAssignMul  = True
  isNano OpAssignDiv  = True
  isNano OpAssignLShift   = True
  isNano OpAssignSpRShift = True
  isNano OpAssignZfRShift = True
  isNano OpAssignBAnd = True
  isNano OpAssignBXor = True
  isNano OpAssignBOr = True
  isNano x            = errortext (text "Not Nano AssignOp!" <+> pp x) 
  -- isNano _        = False

instance IsNano PrefixOp where
  isNano PrefixLNot   = True
  isNano PrefixMinus  = True 
  isNano PrefixPlus   = True 
  isNano PrefixTypeof = True 
  isNano PrefixBNot   = True 
  isNano e            = errortext (text "Not Nano PrefixOp!" <+> pp e)
  -- isNano _            = False

instance IsNano (Statement a) where
  isNano (EmptyStmt _)            = True                   --  skip
  isNano (ExprStmt _ e)           = isNanoExprStatement e  --  x = e
  isNano (BlockStmt _ ss)         = isNano ss              --  sequence
  isNano (IfSingleStmt _ b s)     = isNano b && isNano s
  isNano (IfStmt _ b s1 s2)       = isNano b && isNano s1 && isNano s2
  isNano (WhileStmt _ b s)        = isNano b && isNano s
  isNano (ForStmt _ i t inc b)    = isNano i && isNano t && isNano inc && isNano b
  isNano (ForInStmt _ init e s)   = isNano init && isNano e && isNano s
  isNano (VarDeclStmt _ ds)       = all isNano ds
  isNano (ReturnStmt _ e)         = isNano e
  isNano (FunctionStmt _ _ _ b)   = isNano b
  isNano (SwitchStmt _ e cs)      = isNano e && not (null cs) && isNano cs
  isNano (ClassStmt _ _ _ _  bd)  = all isNano bd
  isNano (ThrowStmt _ e)          = isNano e
  isNano (FuncAmbDecl _ _ _)      = True
  isNano (FuncOverload _ _ _)     = True
  isNano (IfaceStmt _ _)          = True
  isNano (ModuleStmt _ _ s)       = all isNano s
  isNano (EnumStmt _ _ _)         = True
  isNano e                        = errortext (text "Not Nano Statement:" $$ pp e)

instance IsNano (ClassElt a) where
  isNano (Constructor _ _ ss)       = all isNano ss
  isNano (MemberMethDecl _ _ _ _ )  = True
  isNano (MemberMethDef _ _ _ _ ss) = all isNano ss
  isNano (MemberVarDecl _ _ _ eo)   = isNano eo

instance IsNano a => IsNano (Maybe a) where 
  isNano (Just x) = isNano x
  isNano Nothing  = True

instance IsNano [(Statement a)] where 
  isNano = all isNano 

instance IsNano (ForInit a) where 
  isNano NoInit        = True
  isNano (VarInit vds) = all isNano vds
  isNano (ExprInit e)  = isNano e

instance IsNano (ForInInit a) where 
  isNano (ForInVar _)  = True
  isNano e             = errortext (text "Not Nano ForInInit:" $$ pp e)


-- | Holds for `Expression` that is a valid side-effecting `Statement` 

isNanoExprStatement :: Expression a -> Bool
isNanoExprStatement (UnaryAssignExpr _ _ lv) = isNano lv
isNanoExprStatement (AssignExpr _ o lv e)    = isNano o && isNano lv && isNano e
isNanoExprStatement (CallExpr _ e es)        = all isNano (e:es)
isNanoExprStatement (Cast _ e)               = isNanoExprStatement e
isNanoExprStatement e@(FuncExpr _ _ _ _ )    = errortext (text "Unannotated function expression" <+> pp e)
isNanoExprStatement e                        = errortext (text "Not Nano ExprStmtZ!" <+> pp e)

-- | Switch Statement

-- Is Nano-js code if each clause ends with a break statement

instance IsNano (CaseClause a) where
  isNano (CaseClause _ e st) = isNano e && holdsInit isNano st' && endsWithBreak st'
    where st' = concatMap flattenStmt st
  isNano (CaseDefault _  st) =             holdsInit isNano st' && endsWithBreak st'
    where st' = concatMap flattenStmt st

class EndsWithBreak a where
  endsWithBreak :: a -> Bool

instance EndsWithBreak (Statement a) where
  endsWithBreak (BlockStmt _ xs)      = endsWithBreak xs
  endsWithBreak (BreakStmt _ Nothing) = True
  endsWithBreak _                     = False

instance EndsWithBreak ([Statement a]) where
  endsWithBreak [] = False
  endsWithBreak xs = endsWithBreak $ last xs

instance IsNano [(CaseClause a)] where 
  isNano [] = False
  isNano xs = all isNano xs && holdsInit (not . defaultC) xs
    where
      defaultC (CaseClause _ _ _) = False
      defaultC (CaseDefault _ _ ) = True

-- | Check if `p` hold for all xs but the last one.  
holdsInit :: (a -> Bool) -> [a] -> Bool
holdsInit _ [] = True
holdsInit p xs = all p $ init xs

-- | Trivial Syntax Checking 
-- TODO: Add check for top-level classes here.

checkTopStmt :: (IsLocated a) => Statement a -> Statement a
checkTopStmt s | checkBody [s] = s
checkTopStmt s | otherwise     = throw $ errorInvalidTopStmt (srcPos s) s

checkBody :: [Statement a] -> Bool
-- Adding support for loops so removing the while check
checkBody stmts = all isNano stmts -- && null (getWhiles stmts) 

flattenStmt (BlockStmt _ ss) = concatMap flattenStmt ss
flattenStmt s                = [s]


--------------------------------------------------------------------------------
-- | Manipulating SSA Ids
--------------------------------------------------------------------------------

mkSSAId :: (F.Symbolic x, IsLocated a) => a -> x -> Int -> Id a
mkSSAId l x n = Id l (F.symbolString (F.symbol x) ++ ssaStr ++ show n)  

mkNextId :: Id a -> Id a
mkNextId (Id a x) =  Id a $ nextStr ++ x

isNextId :: Id a -> Maybe (Id a)
isNextId (Id a s) = Id a <$> stripPrefix nextStr s

mkKeysId :: Id a -> Id a
mkKeysId (Id a x) =  Id a $ keysStr ++ x

mkKeysIdxId :: Id a -> Id a
mkKeysIdxId (Id a x) =  Id a $ keysIdxStr ++ x

mkCtorId l (Id _ x) = Id l $ mkCtorStr x
mkCtorStr x         = x ++ ctorStr

nextStr    = "_NEXT_"
ssaStr     = "_SSA_"
keysIdxStr = "_KEYS_IDX_"
keysStr    = "_KEYS_"
ctorStr    = "_CTOR_"



data SyntaxKind = 
    FuncDefKind 
  | FuncAmbientKind
  | FuncOverloadKind
  | MethDefKind 
  | MethDeclKind
  | FieldDefKind
  | CtorDefKind
  | VarDeclKind
  | AmbVarDeclKind
  | ClassDefKind
  | ModuleDefKind
  | EnumDefKind
  deriving ( Eq )

instance PP SyntaxKind where
  pp FuncDefKind      = text "FuncDefKind"
  pp FuncOverloadKind = text "FuncOverloadKind"
  pp FuncAmbientKind  = text "FuncAmbientKind"
  pp MethDefKind      = text "MethDefKind"
  pp MethDeclKind     = text "MethDeclKind"
  pp FieldDefKind     = text "FieldDefKind"
  pp CtorDefKind      = text "CtorDefKind"
  pp VarDeclKind      = text "VarDeclKind"
  pp ClassDefKind     = text "ClassDefKind"
  pp ModuleDefKind    = text "ModuleDefKind"
  pp EnumDefKind      = text "EnumDefKind"
  pp AmbVarDeclKind   = text "AmbVarDeclKind" 


data MemberKind = MemDefinition | MemDeclaration deriving ( Eq )

type VarInfo r = (SyntaxKind, Visibility, Assignability, RType r, Initialization)
