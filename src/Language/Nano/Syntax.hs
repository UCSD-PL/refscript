-- |ECMAScript 3 syntax. /Spec/ refers to the ECMA-262 specification,
-- 3rd edition.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}

module Language.Nano.Syntax (JavaScript(..)
                            ,unJavaScript
                            ,Statement(..)
                            ,ClassElt(..)
                            ,isIterationStmt
                            ,CaseClause(..)
                            ,CatchClause(..)
                            ,ForInit(..)
                            ,ForInInit(..)
                            ,VarDecl(..)
                            ,Expression(..)
                            ,InfixOp(..)
                            ,AssignOp(..)
                            ,Id(..)
                            ,unId
                            ,PrefixOp(..)
                            ,Prop(..)
                            ,UnaryAssignOp(..)
                            ,LValue (..)
                            ,EnumElt(..)
                            ,SourcePos
                            ) where

import Text.Parsec.Pos(initialPos,SourcePos) -- used by data JavaScript
import Data.Generics(Data,Typeable)
import Data.Text
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Data.Default
import GHC.Generics

data JavaScript a   -- | A script in \<script\> ... \</script\> tags.
  = Script a [Statement a]
  deriving (Show,Data,Typeable,Eq,Ord,Functor,Foldable,Traversable, Generic)

instance Default a => Default (JavaScript a) where
  def = Script def []

-- | extracts statements from a JavaScript type
unJavaScript :: JavaScript a -> [Statement a]
unJavaScript (Script _ stmts) = stmts

{-instance Default SourcePos where-}
{-  def = initialPos ""-}

data Id a = Id a String 
          deriving (Show,Eq,Ord,Data,Typeable,Functor,Foldable,Traversable,Generic)

unId :: Id a -> String
unId (Id _ s) = s

-- | Infix operators: see spec 11.5-11.11
data InfixOp = OpLT -- ^ @<@
             | OpLEq -- ^ @<=@
             | OpGT -- ^ @>@
             | OpGEq -- ^ @>=@
             | OpIn -- ^ @in@
             | OpInstanceof -- ^ @instanceof@
             | OpEq -- ^ @==@
             | OpNEq -- ^ @!=@
             | OpStrictEq -- ^ @===@
             | OpStrictNEq -- ^ @!===@
             | OpLAnd -- ^ @&&@
             | OpLOr -- ^ @||@
             | OpMul -- ^ @*@
             | OpDiv -- ^ @/@
             | OpMod -- ^ @%@
             | OpSub -- ^ @-@
             | OpLShift -- ^ @<<@
             | OpSpRShift -- ^ @>>@
             | OpZfRShift -- ^ @>>>@
             | OpBAnd -- ^ @&@
             | OpBXor -- ^ @^@
             | OpBOr -- ^ @|@
             | OpAdd -- ^ @+@
    deriving (Show,Data,Typeable,Eq,Ord,Enum,Generic)

-- | Assignment operators: see spec 11.13
data AssignOp = OpAssign -- ^ simple assignment, @=@
              | OpAssignAdd -- ^ @+=@
              | OpAssignSub -- ^ @-=@
              | OpAssignMul -- ^ @*=@
              | OpAssignDiv -- ^ @/=@
              | OpAssignMod -- ^ @%=@
              | OpAssignLShift -- ^ @<<=@
              | OpAssignSpRShift -- ^ @>>=@
              | OpAssignZfRShift -- ^ @>>>=@
              | OpAssignBAnd -- ^ @&=@
              | OpAssignBXor -- ^ @^=@
              | OpAssignBOr -- ^ @|=@
  deriving (Show,Data,Typeable,Eq,Ord,Generic)

-- | Unary assignment operators: see spec 11.3, 11.4.4, 11.4.5
data UnaryAssignOp = PrefixInc -- ^ @++x@
                   | PrefixDec -- ^ @--x@
                   | PostfixInc -- ^ @x++@
                   | PostfixDec -- ^ @x--@
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

-- | Prefix operators: see spec 11.4 (excluding 11.4.4, 11.4.5)
data PrefixOp = PrefixLNot -- ^ @!@
              | PrefixBNot -- ^ @~@
              | PrefixPlus -- ^ @+@
              | PrefixMinus -- ^ @-@
              | PrefixTypeof -- ^ @typeof@
              | PrefixVoid -- ^ @void@
              | PrefixDelete -- ^ @delete@
  deriving (Show,Data,Typeable,Eq,Ord,Generic)

-- | Property names in an object initializer: see spec 11.1.5
data Prop a = PropId a (Id a) -- ^ property name is an identifier, @foo@
            | PropString a String -- ^ property name is a string, @\"foo\"@
            | PropNum a Integer -- ^ property name is an integer, @42@
  deriving (Show,Data,Typeable,Eq,Ord,Functor,Foldable,Traversable, Generic)
 
-- | Left-hand side expressions: see spec 11.2
data LValue a
  = LVar a String -- ^ variable reference, @foo@
  | LDot a (Expression a) String -- ^ @foo.bar@
  | LBracket a (Expression a) (Expression a) -- ^ @foo[bar]@
  deriving (Show, Eq, Ord, Data, Typeable, Functor,Foldable,Traversable,Generic) 

-- | Expressions, see spec 11
data Expression a
  = StringLit a String -- ^ @\"foo\"@, spec 11.1.3, 7.8
  | RegexpLit a String Bool Bool 
    -- ^ @RegexpLit a regexp global?  case_insensitive?@ -- regular
    -- expression, see spec 11.1.3, 7.8
  | NumLit a Double -- ^ @41.99999@, spec 11.1.3, 7.8
  | IntLit a Int -- ^ @42@, spec 11.1.3, 7.8
  | BoolLit a Bool -- ^ @true@, spec 11.1.3, 7.8
  | NullLit a -- ^ @null@, spec 11.1.3, 7.8
  | ArrayLit a [Expression a] -- ^ @[1,2,3]@, spec 11.1.4
  | ObjectLit a [(Prop a, Expression a)] -- ^ @{foo:\"bar\", baz: 42}@, spec 11.1.5

  -- 
  -- RefScript Hex literal -- Used as BitVector
  --
  | HexLit a String

  | ThisRef a -- ^ @this@, spec 11.1.1
  | VarRef a (Id a) -- ^ @foo@, spec 11.1.2
  | DotRef a (Expression a) (Id a) -- ^ @foo.bar@, spec 11.2.1
  | BracketRef a (Expression a) {- container -} (Expression a) {- key -} 
    -- ^ @foo[bar]@, spec 11.2.1
  | NewExpr a (Expression a) {- constructor -} [Expression a] 
    -- ^ @new foo(bar)@, spec 11.2.2
  | PrefixExpr a PrefixOp (Expression a) 
    -- ^ @\@e@, spec 11.4 (excluding 11.4.4, 111.4.5)
  | UnaryAssignExpr a UnaryAssignOp (LValue a) 
    -- ^ @++x@, @x--@ etc., spec 11.3, 11.4.4, 11.4.5
  | InfixExpr a InfixOp (Expression a) (Expression a) 
    -- ^ @e1\@e2@, spec 11.5, 11.6, 11.7, 11.8, 11.9, 11.10, 11.11
  | CondExpr a (Expression a) (Expression a) (Expression a)
    -- ^ @e1 ? e2 : e3@, spec 11.12
  | AssignExpr a AssignOp (LValue a) (Expression a)
    -- ^ @e1 \@=e2@, spec 11.13
  | ListExpr a [Expression a] -- ^ @e1, e2@, spec 11.14
  | CallExpr a (Expression a) [Expression a] -- ^ @f(x,y,z)@, spec 11.2.3
  | SuperRef a -- ^ @super@, TS spec
  --funcexprs are optionally named
  | FuncExpr a (Maybe (Id a)) [Id a] [Statement a]
    -- ^ @function f (x,y,z) {...}@, spec 11.2.5, 13

  -- 
  -- RefScript Cast expressions
  --
  | Cast  a (Expression a)    -- ^ User inserted cast
  | Cast_ a (Expression a)    -- ^ Implicitly created cast
  deriving (Show,Data,Typeable,Eq,Ord,Functor,Foldable,Traversable,Generic)

-- | Case clauses, spec 12.11
data CaseClause a = CaseClause a (Expression a) [Statement a]
                    -- ^ @case e: stmts;@
                  | CaseDefault a [Statement a]
                    -- ^ @default: stmts;@
  deriving (Show,Data,Typeable,Eq,Ord,Functor,Foldable,Traversable, Generic)

-- | Catch clause, spec 12.14
data CatchClause a = CatchClause a (Id a) (Statement a) 
                     -- ^ @catch (x) {...}@
  deriving (Show,Data,Typeable,Eq,Ord,Functor,Foldable,Traversable, Generic)

-- | A variable declaration, spec 12.2
data VarDecl a = VarDecl a (Id a) (Maybe (Expression a)) 
                 -- ^ @var x = e;@
  deriving (Show,Data,Typeable,Eq,Ord,Functor,Foldable,Traversable, Generic)
  
-- | for initializer, spec 12.6
data ForInit a = NoInit -- ^ empty
               | VarInit [VarDecl a] -- ^ @var x, y=42@
               | ExprInit (Expression a) -- ^ @expr@
  deriving (Show,Data,Typeable,Eq,Ord,Functor,Foldable,Traversable, Generic)

-- | for..in initializer, spec 12.6
data ForInInit a = ForInVar (Id a) -- ^ @var x@
                 | ForInLVal (LValue a) -- ^ @foo.baz@, @foo[bar]@, @z@
 deriving (Show,Data,Typeable,Eq,Ord,Functor,Foldable,Traversable, Generic)
  
-- | Statements, spec 12.
data Statement a 
  = BlockStmt a [Statement a] -- ^ @{stmts}@, spec 12.1
  | EmptyStmt a -- ^ @;@, spec 12.3
  | ExprStmt a (Expression a) -- ^ @expr;@, spec 12.4
  | IfStmt a (Expression a) (Statement a) (Statement a) 
    -- ^ @if (e) stmt@, spec 12.5
  | IfSingleStmt a (Expression a) (Statement a)
    -- ^ @if (e) stmt1 else stmt2@, spec 12.5
  | SwitchStmt a (Expression a) [CaseClause a]
    -- ^ @switch (e) clauses@, spec 12.11
  | WhileStmt a (Expression a) (Statement a)
    -- ^ @while (e) do stmt@, spec 12.6
  | DoWhileStmt a (Statement a) (Expression a)
    -- ^ @do stmt while (e);@, spec 12.6
  | BreakStmt a (Maybe (Id a)) -- ^ @break lab;@, spec 12.8
  | ContinueStmt a (Maybe (Id a)) -- ^ @continue lab;@, spec 12.7
  | LabelledStmt a (Id a) (Statement a) -- ^ @lab: stmt@, spec 12.12
  | ForInStmt a (ForInInit a) (Expression a) (Statement a) 
    -- ^ @for (x in o) stmt@, spec 12.6
  | ForStmt a (ForInit a)        
              (Maybe (Expression a)) -- test
              (Maybe (Expression a)) -- increment
              (Statement a)          -- body 
    -- ^ @ForStmt a init test increment body@, @for (init; test,
    -- increment) body@, spec 12.6
  | TryStmt a (Statement a) {-body-} (Maybe (CatchClause a))
      (Maybe (Statement a)) {-finally-}
    -- ^ @try stmt catch(x) stmt finally stmt@, spec 12.14
  | ThrowStmt a (Expression a)
    -- ^ @throw expr;@, spec 12.13
  | ReturnStmt a (Maybe (Expression a))
    -- ^ @return expr;@, spec 12.9
  | WithStmt a (Expression a) (Statement a)
    -- ^ @with (o) stmt@, spec 12.10
  | VarDeclStmt a [VarDecl a]
    -- ^ @var x, y=42;@, spec 12.2
  | FunctionStmt a (Id a) {-name-} [Id a] {-args-} [Statement a] {-body-}
    -- ^ @function f(x, y, z) {...}@, spec 13

  -- ^ TypeScipt
  
  | FuncAmbDecl a (Id a) {-name-} [Id a] {-args-}
  | FuncOverload a (Id a) {-name-} [Id a] {-args-}
    -- ^ @declare function f(x, y, z); @
  | ClassStmt a (Id a) (Maybe (Id a)) {-extends-} [Id a] {-implem-} [ClassElt a]
    -- ^ @class C<V> extends C'<T> {...}@
  | ModuleStmt a (Id a) [Statement a]
    -- ^ @module M {...}@
  | IfaceStmt a (Id a) 
    -- ^ @interface A {...}@ -- Placeholder for interface annotations
  | EnumStmt a (Id a) [EnumElt a]

  deriving (Show,Data,Typeable,Eq,Ord,Functor,Foldable,Traversable,Generic)


--------------------------------------------------------------------------------
-- | TypeScript
--------------------------------------------------------------------------------
--
-- http://www.typescriptlang.org/Content/TypeScript%20Language%20Specification.pdf
--

data ClassElt a   -- Class element, spec 8.1.2
  = Constructor     a                        [Id a] [Statement a]

  | MemberVarDecl   a Bool {-static-} (Id a)        (Maybe (Expression a)) 

  | MemberMethDecl  a Bool {-static-} (Id a) [Id a]

  | MemberMethDef   a Bool {-static-} (Id a) [Id a] [Statement a] 

  deriving (Show,Data,Typeable,Eq,Ord,Functor,Foldable,Traversable, Generic)

data EnumElt a = EnumElt a (Id a) (Expression a)
  deriving (Show,Data,Typeable,Eq,Ord,Functor,Foldable,Traversable, Generic)


-- | Returns 'True' if the statement is an /IterationStatement/
-- according to spec 12.6.
isIterationStmt :: Statement a -> Bool
isIterationStmt s = case s of
  WhileStmt {}   -> True
  DoWhileStmt {} -> True
  ForStmt {}     -> True
  ForInStmt {}   -> True
  _              -> False

