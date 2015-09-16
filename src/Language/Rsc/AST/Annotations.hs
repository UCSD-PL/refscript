{-# LANGUAGE FlexibleInstances #-}

module Language.Rsc.AST.Annotations where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad.State     hiding (mapM)
import           Data.Traversable
import           Language.Rsc.AST.Syntax
import           Language.Rsc.Locations
import           Language.Rsc.Names
import           Prelude                 hiding (mapM)

-- | Removes annotations from a tree
removeAnnotations :: Traversable t => t a -> t ()
removeAnnotations = reannotate (const ())

-- | Changes all the labels in the tree to another one, given by a function.
reannotate :: Traversable t => (a -> b) -> t a -> t b
reannotate f tree = traverse (pure . f) tree ()

-- | add an extra field to the AST labels (the label would look like @(a, b)@)
addExtraAnnotationField :: Traversable t => b -> t a -> t (a, b)
addExtraAnnotationField def t = traverse (\z -> pure (z, def)) t ()

-- | remove an extra field
removeExtraAnnotationField :: Traversable t => t (a, b) -> t a
removeExtraAnnotationField t = traverse (pure . fst) t ()

-- | Assigns unique numeric (Int) ids to each node in the AST. Returns a pair:
-- the tree annotated with UID's and the last ID that was assigned.
assignUniqueIds :: Traversable t => Int -- ^ starting id
                                 -> t a -- ^ tree root
                                 -> (t (a, Int), Int)
assignUniqueIds first tree =
  (returnA *** \i -> i-1) $ runState (mapM f tree) first
  where f :: a -> State Int (a, Int)
        f a = do i <- get
                 put (i+1)
                 return (a, i)

class Annotated a where
  -- | Returns the annotation of the root of the tree
  getAnnotation :: a b -> b

instance Annotated Expression where
  getAnnotation e = case e of
    StringLit        a  _     -> a
    RegexpLit        a  _ _ _ -> a
    NumLit           a  _     -> a
    HexLit           a  _     -> a
    IntLit           a  _     -> a
    BoolLit          a  _     -> a
    NullLit          a        -> a
    ArrayLit         a  _     -> a
    ObjectLit        a  _     -> a
    ThisRef          a        -> a
    VarRef           a  _     -> a
    DotRef           a  _ _   -> a
    BracketRef       a  _ _   -> a
    NewExpr          a  _ _   -> a
    PrefixExpr       a  _ _   -> a
    UnaryAssignExpr  a  _ _   -> a
    InfixExpr        a  _ _ _ -> a
    CondExpr         a  _ _ _ -> a
    AssignExpr       a  _ _ _ -> a
    ListExpr         a  _     -> a
    CallExpr         a  _ _   -> a
    FuncExpr         a  _ _ _ -> a
    Cast             a  _     -> a
    Cast_            a  _     -> a
    SuperRef         a        -> a

instance Annotated Statement where
  getAnnotation s = case s of
    BlockStmt      a  _       -> a
    EmptyStmt      a          -> a
    ExprStmt       a  _       -> a
    IfStmt         a  _ _ _   -> a
    IfSingleStmt   a  _ _     -> a
    SwitchStmt     a  _ _     -> a
    WhileStmt      a  _ _     -> a
    DoWhileStmt    a  _ _     -> a
    BreakStmt      a  _       -> a
    ContinueStmt   a  _       -> a
    LabelledStmt   a  _ _     -> a
    ForInStmt      a  _ _ _   -> a
    ForStmt        a  _ _ _ _ -> a
    TryStmt        a  _ _ _   -> a
    ThrowStmt      a  _       -> a
    ReturnStmt     a  _       -> a
    WithStmt       a  _ _     -> a
    VarDeclStmt    a  _       -> a
    FunctionStmt   a  _ _ _   -> a
    ClassStmt      a  _ _ _ _ -> a
    ModuleStmt     a  _ _     -> a
    InterfaceStmt  a  _       -> a
    EnumStmt       a  _ _     -> a

instance Annotated LValue where
  getAnnotation lv = case lv of
    LVar      a  _    -> a
    LDot      a  _ _  -> a
    LBracket  a  _ _  -> a

instance Annotated VarDecl where
  getAnnotation (VarDecl a _ _) = a

instance Annotated Prop  where
  getAnnotation p = case p of
    PropId     a _ -> a
    PropString a _ -> a
    PropNum    a _ -> a

instance Annotated CaseClause where
  getAnnotation c = case c of
    CaseClause  a _ _ -> a
    CaseDefault a _   -> a

instance Annotated CatchClause where
  getAnnotation (CatchClause a _ _) = a

instance Annotated Id where
  getAnnotation (Id a _) = a

instance Annotated ClassElt where
  getAnnotation e = case e of
    Constructor    a _ _     -> a
    MemberVarDecl  a _ _ _   -> a
    MemberMethDecl a _ _ _ _ -> a

instance (Annotated thing, IsLocated a) => IsLocated (thing a) where
  srcPos  = srcPos . getAnnotation

