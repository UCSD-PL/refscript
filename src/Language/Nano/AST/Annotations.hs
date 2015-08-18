module Language.Nano.AST.Annotations where

import           Language.Nano.AST.Syntax

import           Control.Applicative
import           Control.Arrow
import           Control.Monad.State      hiding (mapM)
import           Data.Traversable
import           Prelude                  hiding (mapM)

-- | Removes annotations from a tree
removeAnnotations :: Traversable t => t a -> t ()
removeAnnotations = reannotate (const ())

-- | Changes all the labels in the tree to another one, given by a
-- function.
reannotate :: Traversable t => (a -> b) -> t a -> t b
reannotate f tree = traverse (pure . f) tree ()

-- | add an extra field to the AST labels (the label would look like @
-- (a, b) @)
addExtraAnnotationField :: Traversable t => b -> t a -> t (a, b)
addExtraAnnotationField def t = traverse (\z -> pure (z, def)) t ()

-- | remove an extra field
removeExtraAnnotationField :: Traversable t => t (a, b) -> t a
removeExtraAnnotationField t = traverse (pure . fst) t ()


-- | Assigns unique numeric (Int) ids to each node in the AST. Returns
-- a pair: the tree annotated with UID's and the last ID that was
-- assigned.
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
   StringLit a s              -> a
   RegexpLit a s g ci         -> a
   NumLit a d                 -> a
   HexLit a d                 -> a
   IntLit a i                 -> a
   BoolLit a b                -> a
   NullLit a                  -> a
   ArrayLit a exps            -> a
   ObjectLit a props          -> a
   ThisRef a                  -> a
   VarRef a id                -> a
   DotRef a exp id            -> a
   BracketRef a container key -> a
   NewExpr a ctor params      -> a
   PrefixExpr a op e          -> a
   UnaryAssignExpr a op lv    -> a
   InfixExpr a op e1 e2       -> a
   CondExpr a g et ef         -> a
   AssignExpr a op lv e       -> a
   ListExpr a es              -> a
   CallExpr a fn params       -> a
   FuncExpr a mid args s      -> a
   Cast a e                   -> a
   Cast_ a e                  -> a
   SuperRef a         	      -> a

instance Annotated Statement where
  getAnnotation = getAnnotationStmt

getAnnotationStmt = go
  where
    go (BlockStmt a _            ) = a
    go (EmptyStmt a              ) = a
    go (ExprStmt a _             ) = a
    go (IfStmt a _ _ _           ) = a
    go (IfSingleStmt a _ _       ) = a
    go (SwitchStmt a _ _         ) = a
    go (WhileStmt a _ _          ) = a
    go (DoWhileStmt a _ _        ) = a
    go (BreakStmt a _            ) = a
    go (ContinueStmt a _         ) = a
    go (LabelledStmt a _ _       ) = a
    go (ForInStmt a _ _ _        ) = a
    go (ForStmt a _ _ _ _        ) = a
    go (TryStmt a _ _ _          ) = a
    go (ThrowStmt a _            ) = a
    go (ReturnStmt a _           ) = a
    go (WithStmt a _ _           ) = a
    go (VarDeclStmt a _          ) = a
    go (FunctionStmt a _ _ _     ) = a
    go (FuncAmbDecl a _ _        ) = a
    go (FuncOverload a _ _       ) = a
    go (ClassStmt a _ _ _ _      ) = a
    go (ModuleStmt a _ _         ) = a
    go (IfaceStmt a _            ) = a
    go (EnumStmt a _ _           ) = a


instance Annotated LValue where
  getAnnotation lv = case lv of
    LVar a _ -> a
    LDot a _ _ -> a
    LBracket a _ _ -> a

instance Annotated VarDecl where
  getAnnotation (VarDecl a _ _) = a

instance Annotated Prop  where
  getAnnotation p = case p of
    PropId a _ -> a
    PropString a _ -> a
    PropNum a _ -> a

instance Annotated CaseClause where
  getAnnotation c = case c of
    CaseClause a _ _ -> a
    CaseDefault a _ -> a

instance Annotated CatchClause where
  getAnnotation (CatchClause a _ _) = a

instance Annotated Id where
  getAnnotation (Id a _) = a

instance Annotated ClassElt where
  getAnnotation e = case e of
    Constructor a _ _       -> a
    MemberVarDecl a _ _ _   -> a
    MemberMethDecl a _ _ _  -> a
    MemberMethDef a _ _ _ _ -> a

