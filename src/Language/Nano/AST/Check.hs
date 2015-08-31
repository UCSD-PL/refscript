{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Language.Nano.AST.Check
    (
      flattenStmt
    , checkTopStmt
    , IsNano(..)
    ) where

import           Control.Applicative           ((<$>))
import           Control.Exception             (throw)
import           Data.Default
import           Data.Foldable                 (Foldable)
import           Data.Generics                 (Data, Typeable)
import           Data.Traversable              (Traversable)
import           GHC.Generics
import           Language.Fixpoint.Misc        (errortext)
import           Language.Fixpoint.PrettyPrint
import           Language.Nano.AST.Annotations ()
import           Language.Nano.AST.Syntax
import           Language.Nano.Errors
import           Language.Nano.Locations
import           Language.Nano.Pretty.Common
import           Language.Nano.Pretty.Syntax
import           Text.Parsec.Pos               (SourcePos)
import           Text.PrettyPrint.HughesPJ

---------------------------------------------------------------------
-- | Wrappers around `Language.Nano.AST`
---------------------------------------------------------------------

-- | `IsNano` is a predicate that describes the **syntactic subset**
--   of Nano that comprises `Nano`.

class IsNano a where
  isNano :: a -> Bool


instance IsNano InfixOp where
  isNano OpLT         = True --  @<@
  isNano OpLEq        = True --  @<=@
  isNano OpGT         = True --  @>@
  isNano OpGEq        = True --  @>=@
  isNano OpStrictEq   = True --  @===@
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

instance IsNano PrefixOp where
  isNano PrefixLNot   = True
  isNano PrefixMinus  = True
  isNano PrefixPlus   = True
  isNano PrefixTypeof = True
  isNano PrefixBNot   = True
  isNano e            = errortext (text "Not Nano PrefixOp!" <+> pp e)

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

flattenStmt (BlockStmt _ ss) = concatMap flattenStmt ss
flattenStmt s                = [s]

checkTopStmt :: (IsLocated a) => Statement a -> Statement a
checkTopStmt s | checkBody [s] = s
checkTopStmt s | otherwise     = throw $ errorInvalidTopStmt (srcPos s) s

checkBody :: [Statement a] -> Bool
checkBody stmts = all isNano stmts
