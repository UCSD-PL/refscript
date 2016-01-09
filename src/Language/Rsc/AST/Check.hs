{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Language.Rsc.AST.Check
    (
      flattenStmt
    , checkTopStmt
    , IsRsc(..)
    ) where

import           Control.Applicative                 ((<$>))
import           Control.Exception                   (throw)
import           Data.Default
import           Data.Foldable                       (Foldable)
import           Data.Generics                       (Data, Typeable)
import           Data.Traversable                    (Traversable)
import           GHC.Generics
import           Language.Fixpoint.Types.PrettyPrint
import           Language.Rsc.AST.Annotations        ()
import           Language.Rsc.AST.Syntax
import           Language.Rsc.Errors
import           Language.Rsc.Locations
import           Language.Rsc.Pretty.Common
import           Language.Rsc.Pretty.Errors
import           Language.Rsc.Pretty.Syntax
import           Text.Parsec.Pos                     (SourcePos)
import           Text.PrettyPrint.HughesPJ

---------------------------------------------------------------------
-- | Wrappers around `Language.Rsc.AST`
---------------------------------------------------------------------

-- | `IsRsc` is a predicate that describes the **syntactic subset**
--   of Rsc that comprises `Rsc`.

class IsRsc a where
  isRsc :: a -> Bool


instance IsRsc InfixOp where
  isRsc OpLT         = True --  @<@
  isRsc OpLEq        = True --  @<=@
  isRsc OpGT         = True --  @>@
  isRsc OpGEq        = True --  @>=@
  isRsc OpStrictEq   = True --  @===@
  isRsc OpStrictNEq  = True --  @!==@

  isRsc OpLAnd       = True --  @&&@
  isRsc OpLOr        = True --  @||@

  isRsc OpSub        = True --  @-@
  isRsc OpAdd        = True --  @+@
  isRsc OpMul        = True --  @*@
  isRsc OpDiv        = True --  @/@
  isRsc OpMod        = True --  @%@
  isRsc OpInstanceof = True --  @instanceof@
  isRsc OpIn         = True --  @in@
  isRsc OpBOr        = True --  @|@
  isRsc OpBXor       = True --  @^@
  isRsc OpBAnd       = True --  @&@

  isRsc OpLShift     = True --  @<<@
  isRsc OpSpRShift   = True --  @>>@
  isRsc OpZfRShift   = True --  @>>>@
  isRsc e            = errortext (text "Not Rsc InfixOp!" <+> pp e)

instance IsRsc (LValue a) where
  isRsc (LVar _ _)        = True
  isRsc (LDot _ e _)      = isRsc e
  isRsc (LBracket _ e e') = isRsc e && isRsc e'

instance IsRsc (VarDecl a) where
  isRsc (VarDecl _ _ (Just e)) = isRsc e
  isRsc (VarDecl _ _ Nothing)  = True

instance IsRsc (Expression a) where
  isRsc (BoolLit _ _)           = True
  isRsc (IntLit _ _)            = True
  isRsc (HexLit _ _)            = True
  isRsc (NullLit _ )            = True
  isRsc (ArrayLit _ es)         = all isRsc es
  isRsc (StringLit _ _)         = True
  isRsc (CondExpr _ e1 e2 e3)   = all isRsc [e1,e2,e3]
  isRsc (VarRef _ _)            = True
  isRsc (InfixExpr _ o e1 e2)   = isRsc o && isRsc e1 && isRsc e2
  isRsc (PrefixExpr _ o e)      = isRsc o && isRsc e
  isRsc (CallExpr _ e es)       = all isRsc (e:es)
  isRsc (ObjectLit _ bs)        = all isRsc $ snd <$> bs
  isRsc (DotRef _ e _)          = isRsc e
  isRsc (BracketRef _ e1 e2)    = isRsc e1 && isRsc e2
  isRsc (AssignExpr _ _ l e)    = isRsc e && isRsc l && isRsc e
  isRsc (UnaryAssignExpr _ _ l) = isRsc l
  isRsc (ThisRef _)             = True
  isRsc (SuperRef _)            = True
  isRsc (FuncExpr _ _ _ s)      = isRsc s
  isRsc (NewExpr _ e es)        = isRsc e && all isRsc es
  isRsc (Cast _ e)              = isRsc e
  isRsc e                       = errortext (text "Not Rsc Expression!" <+> pp e)

instance IsRsc AssignOp where
  isRsc OpAssign     = True
  isRsc OpAssignAdd  = True
  isRsc OpAssignSub  = True
  isRsc OpAssignMul  = True
  isRsc OpAssignDiv  = True
  isRsc OpAssignLShift   = True
  isRsc OpAssignSpRShift = True
  isRsc OpAssignZfRShift = True
  isRsc OpAssignBAnd = True
  isRsc OpAssignBXor = True
  isRsc OpAssignBOr = True
  isRsc x            = errortext (text "Not Rsc AssignOp!" <+> pp x)

instance IsRsc PrefixOp where
  isRsc PrefixLNot   = True
  isRsc PrefixMinus  = True
  isRsc PrefixPlus   = True
  isRsc PrefixTypeof = True
  isRsc PrefixBNot   = True
  isRsc e            = errortext (text "Not Rsc PrefixOp!" <+> pp e)

instance IsRsc (Statement a) where
  isRsc (EmptyStmt _)            = True                   --  skip
  isRsc (ExprStmt _ e)           = isRscExprStatement e  --  x = e
  isRsc (BlockStmt _ ss)         = isRsc ss              --  sequence
  isRsc (IfSingleStmt _ b s)     = isRsc b && isRsc s
  isRsc (IfStmt _ b s1 s2)       = isRsc b && isRsc s1 && isRsc s2
  isRsc (WhileStmt _ b s)        = isRsc b && isRsc s
  isRsc (ForStmt _ i t inc b)    = isRsc i && isRsc t && isRsc inc && isRsc b
  isRsc (ForInStmt _ init e s)   = isRsc init && isRsc e && isRsc s
  isRsc (VarDeclStmt _ ds)       = all isRsc ds
  isRsc (ReturnStmt _ e)         = isRsc e
  isRsc (FunctionStmt _ _ _ b)   = isRsc b
  isRsc (SwitchStmt _ e cs)      = isRsc e && not (null cs) && isRsc cs
  isRsc (ClassStmt _ _ bd)       = all isRsc bd
  isRsc (ThrowStmt _ e)          = isRsc e
  isRsc (InterfaceStmt _ _)          = True
  isRsc (ModuleStmt _ _ s)       = all isRsc s
  isRsc (EnumStmt _ _ _)         = True
  isRsc e                        = errortext (text "Not Rsc Statement:" $$ pp e)

instance IsRsc (ClassElt a) where
  isRsc (Constructor _ _ ss)        = all isRsc ss
  isRsc (MemberMethDecl _ _ _ _ ss) = all isRsc ss
  isRsc (MemberVarDecl _ _ _ eo)    = isRsc eo

instance IsRsc a => IsRsc (Maybe a) where
  isRsc (Just x) = isRsc x
  isRsc Nothing  = True

instance IsRsc [(Statement a)] where
  isRsc = all isRsc

instance IsRsc (ForInit a) where
  isRsc NoInit        = True
  isRsc (VarInit vds) = all isRsc vds
  isRsc (ExprInit e)  = isRsc e

instance IsRsc (ForInInit a) where
  isRsc (ForInVar _)  = True
  isRsc e             = errortext (text "Not Rsc ForInInit:" $$ pp e)


-- | Holds for `Expression` that is a valid side-effecting `Statement`

isRscExprStatement :: Expression a -> Bool
isRscExprStatement (UnaryAssignExpr _ _ lv) = isRsc lv
isRscExprStatement (AssignExpr _ o lv e)    = isRsc o && isRsc lv && isRsc e
isRscExprStatement (CallExpr _ e es)        = all isRsc (e:es)
isRscExprStatement (Cast _ e)               = isRscExprStatement e
isRscExprStatement e@(FuncExpr _ _ _ _ )    = errortext (text "Unannotated function expression" <+> pp e)
isRscExprStatement e                        = errortext (text "Not Rsc ExprStmtZ!" <+> pp e)

-- | Switch Statement

-- Is Rsc-js code if each clause ends with a break statement

instance IsRsc (CaseClause a) where
  isRsc (CaseClause _ e st) = isRsc e && holdsInit isRsc st' && endsWithBreak st'
    where st' = concatMap flattenStmt st
  isRsc (CaseDefault _  st) =             holdsInit isRsc st' && endsWithBreak st'
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

instance IsRsc [(CaseClause a)] where
  isRsc [] = False
  isRsc xs = all isRsc xs && holdsInit (not . defaultC) xs
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
checkBody stmts = all isRsc stmts
