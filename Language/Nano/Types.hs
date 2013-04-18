{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleInstances      #-}

module Language.Nano.Types (
  -- * Configuration Options
    Config (..)

  -- * Valid Nano Entitities
  , IsNano (..)

  -- * Type synonym for Nano Programs
  , Nano 

  -- * Some simple helpers
  , pAnd
  , pOr
  ) where

import           Data.Typeable                      (Typeable)
import           Data.Generics                      (Data)   
import           Language.ECMAScript3.Syntax 
import           Language.ECMAScript3.PrettyPrint   (pp)

import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Misc
import           Text.PrettyPrint.HughesPJ

---------------------------------------------------------------------------
-- | Command Line Configuration Options
---------------------------------------------------------------------------

data Config = Config { 
    files   :: [FilePath]     -- ^ source files to check
  , incdirs :: [FilePath]     -- ^ path to directory for including specs
  } deriving (Data, Typeable, Show, Eq)

---------------------------------------------------------------------
-- | Wrappers around `Language.ECMAScript3.Syntax` ------------------
---------------------------------------------------------------------

-- | `isNano` is a predicate that describes the **syntactic subset** 
--   of ECMAScript3 that comprises `Nano`.

class IsNano a where 
  isNano :: a -> Bool

instance IsNano InfixOp where
  isNano OpLT   = True -- ^ @<@
  isNano OpLEq  = True -- ^ @<=@
  isNano OpGT   = True -- ^ @>@
  isNano OpGEq  = True -- ^ @>=@
  isNano OpEq   = True -- ^ @==@
  isNano OpNEq  = True -- ^ @!=@
  
  isNano OpLAnd = True -- ^ @&&@
  isNano OpLOr  = True -- ^ @||@

  isNano OpSub  = True -- ^ @-@
  isNano OpAdd  = True -- ^ @+@
  isNano OpMul  = True -- ^ @*@
  isNano OpDiv  = True -- ^ @/@
  isNano OpMod  = True -- ^ @%@
  isNano _      = False

instance IsNano (Expression a) where 
  isNano (BoolLit _ _)         = True
  isNano (IntLit _ _)          = True
  isNano (VarRef _ _)          = True
  isNano (InfixExpr _ o e1 e2) = isNano o && isNano e1 && isNano e2
  isNano (PrefixExpr _ o e)    = isNano o && isNano e
  isNano _                     = False

instance IsNano AssignOp where
  isNano OpAssign = True
  isNano _        = False

instance IsNano PrefixOp where
  isNano PrefixLNot  = True
  isNano PrefixMinus = True 
  isNano _           = False

instance IsNano (Statement a) where
  isNano (EmptyStmt _)         = True                   -- ^ skip
  isNano (ExprStmt _ e)        = isNanoExprStatement e  -- ^ x = e
  isNano (BlockStmt _ ss)      = isNano ss              -- ^ sequence
  isNano (IfSingleStmt _ b s)  = isNano b && isNano s   
  isNano (IfStmt _ b s1 s2)    = isNano b && isNano s1 && isNano s2
  isNano (WhileStmt _ b s)     = isNano b && isNano s
  isNano _                     = False

instance IsNano [(Statement a)] where 
  isNano = all isNano 


-- | Holds for `Expression` that is a valid side-effecting `Statement` 

isNanoExprStatement :: Expression a -> Bool
isNanoExprStatement (AssignExpr _ o lv e) = True

---------------------------------------------------------------
-- | Top-level Nano Program -----------------------------------
---------------------------------------------------------------

{-@ type Nano = {v: [(Statement SourcePos)] | (isNano v)} @-}

type Nano = [Statement SourcePos] 

--------------------------------------------------------------------------------------
-- | Converting `ECMAScript3` values into `Fixpoint` values, 
--   i.e. *language* level entities into *logic* level entities.
--------------------------------------------------------------------------------------

instance F.Symbolic   (Id a) where
  symbol (Id _ x)   = F.symbol x 

instance F.Symbolic (LValue a) where
  symbol (LVar _ x) = F.symbol x
  symbol lv         = errortext $ text "Cannot convert to F.Symbol:" <+> pp lv

instance F.Expression (Id a) where
  expr = F.eVar

instance F.Expression (LValue a) where
  expr = F.eVar

instance F.Expression (Expression a) where
  expr (IntLit _ i)                 = F.expr i
  expr (VarRef _ x)                 = F.expr x
  expr (InfixExpr _ o e1 e2)        = F.EBin (bop o) (F.expr e1) (F.expr e2)
  expr (PrefixExpr _ PrefixMinus e) = F.EBin F.Minus (F.expr (0 :: Int)) (F.expr e)  
  expr e                            = errortext msg
    where msg                       = text "Cannot convert to F.Expression:" <+> pp e



instance F.Predicate  (Expression a) where 
  prop (BoolLit _ True)            = F.PTrue
  prop (BoolLit _ False)           = F.PFalse
  prop (PrefixExpr _ PrefixLNot e) = F.PNot (F.prop e)
  prop (InfixExpr _ o e1 e2)       = eProp o e1 e2

------------------------------------------------------------------
eProp :: InfixOp -> Expression a -> Expression a -> F.Pred
------------------------------------------------------------------

eProp OpLT  e1 e2  = F.PAtom F.Lt (F.expr e1) (F.expr e2) 
eProp OpLEq e1 e2  = F.PAtom F.Le (F.expr e1) (F.expr e2) 
eProp OpGT  e1 e2  = F.PAtom F.Gt (F.expr e1) (F.expr e2)  
eProp OpGEq e1 e2  = F.PAtom F.Ge (F.expr e1) (F.expr e2)  
eProp OpEq  e1 e2  = F.PAtom F.Eq (F.expr e1) (F.expr e2) 
eProp OpNEq e1 e2  = F.PAtom F.Ne (F.expr e1) (F.expr e2) 
 
eProp OpLAnd e1 e2 = pAnd (F.prop e1) (F.prop e2) 
eProp OpLOr e1 e2  = pOr  (F.prop e1) (F.prop e2)

------------------------------------------------------------------
bop       :: InfixOp -> F.Bop
------------------------------------------------------------------

bop OpSub = F.Minus 
bop OpAdd = F.Plus
bop OpMul = F.Times
bop OpDiv = F.Div
bop OpMod = F.Mod

---------------------------------------------------------------------
pAnd p q  = F.pAnd [p, q] 
pOr  p q  = F.pOr  [p, q]

