{-# LANGUAGE DeriveDataTypeable     #-}
{- LANGUAGE MultiParamTypeClasses  #-}
{- LANGUAGE TypeSynonymInstances   #-}
{- LANGUAGE FlexibleInstances      #-}
{- LANGUAGE FlexibleContexts       #-} 
{- LANGUAGE OverlappingInstances   #-}

module Language.Nano.Types (
  -- * Configuration Options
    Config (..)

  -- * Valid Nano Entitities
  , IsNano (..)

  -- * Type synonym for Nano Programs
  , Nano 
  ) where

import           Data.Typeable                (Typeable)
import           Data.Generics                (Data)   
import           Language.ECMAScript3.Syntax 
import qualified Language.Fixpoint.Types as F

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
  isNano OpMul  = True -- ^ @*@
  isNano OpDiv  = True -- ^ @/@
  isNano OpMod  = True -- ^ @%@
  isNano OpSub  = True -- ^ @-@
  isNano OpAdd  = True -- ^ @+@
  isNano _      = False

instance IsNano (Expression a) where 
  isNano (BoolLit _ _)         = True
  isNano (IntLit _ _)          = True
  isNano (VarRef _ _)          = True
  isNano (InfixExpr _ o e1 e2) = isNano o && isNano e1 && isNano e2
  isNano _                     = False

instance IsNano AssignOp where
  isNano OpAssign = True
  isNano _        = False

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

instance F.Symbolic (LVar a) where
  symbol (LVar _ x) = F.symbol x

instance F.Expression (Id a) where
  expr = F.eVar

instance F.Expression (LVar a) where
  expr = F.eVar

instance F.Expression (Expression a) where
  expr (IntLit _ i) = expr i
  expr (VarRef _ x) = expr x
  expr (InfixExpr _ o e1 e2) = infixExpr o (F.expr e1) (F.expr e2)


infixExpr 
instance F.Predicate  (Expression a) where 
  prop e   = undefined

