{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleInstances      #-}

module Language.Nano.Types (
  -- * Configuration Options
    Config (..)

  -- * Some Operators on Pred
  , pAnd
  , pOr

  -- * Nano Definition
  , IsNano (..)

  -- * Located Values
  , Located (..) 
  , IsLocated (..)

  -- * Accessing Spec Annotations
  , getSpec
  , getRequires
  , getEnsures
  , getAssume
  , getAssert
  , getInvariant
  , isSpecification
  , returnSymbol
  , returnId

  -- * Error message
  , convertError

  -- * Deconstructing Id
  , idName
  , idLoc 
  
  ) where

import           Control.Applicative          ((<$>))
-- import qualified Data.HashMap.Strict as M
import           Data.Hashable
import           Data.Typeable                      (Typeable)
import           Data.Generics                      (Data)   
import           Data.Monoid                        (Monoid (..))
import           Data.Maybe                         (catMaybes)
import           Language.ECMAScript3.Syntax 
import           Language.ECMAScript3.PrettyPrint   (PP (..))
-- import           Language.ECMAScript3.Parser        (parseJavaScriptFromFile)

import qualified Language.Fixpoint.Types as F

import           Language.Fixpoint.PrettyPrint
import           Language.Fixpoint.Misc
import           Text.PrettyPrint.HughesPJ
import           Text.Parsec                        

---------------------------------------------------------------------
-- | Command Line Configuration Options
---------------------------------------------------------------------

data Config 
  = Esc    { files   :: [FilePath]     -- ^ source files to check
           , incdirs :: [FilePath]     -- ^ path to directory for include specs
           } 
  | Liquid { files   :: [FilePath]     -- ^ source files to check
           , incdirs :: [FilePath]     -- ^ path to directory for include specs
           }
  deriving (Data, Typeable, Show, Eq)


data Located a 
  = Loc { loc :: !SourcePos
        , val :: a
        }
 
instance Functor Located where 
  fmap f (Loc l x) = Loc l (f x)

-- | `IsLocated` is a predicate that describes values for which we have 
--    a SourcePos

class IsLocated a where 
  srcPos :: a -> SourcePos 

instance IsLocated (Id SourcePos) where 
  srcPos (Id l _) = l

instance IsLocated (Located a) where 
  srcPos = loc

instance Eq a => Eq (Located a) where 
  x == y = val x == val y

---------------------------------------------------------------------
-- | Wrappers around `Language.ECMAScript3.Syntax` ------------------
---------------------------------------------------------------------

-- | `IsNano` is a predicate that describes the **syntactic subset** 
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

instance IsNano (LValue a) where 
  isNano (LVar _ _) = True
  isNano _          = False

instance IsNano (VarDecl a) where
  isNano (VarDecl _ _ (Just e)) = isNano e
  isNano (VarDecl _ _ Nothing)  = True

instance IsNano (Expression a) where 
  isNano (BoolLit _ _)         = True
  isNano (IntLit _ _)          = True
  isNano (VarRef _ _)          = True
  isNano (InfixExpr _ o e1 e2) = isNano o && isNano e1 && isNano e2
  isNano (PrefixExpr _ o e)    = isNano o && isNano e
  isNano (CallExpr _ e es)     = all isNano (e:es)
  isNano e                     = errortext (text "Not Nano Expression!" <+> pp e) 
  -- isNano _                     = False

instance IsNano AssignOp where
  isNano OpAssign = True
  isNano x        = errortext (text "Not Nano AssignOp!" <+> pp x) 
  -- isNano _        = False

instance IsNano PrefixOp where
  isNano PrefixLNot  = True
  isNano PrefixMinus = True 
  isNano e           = errortext (text "Not Nano PrefixOp!" <+> pp e) 
  -- isNano _           = False

instance IsNano (Statement a) where
  isNano (EmptyStmt _)         = True                   -- ^ skip
  isNano (ExprStmt _ e)        = isNanoExprStatement e  -- ^ x = e
  isNano (BlockStmt _ ss)      = isNano ss              -- ^ sequence
  isNano (IfSingleStmt _ b s)  = isNano b && isNano s   
  isNano (IfStmt _ b s1 s2)    = isNano b && isNano s1 && isNano s2
  isNano (WhileStmt _ b s)     = isNano b && isNano s
  isNano (VarDeclStmt _ ds)    = all isNano ds 
  isNano (ReturnStmt _ e)      = isNano e 
  isNano e                     = errortext (text "Not Nano Statement!" <+> pp e) 
  -- isNano _                     = False

instance IsNano a => IsNano (Maybe a) where 
  isNano (Just x) = isNano x
  isNano Nothing  = True

instance IsNano [(Statement a)] where 
  isNano = all isNano 


-- | Holds for `Expression` that is a valid side-effecting `Statement` 

isNanoExprStatement :: Expression a -> Bool
isNanoExprStatement (AssignExpr _ o lv e) = isNano o && isNano lv && isNano e 
isNanoExprStatement (CallExpr _ e es)     = all isNano (e:es)
isNanoExprStatement e                     = errortext (text "Not Nano ExprStmt!" <+> pp e) 
-- isNanoExprStatement _                     = False

-----------------------------------------------------------------------------------
-- | Helpers for extracting specifications from @ECMAScript3@ @Statement@ 
-----------------------------------------------------------------------------------

-- Ideally, a la JML, we'd modify the parser to take in annotations for 
-- 
--   * assert(p)
--   * assume(p)
--   * invariant(p) 
--
-- For now, we hack them with function calls.

returnName :: String
returnName = "$result"

returnId   :: a -> Id a
returnId x = Id x returnName 

returnSymbol :: F.Symbol
returnSymbol = F.stringSymbol returnName

isSpecification :: Statement a -> Bool
isSpecification s  = not $ null $ catMaybes $ ($ s) <$> specs 
  where 
    specs          = [getAssert, getAssume, getInv, getRequires, getEnsures]

getInvariant :: Statement a -> F.Pred 

getInvariant = getSpec getInv . flattenStmt

flattenStmt (BlockStmt _ ss) = concatMap flattenStmt ss
flattenStmt s                = [s]


getAssume    :: Statement a -> Maybe F.Pred 
getAssume    = getStatementPred "assume"

getAssert    :: Statement a -> Maybe F.Pred 
getAssert    = getStatementPred "assert"

getRequires  = getStatementPred "requires"
getEnsures   = getStatementPred "ensures"
getInv       = getStatementPred "invariant"

getStatementPred :: String -> Statement a -> Maybe F.Pred 
getStatementPred name (ExprStmt _ (CallExpr _ (VarRef _ (Id _ f)) [p]))
  | name == f 
  = Just $ F.prop p
getStatementPred _ _ 
  = Nothing 

getSpec   :: (Statement a -> Maybe F.Pred) -> [Statement a] -> F.Pred 
getSpec g = mconcat . catMaybes . map g

------------------------------------------------------------------
-- | Converting `ECMAScript3` values into `Fixpoint` values, 
--   i.e. *language* level entities into *logic* level entities.
------------------------------------------------------------------

instance F.Symbolic   (Id a) where
  symbol (Id _ x)   = F.symbol x 

instance F.Symbolic (LValue a) where
  symbol (LVar _ x) = F.symbol x
  symbol lv         = convertError "F.Symbol" lv

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

eProp (InfixExpr _ OpLT   e1 e2) = F.PAtom F.Lt (F.expr e1) (F.expr e2) 
eProp (InfixExpr _ OpLEq  e1 e2) = F.PAtom F.Le (F.expr e1) (F.expr e2) 
eProp (InfixExpr _ OpGT   e1 e2) = F.PAtom F.Gt (F.expr e1) (F.expr e2)  
eProp (InfixExpr _ OpGEq  e1 e2) = F.PAtom F.Ge (F.expr e1) (F.expr e2)  
eProp (InfixExpr _ OpEq   e1 e2) = F.PAtom F.Eq (F.expr e1) (F.expr e2) 
eProp (InfixExpr _ OpNEq  e1 e2) = F.PAtom F.Ne (F.expr e1) (F.expr e2) 
eProp (InfixExpr _ OpLAnd e1 e2) = pAnd (F.prop e1) (F.prop e2) 
eProp (InfixExpr _ OpLOr  e1 e2) = pOr  (F.prop e1) (F.prop e2)
eProp e                          = convertError "InfixExpr -> F.Prop" e

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
------------------------------------------------------------------
-- SourcePos Instances -------------------------------------------
------------------------------------------------------------------

instance Hashable SourcePos where 
  hashWithSalt i = hashWithSalt i . sourcePosElts

instance Hashable a => Hashable (Id a) where 
  hashWithSalt i x = hashWithSalt i (idLoc x, idName x)

idName (Id _ x) = x
idLoc  (Id l _) = l

instance PP SourcePos where 
  pp = ppSourcePos 
    
instance F.Fixpoint SourcePos where
  toFix = pp 

instance (Ord a, F.Fixpoint a) => PP (F.FixResult a) where
  pp = F.resultDoc
 
sourcePosElts s = (src, line, col)
  where 
    src         = sourceName   s 
    line        = sourceLine   s
    col         = sourceColumn s 

ppSourcePos src = parens 
                $ text ("file: " ++ f) <> comma <+> int l <> comma <+> int c
  where 
    (f,l,c)     = sourcePosElts src 

instance PP F.Pred where 
  pp = pprint

instance PP (Id a) where
  pp (Id _ x) = text x

instance PP a => PP (Located a) where
  pp x = pp (val x) <+> text "at:" <+> pp (loc x)
--------------------------------------------------------------------------------


