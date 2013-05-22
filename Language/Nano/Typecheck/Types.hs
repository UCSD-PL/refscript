-- | Global type definitions for Refinement Type Checker

{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}

module Language.Nano.Typecheck.Types (

  -- * Programs
    Nano (..)
  , NanoBare
  , NanoSSA
  , NanoType
  , Source (..)
  , FunctionStatement
  , mapCode
  -- , sourceNano
  -- , sigsNano

  -- * (Refinement) Types
  , RType (..)
  , toType
  , ofType
  , strengthen 

  -- * Deconstructing Types
  , bkFun
  , bkAll

  -- * Regular Types
  , Type
  , TVar (..)
  , TCon (..)
  , TBody (..)

  -- * Primitive Types
  , tInt
  , tBool
  , tVoid
  , tErr
  , tVar
  , tProp
  , tcProp

  -- * Operator Types
  , infixOpTy
  , prefixOpTy 
  
  -- * Annotations
  , Annot (..)
  , Fact (..)
  , AnnBare
  , AnnSSA
  , AnnType
  , AnnInfo
  ) where 

import           Text.Printf
import           Data.Generics.Aliases
import           Data.Generics.Schemes
import           Data.Hashable
import           Data.Maybe             (fromMaybe, isJust)
import           Data.Monoid            hiding ((<>))            
import qualified Data.List               as L
import qualified Data.HashMap.Strict     as M
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations
import           Language.ECMAScript3.PrettyPrint

import           Language.Nano.Types
import           Language.Nano.Errors
import           Language.Nano.Env

import           Language.Fixpoint.Names (propConName)
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Misc
import           Language.Fixpoint.PrettyPrint
import           Text.PrettyPrint.HughesPJ

import           Control.Applicative 
import           Control.Monad.Error ()

-- | Type Variables
newtype TVar = TV F.Symbol deriving (Eq, Show, Ord)

instance Hashable TVar where 
  hashWithSalt i (TV a) = hashWithSalt i a

instance PP TVar where 
  pp (TV x) = pprint x

instance F.Symbolic TVar where
  symbol (TV α) = α  

instance F.Symbolic a => F.Symbolic (Located a) where 
  symbol = F.symbol . val

-- | Constructed Type Bodies
data TBody r 
  = TD { td_con  :: !TCon
       , td_args :: ![TVar]
       , td_body :: !(RType r)
       , td_pos  :: !SourcePos
       } deriving (Show, Functor)

-- | Type Constructors
data TCon 
  = TInt                   
  | TBool                
  | TVoid              
  | TDef  F.Symbol
    deriving (Eq, Ord, Show)

-- | (Raw) Refined Types 
data RType r  
  = TApp TCon [RType r] r
  | TVar TVar r 
  | TFun [RType r] (RType r)
  | TAll TVar (RType r)
    deriving (Eq, Ord, Show, Functor)

-- | Standard Types
type Type    = RType ()

-- | Stripping out Refinements 
toType :: RType a -> Type
toType = fmap (const ())
  
-- | Adding in Refinements
ofType :: (F.Reftable r) => Type -> RType r
ofType = fmap (const F.top)

bkFun :: RType a -> Maybe ([TVar], [RType a], RType a)
bkFun t = do let (αs, t') = bkAll t
             (xts, t'')  <- bkArr t'
             return        (αs, xts, t'')
         
bkArr (TFun xts t) = Just (xts, t)
bkArr _            = Nothing

bkAll                :: RType a -> ([TVar], RType a)
bkAll t              = go [] t
  where 
    go αs (TAll α t) = go (α : αs) t
    go αs t          = (reverse αs, t)

---------------------------------------------------------------------------------
strengthen                   :: F.Reftable r => RType r -> r -> RType r
---------------------------------------------------------------------------------
strengthen (TApp c ts r) r'  = TApp c ts $ r' `F.meet` r 
strengthen (TVar α r)    r'  = TVar α    $ r' `F.meet` r 
strengthen t _               = t                         

-- NOTE: r' is the OLD refinement. 
--       We want to preserve its VV binder as it "escapes", 
--       e.g. function types. Sigh. Should have used a separate function binder.

---------------------------------------------------------------------------------
-- | Nano Program = Code + Types for all function binders
---------------------------------------------------------------------------------

data Nano a t = Nano { code   :: !(Source a) 
                     , env    :: !(Env t)
                     , consts :: !(Env t) 
                     , quals  :: ![F.Qualifier] 
                     } deriving (Functor)

type NanoBare    = Nano AnnBare Type 
type NanoSSA     = Nano AnnSSA  Type 
type NanoType    = Nano AnnType Type 

-- sourceNano z     = Nano z envEmpty envEmpty []
-- sigsNano xts     = Nano (Src []) (envFromList xts) envEmpty []

{-@ measure isFunctionStatement :: (Statement SourcePos) -> Prop 
    isFunctionStatement (FunctionStmt {}) = true
    isFunctionStatement (_)               = false
  @-}

{-@ type FunctionStatement = {v:(Statement SourcePos) | (isFunctionStatement v)} @-}
type FunctionStatement a = Statement a 

{-@ newtype Source a = Src [FunctionStatement a] @-}
newtype Source a = Src [FunctionStatement a]

instance Functor Source where 
  fmap f (Src zs) = Src (map (fmap f) zs)

instance PP t => PP (Nano a t) where
  pp pgm@(Nano {code = (Src s) }) 
    =   text "********************** CODE **********************"
    $+$ pp s
    $+$ text "********************** ENV ***********************"
    $+$ pp (env    pgm)
    $+$ text "********************** CONSTS ********************"
    $+$ pp (consts pgm) 
    $+$ text "********************** QUALS *********************"
    $+$ F.toFix (quals  pgm) 
    $+$ text "**************************************************"

instance Monoid (Nano a t) where 
  mempty        = Nano (Src []) envEmpty envEmpty []
  mappend p1 p2 = Nano ss e cs qs 
    where 
      ss        = Src $ s1 ++ s2
      Src s1    = code p1
      Src s2    = code p2
      e         = envFromList ((envToList $ env p1) ++ (envToList $ env p2))
      cs        = envFromList $ (envToList $ consts p1) ++ (envToList $ consts p2)
      qs        = quals p1 ++ quals p2 

mapCode :: (a -> b) -> Nano a t -> Nano b t
mapCode f n = n { code = fmap f (code n) }
-- SYB examples at: http://web.archive.org/web/20080622204226/http://www.cs.vu.nl/boilerplate/#suite
-- getFunctionIds :: [Statement SourcePos] -> [Id SourcePos]
-- getFunctionIds stmts = everything (++) ([] `mkQ` fromFunction) stmts
--   where 
--     fromFunction (FunctionStmt _ x _ _) = [x] 
--     fromFunction _                      = []

---------------------------------------------------------------------------
-- | Pretty Printer Instances ---------------------------------------------
---------------------------------------------------------------------------

instance PP () where 
  pp _ = text ""

instance PP a => PP [a] where 
  pp = ppArgs brackets comma 

instance PP a => PP (Maybe a) where 
  pp = maybe (text "Nothing") pp 

instance F.Reftable r => PP (RType r) where
  pp (TVar α r)     = F.ppTy r $ pp α 
  pp (TFun ts t)    = ppArgs parens comma ts <+> text "=>" <+> pp t 
  pp t@(TAll _ _)   = text "forall" <+> ppArgs id space αs <> text "." <+> pp t' where (αs, t') = bkAll t
  pp (TApp c [] r)  = F.ppTy r $ ppTC c 
  pp (TApp c ts r)  = F.ppTy r $ parens (ppTC c <+> ppArgs id space ts)  

ppArgs p sep          = p . intersperse sep . map pp

ppTC TInt             = text "int"
ppTC TBool            = text "boolean"
ppTC TVoid            = text "void"
ppTC (TDef x)         = pprint x

-----------------------------------------------------------------------------
-- | Annotations ------------------------------------------------------------
-----------------------------------------------------------------------------

-- | Annotations: Extra-code decorations needed for Refinement Type Checking
--   Ideally, we'd have "room" for these inside the @Statement@ and
--   @Expression@ type, but are tucking them in using the @a@ parameter.

data Fact 
  = PhiVar  !(Id SourcePos) 
  | TypInst ![Type]
    deriving (Eq, Ord, Show)

data Annot b a = Ann { ann :: a, ann_fact :: [b] } deriving (Show)
type AnnBare   = Annot Fact SourcePos -- NO facts
type AnnSSA    = Annot Fact SourcePos -- Only Phi       facts
type AnnType   = Annot Fact SourcePos -- Only Phi + Typ facts
type AnnInfo   = M.HashMap SourcePos [Fact] 


instance HasAnnotation (Annot b) where 
  getAnnotation = ann 


instance IsLocated (Annot a SourcePos) where 
  srcPos = ann

instance PP Fact where
  pp (PhiVar x)   = text "phi"  <+> pp x
  pp (TypInst ts) = text "inst" <+> pp ts 

instance PP AnnInfo where
  pp             = vcat . (ppB <$>) . M.toList 
    where 
      ppB (x, t) = pp x <+> dcolon <+> pp t

instance (PP a, PP b) => PP (Annot b a) where
  pp (Ann x ys) = text "Annot: " <+> pp x <+> pp ys

-----------------------------------------------------------------------
-- | Primitive / Base Types -------------------------------------------
-----------------------------------------------------------------------

tVar   :: (F.Reftable r) => TVar -> RType r
tVar   = (`TVar` F.top) 

tInt, tBool, tVoid, tErr, tProp :: (F.Reftable r) => RType r
tInt   = TApp TInt  [] F.top 
tBool  = TApp TBool [] F.top
tVoid  = TApp TVoid [] F.top
tErr   = tVoid

tProp  = TApp tcProp [] F.top 
tcProp = TDef $ F.S propConName 

-----------------------------------------------------------------------
-- | Operator Types ---------------------------------------------------
-----------------------------------------------------------------------


-----------------------------------------------------------------------
infixOpTy :: InfixOp -> Env t -> t 
-----------------------------------------------------------------------
infixOpTy o g = fromMaybe err $ envFindTy ox g
  where 
    err       = errorstar $ printf "Cannot find infixOpTy %s" (ppshow ox) -- (ppshow g)
    ox        = infixOpId o

infixOpId OpLT  = builtinId "OpLT"         
infixOpId OpLEq = builtinId "OpLEq"      
infixOpId OpGT  = builtinId "OpGT"       
infixOpId OpGEq = builtinId "OpGEq"      
infixOpId OpEq  = builtinId "OpEq"       
infixOpId OpNEq = builtinId "OpNEq"      
infixOpId OpLAnd= builtinId "OpLAnd"     
infixOpId OpLOr = builtinId "OpLOr"      
infixOpId OpSub = builtinId "OpSub"      
infixOpId OpAdd = builtinId "OpAdd"      
infixOpId OpMul = builtinId "OpMul"      
infixOpId OpDiv = builtinId "OpDiv"      
infixOpId OpMod = builtinId "OpMod"       
infixOpId o     = convertError "infixOpId" o

-----------------------------------------------------------------------
prefixOpTy :: PrefixOp -> Env t -> t 
-----------------------------------------------------------------------
prefixOpTy o g = fromMaybe err $ envFindTy (prefixOpId o) g
  where 
    err       = convertError "prefixOpTy" o

prefixOpId PrefixMinus = builtinId "PrefixMinus"
prefixOpId PrefixLNot  = builtinId "PrefixLNot"

builtinId       = mkId . ("builtin_" ++)


-- -----------------------------------------------------------------------
-- infixOpTy              :: InfixOp -> Type
-- -----------------------------------------------------------------------
-- infixOpTy OpLT         = TAll tvA $ TFun [tA, tA] tBool  
-- infixOpTy OpLEq        = TAll tvA $ TFun [tA, tA] tBool
-- infixOpTy OpGT         = TAll tvA $ TFun [tA, tA] tBool
-- infixOpTy OpGEq        = TAll tvA $ TFun [tA, tA] tBool
-- infixOpTy OpEq         = TAll tvA $ TFun [tA, tA] tBool
-- infixOpTy OpNEq        = TAll tvA $ TFun [tA, tA] tBool
-- 
-- infixOpTy OpLAnd       = TFun [tBool, tBool] tBool 
-- infixOpTy OpLOr        = TFun [tBool, tBool] tBool
-- 
-- infixOpTy OpSub        = TFun [tInt, tInt]   tInt 
-- infixOpTy OpAdd        = TFun [tInt, tInt]   tInt 
-- infixOpTy OpMul        = TFun [tInt, tInt]   tInt 
-- infixOpTy OpDiv        = TFun [tInt, tInt]   tInt 
-- infixOpTy OpMod        = TFun [tInt, tInt]   tInt  
-- infixOpTy o            = convertError "infixOpTy" o
-- 
-- tvA                    = TV (F.symbol "Z")
-- tA                     = tVar tvA
-- 
-- -----------------------------------------------------------------------
-- prefixOpTy             :: PrefixOp -> Type
-- -----------------------------------------------------------------------
-- prefixOpTy PrefixMinus = TFun [tInt] tInt
-- prefixOpTy PrefixLNot  = TFun [tBool] tBool
-- prefixOpTy o           = convertError "prefixOpTy" o
