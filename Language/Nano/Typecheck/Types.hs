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
  , Bind (..)
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

  -- * Primitive Types
  , tInt
  , tBool
  , tVoid
  , tErr
  , tFunErr
  , tVar

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
import           Data.Hashable
import           Data.Maybe             (fromMaybe) --, isJust)
import           Data.Monoid            hiding ((<>))            
-- import qualified Data.List               as L
import qualified Data.HashMap.Strict     as M
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations
import           Language.ECMAScript3.PrettyPrint
import           Language.ECMAScript3.Parser        (SourceSpan (..))
import           Language.Nano.Types
import           Language.Nano.Errors
import           Language.Nano.Env

-- import           Language.Fixpoint.Names (propConName)
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Misc
import           Language.Fixpoint.PrettyPrint
import           Text.PrettyPrint.HughesPJ

import           Control.Applicative 
import           Control.Monad.Error ()

-- | Type Variables
data TVar = TV { tv_sym :: F.Symbol
               , tv_loc :: SourceSpan 
               }
            deriving (Show, Ord)

instance Eq TVar where 
  a == b = tv_sym a == tv_sym b

instance IsLocated TVar where 
  srcPos = tv_loc

instance Hashable TVar where 
  hashWithSalt i α = hashWithSalt i $ tv_sym α 

instance F.Symbolic TVar where
  symbol = tv_sym 

instance PP TVar where 
  pp     = pprint . F.symbol

instance F.Symbolic a => F.Symbolic (Located a) where 
  symbol = F.symbol . val

-- | Constructed Type Bodies
-- data TBody r 
--   = TD { td_con  :: !TCon
--        , td_args :: ![TVar]
--        , td_body :: !(RType r)
--        , td_pos  :: !SourceSpan
--        } deriving (Show, Functor)

-- | Type Constructors
data TCon 
  = TInt                   
  | TBool                
  | TVoid              
  | TDef  F.Symbol
  | TUn
    deriving (Eq, Ord, Show)

-- | (Raw) Refined Types 
data RType r  
  = TApp TCon [RType r]     r
  | TVar TVar               r 
  | TFun [Bind r] (RType r) r
  | TAll TVar (RType r)
    deriving (Eq, Ord, Show, Functor)


data Bind r
  = B { b_sym  :: F.Symbol
      , b_type :: !(RType r)
      } 
    deriving (Eq, Ord, Show, Functor)


-- | Standard Types
type Type    = RType ()

-- | Stripping out Refinements 
toType :: RType a -> Type
toType = fmap (const ())
  
-- | Adding in Refinements
ofType :: (F.Reftable r) => Type -> RType r
ofType = fmap (const F.top)

bkFun :: RType r -> Maybe ([TVar], [Bind r], RType r)
bkFun t = do let (αs, t') = bkAll t
             (xts, t'')  <- bkArr t'
             return        (αs, xts, t'')
         
bkArr (TFun xts t _) = Just (xts, t)
bkArr _              = Nothing

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

data Nano a t = Nano { code   :: !(Source a)        -- ^ Code to check
                     , specs  :: !(Env t)           -- ^ Imported Specifications
                     , defs   :: !(Env t)           -- ^ Signatures for Code
                     , consts :: !(Env t)           -- ^ Measure Signatures 
                     , quals  :: ![F.Qualifier]     -- ^ Qualifiers
                     } deriving (Functor)

type NanoBare    = Nano AnnBare Type 
type NanoSSA     = Nano AnnSSA  Type 
type NanoType    = Nano AnnType Type 

{-@ measure isFunctionStatement :: (Statement SourceSpan) -> Prop 
    isFunctionStatement (FunctionStmt {}) = true
    isFunctionStatement (_)               = false
  @-}

{-@ type FunctionStatement = {v:(Statement SourceSpan) | (isFunctionStatement v)} @-}
type FunctionStatement a = Statement a 

{-@ newtype Source a = Src [FunctionStatement a] @-}
newtype Source a = Src [FunctionStatement a]

instance Functor Source where 
  fmap f (Src zs) = Src (map (fmap f) zs)

instance PP t => PP (Nano a t) where
  pp pgm@(Nano {code = (Src s) }) 
    =   text "********************** CODE **********************"
    $+$ pp s
    $+$ text "********************** SPECS *********************"
    $+$ pp (specs pgm)
    $+$ text "********************** DEFS *********************"
    $+$ pp (defs  pgm)
    $+$ text "********************** CONSTS ********************"
    $+$ pp (consts pgm) 
    $+$ text "********************** QUALS *********************"
    $+$ F.toFix (quals  pgm) 
    $+$ text "**************************************************"

instance Monoid (Nano a t) where 
  mempty        = Nano (Src []) envEmpty envEmpty envEmpty [] 
  mappend p1 p2 = Nano ss e e' cs qs 
    where 
      ss        = Src $ s1 ++ s2
      Src s1    = code p1
      Src s2    = code p2
      e         = envFromList ((envToList $ specs p1) ++ (envToList $ specs p2))
      e'        = envFromList ((envToList $ defs p1)  ++ (envToList $ defs p2))
      cs        = envFromList $ (envToList $ consts p1) ++ (envToList $ consts p2)
      qs        = quals p1 ++ quals p2 

mapCode :: (a -> b) -> Nano a t -> Nano b t
mapCode f n = n { code = fmap f (code n) }

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
  pp (TFun xts t _) = ppArgs parens comma xts <+> text "=>" <+> pp t 
  pp t@(TAll _ _)   = text "forall" <+> ppArgs id space αs <> text "." <+> pp t' where (αs, t') = bkAll t
  pp (TApp c [] r)  = F.ppTy r $ ppTC c 
  pp (TApp c ts r)  = F.ppTy r $ parens (ppTC c <+> ppArgs id space ts)  


instance F.Reftable r => PP (Bind r) where 
  pp (B x t)        = pp x <> colon <> pp t 

ppArgs p sep          = p . intersperse sep . map pp
ppTC TInt             = text "int"
ppTC TBool            = text "boolean"
ppTC TVoid            = text "void"
ppTC TUn              = text "union:"
ppTC (TDef x)         = pprint x

-----------------------------------------------------------------------------
-- | Annotations ------------------------------------------------------------
-----------------------------------------------------------------------------

-- | Annotations: Extra-code decorations needed for Refinement Type Checking
--   Ideally, we'd have "room" for these inside the @Statement@ and
--   @Expression@ type, but are tucking them in using the @a@ parameter.

data Fact 
  = PhiVar  !(Id SourceSpan) 
  | TypInst ![Type]
    deriving (Eq, Ord, Show)

data Annot b a = Ann { ann :: a, ann_fact :: [b] } deriving (Show)
type AnnBare   = Annot Fact SourceSpan -- NO facts
type AnnSSA    = Annot Fact SourceSpan -- Only Phi       facts
type AnnType   = Annot Fact SourceSpan -- Only Phi + Typ facts
type AnnInfo   = M.HashMap SourceSpan [Fact] 


instance HasAnnotation (Annot b) where 
  getAnnotation = ann 


instance IsLocated (Annot a SourceSpan) where 
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

tInt, tBool, tVoid, tErr :: (F.Reftable r) => RType r
tInt   = TApp TInt  [] F.top 
tBool  = TApp TBool [] F.top
tVoid  = TApp TVoid [] F.top
tErr   = tVoid
tFunErr = ([],[],tErr)

-- tProp :: (F.Reftable r) => RType r
-- tProp  = TApp tcProp [] F.top 
-- tcProp = TDef $ F.S propConName 

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
infixOpId o     = errorstar $ "Cannot handle: infixOpId " ++ ppshow o

-----------------------------------------------------------------------
prefixOpTy :: PrefixOp -> Env t -> t 
-----------------------------------------------------------------------
prefixOpTy o g = fromMaybe err $ envFindTy (prefixOpId o) g
  where 
    err       = convertError "prefixOpTy" o

prefixOpId PrefixMinus = builtinId "PrefixMinus"
prefixOpId PrefixLNot  = builtinId "PrefixLNot"
prefixOpId o           = errorstar $ "Cannot handle: prefixOpId " ++ ppshow o

builtinId       = mkId . ("builtin_" ++)


