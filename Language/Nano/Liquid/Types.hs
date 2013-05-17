-- | Global type definitions for Refinement Type Checker

{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}

module Language.Nano.Liquid.Types (

  -- * Programs
    Nano (..)
  , NanoBare
  , NanoSSA
  , NanoType
  , Spec   (..)
  , Source (..)
  , FunctionStatement
  , mkNano

  -- * Environments 
  , Env    
  , envFromList 
  , envToList
  , envAdd 
  , envAdds 
  , envFindTy
  , envAddReturn
  , envFindReturn
  , envMem
  , envMap
  , envLefts
  , envRights
  , envIntersectWith
  , envEmpty

  -- * Accessors
  , code
  , env

  -- * Refinement Types
  , RType (..)
  , RefType
  , toType
  
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

import           Data.Maybe             (isJust)
import           Data.Hashable
import           Data.Monoid            hiding ((<>))            
import           Data.Ord               (comparing) 
import qualified Data.List               as L
import           Data.Generics.Aliases
import           Data.Generics.Schemes
import qualified Data.HashMap.Strict     as M
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.Syntax.Annotations
import           Language.ECMAScript3.PrettyPrint
import           Language.Nano.Types
import           Language.Nano.Errors
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Misc
import           Language.Fixpoint.PrettyPrint
import           Text.PrettyPrint.HughesPJ
import           Control.Applicative 
import           Control.Monad
import           Control.Monad.Error

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
    deriving (Eq, Show, Functor)

-- | Standard Types
type Type    = RType ()

-- | (Real) Refined Types
type RefType = RType F.Reft 

-- | Stripping out Refinements 
toType :: RType a -> Type
toType = fmap (const ())


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
-- | Nano Program = Code + Types for all function binders
---------------------------------------------------------------------------------

data Nano a r = Nano { code :: !(Source a) 
                     , env  :: !(Env (RType r))
                     }

type NanoBare = Nano AnnBare   ()
type NanoSSA  = Nano AnnSSA    ()
type NanoType = Nano AnnType   ()


-- | Type Specification for function binders
data Spec = Spec { sigs :: [(Id SourcePos, Type)] }

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

instance PP (RType r) => PP (Nano a r) where
  pp (Nano (Src s) env) 
    =   text "********************** CODE **********************"
    $+$ pp s
    $+$ text "********************** SPEC **********************"
    $+$ pp env

instance PP a => PP (Maybe a) where 
  pp = maybe (text "Nothing") pp 

instance PP F.Symbol where 
  pp = pprint

instance PP t => PP (Env t) where
  pp = vcat . (ppBind <$>) . F.toListSEnv . fmap val 

ppBind (x, t) = pprint x <+> dcolon <+> pp t

instance Monoid Spec where 
  mempty      = Spec []
  mappend x y = Spec $ sigs x ++ sigs y

--------------------------------------------------------------------------
-- | Environments
--------------------------------------------------------------------------

type Env t  = F.SEnv (Located t) 

envEmpty        = F.emptySEnv
envMap    f     = F.mapSEnv (fmap f) 
envFilter f     = F.filterSEnv (f . val) 
envMem i γ      = isJust $ envFind i γ
envFind    i γ  = F.lookupSEnv (F.symbol i) γ
envFindLoc i γ  = fmap loc $ envFind i γ 
envFindTy  i γ  = fmap val $ envFind i γ
envAdd   i t γ  = F.insertSEnv (F.symbol i) (Loc (srcPos i) t) γ
envAdds  xts γ  = L.foldl' (\γ (x,t) -> envAdd x t γ) γ xts
envFromList xts = F.fromListSEnv [(F.symbol x, (Loc (srcPos x) t)) | (x, t) <- xts]
envToList  γ    = [ (Id l (F.symbolString x), t) | (x, Loc l t) <- F.toListSEnv γ]
envAddReturn f  = envAdd (returnId (srcPos f))
envFindReturn   = maybe msg val . F.lookupSEnv returnSymbol  
  where 
    msg = errorstar "bad call to envFindReturn"

envIntersectWith :: (a -> b -> c) -> Env a -> Env b -> Env c
envIntersectWith f = F.intersectWithSEnv (\v1 v2 -> Loc (loc v1) (f (val v1) (val v2)))


envRights :: Env (Either a b) -> Env b
envRights = envMap (\(Right z) -> z) . envFilter isRight

envLefts :: Env (Either a b) -> Env a
envLefts = envMap (\(Left z) -> z) . envFilter isLeft

isRight (Right _) = True
isRight (_)       = False
isLeft            = not . isRight


--------------------------------------------------------------------------
-- | Combining Source and Spec into Nano ---------------------------------
--------------------------------------------------------------------------

mkNano  :: [Statement SourcePos] -> Spec -> Either Doc NanoBare 
mkNano stmts spec 
  = do src   <- Src <$> mapM checkFun stmts
       env   <- mkEnv $ sigs spec
       return $ Nano (fmap (\src -> Ann src []) src) env

-- padSrc :: [Statement SourcePos] -> [Statement AnnBare]

-- | Trivial Syntax Checking 

checkFun :: Statement SourcePos -> Either Doc (Statement SourcePos) 
checkFun f@(FunctionStmt _ _ _ b) 
  | checkBody b = Right f
checkFun s      = Left (text "Invalid top-level statement" <+> pp s) 

checkBody :: [Statement SourcePos] -> Bool
checkBody stmts = all isNano stmts && null (getWhiles stmts) 
    
getWhiles :: [Statement SourcePos] -> [Statement SourcePos]
getWhiles stmts = everything (++) ([] `mkQ` fromWhile) stmts
  where 
    fromWhile s@(WhileStmt {}) = [s]
    fromWhile _                = [] 

getFunctionIds :: [Statement SourcePos] -> [Id SourcePos]
getFunctionIds stmts = everything (++) ([] `mkQ` fromFunction) stmts
  where 
    fromFunction (FunctionStmt _ x _ _) = [x] 
    fromFunction _                      = []

-- SYB examples at: http://web.archive.org/web/20080622204226/http://www.cs.vu.nl/boilerplate/#suite

-----------------------------------------------------------------------
-- | Building Type Environments ---------------------------------------
-----------------------------------------------------------------------

mkEnv :: [(Id SourcePos, Type)] -> Either Doc (Env Type)
mkEnv = foldM step F.emptySEnv 
  where 
    step γ (i, t) = case envFindLoc i γ of
                      Nothing -> Right $ envAdd i t γ 
                      Just l' -> Left  $ text $ errorDuplicate i (srcPos i) l'


-- mkEnv         :: [Id SourcePos] -> [(Id SourcePos, Type)] -> Either Doc (F.SEnv Type) 
-- mkEnv is its 
--   | nI < nT   = mkE <$> zipWithM joinName is' its'
--   | otherwise = Left (text "Missing Type Specifications!")  
--   where
--     is'       = orderIds idName (comparing idLoc) is
--     its'      = orderIds (idName . fst) (comparing (idLoc . fst)) its 
--     (fts,sts) = splitAt nI its 
--     mkE       = F.fromListSEnv . fmap (mapFst F.symbol)
--     nI        = length is
--     nT        = length its 
-- 
-- joinName (name, i) (name', (_,t)) 
--   | name == name' = Right (i, t)
--   | otherwise     = Left (text "Missing Type Specification: " <+> pp i) 
-- 
-- -- orderIds :: (Ord t, Hashable t) =>(a -> t) -> (a -> a -> Ordering) -> [a] -> [(t, a)]
-- orderIds fn fl = concatMap (\(x,ys) -> (x,) <$> ys) 
--                . hashMapToAscList 
--                . (L.sortBy fl <$>) 
--                . groupMap fn 

-----------------------------------------------------------------------
-- | Primitive / Base Types -------------------------------------------
-----------------------------------------------------------------------

tVar   = (`TVar` ()) 
tInt   = TApp TInt  [] ()
tBool  = TApp TBool [] ()
tVoid  = TApp TVoid [] ()
tErr   = tVoid

-----------------------------------------------------------------------
-- | Operator Types Types ---------------------------------------------
-----------------------------------------------------------------------

-----------------------------------------------------------------------
infixOpTy              :: InfixOp -> Type
-----------------------------------------------------------------------

tvA                    = TV (F.symbol "Z")
tA                     = tVar tvA

infixOpTy OpLT         = TAll tvA $ TFun [tA, tA] tBool  
infixOpTy OpLEq        = TAll tvA $ TFun [tA, tA] tBool
infixOpTy OpGT         = TAll tvA $ TFun [tA, tA] tBool
infixOpTy OpGEq        = TAll tvA $ TFun [tA, tA] tBool
infixOpTy OpEq         = TAll tvA $ TFun [tA, tA] tBool
infixOpTy OpNEq        = TAll tvA $ TFun [tA, tA] tBool

infixOpTy OpLAnd       = TFun [tBool, tBool] tBool 
infixOpTy OpLOr        = TFun [tBool, tBool] tBool

infixOpTy OpSub        = TFun [tInt, tInt]   tInt 
infixOpTy OpAdd        = TFun [tInt, tInt]   tInt 
infixOpTy OpMul        = TFun [tInt, tInt]   tInt 
infixOpTy OpDiv        = TFun [tInt, tInt]   tInt 
infixOpTy OpMod        = TFun [tInt, tInt]   tInt  

infixOpTy o            = convertError "infixOpTy" o

-----------------------------------------------------------------------
prefixOpTy             :: PrefixOp -> Type
-----------------------------------------------------------------------

prefixOpTy PrefixMinus = TFun [tInt] tInt
prefixOpTy PrefixLNot  = TFun [tBool] tBool
prefixOpTy o           = convertError "prefixOpTy" o


-- instance Show r => PP (RType r) where 
--   pp = text . show

instance PP Type where 
  pp = ppType

instance PP a => PP [a] where 
  pp = ppArgs brackets comma 

ppType (TVar α _)     = pp α 
ppType (TFun ts t)    = ppArgs parens comma ts <+> text "=>" <+> ppType t 
ppType t@(TAll _ _)   = text "forall" <+> ppArgs id space αs <> text "." <+> ppType t' where (αs, t') = bkAll t
ppType (TApp c [] _)  = ppTC c 
ppType (TApp c ts _)  = parens $ ppTC c <+> ppArgs id space ts  

ppArgs p sep          = p . intersperse sep . map pp

ppTC TInt             = text "int"
ppTC TBool            = text "bool"
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
    deriving (Eq, Show)

data Annot b a = Ann { ann :: a, ann_fact :: [b] } deriving (Show)
type AnnBare   = Annot Fact SourcePos -- NO facts
type AnnSSA    = Annot Fact SourcePos -- Only Phi       facts
type AnnType   = Annot Fact SourcePos -- Only Phi + Typ facts
type AnnInfo   = M.HashMap SourcePos [Fact] 

instance HasAnnotation (Annot b) where 
  getAnnotation = ann 
