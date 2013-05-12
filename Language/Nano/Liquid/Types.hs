-- | Global type definitions for Refinement Type Checker

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module Language.Nano.Liquid.Types (

  -- * Programs
    Nano   
  , Spec   (..)
  , Source (..)
  , mkNano

  -- * Environments 
  , Env    (..)
  , envFromList 
  , envToList
  , envEmpty 
  , envAdd 
  , envAdds

  -- * Accessors
  , code
  , env

  -- * Refinement Types
  , RType (..)
  , RefType
  , toType
  
  -- * Deconstructing Types
  , bkFun

  -- * Regular Types
  , Type (..)
  , TVar (..)
  , TCon (..)

  ) where 

import           Data.Ord                       (comparing) 
import qualified Data.List               as L
import           Data.Generics.Aliases
import           Data.Generics.Schemes
import qualified Data.HashMap.Strict     as M
import           Language.ECMAScript3.Syntax
import           Language.ECMAScript3.PrettyPrint
import           Language.Nano.Types
import qualified Language.Fixpoint.Types as F
import           Language.Fixpoint.Misc
import           Text.Parsec
import           Text.PrettyPrint.HughesPJ
import           Control.Applicative 
import           Control.Monad

-- | Type Variables
newtype TVar = TV (Located F.Symbol)
              
instance Show TVar where 
  show (TV a) = show (val a)

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
    deriving (Show, Functor)

-- | Standard Types
type Type    = RType ()

-- | (Real) Refined Types
type RefType = RType F.Reft 

instance Show r => PP (RType r) where 
  pp = text . show

-- | Stripping out Refinements 
toType :: RType a -> Type
toType = fmap (const ())


bkFun :: RType a -> Maybe ([TVar], [RType a], RType a)
bkFun t = do (αs, t')   <- bkAll t
             (xts, t'') <- bkArr t'
             return        (αs, xts, t'')
         
bkArr (TFun xts t) = Just (xts, t)
bkArr _            = Nothing

bkAll                :: RType a -> ([RVar], RType a)
bkAll t              = go [] t
  where 
    go αs (TAll α t) = go (α : αs) t
    go αs t          = (reverse αs, t)

---------------------------------------------------------------------------------
-- | Nano Program = Code + Types for all function binders
---------------------------------------------------------------------------------

data Nano = Nano { code :: !Source 
                 , env  :: !(Env Type)
                 }

-- | Type Specification for function binders
data Spec = Spec { sigs :: !(Env Type) }
            deriving (Show)

{-@ measure isFunctionStatement :: (Statement SourcePos) -> Prop 
    isFunctionStatement (FunctionStmt {}) = true
    isFunctionStatement (_)               = false
  @-}

{-@ type FunctionStatement = {v:(Statement SourcePos) | (isFunctionStatement v)} @-}
type FunctionStatement = Statement SourcePos

{-@ newtype Source = Src [FunctionStatement] @-}
newtype Source = Src [FunctionStatement]

instance PP Nano where
  pp (Nano (Src s) env) 
    =   text "********************** CODE **********************"
    $+$ pp s
    $+$ text "********************** SPEC **********************"
    $+$ pp env


--------------------------------------------------------------------------
-- | Environments
--------------------------------------------------------------------------

newtype Env t       = TE (M.HashMap (Id SourcePos) t)
                      deriving (Show)

envToList (TE m)    = M.toList m
envFromList         = TE . M.fromList 
envEmpty            = envFromList []

envAdd (TE m) (x,t) = TE (M.insert x t m)
envAdds Γ xts       = foldl' envAdd Γ xts


envFind x (TE m)    = M.lookup x m

instance PP t => PP (Env t) where 
  pp = vcat . (ppBind <$>) . envToList

ppBind (x, t) = pp x <+> dcolon <+> pp t

--------------------------------------------------------------------------
-- | Combining Source and Spec into Nano ---------------------------------
--------------------------------------------------------------------------

mkNano  :: [Statement SourcePos] -> Spec -> Either Doc Nano 
mkNano stmts spec 
  = do src   <- Src <$> mapM checkFun stmts
       env   <- mkEnv (getFunctionIds stmts) (sigs spec)
       return $ Nano src env

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

mkEnv         :: [Id SourcePos] -> Env Type -> Either Doc (Env Type) 
mkEnv ids env = envFromList <$> zipWithM joinName ids' its'
  where
    ids' = orderIds idName (comparing idLoc) ids
    its' = orderIds (idName . fst) (comparing (idLoc . fst)) its 
    its  = envToList env

joinName (name, i) (name', (_,t)) 
  | name == name' = Right (i, t)
  | otherwise     = Left (text "Missing Type Specification: " <+> pp i) 

-- orderIds :: (Ord t, Hashable t) =>(a -> t) -> (a -> a -> Ordering) -> [a] -> [(t, a)]
orderIds fn fl = concatMap (\(x,ys) -> (x,) <$> ys) 
               . hashMapToAscList 
               . (L.sortBy fl <$>) 
               . groupMap fn 


-- ids ==> [(String, [Id a])] ==> [(String, Id a)]
-- env ==> [(String, [Type])] ==> [(String, Type)] 
-- 
-- safeZipWith 
-- (name, id) .... 
-- (name, t ) ....




