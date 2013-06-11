{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleInstances      #-}

module Language.Nano.ESC.Types (
  
  -- * Nano Programs For VC
    Nano
  , Fun(..) 
  , parseNanoFromFile 

  -- * Verification Conditions
  , VCond
  , newVCond
  , obligationsVCond 

  ) where

import           Control.Applicative          ((<$>))
import qualified Data.HashMap.Strict as M
-- import           Data.Hashable
-- import           Data.Typeable                      (Typeable)
-- import           Data.Generics                      (Data)   
import           Data.Monoid                        (Monoid (..))
import           Data.Maybe                         ({-catMaybes,-} fromMaybe)
import           Language.ECMAScript3.Syntax 
import           Language.ECMAScript3.PrettyPrint   (PP (..))
import           Language.ECMAScript3.Parser        (parseJavaScriptFromFile)

import qualified Language.Fixpoint.Types as F
-- import           Language.Fixpoint.PrettyPrint
import           Language.Fixpoint.Misc
import           Language.Nano.Types
import           Text.PrettyPrint.HughesPJ
-- import           Text.Parsec                        

---------------------------------------------------------------------
-- | Top-level Parser 
---------------------------------------------------------------------

parseNanoFromFile f 
  = do s     <- parseJavaScriptFromFile f
       return $ fromMaybe err (mkNano s)
    where
       err    = errorstar $ "Invalid Input File: " ++ f

------------------------------------------------------------------
-- | Nano Programs : Wrapper around EcmaScript -------------------
------------------------------------------------------------------

type Nano        = [Fun SourcePos] 

data Fun a       = Fun { floc  :: a             -- ^ sourceloc 
                       , fname :: Id a          -- ^ name
                       , fargs :: [Id a]        -- ^ parameters
                       , fbody :: [Statement a] -- ^ body
                       , fpre  :: F.Pred        -- ^ precondition
                       , fpost :: F.Pred        -- ^ postcondition
                       }

-- functions fns = fns

mkNano :: [Statement SourcePos] -> Maybe Nano
mkNano =  sequence . map mkFun 

mkFun :: Statement a -> Maybe (Fun a)
mkFun (FunctionStmt l f xs body) 
  | all isNano body = Just $ Fun l f xs body pre post
  | otherwise       = Nothing
  where 
    pre             = getSpec getRequires body 
    post            = getSpec getEnsures  body

mkFun s             = convertError "mkFun" s 

------------------------------------------------------------------
-- | Verification Conditions -------------------------------------
------------------------------------------------------------------

-- | `VCond` are formulas indexed by `SourcePos` from which 
--   the obligation arises.

newtype VCond_ a = VC (M.HashMap SourcePos a)

type VCond       = VCond_ F.Pred

instance Functor VCond_ where 
  fmap f (VC m) = VC (fmap f m)

instance Monoid VCond where 
  mempty                = VC M.empty
  mappend (VC x) (VC y) = VC (M.unionWith pAnd x y)

instance PP VCond where 
  pp = ppObligations . obligationsVCond 

instance PP (Fun SourcePos) where 
  pp (Fun l n xs _ pre post) =   pp n <+> text "defined at" <+> pp l
                             $+$ text "formals: "  <+> intersperse comma (pp <$> xs)
                             $+$ text "requires: " <+> pp pre
                             $+$ text "ensures: "  <+> pp post

ppObligations lps   =   text "Verification Condition" 
                    $+$ vcat (map ppObligation lps)

ppObligation (l, p) = text "for" <+> pp l <+> dcolon <+> pp p

------------------------------------------------------------------
obligationsVCond :: VCond_ a -> [(SourcePos, a)] 
------------------------------------------------------------------

obligationsVCond (VC x) = M.toList x

------------------------------------------------------------------
newVCond     :: SourcePos -> F.Pred -> VCond
------------------------------------------------------------------

newVCond l p = VC $ M.singleton l p


-----------------------------------------------------------------

