{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ImpredicativeTypes         #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}


module Language.Nano.Locations (

  -- * Located Values
    Located (..) 
  , IsLocated (..)
  , SourceSpan (..)
  , SrcSpan (..)
  , sourcePos
 
  -- * Manipulating SourceSpan
  -- , SourceSpan (..)
  , dummySpan
  , srcSpanFile
  , srcSpanStartLine
  , srcSpanEndLine
  , srcSpanStartCol
  , srcSpanEndCol
  
  , sourceSpanSrcSpan



) where

import           Data.Typeable                      (Typeable)
import           Data.Generics                      (Data)   
import           Language.ECMAScript3.Syntax 
import           Language.ECMAScript3.Syntax.Annotations
import           Language.ECMAScript3.PrettyPrint   (PP (..))
import           Language.ECMAScript3.Parser.Type   (SourceSpan (..))

import qualified Language.Fixpoint.Types as F

import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc
import           Language.Fixpoint.PrettyPrint


---------------------------------------------------------------------
-- | Tracking Source Code Locations --------------------------------- 
---------------------------------------------------------------------

data Located a
  = Loc { loc :: !SourceSpan
        , val :: a
        }
    deriving (Data, Typeable)
 
instance Functor Located where 
  fmap f (Loc l x) = Loc l (f x)

--------------------------------------------------------------------------------
-- | `IsLocated` is a predicate for values which have a SourceSpan
--------------------------------------------------------------------------------

sourcePos :: IsLocated a => a -> SourcePos
sourcePos = sp_begin . srcPos 

class IsLocated a where 
  srcPos :: a -> SourceSpan

instance IsLocated SrcSpan where 
  srcPos (SS a b) = Span a b

instance IsLocated Error where
  srcPos = srcPos . errLoc 

instance IsLocated SourceSpan where 
  srcPos x = x 

instance IsLocated (Located a) where 
  srcPos = loc

instance IsLocated SourcePos where
  srcPos x = Span x x 

instance IsLocated (F.Located a) where
  srcPos = srcPos . F.loc

-- instance IsLocated a => IsLocated (Id a) where 
--   srcPos (Id x _) = srcPos x

instance (HasAnnotation thing, IsLocated a) => IsLocated (thing a) where 
  srcPos  = srcPos . getAnnotation  

instance IsLocated F.Symbol where 
  srcPos _ = srcPos dummySpan

instance IsLocated (SourceSpan, r) where 
  srcPos = srcPos . fst

instance Eq a => Eq (Located a) where 
  x == y = val x == val y

instance Ord a => Ord (Located a) where 
  x `compare` y = val x `compare` val y

instance F.Fixpoint SourceSpan where
  toFix = pp 

sourceSpanSrcSpan sp = SS (sp_begin sp') (sp_end sp') where sp' = srcPos sp


--------------------------------------------------------------------------------
-- | SourceSpans ---------------------------------------------------------------
--------------------------------------------------------------------------------

instance PP SrcSpan where
  pp    = pprint

instance PP SourceSpan where 
  pp    = pp . tx where tx (Span x y) = SS x y


srcSpanStartLine = snd3 . sourcePosElts . sp_start . sourceSpanSrcSpan   
srcSpanEndLine   = snd3 . sourcePosElts . sp_stop  . sourceSpanSrcSpan
srcSpanStartCol  = thd3 . sourcePosElts . sp_start . sourceSpanSrcSpan 
srcSpanEndCol    = thd3 . sourcePosElts . sp_stop  . sourceSpanSrcSpan 
srcSpanFile      = fst3 . sourcePosElts . sp_start . sourceSpanSrcSpan

