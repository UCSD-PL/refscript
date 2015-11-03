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
  , SrcSpan (..)
  , sourcePos

  -- * Manipulating SrcSpan
  -- , SrcSpan (..)
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
import           Language.Nano.Syntax
import           Language.Nano.Syntax.Annotations
import           Language.Nano.Syntax.PrettyPrint   (PP (..))
-- import           Language.ECMAScript3.Parser.Type   (SrcSpan (..))

import qualified Language.Fixpoint.Types as F

import           Language.Fixpoint.Errors
import           Language.Fixpoint.Misc
import           Language.Fixpoint.PrettyPrint


---------------------------------------------------------------------
-- | Tracking Source Code Locations ---------------------------------
---------------------------------------------------------------------

data Located a
  = Loc { loc :: !SrcSpan
        , val :: a
        }
    deriving (Data, Typeable)

instance Show a => Show (Located a) where
  show = show . val
  
instance Functor Located where
  fmap f (Loc l x) = Loc l (f x)

--------------------------------------------------------------------------------
-- | `IsLocated` is a predicate for values which have a SrcSpan
--------------------------------------------------------------------------------

sourcePos :: IsLocated a => a -> SourcePos
sourcePos = sp_start . srcPos

class IsLocated a where
  srcPos :: a -> SrcSpan

instance IsLocated Error where
  srcPos = srcPos . errLoc

instance IsLocated SrcSpan where
  srcPos x = x

instance IsLocated (Located a) where
  srcPos = loc

instance IsLocated SourcePos where
  srcPos x = SS x x

instance IsLocated (F.Located a) where
  srcPos = srcPos . F.loc

instance (HasAnnotation thing, IsLocated a) => IsLocated (thing a) where
  srcPos  = srcPos . getAnnotation

instance IsLocated F.Symbol where
  srcPos _ = srcPos dummySpan

instance IsLocated (SrcSpan, r) where
  srcPos = srcPos . fst

instance Eq a => Eq (Located a) where
  x == y = val x == val y

instance Ord a => Ord (Located a) where
  x `compare` y = val x `compare` val y

instance F.Fixpoint SrcSpan where
  toFix = pp

sourceSpanSrcSpan sp = SS (sp_start sp') (sp_stop sp') where sp' = srcPos sp


--------------------------------------------------------------------------------
-- | SrcSpans ---------------------------------------------------------------
--------------------------------------------------------------------------------

instance PP SrcSpan where
  pp    = pprint

srcSpanStartLine = snd3 . sourcePosElts . sp_start . sourceSpanSrcSpan
srcSpanEndLine   = snd3 . sourcePosElts . sp_stop  . sourceSpanSrcSpan
srcSpanStartCol  = thd3 . sourcePosElts . sp_start . sourceSpanSrcSpan
srcSpanEndCol    = thd3 . sourcePosElts . sp_stop  . sourceSpanSrcSpan
srcSpanFile      = fst3 . sourcePosElts . sp_start . sourceSpanSrcSpan
