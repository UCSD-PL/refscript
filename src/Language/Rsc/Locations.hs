{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}


module Language.Rsc.Locations (

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

import           Data.Generics            (Data)
import           Data.Typeable            (Typeable)
import           Language.Fixpoint.Types.Errors
import           Language.Fixpoint.Misc
import qualified Language.Fixpoint.Types  as F
import           Text.Parsec.Pos          (SourcePos)


---------------------------------------------------------------------
-- | Tracking Source Code Locations
---------------------------------------------------------------------

data Located a
  = Loc { loc :: !SrcSpan
        , val :: a
        }
    deriving (Data, Typeable)

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

instance {-# OVERLAPPING #-} IsLocated (Located a) where
  srcPos = loc

instance IsLocated SourcePos where
  srcPos x = SS x x

instance {-# OVERLAPPING #-} IsLocated (F.Located a) where
  srcPos = srcPos . F.loc

instance IsLocated F.Symbol where
  srcPos _ = srcPos dummySpan

instance IsLocated (SrcSpan, r) where
  srcPos = srcPos . fst

instance Eq a => Eq (Located a) where
  x == y = val x == val y

instance Ord a => Ord (Located a) where
  x `compare` y = val x `compare` val y

sourceSpanSrcSpan sp = SS (sp_start sp') (sp_stop sp') where sp' = srcPos sp

srcSpanStartLine = snd3 . sourcePosElts . sp_start . sourceSpanSrcSpan
srcSpanEndLine   = snd3 . sourcePosElts . sp_stop  . sourceSpanSrcSpan
srcSpanStartCol  = thd3 . sourcePosElts . sp_start . sourceSpanSrcSpan
srcSpanEndCol    = thd3 . sourcePosElts . sp_stop  . sourceSpanSrcSpan
srcSpanFile      = fst3 . sourcePosElts . sp_start . sourceSpanSrcSpan
