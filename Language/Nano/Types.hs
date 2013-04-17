{-# LANGUAGE DeriveDataTypeable     #-}
{- LANGUAGE MultiParamTypeClasses  #-}
{- LANGUAGE TypeSynonymInstances   #-}
{- LANGUAGE FlexibleInstances      #-}
{- LANGUAGE FlexibleContexts       #-} 
{- LANGUAGE OverlappingInstances   #-}



module Language.Nano.JS.Types (
  -- * Configuration Options
  Config (..)

  ) where

import Data.Typeable                (Typeable)
import Data.Generics                (Data)   

data Config = Config { 
    files   :: [FilePath]     -- ^ source files to check
  , incdirs :: [FilePath]     -- ^ path to directory for including specs
  } deriving (Data, Typeable, Show, Eq)


