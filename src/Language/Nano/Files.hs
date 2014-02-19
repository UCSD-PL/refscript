{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains Haskell variables representing globally visible 
-- names for files, paths, extensions.


module Language.Nano.Files (
  -- * Hardwired paths
  getPreludePath
  ) 
  where

import Paths_nano_js 

getPreludePath = getDataFileName "include/prelude.json" 
