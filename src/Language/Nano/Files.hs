{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains Haskell variables representing globally visible 
-- names for files, paths, extensions.


module Language.Nano.Files (
  -- * Hardwired paths
    getPreludeJSONPath
  , getPreludeTSPath
  ) 
  where

import Paths_RefScript

getPreludeJSONPath = getDataFileName "include/prelude.json" 
getPreludeTSPath   = "include/prelude.ts"       
