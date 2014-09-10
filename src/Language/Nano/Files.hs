{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains Haskell variables representing globally visible 
-- names for files, paths, extensions.


module Language.Nano.Files (
  -- * Hardwired paths
    getPreludeJSONPath
  , getPreludeTSPath
  , getDomJSONPath
  , getDomTSPath
  ) 
  where

import Paths_RefScript

getPreludeJSONPath = getDataFileName "include/prelude.json"
getPreludeTSPath   = getDataFileName "include/prelude.ts"

getDomJSONPath = getDataFileName "include/dom.json"
getDomTSPath   = getDataFileName "include/dom.ts"
