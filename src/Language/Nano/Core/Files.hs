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

import           Control.Applicative
import           System.FilePath

import           Paths_RefScript

getPreludeTSPath   = getDataFileName "include/prelude.ts"
getDomTSPath       = getDataFileName "include/ambient/dom.ts"

getPreludeJSONPath = (`replaceExtension` ".json") <$> getPreludeTSPath
getDomJSONPath     = (`replaceExtension` ".json") <$> getDomTSPath

-- getPreludeJSONPath = getDataFileName "include/prelude.json"
-- getDomJSONPath     = getDataFileName "include/dom.json"
