{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains Haskell variables representing globally visible
-- names for files, paths, extensions.


module Language.Rsc.Core.Files (
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


-------------------------------------------------------------------------------
getPreludeTSPath, getDomTSPath, getPreludeJSONPath, getDomJSONPath :: IO FilePath
-------------------------------------------------------------------------------
getPreludeTSPath   = getDataFileName "include/prelude.ts"
getDomTSPath       = getDataFileName "include/ambient/dom.ts"
getPreludeJSONPath = (`replaceExtension` ".json") <$> getPreludeTSPath
getDomJSONPath     = (`replaceExtension` ".json") <$> getDomTSPath
