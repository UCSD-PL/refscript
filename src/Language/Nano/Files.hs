{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains Haskell variables representing globally visible
-- names for files, paths, extensions.


module Language.Nano.Files (
  -- * Hardwired paths
    getPreludeJSONPath
  , getPreludeTSPath
  , getDomJSONPath
  , getDomTSPath
  , getIncludePath
  )
  where

-- import Control.Applicative
import System.FilePath

import Paths_refscript

getIncludePath :: FilePath -> IO FilePath
getIncludePath f   = getDataFileName $ "include" </> f

getPreludeTSPath   :: IO FilePath
getPreludeTSPath   = getIncludePath "prelude.ts"

getDomTSPath :: IO FilePath
getDomTSPath       = getIncludePath "ambient/dom.ts"

getPreludeJSONPath :: IO FilePath
getPreludeJSONPath = (`replaceExtension` ".json") <$> getPreludeTSPath

getDomJSONPath :: IO FilePath
getDomJSONPath     = (`replaceExtension` ".json") <$> getDomTSPath

-- getPreludeJSONPath = getDataFileName "include/prelude.json"
-- getDomJSONPath     = getDataFileName "include/dom.json"
