{-# LANGUAGE DeriveDataTypeable #-}

module Language.Rsc.Options where

import           Data.Generics

-----------------------------------------------------------------------------
-- | RSC Options
-----------------------------------------------------------------------------

data RscOption = RealOption
    deriving (Eq, Show, Data, Typeable)


