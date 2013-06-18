{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleInstances      #-}



module Language.Nano.Misc (
  -- * Helper functions

    mapFstM
  , mapSndM
  , mapPairM
  , mkEither

) where

-- import           Control.Applicative                ((<$>))
import           Control.Monad                      (liftM2)


-------------------------------------------------------------------------------
mapFstM :: (Functor m, Monad m) => (a -> m c) -> (a, b) -> m (c, b)
-------------------------------------------------------------------------------
mapFstM f = mapPairM f return  
-- mapFstM f (x,y) = (,y) <$> f x

-------------------------------------------------------------------------------
mapSndM :: (Functor m, Monad m) => (b -> m c) -> (a, b) -> m (a, c)
-------------------------------------------------------------------------------
mapSndM f = mapPairM return f
-- mapSndM f (x,y) = (x,) <$> f y

-------------------------------------------------------------------------------
mapPairM :: (Functor m, Monad m) => (a -> m c) -> (b -> m d) -> (a, b) -> m (c, d)
-------------------------------------------------------------------------------
mapPairM f g (x,y) =  liftM2 (,) (f x) (g y)
-- mapPairM f g p =  mapFstM f p >>= mapSndM g


-------------------------------------------------------------------------------
mkEither :: Bool -> s -> a -> Either s a
-------------------------------------------------------------------------------
mkEither b s a | b         = Right a
mkEither b s a | otherwise = Left s
