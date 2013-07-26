{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleInstances      #-}



module Language.Nano.Misc (
  -- * Helper functions

    mapFstM
  , mapSndM
  , mapPairM
  , mkEither
  , unique

) where

-- import           Control.Applicative                ((<$>))
import           Control.Monad                      (liftM2)
import qualified Data.Set                 as S
import qualified Data.List                as L
import qualified Language.Fixpoint.Types as F
import           Language.ECMAScript3.PrettyPrint
import           Text.PrettyPrint.HughesPJ
import           Language.Nano.Typecheck.Types()

-------------------------------------------------------------------------------
mapFstM :: (Functor m, Monad m) => (a -> m c) -> (a, b) -> m (c, b)
-------------------------------------------------------------------------------
mapFstM f = mapPairM f return  

-------------------------------------------------------------------------------
mapSndM :: (Functor m, Monad m) => (b -> m c) -> (a, b) -> m (a, c)
-------------------------------------------------------------------------------
mapSndM = mapPairM return 

-------------------------------------------------------------------------------
mapPairM :: (Functor m, Monad m) => (a -> m c) -> (b -> m d) -> (a, b) -> m (c, d)
-------------------------------------------------------------------------------
mapPairM f g (x,y) =  liftM2 (,) (f x) (g y)

-------------------------------------------------------------------------------
mkEither :: Bool -> s -> a -> Either s a
-------------------------------------------------------------------------------
mkEither True  _ a = Right a
mkEither False s _ = Left s


unique :: (Eq a) => [a] -> Bool
unique xs = length xs == length (L.nub xs)


instance PP Bool where 
  pp True  = text "true"
  pp False = text "false"

instance PP a => PP (S.Set a) where
  pp = pp . S.toList

instance PP Char where
  pp = char

instance F.Fixpoint Char where
  toFix = char
