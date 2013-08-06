{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE Rank2Types             #-} 



module Language.Nano.Misc (
  -- * Helper functions

    mapFstM
  , mapSndM
  , mapPairM
  , mkEither
  , either2Bool
  , maybeM, maybeM_
  , unique

  , fst4
  , snd4
  , thd4
  , fth4

  , everywhereM'
) where

-- import           Control.Applicative                ((<$>))
import           Control.Monad                      (liftM2, when)
import           Data.Data
import qualified Data.Set                 as S
import qualified Data.List                as L
import qualified Language.Fixpoint.Types as F
import           Language.ECMAScript3.PrettyPrint
import           Text.PrettyPrint.HughesPJ
import           Language.Nano.Typecheck.Types()

import           Data.Generics.Aliases
import           Data.Generics.Schemes
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

-------------------------------------------------------------------------------
either2Bool :: Either a b -> Bool
-------------------------------------------------------------------------------
either2Bool = either (const False) (const True)

-------------------------------------------------------------------------------
maybeM :: (Monad m) => b -> (a -> m b) -> Maybe a -> m b
-------------------------------------------------------------------------------
maybeM d f a = maybe (return d) f a

-------------------------------------------------------------------------------
maybeM_ :: (Monad m) => (a -> m ()) -> Maybe a -> m ()
-------------------------------------------------------------------------------
maybeM_ = maybeM ()

-------------------------------------------------------------------------------
unique :: (Eq a) => [a] -> Bool
-------------------------------------------------------------------------------
unique xs = length xs == length (L.nub xs)


fst4 (a,_,_,_) = a
snd4 (_,b,_,_) = b
thd4 (_,_,c,_) = c
fth4 (_,_,_,d) = d


instance PP Bool where 
  pp True  = text "true"
  pp False = text "false"

instance PP a => PP (S.Set a) where
  pp = pp . S.toList

instance PP Char where
  pp = char

instance F.Fixpoint Char where
  toFix = char
 

--------------------------------------------------------------------------------
everywhereM' :: Monad m => GenericM m -> GenericM m
--------------------------------------------------------------------------------
everywhereM' f x = do { x' <- f x;
                        gmapM (everywhereM' f) x' }





