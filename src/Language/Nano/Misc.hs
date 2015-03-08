{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE Rank2Types             #-} 

module Language.Nano.Misc (
  -- * Helper functions

  -- Either
    mkEither, either2Bool

  -- List
  , unique, exists, mapi

  -- Tuples
  , fst4, snd4, thd4, fth4
  , mapFstM, mapSndM, mapPairM
  , setFst3, setSnd3, setThd3
  , setFst4, setSnd4, setThd4, setFth4
  , appFst4, appSnd4, appThd4, appFth4

  -- SYB
  , everywhereM'
  
  -- Zip
  , zipWith3M, zipWith3M_
  , unzip4

  -- Maybe
  , maybeM, maybeM_, fromJust', maybeToEither, mseq
                                               
  -- Container operations
  , isProperSubsetOf, isEqualSet, isProperSubmapOf
  , equalKeys

  -- * Error message
  , convertError

  , foldM1
  
  , withSingleton, withSingleton'

  , dup

  , mappendM

) where

-- import           Control.Applicative                ((<$>))
import           Control.Monad                        (liftM2, foldM)
import           Data.Data
import           Data.Maybe                           (isJust)
import           Data.Monoid                          (Monoid, mappend)
import           Data.Generics.Aliases
import           Data.HashSet
import           Data.Function                        (on)
import           Data.Hashable
import qualified Data.HashMap.Strict                  as M
import qualified Data.List                            as L

import qualified Language.Fixpoint.Types              as F
import           Language.Fixpoint.Misc
import           Language.Nano.Syntax.PrettyPrint
import           Text.PrettyPrint.HughesPJ

-------------------------------------------------------------------------------
mapi :: (Int -> a -> b) -> [a] -> [b] 
-------------------------------------------------------------------------------
mapi f          = go 0
  where
    go i (x:xs) = f i x : go (i+1) xs
    go _ []     = []



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
mseq :: (Monad m) => m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b) 
-------------------------------------------------------------------------------
mseq act k = do z <- act
                case z of
                  Nothing -> return Nothing
                  Just x  -> k x



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

-------------------------------------------------------------------------------
exists :: (a -> Bool) -> [a] -> Bool
-------------------------------------------------------------------------------
exists f = isJust . L.find f

setFst3 (_,b,c) a' = (a',b,c)
setSnd3 (a,_,c) b' = (a,b',c)
setThd3 (a,b,_) c' = (a,b,c')

fst4 (a,_,_,_) = a
snd4 (_,b,_,_) = b
thd4 (_,_,c,_) = c
fth4 (_,_,_,d) = d

setFst4 (_,b,c,d) a' = (a',b,c,d)
setSnd4 (a,_,c,d) b' = (a,b',c,d)
setThd4 (a,b,_,d) c' = (a,b,c',d)
setFth4 (a,b,c,_) d' = (a,b,c,d')

appFst4 (a,b,c,d) f = (f a,b,c,d)
appSnd4 (a,b,c,d) f = (a,f b,c,d)
appThd4 (a,b,c,d) f = (a,b,f c,d)
appFth4 (a,b,c,d) f = (a,b,c,f d)

instance F.Fixpoint Char where
  toFix = char
 

--------------------------------------------------------------------------------
everywhereM' :: Monad m => GenericM m -> GenericM m
--------------------------------------------------------------------------------
everywhereM' f x = do { x' <- f x;
                        gmapM (everywhereM' f) x' }

--------------------------------------------------------------------------------
zipWith3M           :: (Monad m) => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m [d]
--------------------------------------------------------------------------------
zipWith3M f xs ys zs =  sequence (zipWith3 f xs ys zs)

--------------------------------------------------------------------------------
zipWith3M_          :: (Monad m) => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m ()
--------------------------------------------------------------------------------
zipWith3M_ f xs ys zs =  sequence_ (zipWith3 f xs ys zs)


--------------------------------------------------------------------------------
unzip4   :: [(a,b,c,d)] -> ([a],[b],[c],[d])
--------------------------------------------------------------------------------
unzip4   =  L.foldr (\(a,b,c,d) ~(as,bs,cs,ds) -> (a:as,b:bs,c:cs,d:ds))
                  ([],[],[],[])


fromJust' _ (Just a) = a
fromJust' s _        = error s

maybeToEither _ (Just a) = Right a
maybeToEither e Nothing  = Left e

-- | Sets / maps

isProperSubsetOf :: (Eq a, Hashable a) => HashSet a -> HashSet a -> Bool
s1 `isProperSubsetOf` s2 = size (s1 \\ s2) == 0 && size (s2 \\ s1) > 0  

isEqualSet :: (Eq a, Hashable a) => HashSet a -> HashSet a -> Bool
s1 `isEqualSet` s2 = size (s1 \\ s2) == 0 && size (s2 \\ s1) == 0  

(\\) :: (Eq a, Hashable a) => HashSet a -> HashSet a -> HashSet a
(\\) = difference

isProperSubmapOf :: (Eq a, Hashable a) => M.HashMap a b -> M.HashMap a b -> Bool
isProperSubmapOf = isProperSubsetOf `on` (fromList . M.keys) 

equalKeys :: (Eq a, Ord a, Hashable a) => M.HashMap a b -> M.HashMap a b -> Bool
equalKeys =  (==) `on` (L.sort . M.keys)


convertError tgt e  = errortext $ msg <+> pp e
  where 
    msg             = text $ "Cannot convert to: " ++ tgt



instance (PP a, PP b, PP c, PP d) => PP (a,b,c,d) where
  pp (a,b,c,d) = pp a <+> text ":" <+>  pp b <+> text ":" <+> pp c <+> text ":" <+> pp d

instance (PP a, PP b, PP c, PP d, PP e) => PP (a,b,c,d,e) where
  pp (a,b,c,d,e) = pp a <+> text ":" <+>  pp b <+> text ":" <+> pp c <+> text ":" <+> pp d <+> text ":" <+> pp e 

foldM1 :: (Monad m) => (a -> a -> m a) -> [a] -> m a
foldM1 _ [] = error "foldM1" "empty list"
foldM1 f (x:xs) = foldM f x xs

withSingleton :: Monad m => (a -> m b) -> m b -> [a] -> m b 
withSingleton f _ [x] = f x
withSingleton _ b _   = b

withSingleton' :: Monad m => m b -> (a -> m b) -> m b -> [a] -> m b 
withSingleton' b _ _  [ ] = b
withSingleton' _ f _  [x] = f x
withSingleton' _ _ e  _   = e

dup f1 f2 a = (f1 a,f2 a)

mappendM :: (Monoid r, Monad m) => m r -> m r -> m r
mappendM = liftM2 mappend
