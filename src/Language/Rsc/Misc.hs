{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}


module Language.Rsc.Misc (
  -- * Helper functions

  -- Either
    mkEither, either2Bool

  -- List
  , uniqueBy, exists, mapi

  -- Tuples
  , fst4, snd4, thd4, fth4
  , mapFst, mapSnd
  , mapFst3, mapSnd3, mapThd3
  , mapFstM, mapSndM, mapPairM
  , setFst3, setSnd3, setThd3
  , setFst4, setSnd4, setThd4, setFth4
  , appFst4, appSnd4, appThd4, appFth4
  , mapPair, mapEither

  -- SYB
  , everywhereM'

  -- Zip
  , zipWith3M, zipWith3M_
  , unzip4

  -- Maybe
  , maybeM, maybeM_, fromJust', maybeToEither, mseq, nths, zwNth

  -- Container operations
  , isProperSubsetOf, isEqualSet, isProperSubmapOf
  , equalKeys

  , foldM1

  , withSingleton, withSingleton'

  , dup

  , concatMapM, mappendM, justM

  -- Convenience
  , single
  , case1, case2, case3
  , (<##>), (<###>)
  , (&)
  , (<//>)
  , (<**>)

) where

import           Control.Monad             (foldM, liftM2)
import           Data.Data
import           Data.Function             (on)
import           Data.Generics.Aliases
import           Data.Hashable
import qualified Data.HashMap.Strict       as M
import           Data.HashSet
import qualified Data.List                 as L
import           Data.Maybe                (isJust)
import qualified Language.Fixpoint.Types   as F
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

mapFst f (x, y)  = (f x, y)
mapSnd f (x, y)  = (x, f y)

mapFst3 f (x, y, z) = (f x, y, z)
mapSnd3 f (x, y, z) = (x, f y, z)
mapThd3 f (x, y, z) = (x, y, f z)

-------------------------------------------------------------------------------
mapSndM :: (Functor m, Monad m) => (b -> m c) -> (a, b) -> m (a, c)
-------------------------------------------------------------------------------
mapSndM = mapPairM return

-------------------------------------------------------------------------------
mapPairM :: (Functor m, Monad m) => (a -> m c) -> (b -> m d) -> (a, b) -> m (c, d)
-------------------------------------------------------------------------------
mapPairM f g (x,y) =  liftM2 (,) (f x) (g y)


-------------------------------------------------------------------------------
mapPair :: (a -> b) -> (a, a) -> (b, b)
-------------------------------------------------------------------------------
mapPair f (x, y) = (f x, f y)

-------------------------------------------------------------------------------
mapEither           :: (a -> Either b c) -> [a] -> ([b], [c])
-------------------------------------------------------------------------------
mapEither f         = go [] []
  where
    go ls rs []     = (reverse ls, reverse rs)
    go ls rs (x:xs) = case f x of
                        Left l  -> go (l:ls) rs  xs
                        Right r -> go ls  (r:rs) xs

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
maybeM = maybe . return

-------------------------------------------------------------------------------
maybeM_ :: (Monad m) => (a -> m ()) -> Maybe a -> m ()
-------------------------------------------------------------------------------
maybeM_ = maybeM ()

-------------------------------------------------------------------------------
uniqueBy :: (Eq a) => (a -> a -> Bool) -> [a] -> Bool
-------------------------------------------------------------------------------
uniqueBy f xs = length xs == length (L.nubBy f xs)

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

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f

mappendM :: (Monoid r, Monad m) => m r -> m r -> m r
mappendM = liftM2 mappend


-- | Auxiliary functions

single a = [a]

case1 g f e        = g (\case [e']            -> f e'         ) [e]
case2 g f e1 e2    = g (\case [e1', e2']      -> f e1' e2'    ) [e1, e2]
case3 g f e1 e2 e3 = g (\case [e1', e2', e3'] -> f e1' e2' e3') [e1, e2, e3]


f <##>  x = ((f <$>) <$>) x
f <###> x = ((f <$>) <$>) <$> x

justM = (Just <$>)

infixl 0 <**>
a <**> b = a <*> pure b

(&) :: a -> (a -> b) -> b
x & f = f x

nths = repeat Nothing
zwNth = (`zip` nths)

s1 <//> s2 = s1 ++ "\n" ++ s2




-- (>>=>) :: IO (Either a b) -> (b -> IO c) -> IO (Either a c)
-- act >>=> k = do
--   r <- act
--   case r of
--     Left l  -> return $  Left l -- (A.NoAnn, l)
--     Right x -> Right <$> k x

-- eAct :: IO (Err a) -> IO a
-- eAct m = do
--   x <- m
--   case x of
--     Left  l -> throw l
--     Right r -> return r


