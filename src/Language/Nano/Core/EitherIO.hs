-- | Code taken from:
--
--  https://github.com/kqr/gists/blob/master/articles/gentle-introduction-monad-transformers.md
--

module Language.Nano.Core.EitherIO where

import           Control.Applicative

data EitherIO a b = EitherIO {
  runEitherIO :: IO (Either a b)
}

instance Functor (EitherIO a) where
  fmap f = EitherIO . fmap (fmap f) . runEitherIO

instance Applicative (EitherIO e) where
  pure    = EitherIO . return . Right
  f <*> x = EitherIO $ liftA2 (<*>) (runEitherIO f) (runEitherIO x)

instance Monad (EitherIO e) where
  return  = pure
  x >>= f = EitherIO $ runEitherIO x >>= either (return . Left) (runEitherIO . f)

liftEither :: Either e a -> EitherIO e a
liftEither x = EitherIO (return x)

liftIO :: IO a -> EitherIO e a
liftIO x = EitherIO (fmap Right x)

