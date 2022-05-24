{-# LANGUAGE DeriveFunctor #-}
module Test.Hspec.Core.Formatters.V1.Free where

import           Prelude ()
import           Test.Hspec.Core.Compat
import           GHC.Types (Total, type(@))


data f @ Free f a => Free f a = Free (f (Free f a)) | Pure a
  deriving Functor

instance (Total f, Functor f) => Applicative (Free f) where
  pure = Pure
  Pure f <*> Pure a = Pure (f a)
  Pure f <*> Free m = Free (fmap f <$> m)
  Free m <*> b = Free (fmap (<*> b) m)

instance (Total f, Functor f) => Monad (Free f) where
  return = pure
  Pure a >>= f = f a
  Free m >>= f = Free (fmap (>>= f) m)

liftF :: Functor f => f a -> Free f a
liftF command = Free (fmap Pure command)
