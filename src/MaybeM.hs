module MaybeM where

import Control.Applicative (Applicative)
import Control.Monad (ap, liftM)
import Control.Monad.Fail (MonadFail)
import Prelude hiding (Just, Nothing)

data MaybeM a = Just a | Nothing
  deriving (Show)

instance Functor MaybeM where
  fmap f (Just a) = Just $ f a
  fmap f Nothing = Nothing

instance Applicative MaybeM where
  pure k = Just k

  -- f (a -> b) -> f a -> f b
  (<*>) _ Nothing = Nothing
  (<*>) (Just k) (Just a) = Just (k a)

instance Monad MaybeM where
  return a = Just a
  (Just a) >>= k = k a
  Nothing >>= k = Nothing

instance MonadFail MaybeM where
  fail _ = Nothing
