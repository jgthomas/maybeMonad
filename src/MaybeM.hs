module MaybeM where

import Prelude hiding (Just, Nothing)

data MaybeM a = Just a | Nothing
  deriving (Show)

instance Functor MaybeM where
  fmap f (Just a) = Just $ f a
  fmap _ Nothing = Nothing

instance Applicative MaybeM where
  pure k = Just k

  -- f (a -> b) -> f a -> f b
  (<*>) _ Nothing = Nothing
  (<*>) Nothing _ = Nothing
  (<*>) (Just k) (Just a) = Just (k a)

instance Monad MaybeM where
  return a = Just a
  (Just a) >>= k = k a
  Nothing >>= _ = Nothing

instance MonadFail MaybeM where
  fail _ = Nothing
