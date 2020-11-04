module MaybeM where

import Control.Applicative (Applicative)
import Control.Monad (ap, liftM)
import Control.Monad.Fail (MonadFail)
import Prelude hiding (Just, Nothing)

data MaybeM a = Just a | Nothing
  deriving (Show)

instance Functor MaybeM where
  fmap = liftM

instance Applicative MaybeM where
  pure = return
  (<*>) = ap

instance Monad MaybeM where
  return a = Just a
  (Just a) >>= k = k a
  Nothing >>= k = Nothing

instance MonadFail MaybeM where
  fail _ = Nothing
