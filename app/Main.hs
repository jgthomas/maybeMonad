module Main where

import MaybeM
import Prelude hiding (Just, Nothing)

safeHead :: [Int] -> MaybeM Int
safeHead [] = Nothing
safeHead (a : _) = Just a

safeAdd :: MaybeM Int -> MaybeM Int -> MaybeM Int
safeAdd (Just x) (Just y) = (Just $ x + y)
safeAdd Nothing _ = Nothing
safeAdd _ Nothing = Nothing

first :: [Int]
first = [1, 2, 3]

second :: [Int]
second = [4, 5, 6]

third :: [Int]
third = []

addTo :: Int -> Int -> Int
addTo x y = x + y

main :: IO ()
main = do
  print $ fmap (addTo 2) (Just 3)
  print $ fmap (addTo 2) Nothing
  print $ pure (addTo 1) <*> (Just 3)
  print $ pure (addTo 1) <*> Nothing
  print $ safeAdd (safeHead first) (safeHead second)
  print $ safeAdd (safeHead first) (safeHead third)
