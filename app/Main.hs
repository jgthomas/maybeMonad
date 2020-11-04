module Main where

import MaybeM
import Prelude hiding (Just, Nothing)

safeHead :: [Int] -> MaybeM Int
safeHead [] = Nothing
safeHead (a : as) = Just a

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

main :: IO ()
main = do
  print $ safeAdd (safeHead first) (safeHead second)
  print $ safeAdd (safeHead first) (safeHead third)
