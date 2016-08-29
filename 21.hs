module Main where

import Helper
import Data.Int
import Data.List

sumDivisors :: Int64 -> Int64
sumDivisors n = sum $ properDivisors n

amicableNumber :: Int64 -> Maybe (Int64, Int64)
amicableNumber n
  | sumDivisors (sumDivs) == n = Just (n, sumDivs)
  | otherwise = Nothing where
    sumDivs = sumDivisors n

amicableNumbers :: [Int64]
amicableNumbers = filterOut $ map amicableNumber [2..9999] where
  filterOut ls = squash $ filter filterPred ls
  filterPred Nothing = False
  filterPred (Just (a,b)) = a < b
  squash ls = foldr (\(Just (a,b)) buildup -> a:b:buildup) [] ls

main :: IO ()
main = do
  print $ sum amicableNumbers
