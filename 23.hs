module Main where

import Helper
import Data.Array
import Data.Int

sums :: Int64 -> [(Int64, Int64)]
sums n = [(a,n-a) | a <- [1..(n `quot` 2)]]

isAbundant :: Int64 -> Bool
isAbundant n = sum (properDivisors n) > n

abundantArray :: Array Int64 Bool
abundantArray = array (1, 28123) assocs where
  assocs = [(n, isAbundant n) | n <- [1..28123]]

nonAbundantSum :: Int64 -> Bool
nonAbundantSum n = and noAbundance where
  noAbundance = map (\(a,b) -> not (abundantArray ! a && abundantArray ! b)) $ sums n

nonAbundantNums :: [Int64]
nonAbundantNums = filter nonAbundantSum [1..28123]

main :: IO ()
main = do
  print $ sum nonAbundantNums
