module Main where

import Data.Int
import Helper

main :: IO ()
main = do
  print $ fst $ head $ filter biggerFiveHundred $ numTriangleDivisors triangleNumbers

triangleFactory :: Int64 -> Int64 -> [Int64]
triangleFactory num tot = newTotal : (triangleFactory (num+1) (newTotal)) where
  newTotal = tot + num

triangleNumbers :: [Int64]
triangleNumbers = triangleFactory 1 0

numDivisors :: Int64 -> Int
numDivisors n = length $ divisors n

numTriangleDivisors :: [Int64] -> [(Int64, Int)]
numTriangleDivisors ls = map (\n -> (n, numDivisors n)) ls

biggerFiveHundred :: (Int64, Int) -> Bool
biggerFiveHundred (n,k) = k > 500
