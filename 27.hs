module Main where

import Helper
import Data.List

type Equation = (Int, Int)

evaluate :: Equation -> Int -> Int
evaluate (a,b) val = val^2 + a*val + b

equations :: [Equation]
equations = [(a,b) | a <- bounds, b <- primes] where
  bounds = [(-999)..999]
  primes = filter isPrime [2..999]


evaluations :: Equation -> [Int]
evaluations eq = map (evaluate eq) [0..]

numPrimes :: [Int] -> Int
numPrimes ls = numPrimesHelper ls 0 where
  numPrimesHelper (x:xs) tot
    | isPrime x = numPrimesHelper xs (tot + 1)
    | otherwise = tot

numEqPrimes :: [(Equation, Int)]
numEqPrimes = map (\(a,b) -> ((a,b), (numPrimes . evaluations) (a,b))) equations

getMaxProd :: Int
getMaxProd = getMaxProdHelper ((0,0), 0) numEqPrimes where
  getMaxProdHelper ((a,b), _) [] = a*b
  getMaxProdHelper (eq, val) ((newEq, newVal):rest)
    | newVal > val = getMaxProdHelper (newEq, newVal) rest
    | otherwise = getMaxProdHelper (eq, val) rest


main :: IO ()
main = do
  print $ getMaxProd
