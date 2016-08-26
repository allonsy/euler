module Main where

import Data.Array
import Data.Int

collatzJump :: Int64 -> Int64
collatzJump 1 = 1
collatzJump n
  | n `mod` 2 == 0 = n `quot` 2
  | otherwise = 3 * n + 1

collatzLength :: Int64 -> Int64
collatzLength 1 = 1
collatzLength n
  | n `mod` 2 == 0 = 1 + (collatzLength (n `quot` 2))
  | otherwise = 1 + (collatzLength (n * 3 + 1))

collatzArr :: Array Int64 Int64
collatzArr = array (1, upperBound) ((1,1) : assocs) where
  assocs = [(n, 1 + (collatzArr ! (collatzJump n))) | n <- [2..upperBound]]
  upperBound = 100000000

largest :: Int64
largest = largestHelper (0, -1) 1 where
  largestHelper (idx, val) k
    | k >= 1000000 = idx
    | collatzArr ! k > val = largestHelper (k,collatzArr ! k) (k+1)
    | otherwise = largestHelper (idx,val) (k+1)

collatzList :: [Int64]
collatzList = map collatzLength [1..999999]

largestList :: Int64
largestList = largestListHelper (0, -1) 1 collatzList where
  largestListHelper (idx, _) _ [] = idx
  largestListHelper (idx, val) curIdx (x:xs)
    | x > val = largestListHelper (curIdx, x) (curIdx + 1) xs
    | otherwise = largestListHelper (idx, val) (curIdx + 1) xs

main :: IO ()
main = do
  print $ largestList
