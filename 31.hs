module Main where

numWays :: Int
numWays = numWaysHelper 8 200 where
  numWaysHelper _ 0 = 1
  numWaysHelper 8 n
    | n < 200 = numWaysHelper 7 n
    | otherwise = numWaysHelper 8 (n-200) + numWaysHelper 7 n
  numWaysHelper 7 n
    | n < 100 = numWaysHelper 6 n
    | otherwise = numWaysHelper 7 (n-100) + numWaysHelper 6 n
  numWaysHelper 6 n
    | n < 50 = numWaysHelper 5 n
    | otherwise = numWaysHelper 6 (n-50) + numWaysHelper 5 n
  numWaysHelper 5 n
    | n < 20 = numWaysHelper 4 n
    | otherwise = numWaysHelper 5 (n-20) + numWaysHelper 4 n
  numWaysHelper 4 n
    | n < 10 = numWaysHelper 3 n
    | otherwise = numWaysHelper 4 (n-10) + numWaysHelper 3 n
  numWaysHelper 3 n
    | n < 5 = numWaysHelper 2 n
    | otherwise = numWaysHelper 3 (n-5) + numWaysHelper 2 n
  numWaysHelper 2 n
    | n < 2 = numWaysHelper 1 n
    | otherwise = numWaysHelper 2 (n-2) + numWaysHelper 1 n
  numWaysHelper 1 n = 1

main :: IO ()
main = print numWays
