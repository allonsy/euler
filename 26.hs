module Main where

import Data.HashMap (insert, empty, Map, member, (!))
import Data.List (elemIndex, maximum)
import Data.Maybe

-- longDivide 1 20 = 1 / 20
longDivide :: Int -> Int -> Int
longDivide num denom = longDivideHelper emptyMap num denom 0 where
  longDivideHelper m n d r
    | n `member` m = r - (m ! n)
    | n < d = longDivideHelper (insert n r m) (n*10) d (r+1)
    | otherwise = longDivideHelper (insert n r m) newNum d (r+1) where
        dividers = n `quot` d
        newNum = (n - d * dividers) * 10
  emptyMap = empty :: Map Int Int


cycles :: [Int]
cycles = map (longDivide 1) [1..999]

main :: IO ()
main = do
  print $ (fromJust $ elemIndex (maximum cycles) cycles) + 1
