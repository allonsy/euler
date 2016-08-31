module Helper where

import Data.Array
import Data.Int

isPrime :: Integral a => a -> Bool
isPrime 1 = False
isPrime n = checkDivisors 2 where
  root = squareRoot n
  checkDivisors k
    | k > root = True
    | n `mod` k == 0 = False
    | otherwise = checkDivisors (k+1)

divisors :: Integral a => a -> [a]
divisors n = divisors' 1 n where
  root = squareRoot n
  divisors' k n
    | k > root = []
    | n `mod` k == 0 = if k*k /= n then k : n `quot` k :  divisors' (k+1) n else k :  divisors' (k+1) n
    | otherwise = divisors' (k+1) n

properDivisors :: Integral a => a -> [a]
properDivisors n = divisors' 2 n where
  root = squareRoot n
  divisors' k n
    | k > root = [1]
    | n `mod` k == 0 = if k*k /= n then k : n `quot` k :  divisors' (k+1) n else k :  divisors' (k+1) n
    | otherwise = divisors' (k+1) n

squareRoot :: Integral a => a -> a
squareRoot n = floor $ sqrt $ fromIntegral n



arrayify :: (Integral a, Ix a) => [[a]] -> Array (a, a) a
arrayify rows = array ((0,0), (numRows - 1, numColumns - 1)) (concat columnTransform) where
  rowTransform = map (zip [0..numColumns - 1]) rows
  columnTransform = zipWith (\rowNum row -> map (\(colNum, colVal) -> ((rowNum, colNum), colVal)) row) [0..numRows - 1] rowTransform

  numColumns = fromIntegral $ length (head rows)
  numRows = fromIntegral $ length rows
  interpolateRowNum k (col, val) = ((k,col), val)

read2DArray :: String -> IO (Array (Int64, Int64) Int64)
read2DArray fname = do
  contents <- readFile "11.txt"
  let linedContent = lines contents
  let nums = map (map read) $ map words linedContent :: [[Int64]]
  let numArray = arrayify nums
  return numArray
