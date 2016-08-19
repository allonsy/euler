module Helper where

import Data.Array
import Data.Int

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = checkDivisors 2 where
  squareRoot = floor $ sqrt $ fromIntegral n
  checkDivisors k
    | k > squareRoot = True
    | n `mod` k == 0 = False
    | otherwise = checkDivisors (k+1)


arrayify :: [[Int64]] -> Array (Int64, Int64) Int64
arrayify rows = array ((0,0), (numRows - 1, numColumns - 1)) (concat columnTransform) where
  rowTransform = map (zip [0..numColumns - 1]) rows
  columnTransform = zipWith (\rowNum row -> map (\(colNum, colVal) -> ((rowNum, colNum), colVal)) row) [0..numRows - 1] rowTransform ::[[((Int64, Int64), Int64)]]

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
