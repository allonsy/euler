module Main where

import Data.Array

type Pyramid = Array Int (Array Int Int)

arrayify :: [[Int]] -> Pyramid
arrayify rows = array (1, numRows) columnIndices where
  columnTransform = map (\row -> array (1, length row) (zip [1..(length row)] row)) rows
  columnIndices = zip [1..numRows] columnTransform
  numRows = length rows

maxVal :: Pyramid -> Int -> (Int, Int) -> Int
maxVal pyr maxRows (row, col)
  | row == maxRows = curVal
  | curVal + downFork > curVal + rightFork = curVal + downFork
  | otherwise = curVal + rightFork where
    curVal = (pyr ! row) ! col
    downFork = maxVal pyr maxRows (row + 1, col)
    rightFork = maxVal pyr maxRows (row+1, col+1)

main :: IO ()
main = do
  contents <- readFile "18.txt"
  let lcontents = lines contents
  let nums = map (map read) $ map words lcontents :: [[Int]]
  let numPyramid = arrayify nums
  print $ maxVal numPyramid (snd (bounds numPyramid)) (1,1)
