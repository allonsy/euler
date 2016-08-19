import Data.Array

import Helper
import Data.Int
import Data.List

type Grid = Array (Int64, Int64) Int64
type Point = (Int64,Int64)

main :: IO ()
main = do
  arr <- read2DArray "11.txt"
  let vals = assocs arr
  let prods = vals >>= getProds arr
  print $ maximum prods

getProds :: Grid -> (Point, Int64) -> [Int64]
getProds grid (coord, _) = [getRightProd grid coord, getDownProd grid coord, getDiagRightProd grid coord, getDiagLeftProd grid coord]

getRightProd :: Grid -> Point -> Int64
getRightProd grid (row,col)
  | col + 3 >= bound = 0
  | otherwise = (grid ! (row, col)) * (grid ! (row, col+1)) * (grid ! (row, col+2)) * (grid ! (row, col+3)) where
    ((_,_), (_, lastCol)) = bounds grid

getDownProd :: Grid -> Point -> Int64
getDownProd grid (row,col)
  | row + 3 >= bound = 0
  | otherwise = (grid ! (row, col)) * (grid ! (row+1, col)) * (grid ! (row+2, col)) * (grid ! (row+3, col)) where
    ((_,_), (lastRow, _)) = bounds grid

getDiagRightProd :: Grid -> Point -> Int64
getDiagRightProd grid (row,col)
  | col + 3 >= bound = 0
  | row + 3 >= bound = 0
  | otherwise = (grid ! (row, col)) * (grid ! (row+1, col+1)) * (grid ! (row+2, col+2)) * (grid ! (row+3, col+3)) where
    ((_,_), (lastRow, lastCol)) = bounds grid

getDiagLeftProd :: Grid -> Point -> Int64
getDiagLeftProd grid (row,col)
  | col - 3 < 0 = 0
  | row + 3 >= bound = 0
  | otherwise = (grid ! (row, col)) * (grid ! (row+1, col-1)) * (grid ! (row+2, col-2)) * (grid ! (row+3, col-3)) where
    ((_,_), (lastRow, lastCol)) = bounds grid


bound = 20
