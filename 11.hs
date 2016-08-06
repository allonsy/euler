import Data.Array

arrayify :: [[Int]] -> Array (Int, Int) Int
arrayify rows where
  rowTransform = map (zip [1..numColumns]) rows
  columnTransform = map (zipWith [1..Rows])
  numColumns = length (head rows)
  numRows = length rows

main :: IO ()
main = do
  contents <- readFile "11.txt"
  let linedContent = lines contents
  let nums = map (map read) $ map words linedContent :: [[Int]]
  let numArray = arrayify nums
  print nums
