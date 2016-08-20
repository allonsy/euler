module Main where


main :: IO ()
main = do
  contents <- readFile "13.txt"
  let intStrings = lines contents
  let ints = map read intStrings :: [Integer]
  let total = sum ints
  let totalString = show total
  print $ take 10 totalString
