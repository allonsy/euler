module Main where

main :: IO ()
main = do
  print "hello"

-- longDivide 1 20 = 1 / 20
longDivide :: Int -> Int -> [Int]
longDivide num denom
  | num < denom = 0 : longDivide (num*10) denom
  | otherwise = dividers : (longDivide (num - denom * dividers) denom) where
      dividers = num `quot` denom
