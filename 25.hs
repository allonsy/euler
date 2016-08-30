module Main where

fib :: (Integer, Integer) -> Integer -> Integer
fib (a,b) count
  | length (show (a+b)) >= 1000 = count
  | otherwise = fib (b, a+b) (count + 1)


main :: IO ()
main = do
  print $ fib (1,1) 3
