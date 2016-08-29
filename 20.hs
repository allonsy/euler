module Main where

factorial :: Integer -> Integer
factorial n = factorialHelper n (n-1) where
  factorialHelper val k
    | k == 1 = val
    | otherwise = factorialHelper (val * k) (k-1)

bigFactorial :: Integer
bigFactorial = factorial 100

main :: IO ()
main = do
  let hundredFact = show bigFactorial
  let digits = map read (map (\c -> [c]) hundredFact) :: [Int]
  let total = sum digits
  print total
