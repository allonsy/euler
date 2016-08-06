isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = checkDivisors 2 where
  squareRoot = floor $ sqrt $ fromIntegral n
  checkDivisors k
    | k > squareRoot = True
    | n `mod` k == 0 = False
    | otherwise = checkDivisors (k+1)

primeList :: [Int]
primeList = primeListIncrement 2 where
  primeListIncrement n
    | n >= 2000000 = []
    | isPrime n = n : primeListIncrement (n+1)
    | otherwise = primeListIncrement (n+1)

result = sum primeList

main :: IO ()
main = print result
