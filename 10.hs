import Helper

primeList :: [Int]
primeList = primeListIncrement 2 where
  primeListIncrement n
    | n >= 2000000 = []
    | isPrime n = n : primeListIncrement (n+1)
    | otherwise = primeListIncrement (n+1)

result = sum primeList

main :: IO ()
main = print result
