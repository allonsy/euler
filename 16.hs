module Main where


bigInt :: Integer
bigInt = 2^1000


main :: IO ()
main = do
  let strRep = show bigInt
  let digits = map read (map (\c -> [c]) strRep) :: [Int]
  let total = sum digits
  print total
