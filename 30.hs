module Main where

power :: Int
power = 5

isSumDigits :: Int -> Bool
isSumDigits n = sum fifthPowers == n where
  fifthPowers = map (^power) digits
  digits = map read $ map (\c -> [c]) (show n)

allNums :: [Int]
allNums = filter isSumDigits [2..1000000]

main :: IO ()
main = print $ sum allNums
