module Main where

import Data.Int

dimension = 1001

numRounds = dimension `quot` 2 + 1

diagTotal :: Int64
diagTotal = 1 + rounds 2 1 where
  rounds roundNum startNum
    | roundNum > numRounds = 0
    | otherwise = genRoundSum startNum diff + rounds (roundNum + 1) nextStart where
      diff = 2 * (roundNum - 1)
      nextStart = startNum + 4*diff

genRoundSum :: Int64 -> Int64 -> Int64
genRoundSum startNum diff = sum [startNum + diff, startNum + 2*diff + startNum + 3*diff, startNum + 4*diff]

main :: IO ()
main = do
  print $ diagTotal
