module Main where

import Data.List

powers :: [Integer]
powers = nub [a^b | a<- [2..100], b <- [2..100]]


main :: IO ()
main = do
  print $ length powers
