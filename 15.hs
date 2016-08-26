module Main where

import Data.Array

upperBound :: Int
upperBound = 21

pathArray :: Array (Int, Int) Int
pathArray = array ((1,1), (upperBound, upperBound)) (((upperBound, upperBound), 1) : assocs) where
  assocs = [((x,y), dynamicPointValue x y) | x <- [1..upperBound], y <- [1..upperBound], (x,y) /= (upperBound, upperBound)]

dynamicPointValue :: Int -> Int -> Int
dynamicPointValue x y = xextension + yextension where
  xextension = if (x+1) <= upperBound then pathArray ! (x+1,y) else 0
  yextension = if (y+1) <= upperBound then pathArray ! (x, y+1) else 0


main :: IO ()
main = do
  print $ pathArray ! (1,1)
