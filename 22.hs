module Main where

import Data.List
import Data.Char
import Data.Int

charValue :: Char -> Int64
charValue c = fromIntegral $ ord c - aVal where
  aVal = (ord 'A') - 1

nameToValue :: String -> Int64
nameToValue str = sum $ map charValue str



main :: IO ()
main = do
  contents <- readFile "22.txt"
  let names = read contents :: [String]
  let sortedNames = sort names
  let numberNames = map nameToValue sortedNames
  let zippedNames = zip [1..(fromIntegral (length numberNames))] numberNames :: [(Int64, Int64)]
  let product = map (\(ind, val) -> ind * val) zippedNames :: [Int64]
  print $ sum product
