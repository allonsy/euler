module Main where

import Data.List (sort)

chars :: String
chars = "012"

perms :: String -> [String]
perms [] = []
perms [c] = [[c]]
perms (x:xs) = concatMap (mix x) (perms xs)

insert :: Char -> Int -> String -> String
insert c 0 str = c:str
insert c _ [] = [c]
insert c k (x:xs) = x : (insert c (k-1) xs)

mixOrdered :: Char -> String -> [String]
mixOrdered c str = map (\pos -> insert c pos str) [0..length str]

mix :: Char -> String -> [String]
mix c str = mixHelper (\s -> s) str where
  mixHelper :: (String -> String) -> String -> [String]
  mixHelper f [] = [f [c]]
  mixHelper f oldStr@(x:xs) = f (c:oldStr) : mixHelper (\s -> f (x:s)) xs

sortedPerms :: String -> [String]
sortedPerms = sort . perms

main :: IO ()
main = do
  print $ (sortedPerms "0123456789") !! 999999
