module Main where


numberToString :: Int -> String
numberToString 1 = "one"
numberToString 2 = "two"
numberToString 3 = "three"
numberToString 4 = "four"
numberToString 5 = "five"
numberToString 6 = "six"
numberToString 7 = "seven"
numberToString 8 = "eight"
numberToString 9 = "nine"
numberToString 10 = "ten"
numberToString 11 = "eleven"
numberToString 12 = "twelve"
numberToString 13 = "thirteen"
numberToString 14 = "fourteen"
numberToString 15 = "fifteen"
numberToString 16 = "sixteen"
numberToString 17 = "seventeen"
numberToString 18 = "eighteen"
numberToString 19 = "nineteen"
numberToString 20 = "twenty"
numberToString 30 = "thirty"
numberToString 40 = "forty"
numberToString 50 = "fifty"
numberToString 60 = "sixty"
numberToString 70 = "seventy"
numberToString 80 = "eighty"
numberToString 90 = "ninety"
numberToString 1000 = "onethousand"
numberToString n = translate (breakUpNumber n) where
  translate [tens, ones]
    | ones /= 0 = numberToString (tens*10) ++ numberToString ones
    | otherwise = numberToString (tens*10)
  translate [hundreds, tens, ones]
    | tens == 0 && ones == 0 = numberToString hundreds ++ "hundred"
    | tens == 0 = numberToString hundreds ++ "hundredand" ++ numberToString ones
    | otherwise = numberToString hundreds ++ "hundredand" ++ numberToString (tens*10 + ones)

breakUpNumber :: Int -> [Int]
breakUpNumber n = map read strRep where
  strRep = map (\c -> [c]) $ show n



main :: IO ()
main = do
  let sumLength = sum $ map length $ map numberToString [1..1000]
  print $ sumLength
