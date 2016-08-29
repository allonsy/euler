module Main where

lastDay :: String -> Int -> Int
lastDay "Jan" _ = 31
lastDay "Feb" year
  | year `mod` 4 == 0 = 29
  | otherwise = 28
lastDay "March" _ = 31
lastDay "April" _ = 30
lastDay "May" _ = 31
lastDay "June" _ = 30
lastDay "July" _ = 31
lastDay "Aug" _ = 31
lastDay "Sep" _ = 30
lastDay "Oct" _ = 31
lastDay "Nov" _ = 30
lastDay "Dec" _ = 31

nextMonth :: String -> Int -> (String, Int)
nextMonth "Jan" y = ("Feb", y)
nextMonth "Feb" y = ("March", y)
nextMonth "March" y = ("April", y)
nextMonth "April" y = ("May", y)
nextMonth "May" y = ("June", y)
nextMonth "June" y = ("July", y)
nextMonth "July" y = ("Aug", y)
nextMonth "Aug" y = ("Sep", y)
nextMonth "Sep" y = ("Oct", y)
nextMonth "Oct" y = ("Nov", y)
nextMonth "Nov" y = ("Dec", y)
nextMonth "Dec" y = ("Jan", y+1)

-- (Month, Day, Year)
addWeek :: (String, Int, Int) -> (String, Int, Int)
addWeek (mon, day, year)
  | day+7 <= lastDay mon year = (mon, day+7, year)
  | otherwise = let
    (newMonth, newYear) = nextMonth mon year
    in
      (newMonth, day+ 7 - (lastDay mon year), newYear)

countSundays :: (String, Int, Int) -> Int -> Int
countSundays date@(mon, day, year) curCount
  | year > 2000 = curCount
  | day == 1 = countSundays (addWeek date) (curCount + 1)
  | otherwise = countSundays (addWeek date) curCount


main :: IO ()
main = do
  print $ countSundays ("Jan", 6, 1901) 0
