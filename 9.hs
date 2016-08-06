thousand = [1..1000]
thousandTriplet = [(a,b,c) | a <- thousand, b <- thousand, c <- thousand]

result :: [(Int, Int, Int)] -> Int
result [] = error "Not Found!"
result ((a, b, c):xs)
  | a + b + c == 1000 && a^2 + b^2 == c^2 = a * b * c
  | otherwise = result xs

main :: IO ()
main = print $ result thousandTriplet
