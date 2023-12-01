import Data.Char

main = do
  day1Data <- readFile "day01.input"
  print $ sum $ map read $ map (\x -> head x:last x:[]) $ map (filter isDigit) $ lines day1Data
 
