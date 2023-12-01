import Data.Char
import Data.List

stringToDigit :: String -> String
stringToDigit "" = ""
stringToDigit s
  | "one"   `isPrefixOf` s = "1" ++ (stringToDigit $ tail s)
  | "two"   `isPrefixOf` s = "2" ++ (stringToDigit $ tail s)
  | "three" `isPrefixOf` s = "3" ++ (stringToDigit $ tail s)
  | "four"  `isPrefixOf` s = "4" ++ (stringToDigit $ tail s)
  | "five"  `isPrefixOf` s = "5" ++ (stringToDigit $ tail s)
  | "six"   `isPrefixOf` s = "6" ++ (stringToDigit $ tail s)
  | "seven" `isPrefixOf` s = "7" ++ (stringToDigit $ tail s)
  | "eight" `isPrefixOf` s = "8" ++ (stringToDigit $ tail s)
  | "nine"  `isPrefixOf` s = "9" ++ (stringToDigit $ tail s)
  | otherwise = head s : (stringToDigit $ tail s)
  
main = do
  day1Data <- readFile "day01.input"
  print $ sum $ map read $ map (\x -> head x:last x:[]) $ map (filter isDigit) $ lines day1Data

  print $ sum $ map read $ map (\x -> head x:last x:[]) $ map (filter isDigit) $ map stringToDigit $ lines day1Data
 
