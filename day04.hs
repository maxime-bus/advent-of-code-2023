import           Data.List.Split

type WinningNumbers = [Int]
type ScratchCardNumbers = [Int]
type Winners = [Int]
type ScratchCard = (WinningNumbers, ScratchCardNumbers)

winners :: ScratchCard -> Winners
winners (winningNumbers, numbers) = filter (\ wn -> wn `elem` numbers) winningNumbers

winnersScore :: Winners -> Int
winnersScore []  = 0
winnersScore [a] = 1
winnersScore n   = 2 * (winnersScore $ tail n)

pointsForCard :: ScratchCard -> Int
pointsForCard = winnersScore . winners

parseCardLine :: String -> ScratchCard
parseCardLine line =
  let [firstHalf, secondHalf] = splitOn "|" line
      [_, winningNumbers] = splitOn ":" firstHalf
  in (map read $ words winningNumbers, map read $ words secondHalf)



main = do
  input <- lines <$> readFile "day04.input"

  print $ sum $ map pointsForCard $ map parseCardLine input
