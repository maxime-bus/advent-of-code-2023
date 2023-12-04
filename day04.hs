import           Data.List.Split
import qualified Data.Map        as Map
import           Data.Maybe

type WinningNumbers = [Int]
type ScratchCardNumbers = [Int]
type Winners = [Int]
type Id = Int
type ScratchCard = (Id, WinningNumbers, ScratchCardNumbers)

cardId :: ScratchCard -> Id
cardId (id, _, _) = id

winners :: ScratchCard -> Winners
winners (_, winningNumbers, numbers) = filter (\ wn -> wn `elem` numbers) winningNumbers

winnersScore :: Winners -> Int
winnersScore []  = 0
winnersScore [a] = 1
winnersScore n   = 2 * (winnersScore $ tail n)

pointsForCard :: ScratchCard -> Int
pointsForCard = winnersScore . winners

hasWon :: ScratchCard -> Bool
hasWon = (/= 0) . pointsForCard

parseCardLine :: String -> ScratchCard
parseCardLine line =
  let [firstHalf, secondHalf] = splitOn "|" line
      [cardId, winningNumbers] = splitOn ":" firstHalf
  in (read $ last $ words cardId, map read $ words winningNumbers, map read $ words secondHalf)

totalGainsForOneScratchCard :: Id -> Map.Map Id Winners -> Int
totalGainsForOneScratchCard cardId cards =
  case Map.lookup cardId cards of
    Nothing -> 0
    Just winners -> do
      let numberOfWinners = length winners
          nextCardsIds = [cardId + 1..cardId + numberOfWinners]

      numberOfWinners + (sum $ map (\ nextCardId -> totalGainsForOneScratchCard nextCardId cards) nextCardsIds)

totalGainsForAllScratchCards :: [ScratchCard] -> Map.Map Id Winners -> Int
totalGainsForAllScratchCards scratchCards m = length scratchCards + (sum $ map (\ cardId -> totalGainsForOneScratchCard cardId m) $ map cardId scratchCards)

computeMapOfScratchCards :: [ScratchCard] -> Map.Map Id Winners
computeMapOfScratchCards = foldr (\ scratchCard acc -> Map.insert (cardId scratchCard) (winners scratchCard) acc) Map.empty

main = do
  input <- lines <$> readFile "day04.input"

  let scratchCards = map parseCardLine input
      mapOfScratchCards = computeMapOfScratchCards scratchCards

  print $ sum $ map pointsForCard scratchCards

  print $ totalGainsForAllScratchCards scratchCards mapOfScratchCards
