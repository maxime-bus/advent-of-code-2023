import           Data.List.Split (splitOn)

data Bag = Bag
  { loadedRedCubes   :: Int
  , loadedGreenCubes :: Int
  , loadedBlueCubes  :: Int
  } deriving (Show)

data Set = Set
  { redCubes   :: Int
  , greenCubes :: Int
  , blueCubes  :: Int
  } deriving (Show)

data Game = Game
  { gameId :: Int
  , sets   :: [Set]
  } deriving (Show)

isSetPlayable :: Set -> Bag -> Bool
isSetPlayable set bag =
  (redCubes set <= loadedRedCubes bag)
  && (greenCubes set <= loadedGreenCubes bag)
  && (blueCubes set <= loadedBlueCubes bag)

isGamePlayable :: Bag -> Game -> Bool
isGamePlayable bag game = foldr (\ set acc -> acc && isSetPlayable set bag) True (sets game)

parseColor :: String -> String -> Int
parseColor _ "" = 0
parseColor color s = let s' = filter (/= ',') s
  in case words s' of
    (x:c:_) -> if c == color then read x else parseColor color $ tail s'
    _       -> 0

parseRed = parseColor "red"

parseGreen = parseColor "green"

parseBlue = parseColor "blue"

parseSet :: String -> Set
parseSet s = Set { redCubes = parseRed s, greenCubes = parseGreen s, blueCubes = parseBlue s }

parseSets :: String -> [Set]
parseSets s = let sets = splitOn ";" s
  in map parseSet sets

parseGame :: String -> Game
parseGame s =
  let [game, sets] = splitOn ":" s
      [_, gameId] = words game
  in Game { gameId = read gameId,  sets = parseSets sets }


main = do
  input <- readFile "day02.input"

  let bag = Bag { loadedRedCubes = 12, loadedGreenCubes = 13, loadedBlueCubes = 14 }
      games = map parseGame $ lines input
      possibleGames = filter (isGamePlayable bag) games
      
  print $ sum $ map gameId possibleGames
