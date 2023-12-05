import           Data.List.Extra
import           Data.Maybe
import           Test.Hspec

data Map = Map Int Int Int deriving Show

parseSeeds :: String -> [Int]
parseSeeds firstLine = map read $ words $ last $ splitOn ":" firstLine

parseMapping :: String -> Map
parseMapping line = Map dest src range
  where [dest, src, range] = map read $ words line

parseMappings :: [String] -> [Map]
parseMappings lines = map parseMapping lines

parseInput :: String -> ([Int], [[Map]])
parseInput input = (seeds'', maps)
  where (seeds':input') = splitOn "\n\n" input
        seeds'' = parseSeeds seeds'
        input'' = map (drop 1) $ map lines $ input'
        maps = map parseMappings input''

withinRange :: Int -> Map -> Bool
withinRange from (Map dest src range) = src <= from && from < src + range

applyMapping :: Int -> Map -> Maybe Int
applyMapping from m@(Map dest src range)
 | withinRange from m = Just $ from + dest - src
 | otherwise = Nothing

applyCategoryMappings :: Int -> [Map] -> Int
applyCategoryMappings from mappings =
  case mapMaybe (applyMapping from) mappings of
    []      -> from
    results -> head results

applyCategoriesMappings :: Int -> [[Map]] -> Int
applyCategoriesMappings from mappings = foldl applyCategoryMappings from mappings

part1 :: [Int] -> [[Map]] -> Int
part1 seeds maps = minimum $ map (`applyCategoriesMappings` maps) seeds

main = do
  input <- readFile "day05.input"

  let (seeds, maps) = parseInput input

  print $ part1 seeds maps

tests :: IO()
tests = hspec $ do
  describe "Mapping a seed" $ do
    it "applyMapping" $ do
      applyMapping 79 (Map 50 98 2) `shouldBe` Nothing
      applyMapping 79 (Map 52 50 48) `shouldBe` Just 81

    it "applyCategoryMappings" $ do
      applyCategoryMappings 79 [Map 50 98 2, Map 52 50 48] `shouldBe` 81

    let sampleMaps
          = [ [Map 50 98 2, Map 52 50 48]
            , [Map 0 15 37, Map 37 52 2, Map 39 0 15]
            , [Map 49 53 8, Map 0 11 42, Map 42 0 7, Map 57 7 4]
            , [Map 88 18 7, Map 18 25 70]
            , [Map 45 77 23, Map 81 45 19, Map 68 64 13]
            , [Map 0 69 1, Map 1 0 69]
            , [Map 60 56 37, Map 56 93 4]
            ]

    it "applyCategoriesMappings" $ do
      applyCategoriesMappings 79 sampleMaps  `shouldBe` 82

    it "part1 example" $ do
      part1 [79, 14, 55, 13] sampleMaps `shouldBe` 35




