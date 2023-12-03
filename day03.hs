import           Data.Char (isLetter, isNumber)

data Coordinates = Coordinates { line :: Int, start :: Int, end :: Int } deriving Show

data SchematicItem
  = Part Int Coordinates
  | Symbol Char Coordinates
  deriving Show

isSchematicPart (Part _ _) = True
isSchematicPart _          = False

isSchematicSymbol (Symbol _ _) = True
isSchematicSymbol _            = False

partNumber (Part n _) = n
partNumber _          = 0

symbolValue (Symbol c _) = c
symbolValue _            = undefined

isBetween :: Int -> (Int, Int) -> Bool
isBetween n (lower, upper) = lower <= n && n <= upper

areTheyNear :: SchematicItem -> SchematicItem -> Bool
areTheyNear s1@(Symbol _ cs) s2@(Part _ cp) = areTheyNear s2 s1
areTheyNear (Part _ cp) (Symbol _ cs) =
  (line cp + 1 == line cs) && ((end cp + 1 == start cs) || (start cp - 1 == start cs) || start cs `isBetween` (start cp, end cp))
  || (line cp == line cs) && ((end cp + 1 == start cs) || (start cp - 1 == start cs))
  || (line cp - 1 == line cs) && ((end cp + 1 == start cs) || (start cp - 1 == start cs) || start cs `isBetween` (start cp, end cp))
  || False

isDot :: Char -> Bool
isDot c = c == '.'

isSymbol :: Char -> Bool
isSymbol c = (not . isLetter $ c) && (not . isNumber $ c) && (not . isDot $ c)

parseLine :: String -> Int -> [SchematicItem]
parseLine line lineNumber = parseLine' line 0 []
  where
    parseLine' :: String -> Int -> [SchematicItem] -> [SchematicItem]
    parseLine' "" _ result = result
    parseLine' line@(l:line') counter result
      | isSymbol l = let symbol = parseSymbol l counter in parseLine' line' (counter + 1) (symbol:result)
      | isNumber l = let (charactersConsumed, part) = parsePart line counter
                     in parseLine' (drop charactersConsumed line) (counter + charactersConsumed) (part:result)
      | otherwise  = parseLine' line' (counter + 1) result

    parsePart :: String -> Int -> (Int, SchematicItem)
    parsePart s currentPosition = let num = takeWhile isNumber s
                                      charactersConsumed = length num
                                  in (charactersConsumed, Part (read num) (Coordinates {line = lineNumber, start = currentPosition, end = currentPosition + charactersConsumed - 1}))

    parseSymbol :: Char -> Int -> SchematicItem
    parseSymbol s currentPosition = Symbol s (Coordinates { line = lineNumber, start = currentPosition, end = currentPosition })

parseLines :: String -> [SchematicItem]
parseLines s = parseLines' (lines s) 0 []
  where
    parseLines' :: [String] -> Int -> [SchematicItem] -> [SchematicItem]
    parseLines' [] _ result = result
    parseLines' (line:lines) lineNumber result = parseLines' lines (lineNumber + 1) ((parseLine line lineNumber) ++ result)

isAGear :: SchematicItem -> [SchematicItem] -> (Bool, [SchematicItem])
isAGear symbol parts
  | symbolValue symbol == '*' = let nearParts = filter (areTheyNear symbol) parts
                                in if length nearParts == 2 then (True, nearParts) else (False, [])
  | otherwise = (False, [])

main = do
  input <- readFile "day03.input"

  let schematicItems = parseLines input
      parts = filter isSchematicPart schematicItems
      symbols = filter isSchematicSymbol schematicItems
      partsNearSymbols = filter (isPartNearAnySymbol symbols) parts

      gears = filter (\ (b, _) -> b == True) $ map (\ symbol -> isAGear symbol parts) symbols

      isPartNearAnySymbol :: [SchematicItem] -> SchematicItem -> Bool
      isPartNearAnySymbol symbols part = any (areTheyNear part) symbols

  print $ sum $ map partNumber partsNearSymbols

  print $ sum $ map ( \ [g1, g2] -> partNumber g1 * partNumber g2 ) $ map snd gears
