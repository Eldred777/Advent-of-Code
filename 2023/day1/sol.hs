import Data.Char (isNumber)

first :: [a] -> a
first [] = error "empty list passed to `first`"
first (x : _) = x

-- converts written numbers into their numeric counterparts
convertToNumber :: String -> String
convertToNumber [] = []
-- convertToNumber ('z' : 'e' : 'r' : 'o' : xs) = '0' : convertToNumber xs
convertToNumber xss@('o' : 'n' : 'e' : xs) = '1' : convertToNumber (tail xss)
convertToNumber xss@('t' : 'w' : 'o' : xs) = '2' : convertToNumber (tail xss)
convertToNumber xss@('t' : 'h' : 'r' : 'e' : 'e' : xs) = '3' : convertToNumber (tail xss)
convertToNumber xss@('f' : 'o' : 'u' : 'r' : xs) = '4' : convertToNumber (tail xss)
convertToNumber xss@('f' : 'i' : 'v' : 'e' : xs) = '5' : convertToNumber (tail xss)
convertToNumber xss@('s' : 'i' : 'x' : xs) = '6' : convertToNumber (tail xss)
convertToNumber xss@('s' : 'e' : 'v' : 'e' : 'n' : xs) = '7' : convertToNumber (tail xss)
convertToNumber xss@('e' : 'i' : 'g' : 'h' : 't' : xs) = '8' : convertToNumber (tail xss)
convertToNumber xss@('n' : 'i' : 'n' : 'e' : xs) = '9' : convertToNumber (tail xss)
convertToNumber xss@(x : xs) = x : convertToNumber (tail xss)

charToInt :: Char -> Int
-- charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9
charToInt _ = error "Bad char input"

constructLineValue :: String -> Int
constructLineValue [] = error "Empty string passed to `constructLineValue`"
constructLineValue x = first xInt * 10 + last xInt
  where
    xInt = map charToInt x

part1 :: IO ()
part1 = do
  content <- readFile "2023/day1/input"
  let contentLines = lines content
  let numbers = map (filter isNumber) contentLines
  let values = map constructLineValue numbers
  let value = sum values
  print value

part2 :: IO ()
part2 = do
  content <- readFile "2023/day1/input"
  let contentLines = lines content
  let convertedLines = map convertToNumber contentLines
  let numbers = map (filter isNumber) convertedLines
  let values = map constructLineValue numbers
  print $ take 50 $ zip contentLines values
  let value = sum values
  print value

main :: IO ()
main = do
  part1
  part2