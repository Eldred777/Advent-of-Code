numeric :: Char -> Bool
numeric '-' = True
numeric '0' = True
numeric '1' = True
numeric '2' = True
numeric '3' = True
numeric '4' = True
numeric '5' = True
numeric '6' = True
numeric '7' = True
numeric '8' = True
numeric '9' = True
numeric _ = False

annihilateNonNumeric :: Char -> Char
annihilateNonNumeric x
  | numeric x = x
  | otherwise = ' '

main :: IO ()
main = do
  file <- readFile "2015/day12/input"
  (print . sum . map (\x -> read x :: Int) . words . map annihilateNonNumeric) file -- part 1
