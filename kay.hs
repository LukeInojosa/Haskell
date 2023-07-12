import Data.List

logMes :: String -> String -> Double
logMes [] _ = 0
logMes _ [] = 0
logMes str str2
  | element == str = foldl (+) 0 toDoubleList
  | otherwise = logMes str (tail str2)
  where
    stringList = splitInStrings str2
    element = head stringList
    toDoubleList = [read (stringList !! (n + 2)) :: Double | n <- elemIndices str stringList]

splitInStrings :: String -> [String]
splitInStrings [] = [""]
splitInStrings (c : cs)
  | c == ';' = "" : rest
  | c == ' ' = "" : rest
  | otherwise = (c : head rest) : tail rest
  where
    rest = splitInStrings cs
    
main = do
  a <- getLine
  b <- getLine
  let result = logMes a b
  print result