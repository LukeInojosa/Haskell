import Data.Char

main = do
  a <- getLine
  let result = sumNumbers a
  print result

sumNumbers :: [Char] -> Int
sumNumbers [] = 0
sumNumbers (a:as) = if isDigit a 
                    then (fromEnum a - 48) + sumNumbers as
                    else sumNumbers as

--runtime error
sumNum :: [Char] -> Int
sumNum str = foldr (+) 0 [fromEnum x - 48| x <- str, isDigit x]
