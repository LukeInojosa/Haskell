main = do
  n <- readLn
  x <- getLine
  print $ remDiv (n :: Int) (words x)

remDiv :: Int -> [a] -> ([a],[a])
remDiv n l = (take (n-1) l, drop n l)