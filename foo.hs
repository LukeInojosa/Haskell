foo :: Integer -> Integer
foo 0 = 16
foo 1 = 3
foo n
  | n < 0             = 0
  | n `mod` 17 == 2   = -43
  | otherwise         = n +3