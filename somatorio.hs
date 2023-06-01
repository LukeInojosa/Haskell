somatorio :: Int -> Int
somatorio 0 = 0
somatorio n = n + somatorio (n-1)
