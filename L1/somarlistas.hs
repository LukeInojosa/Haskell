main :: IO ()

main = do
       input1 <- getLine
       input2 <- getLine
       let result = somarListas (read input1 :: [Int])  (read input2 :: [Int])
       print result


soma :: [Int] -> [Int] -> Int -> [Int]

soma [] [] rest | rest == 1 =  [1]
                | rest == 0 =  []
soma [] b rest  = soma [] (init b) ((last b + rest) `div` 10) ++ [(last b + rest) `mod` 10]
soma a [] rest  = soma (init a) [] ((last a + rest) `div` 10) ++ [(last a + rest) `mod` 10]
soma a b rest = soma (init a) (init b) ((last a + last b + rest) `div` 10) ++ [(last a + last b + rest) `mod` 10] 

somarListas :: [Int] -> [Int] -> [Int]
somarListas a b =  soma a b 0

              
              