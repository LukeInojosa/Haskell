mapping :: [Int] -> [Int]
mapping vetor = map (\x -> x^2) vetor

folding :: [Int] -> Int
folding vetor = foldr (+) 0 (mapping vetor)

filtering :: [Int] -> [Int]
filtering vetor = filter (> 0) vetor  