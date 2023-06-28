esta_em :: (Eq a) => a -> [a] -> Bool
esta_em _ [] = False
esta_em e (a:as) = e == a || esta_em e as

conjunto :: (Eq a) => [a] -> [a]
conjunto [] = []
conjunto (a:as) | a `esta_em` as = conjunto as
                | otherwise = a : conjunto as

isAsc :: [Int] -> Bool
isAsc [] = True
isAsc (a:b:as) | null as = True
               | otherwise = a <= b && isAsc (b:as)

hasPath :: [Int,Int] -> Int -> Int -> Bool

