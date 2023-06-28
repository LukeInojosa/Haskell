fold :: (Eq b) => (Eq a) => (a -> b -> b) -> b -> [a] -> b
fold f o [] = o
fold f o (a:as) = f a (fold f o as)
