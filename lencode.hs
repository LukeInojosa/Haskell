rLenDecode0 :: [Int] -> [Int]
rLenDecode0 [] = []
rLenDecode0 (0:a:as) = (take a (cycle [0]) ++ rLenDecode0 as)
rLenDecode0 (a:as) = a : rLenDecode0 as


                       
rLenCode0 :: [Int] -> [Int]
rLenCode0 [] = []
rLenCode0 (0:as) = let c = (compress as) + 1 in  0:c:rLenCode0 (drop (c-1) as)
                where 
                    compress [] = 0
                    compress (0:bs) = 1 + compress bs
                    compress _ = 0     
rLenCode0 (a:as) = a:(rLenCode0 as)


rLenCodeLetras :: String -> String
rLenCodeLetras [] = []
rLenCodeLetras (a:as) | compress a as == 0 = a:rLenCodeLetras as
                      | otherwise =  let n = compress a as + 1 in a:((show n) ++ rLenCodeLetras (drop (n-1) as))
                where 
                    compress :: Char -> String -> Int
                    compress a [] = 0
                    compress a (b:bs) | b == a = 1 + compress a bs
                                      | otherwise = 0

isDigit :: Char -> Bool
isDigit a = a >= '0' && a <= '9'

rLenDecodeLetras :: String -> String
rLenDecodeLetras (a:[]) = a:[]
rLenDecodeLetras (a:b:as) | isDigit b = (take (read [b] :: Int) (cycle [a]) ++ rLenDecodeLetras as)
                          | otherwise = a : rLenDecodeLetras (b:as)

data Letra = Unica Char | Repetida Char Int 
            deriving Show
        
rLenDecodeLetrasCodigo :: String -> [Letra]
rLenDecodeLetrasCodigo (a:[]) = (Unica a):[]
rLenDecodeLetrasCodigo (a:b:as) | isDigit b = (Repetida a (read [b] :: Int) : rLenDecodeLetrasCodigo as)
                                | otherwise = (Unica a):rLenDecodeLetrasCodigo (b:as)

rLenCodeLetrasCodigo :: String -> [Letra]
rLenCodeLetras [] = []
rLenCodeLetras (a:as) | compress a as == 0 = (Unica a):rLenCodeLetras as
                      | otherwise =  let n = compress a as + 1 in a:((show n) ++ rLenCodeLetras (drop (n-1) as))
                where 
                    compress :: Char -> String -> Int
                    compress a [] = 0
                    compress a (b:bs) | b == a = 1 + compress a bs
                                      | otherwise = 0