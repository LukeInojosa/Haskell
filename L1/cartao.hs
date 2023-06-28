takeUntilEnd :: String -> String
takeUntilEnd [] = []
takeUntilEnd (a:as) = if isEnd a then [] else a:takeUntilEnd as
                      where isEnd a = a==';'

transformList :: String -> [(Int,String,String,Double)]
transformList [] = []
transformList str = (dia str,mes str,loja str,valor str): transformList (next str)
                    where 
                        dia str = read (take 2 str) :: Int
                        mes str = take 3 (drop 3 str)
                        loja str = takeUntilEnd (drop 7 str)
                        val str = takeUntilEnd (drop (8+ (length $ loja str)) str)
                        valor str = (read (val str)) :: Double
                        next str = drop (9+length (loja str)+ length (val str)) str
                        
fourth :: (x,y,z,w) -> w
fourth (a,b,c,d) = d  

second :: (x,y,z,w) -> y
second (a,b,c,d) = b

logMes :: String -> String -> Double
logMes mes str = foldl (+) 0 [fourth x|x<-bd, second x == mes]
                 where bd = transformList str

