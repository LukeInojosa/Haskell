main = do
    a <- getLine
    let result = minMaxCartao a
    print result


ehNumero :: Char -> Bool
ehNumero n = if n >= '0' && n <= '9' then True else False

takeUntilEnd :: String -> String
takeUntilEnd [] = []
takeUntilEnd (a:as) = if isEnd a then [] else a:takeUntilEnd as
                    where isEnd a = a==';'

gastos :: String -> [Double]
gastos [] = []
gastos str = ((read $ numero str) :: Double) : gastos (drop (5 + (length $ numero str)) str)
             where 
                numero str = takeUntilEnd (drop 4 str)


minMaxCartao :: String -> (Double, Double)
minMaxCartao str = ( minimum $ filtroGastos, maximum $ filtroGastos)
                    where filtroGastos =  gastos [x|x<-str, ehNumero x || x==';' || x == '.']