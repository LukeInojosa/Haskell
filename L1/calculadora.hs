type Comando = String
type Valor = Int

main = do
    a <- getLine
    let result = executa (read a)
    print result
    
executa :: [(Comando, Valor)] -> Int
executa comandos | comandos == [] =  0
                 | not (null [comando|comando<-comandos, comando == ("Divide",0)]) = -666
                 | otherwise = operacao (fst $ last comandos) (executa $ init comandos) (snd $ last comandos)

operacao :: String -> (Int -> Int -> Int)
operacao a = case a of
            "Soma" -> (+)
            "Subtrai" -> (-)
            "Multiplica" -> (*)
            "Divide" -> (div)   