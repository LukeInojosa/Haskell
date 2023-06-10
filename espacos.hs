addEspacos :: Int -> [Char]
addEspacos 0  = ""
addEspacos n  = " " ++ addEspacos (n-1)

paraDireita :: Int -> [Char] -> [Char]
paraDireita n str = addEspacos n ++ str

vendas :: Int -> Int 
vendas 0 = 12
vendas 1 = 14
vendas 2 = 15
vendas 3 = 25
vendas 4 = 29
vendas 5 = 10
vendas 6 = 100
vendas 7 = 1
vendas 8 = 0 

totalVendas :: Int -> Int 
totalVendas n | n < 0 = 0
              | otherwise = vendas n + totalVendas (n-1)

mediaVendas :: Int -> Float 
mediaVendas n = fromIntegral (totalVendas n)/fromIntegral (n+1)

cabecalho = "Semana   Venda \n"
qtd = 3

imprimeSemanas :: Int -> [Char]
imprimeSemanas (-1) = ""
imprimeSemanas n = imprimeSemanas (n-1) ++ paraDireita qtd (show n) ++ paraDireita (2*qtd) (show (vendas n)) ++ "\n"

imprimeTotal :: Int -> [Char]
imprimeTotal n = "Total     " ++ show (totalVendas n) ++ "\n"

imprimeMedia n = "Media    " ++ show (mediaVendas n) ++ "\n"

imprimeTabela :: Int -> IO()
imprimeTabela n = putStr(cabecalho
                         ++ imprimeSemanas n
                         ++ imprimeTotal n
                         ++ imprimeMedia n)




