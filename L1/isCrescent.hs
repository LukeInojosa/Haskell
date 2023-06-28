vendas :: Int -> Int
vendas 0 = 1 
vendas 1 = 2
vendas 2 = 3
vendas 3 = 4
vendas 4 = 5
vendas 5 = 6
vendas 6 = 7
vendas 7 = 8
vendas 8 = 9
vendas 9 = 10
vendas 10 = 11
vendas 11 = 9
vendas 12 = 13
vendas 13 =  14
vendas _ = 100

isCrescent :: (Int -> Int) -> Int -> Bool
isCrescent func 0 = True
isCrescent func n = isCrescent func (n-1) && func (n-1) <= func n