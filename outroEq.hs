data Estacao = Inverno | Verao
     
instance Eq Estacao where 
    (==) Inverno Verao = True 
    (==) Inverno Inverno = False
    (==) Verao Verao = False 
    (==) Verao Inverno = True 
