type Vertice = Int
type Graph = [(Vertice,Vertice)]

suc :: Graph -> Vertice -> Vertice
suc [] v = -1
suc ((i,j):as) v | i == v = j
                 | otherwise = suc as v

path :: Graph -> Vertice -> Vertice -> Bool
path [] i j = False
path graph i j  = if 