data Shape = Circle Float | Retangle Float Float | Triangle Float Float Float 

area :: Shape -> Float 
area (Circle x) = pi*(x**2)
area (Retangle x y) = x*y
area (Triangle x y z) =  sqrt (p*(p-x)*(p-y)*(p-z))
                         where p = (x+y+z)/2

data Expr = Lit Int
            |Add Expr Expr
            |Sub Expr Expr

showExpr :: Expr -> String 
showExpr (Lit x) = show x
showExpr (Add x y) = "(" ++ showExpr x ++ " + " ++ showExpr y ++ ")"
showExpr (Sub x y) = "(" ++ showExpr x ++ " - " ++ showExpr y ++ ")"

data List t = Nil 
             | Cons t (List t)
    deriving Show
toList :: List t -> [t]
toList Nil = []
toList (Cons a as) = a : (toList as)

fromList :: [t] -> List t
fromList [] = Nil
fromList (a:as) = Cons a (fromList as)

data Tree t = None
             | Node t (Tree t) (Tree t)
             deriving (Show,Eq)

depth :: Tree t -> Int 
depth None = 0
depth (Node key tree1 tree2) = 1 + max (depth tree1) (depth tree2)

collapse :: Tree t -> [t]
collapse None = []
collapse (Node key t1 t2) = collapse t1 ++ collapse t2 ++ [key]

