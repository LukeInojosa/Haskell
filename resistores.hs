data Resistor = Ohms Double 
    deriving (Show, Eq)

(//) :: Resistor -> Resistor -> Resistor 
(//) (Ohms r1) (Ohms r2) = Ohms (product/soma)
            where 
                product = r1*r2
                soma = r1+r2
instance Num Resistor where
    (+) (Ohms r1) (Ohms r2) = Ohms (r1+r2)
    (*) _ _ = error "Multiplication not supported for MeuTipo"
    abs _ = error "abs not supported for MeuTipo"
    signum _ = error "signum not supported for MeuTipo"
    fromInteger _ = error "fromInteger not supported for MeuTipo"
    negate _ = error "negate not supported for MeuTipo"


