


somarMultiplos :: [Int] -> Int -> [Int]
somarMultiplos [] r = []
somarMultiplos list 0 = [0|_<-list]
somarMultiplos list r = [if x > 0 then r*(((1+(div x r))*(div x r)) `div` 2) else 0| x<-list]


