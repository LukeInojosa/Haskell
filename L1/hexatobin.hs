htob :: String -> String
htob [] = []
htob (a:as) = (convert a) ++ htob as
              where 
                convert :: Char -> String
                convert '0' = "0000"
                convert '1' = "0001"
                convert '2' = "0010" 
                convert '3' = "0011"
                convert '4' = "0100"
                convert '5' = "0101"
                convert '6' = "0110"
                convert '7' = "0111"
                convert '8' = "1000"
                convert '9' = "1001"
                convert 'A' = "1010"
                convert 'B' = "1011"
                convert 'C' = "1100"
                convert 'D' = "1101"
                convert 'E' = "1110"
                convert 'F' = "1111"
                convert _ = ""
