import Data.Map
decEnigma :: String -> [(Char, Char)] -> String
decEnigma  str tradutor =  [fromList tradutor ! x| x <- str]