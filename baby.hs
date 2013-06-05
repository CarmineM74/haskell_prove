doubleMe x = x + x
doubleUs x y = x * 2 + y * 2
doubleSmallNumber x = if x > 100
                        then x
                        else x * 2

removeNonUpperCase :: [Char] -> [Char]
removeNonUpperCase st = [ c | c <- st, elem c ['A'..'Z']]

factorial :: Integer -> Integer
factorial n = product [1..n]