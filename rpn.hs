import Data.List

solveRPN :: String -> Float
solveRPN = head . foldl compute [] . words
    where
        compute (x:y:ys) "+" = (x+y):ys
        compute (x:y:ys) "-" = (y-x):ys
        compute (x:y:ys) "*" = (x*y):ys
        compute (x:y:ys) "/" = (y/x):ys
        compute (x:y:ys) "^" = (y ** x):ys
        compute (x:xs) "ln" = log x:xs
        compute xs "sum" = [sum xs]
        compute xs numberString = read numberString:xs
--        compute (x:y:ys) tk
--            | tk == "+" = (x+y):ys
--            | tk == "-" = (y-x):ys
--            | tk == "*" = (x*y):ys
--            | otherwise = (read tk):(x:y:ys)
-- Con le guards non funziona perchè dice che il pattern non è esaustivo 
-- Infatti sono coperti SOLO i casi in cui lo stack NON è vuoto ed è costituito
-- da ALMENO 2 elementi!!!