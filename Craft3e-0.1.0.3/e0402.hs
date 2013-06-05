weakAscendingOrder :: Integer -> Integer -> Integer -> Bool
weakAscendingOrder m n p = (m <= n) && (n <= p)

between :: Integer -> Integer -> Integer -> Bool
between m n p = (weakAscendingOrder m n p) || (all (==True) [m-n>=0,n-p>=0])

howManyEqual :: Integer -> Integer -> Integer -> Integer
howManyEqual m n p = toInteger(length . filter (==0) $ [toInteger(x-y) | x <- [m,n,p], y <- [m,n,p]]) - toInteger(3)

