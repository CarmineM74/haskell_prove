mysqrt :: Int -> Int
mysqrt n = head [ x | x <- [n,n-1..1], x*x <= n]

mysqrt' :: Int -> Int
mysqrt' n = mysqrt'' (n-1) n

mysqrt'' :: Int -> Int -> Int
mysqrt'' n m
  | (n*n) <= m = n
  | otherwise = mysqrt'' (n-1) m
  
