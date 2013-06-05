mymult :: Int -> Int -> Int
mymult n m
  | m == 0 = 0
  | otherwise = n + mymult n (m-1)
