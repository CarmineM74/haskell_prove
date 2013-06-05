module Ex05 where

-- Ex 5.1
maxOccurs :: Int -> Int -> (Int, Int)
maxOccurs m n 
  | m > n = (m,1)
  | n > m = (n,1)
  | otherwise = (n,2)

maxThreeOccurs :: Int -> Int -> Int -> (Int, Int)
maxThreeOccurs m n p 
  | q > p = (q,cnt1)
  | q == p = (q,cnt1+1)
  | q < p = (p,1)
	where
		(q,cnt1) = maxOccurs m n
		(r,cnt2) = maxOccurs q p

 -- Ex 5.2
 
minTwo :: Int -> Int -> Int
minTwo m n
    | m <= n = m
    | otherwise = n
   
maxTwo :: Int -> Int -> Int
maxTwo m n
    | m <= n = n
    | otherwise = m
 
minThree :: Int -> Int -> Int -> Int
minThree m n p = minTwo (minTwo m n) p

maxThree :: Int -> Int -> Int -> Int
maxThree m n p = maxTwo (maxTwo m n) p
 
middleThree :: Int -> Int -> Int -> Int
middleThree m n p 
    | m > m' && m < m'' = m
    | n > m' && n < m'' = n
    | p > m' && p < m'' = p
    | otherwise = m
    where
        m' = minThree m n p
        m'' = maxThree m n p
        
orderTriple :: (Int, Int, Int) -> (Int, Int, Int)
orderTriple (m,n,p) = ((minThree m n p),(middleThree m n p),(maxThree m n p))