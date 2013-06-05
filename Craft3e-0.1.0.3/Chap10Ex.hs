module Chap10Ex where

-- 10.2
returnOne :: a -> Int
returnOne _ = 1

mylength :: [a] -> Int
mylength = sum . map returnOne

-- 10.6
squares :: [Int] -> [Int]
squares = map (^2)

sumsquares :: [Int] -> Int
sumsquares = sum . squares

allGTzero :: [Int] -> Bool
allGTzero ns = length (filter (>0) ns) == length ns

-- 10.7
iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)

ins :: Int -> [Int] -> [Int]
ins x [] = [x]
ins x (y:ys)
    | x <= y = x:(y:ys)
    | otherwise = y : (ins x ys)
    
isSorted :: [Int] -> Bool
isSorted xs = (iSort xs) == xs  
  
minimum' :: (Int -> Int) -> [Int] -> Int
minimum' f xs = head $ iSort (map f xs)

allEqual :: (Int -> Int) -> [Int] -> Bool
allEqual f xs = (length $ filter (== h) ys) == length ys
    where
        ys = map f xs
        h = head ys
        
multiple' :: (Int -> Int) -> [Int] -> Bool
multiple' f xs = allGTzero ys && isSorted ys
    where
        ys = map f xs

-- 10.9
iter :: Int -> (Int -> Int) -> Int -> Int
iter 0 f x = x
iter n f x = f (iter (n-1) f x)

iter' :: Int -> (a -> a) -> a -> a
iter' 0 f x = x
iter' n f x = f (iter' (n-1) f x)

-- 10.10
double :: Int -> Int
double x = 2 * x

powertwo :: Int -> Int
powertwo n = iter n double 1

-- 10.15 unZip, last, init
unzip' :: [(a,b)] -> ([a],[b])
unzip' = foldr f ([],[])
    where
        f (a,b) ([],[]) = ([a],[b])
        f (a,b) (l,r) = (a:l,b:r)

last' :: [a] -> a
last' = head . foldr f [] 
    where
        f x [] = [x]
        f _ [x] = [x]

-- Greggery Peccery --> Greggery Peccer
init' :: [a] -> [a]
init' xs = foldr f [] (zip xs [1..])
    where
        len = length xs
        f (x,pos) acc
            | pos < len = x : acc
            | otherwise = acc

-- 10.17
formatList :: (a -> String) -> [a] -> String
formatList f xs = foldr (++) "" $ map f xs

-- 10.18
-- takeWhile p xs ++ (tail $ dropWhile p xs)
-- When "all p xs" this will throw an exception because of standard prelude
-- tail not handling [] lists.

filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst f xs = undefined

filterLast ::  (a -> Bool) -> [a] -> [a]
--filterLast = reverse . reverse . filterFirst 
filterLast = undefined

-- 10.20
switchMap :: (a -> b) -> (a -> b) -> [a] -> [b]
switchMap f g xs = foldr h [] indexed
    where
        indexed = zip xs [0..]
        h (x,idx) acc
            | even idx = f x : acc
            | otherwise = g x : acc

switchMap' :: (a -> b) -> (a -> b) -> [a] -> [b]
switchMap' f g xs = map h indexed
    where
        indexed = zip xs [0..]
        h (x,idx)
            | even idx = f x 
            | otherwise = g x 

-- 10.21

switchMap'' :: (a -> b -> b) -> (a -> b -> b) -> b -> [a] -> b
switchMap'' f g start xs = foldr h start indexed
    where 
        indexed = zip xs [0..]
        h (x,idx) acc
            | even idx = f x acc
            | otherwise = g x acc

split :: [a] -> ([a],[a])
split xs = switchMap'' f g ([],[]) xs
    where
        f x (l,r) = (x:l,r)
        g x (l,r) = (l,x:r)
        
merge :: ([a],[a]) -> [a]
merge (ls,rs) = merge' ls rs
    where
        merge' xs [] = xs
        merge' [] ys = ys
        merge' (x:xs) (y:ys) = x:y:merge' xs ys
        