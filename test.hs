factorial n = product [1..n]
average ns = sum ns `div` length ns

script_with_errors = a `div` length xs
                        where
                            a = 10
                            xs = [1,2,3,4,5]
                            
mylast xs = xs !! ((length xs)-1)

myinit xs = take ((length xs)-1) xs

myinit2 (x:[]) = []
myinit2 (x:xs) = [x] ++ myinit2 xs

add' :: Int -> (Int -> Int)
add' x y = x + y

second xs = head $ tail xs
swap (x,y) = (y,x)
pair x y = (x,y)

double x = x * 2

odds :: Int -> [Int]
odds n = map (\x -> 2*x+1) [0..n-1]

--(n+k) Pattern non sono più supportati in Haskell!!!
--pred :: Int -> Int
--pred 0 = 0
--pred (n + 1) = n

signum n | n < 0 = -1
         | n == 0 = 0
         | otherwise = 1
         
halve :: [a] -> ([a],[a])
halve xs = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)

halve' :: [a] -> ([a],[a])
halve' xs = (take n xs, drop n xs)
            where
                n = length xs `div` 2
                
safetail_a :: [a] -> [a]
safetail_a xs = if length xs == 0
                    then []
                    else tail xs

safetail_b :: [a] -> [a]
safetail_b xs 
    | null xs = []
    | otherwise = tail xs
    
safetail_c :: [a] -> [a]
safetail_c [] = []
safetail_c (x:xs) = xs

fstsumofsq :: Int -> Int
fstsumofsq n = sum [ x^2 | x <- [1..n]]

pyths :: Int -> [(Int,Int,Int)]
pyths n = [(a,b,c) | a <- [1..n], b <- [1..n], c <- [1..n], a^2+b^2==c^2]

factors :: Integer -> [Integer]
factors n = [ x | x <- [1..n], (n `mod` x) == 0]

isprime :: Integer -> Bool
isprime n | factors n == [1,n] = True
          | otherwise = False

perfects :: Integer -> [Integer]
perfects n = [ x | x <- [1..n], x == (sum $ init $ factors x)]

-- Esercizio 5.5
-- [(x,y) | x <- [1,2,3], y <- [4,5,6]]
-- concat [[ (x,y) | x <- [1,2,3]] | y <- [4,5,6]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]

-- define append in order to make it work in linear time

fibs :: Int -> Int
fibs 0 = 0
fibs 1 = 1
fibs n = fibs (n-1) + fibs (n-2) 

fib :: Int -> [Int]
fib n =  map fibs [1..n]    

fibs' :: [Integer]
fibs' = 1 : 1 : zipWith (+) fibs' (tail fibs')

-- La soluzione che ho proposta E' COMPLETAMENTE SBAGLIATA!!
find_largest_prime :: Integer -> Integer
find_largest_prime n = last . filter isprime $ factors n

primes = 2 : filter ((==1) . length . primeFactors) [3,5..]
primeFactors n = factor n primes
  where
    factor n (p:ps) 
        | p*p > n        = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise      = factor n ps

ispalindromic :: Int -> Bool
ispalindromic n = (show n) == (reverse $ show n)

find_largest_palindrome_3 = foldr (\x y -> if x>=y then x else y) 0 [(x*y) | x <- [0..999], y <- [0..999], ispalindromic (x*y)]
find_largest_palindrome_3' = maximum [(x*y) | x <- [0..999], y <- [0..999], ispalindromic (x*y)]

-- Non e' corretto perche' zipWith scorre in parallelo le 2 liste!
--find_largest_palindrome_3' = head . reverse . filter ispalindromic $ zipWith (\x y -> x*y) [0..999] [0..999]

type Parser a = String -> [(a,String)]

item :: Parser Char
--item :: [Char] -> [(Char,[Char])]
item = \inp -> case inp of
    [] -> []
    (x:xs) -> [(x,xs)]

failure :: Parser a
failure = \inp -> []

return :: a -> Parser a
return v = \inp -> [(v,inp)]

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \inp -> case p inp of
    [] -> parse q inp
    [(v,out)] -> [(v,out)]
    
parse :: Parser a -> String -> [(a,String)]
parse p inp = p inp

ex681 :: Int -> Int -> Int
ex681 _ 0 = 1
ex681 0 _ = 0
ex681 a b = a * ex681 a (b-1)

ex68and :: [Bool] -> Bool
ex68and [] = True
ex68and (b:bs) = b && ex68and bs

ex68concat :: [[a]] -> [a]
ex68concat [] = []
ex68concat (ls:lss) = ls ++ ex68concat lss

ex68replicate :: Int -> a -> [a]
ex68replicate 0 _ = []
ex68replicate n a = a : ex68replicate (n-1) a

ex68take :: [a] -> Int -> a
(ex68take) (x:xs) 0 = x
(ex68take) (x:xs) n =  (ex68take) xs (n-1)

ex68elem :: Eq a => a -> [a] -> Bool
ex68elem _ [] = False
ex68elem a (x:xs) =  if a == x then True else ex68elem a xs
--ex68elem a (x:xs)   | a == x = True
--                    | otherwise = ex68elem a xs


ex7all :: (a -> Bool) -> [a] -> Bool
ex7all p = and . map p

ex7any :: (a -> Bool) -> [a] -> Bool
ex7any p = or . map p

ex7takeWhile :: (a -> Bool) -> [a] -> [a]
ex7takeWhile p (x:xs)
    | p x = x : ex7takeWhile p xs
    | otherwise = []
    
ex7dropWhile :: (a -> Bool) -> [a] -> [a]
ex7dropWhile p (x:xs)
    | p x = ex7dropWhile p xs
    | otherwise = x:xs
    
ex7filter :: (a -> Bool) -> [a] -> [a]
ex7filter p = foldr (\x v -> if p x then x:v else v) []

ex7map :: (a -> b) -> [a] -> [b]
ex7map f = foldr (\x v -> (f x):v) []

ex7dec2int :: [Int] -> Int
-- [2,3,4,5] -> 2345
-- (2 * 10^3) + (3 * 10^2) + (4 * 10^1) + (5 * 10^0)
ex7dec2int (x:xs) = sum $ zipWith (\x y -> x*y) powersOfTen (x:xs)
    where
        powersOfTen = reverse $ take (length (x:xs)) $ iterate (*10) 1

--2 * 10 * 10 * 10 + 3 * 10 * 10 + 4 * 10 + 5 * 1
-- Funziona sul principio dello shift a sinistra
-- 10 * 0 + 2 -> 10 * 2 + 3 -> 10 * 23 + 4 -> 10 * 234 + 5 --> 2345
ex7dec2int' = foldl (\x y -> 10 * x + y) 0

ex7curry :: ((a, b) -> c) -> a -> b -> c
ex7curry f x y = f (x,y)

ex7uncurry :: (a -> b -> c) -> (a, b) -> c
ex7uncurry f (x,y)= f x y 

--interactive_dialog pr p t s n w = do
--    pr
--    xs <- getLine
--    if (p(xs))
--        then t xs
--        else do
--            s
--            interactive_dialog pr p t s n (n w)


data Shape = Circle Float | Rect Float Float

area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Rect x y) = x * y

data Tree = Leaf Int | Node Tree Int Tree

occurs :: Int -> Tree -> Bool
occurs n (Leaf x) = n == x
occurs n (Node l x r) = n == x || (occurs n l) || (occurs n r)

t :: Tree
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

flatten :: Tree -> [Int]
flatten (Leaf n) = [n]
flatten (Node l n r) = flatten l ++ [n] ++ flatten r


occurs' :: Int -> Tree -> Bool
occurs' n (Leaf x) = n == x
occurs' n (Node l x r) 
    | n == x = True
    | n < x = occurs' n l
    | otherwise = occurs' n r

occurs'' :: Int -> Tree -> Bool
occurs'' n (Leaf x) = n == x
occurs'' n (Node l x r) = case compare n x of
    LT -> occurs'' n l
    EQ -> True
    GT -> occurs'' n r

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : (rmdups $ filter (/= x) xs)

data Prop = Const Bool
    | Var Char
    | Not Prop
    | And Prop Prop
    | Or Prop Prop
    | Imply Prop Prop
    | Equiv Prop Prop

type Assoc k v = [(k,v)]
type Subst = Assoc Char Bool

find :: Eq k => k -> Assoc k v -> v
find k a = head [v | (k',v) <- a, k == k'] 

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var c) = find c s
eval s (Not p) = not (eval s p)
eval s (And p1 p2) = and [eval s p1, eval s p2]
eval s (Or p1 p2) = or [eval s p1, eval s p2]
eval s (Imply p1 p2) = (eval s p1) <= (eval s p2)
eval s (Equiv p1 p2) = eval s p1 == eval s p2

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var c) = [c]
vars (Not p) = vars p
vars (And p1 p2) = vars p1 ++ vars p2
vars (Or p1 p2) = vars p1 ++ vars p2
vars (Imply p1 p2) = vars p1 ++ vars p2
vars (Equiv p1 p2) = vars p1 ++ vars p2

-- Converte un numero intero nel corrispettivo binario, i bit sono ordinati
-- come segue: [lsb ... msb]
type Bit = Int
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = (n `mod` 2) : int2bin (n `div` 2)

-- Allinea la rappresentazione binaria di un intero al numero di bit specificati
make :: Int -> [Bit] -> [Bit]
make l bs = take l (bs ++ repeat 0)
    
-- Genera le permutazioni di classe N valori booleani
bools :: Int -> [[Bool]]
bools n = map (map conv) vs
    where
        limit = (2^n)-1
        src = [0..limit]
        vs = [make n $ int2bin x | x <- src]
        conv 0 = False
        conv 1 = True

bools' :: Int -> [[Bool]]
bools' 0 = [[]]
bools' n = map (False:) bss ++ map (True:) bss
    where
        bss = bools' (n-1)
      
-- Genera un "dizionario" di tutte le combinazioni di valori booleani per
-- le variabili presenti nella proposizione.
substs :: Prop -> [Subst]
substs p = map (zip variables) bss
    where
        variables = rmdups $ vars p
        bss = bools' $ length variables

-- Controlla se la proposizione è una tautologia        
isTaut :: Prop -> Bool
isTaut p = and [ eval s p | s <- substs p]

data Expr = Val Int | Add Expr Expr
value :: Expr -> Int
value (Val n) = n
value (Add l r) = value l + value r

--data Bool = False | True
--            deriving (Eq, Ord, Show, Read)

data Nat = Zero | Succ Nat
            deriving Show

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + (nat2int n)

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
--add m n = int2nat (nat2int m + nat2int n)
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult _ Zero = Zero
mult Zero _ = Zero
mult (Succ Zero) m = m
mult n (Succ Zero) = n
mult (Succ m) n = add n (mult m n)

data Tree2 = Leaf2 Int | Node2 Tree2 Tree2
    deriving Show

countLeaves :: Tree2 -> Int
countLeaves (Leaf2 x) = 1
countLeaves (Node2 l r) = countLeaves l + countLeaves r

balanced :: Tree2 -> Bool
balanced (Leaf2 x) = True
balanced (Node2 l r) = abs(countLeaves l - countLeaves r) <= 1

halveTree :: [Int] -> ([Int],[Int])
halveTree xs = splitAt (length xs `div` 2) xs

balance :: [Int] -> Tree2
balance [x] = Leaf2 x
balance xs = Node2 (balance l) (balance r)
            where
                (l,r) = halveTree xs
                
--instance Monad Maybe where
--    return :: a -> Maybe a
--    return x = Just x
--    
--    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
--    Nothing >>= _ = Nothing
--    (Just x) >>= f = f x
--    
--instance Monad [] where
--    return :: a -> [a]
--    return x = [x]
--    
--    (>>=) :: [a] -> (a -> [b]) -> [b]
--    xs >>= f = concat (map f xs)