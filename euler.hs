import Control.Arrow
import Data.Char
import Data.Array

-- Project Euler
-- Prob 6
-- The sum of the squares of the first ten natural numbers is: 1^2 + 2^2 + ... + 10^2 = 385
-- The square of the sum of the first ten natural numbers is: (1 + 2 + ... + 10)^2 = 55^2 = 3025
-- Hence the difference between the sum of the squares of the first ten natural numbers 
-- and the square of the sum is 3025  385 = 2640.
-- Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

p6 :: Integer -> Integer
p6 n = sqOfSum n - sumOfSq n
        where
            sqOfSum n = (sum [1..n])^2
            sumOfSq n = sum $ map (^2) [1..n]
            
-- Prob 7
-- Find the first 10001 prime numbers.

p7 :: [Int]
p7 = 2 : [x | x <- [3,5..], isPrime x]
        where
            isPrime x = length [y | y <- [3,5..x], x `mod` y == 0] == 1
            
primes :: [Integer]
primes = 2 : [x | x <- [3,5..], isPrime x]
        where
            isPrime x = (length $ primeFactors x) == 1
            primeFactors x = factors x primes
            factors x (p:ps) 
                    | p*p > x = [x]
                    | x `mod` p == 0 = p : factors (x `mod` p) (p:ps)
                    | otherwise = factors x ps

-- Prob 8
-- Find the greatest product of five consecutive digits in the 1000-digit number
-- 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450

group_of_5 :: String -> [String]
group_of_5 s
        | length s < 5 = []
        | otherwise = take 5 s : group_of_5 (tail s)

group_of :: Int -> [a] -> [[a]]
group_of n items 
    | length items < n = []
    | otherwise = take n items : group_of n (tail items)

p8 n = maximum [ foldr (\x y -> (digitToInt x) * y) 1 cs | cs <- group_of_5 $ show n ]

-- Prob 9
-- Find the only Pythagorean triplet, {a, b, c}, for which a + b + c = 1000.

p9 = take 1 [ (x,y,z) | x <- [1..1000], y <- [x..1000], z <- [y..1000], x+y+z == 1000, x^2+y^2 == z^2 ]

-- Prob 10
-- Find the sum of all the primes below two million.
p10 = sum ( takeWhile (<2000000) primes)

-- Prob 11
-- What is the greatest product of four adjacent numbers in any direction (up, down, left, right, or diagonally) 
-- in the 2020 grid? (file: 20x20.log)
matrixFromText :: String -> [[Int]]
matrixFromText m = map (\row -> map (\n -> read n :: Int) $ words row) $ lines m

compute_search_space ps = map (\(x,y) -> computePositions (x,y) ) ps
                    where 
                        computePositions (x,y) = [ map (\(ox,oy) -> (x+ox,y+oy)) ds | ds <- offset_list ]
                        offset_list = [[(0,0),(0,1),(0,2),(0,3)],[(0,0),(0,-1),(0,-2),(0,-3)],[(0,0),(1,0),(2,0),(3,0)],[(0,0),(-1,0),(-2,0),(-3,0)],[(0,0),(1,1),(2,2),(3,3)],[(0,0),(-1,-1),(-2,-2),(-3,-3)],[(0,0),(-1,1),(-2,2),(-3,3)],[(0,0),(1,-1),(2,-2),(3,-3)]]

compute_product m [] = []
compute_product m (c:cs) = [ (product $ (map (\(x,y) -> if inRange (bounds m) (x,y) then m ! (x,y) else 1) d),d) | d <- c] : compute_product m cs 

-- Res 70600674
p11 = do {
    ; txt <- readFile "20x20.log"
    ; let matrix = listArray ((1,1),(20,20)) $ map (\n -> read n :: Int) $ words txt
    ; let posizioni_da_esaminare = [(x,y) | x <- [1..20], y <- [1..20]]
    ; let coord = compute_search_space posizioni_da_esaminare
    ; let result = compute_product matrix coord
    ; let max_product = foldr (\(p,c) (maxnum,coords) -> if p > maxnum then (p,c) else (maxnum,coords)) (0,[])  $ concat result
    ; return max_product
}