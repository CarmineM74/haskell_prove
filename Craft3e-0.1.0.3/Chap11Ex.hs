module Chap11Ex where

-- 11.7
myfun :: Char -> Bool
myfun = (\c -> (not . elem c) " \t\n")

-- 11.8
total :: (Integer -> Integer) -> (Integer -> Integer)
total f = (\n -> (sum . map f) [0..n])

-- 11.10
flip :: (a -> b -> c) -> (b -> a -> c)
flip f = (\b a -> f a b)

-- 11.13
mapFuns :: [a -> b] -> a -> [b]
mapFuns fs x = map ($ x) fs

-- 11.17
curry :: ((a,b) -> c) -> (a -> b -> c)
curry g x y = g (x,y)

uncurry :: (a -> b -> c) -> ((a,b) -> c)
uncurry f (x,y) = f x y

curry3 :: ((a,b,c) -> d) -> (a -> b -> c -> d)
curry3 g x y z = g (x,y,z)

curry3' :: ((a,b,c) -> d) -> (a -> b -> c -> d)
curry3' g x y z = Chap11Ex.curry (\(j,k) -> g (j,k,z)) x y

uncurry3 :: (a -> b -> c -> d) -> ((a,b,c) -> d)
uncurry3 f (x,y,z) = f x y z

uncurry3' :: (a -> b -> c -> d) -> ((a,b,c) -> d)
uncurry3' f (x,y,z) = Chap11Ex.uncurry (\j k -> f j k z) (x,y)

-- 11.21
iter' :: Int -> (a -> a) -> (a -> a)
iter' n = (foldr (\g r -> g . r) id) . replicate n

-- 11.22
mapFuns' :: [a -> b] -> a -> [b]
mapFuns' fs = (Chap11Ex.flip map) fs . Chap11Ex.flip ($)

-- 11.23
-- Ref: http://labmaster.mi.infn.it/Laboratorio2/CompilerCD/clang/l12/dfridr_c.html
-- 08.07.2012 : Sostituire findBetterApproximation con un sort

safe = 2.0 :: Float
con = 1.4 :: Float
con2 = con * con :: Float
big = 1.0e50 :: Float
ntabDimension = 10 :: Int

type NevValue = (Float, Float)
type NevRow = [NevValue]
type NevMatrix = [NevRow]

computeValue :: (Float -> Float) -> Float -> Float -> NevValue
computeValue f x hh = ((f (x+hh) - f (x-hh)) / (2.0 * hh), big)

computeFirstRow :: (Float -> Float) -> Float -> Float -> NevMatrix
computeFirstRow f x hh = [map (computeValue f x) hhs]
        where
            hhs = take ntabDimension $ iterate (/con) hh
 
computeExtrapolation :: Float -> NevValue -> NevValue -> NevValue
computeExtrapolation fac (av,ae) (lv,le) = (value, errt)    
    where
        value = ((av*fac) - lv) / (fac - 1.0)
        errt1 = abs(value - av)
        errt2 = abs(value - lv)
        errt = if errt1 > errt2 then errt1 else errt2
        
computeExtrapolationsRow :: NevRow -> Float -> Int -> NevRow
computeExtrapolationsRow prevRow fac colIdx =  zipWith (computeExtrapolation fac) aboves lefts
    where
        colRange = [1 .. ((length prevRow)-1)]
        aboves = map (prevRow !!) colRange
        lefts = map (\c -> prevRow !! (c-1)) colRange

computeExtrapolations :: NevMatrix -> Float -> NevMatrix
computeExtrapolations prevRow fac = foldl (\mx (idx,fs) -> (computeExtrapolationsRow (head mx) fs idx) : mx ) prevRow params
    where
        idxs = [1..ntabDimension]
        facPwrs = map (\c -> fac * (con2 ** (fromIntegral c))) idxs
        params = zip idxs facPwrs        
            
dfridr :: (Float -> Float) -> Float -> Float -> Float -> Float
dfridr f x h err = fst $ foldr (\row (result,minErr) ->  findBetterApproximation (result,minErr) row) (0.0,err) approximations
   where
       neville = computeFirstRow f x h
       approximations = dropWhile (\a -> a == []) $ computeExtrapolations neville con2
       findBetterApproximation (re,me) r = foldr (\(v,e) (bv,be) -> if e < be then (v,e) else (bv,be)) (re,me) r
       
slope :: (Float -> Float) -> (Float -> Float)
slope f = (\x -> dfridr f x 0.56 big)