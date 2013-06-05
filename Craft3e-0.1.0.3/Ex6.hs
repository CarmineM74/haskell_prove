module Ex6 where
import Pictures hiding (superimpose, printPicture)
import Data.List

-- 123              741
-- 456      -->    852
-- 789              963

extractColumn :: Int -> Picture -> [Char]
extractColumn col pic = [ r  !! col | r <- pic ]    

extractColumns :: Picture -> Picture
extractColumns pic = [ extractColumn col_idx pic | col_idx <- [0..(width pic)-1]]

-- 6.8
rotate90 :: Picture -> Picture
rotate90 = flipV . extractColumns

rotate90' :: Picture -> Picture
rotate90' pic = [reverse r | r <- (transpose pic)]

-- 6.4
superimposeChar :: Char -> Char -> Char
superimposeChar c1 c2 
    | c1 == c2 && c1 == '.' = '.'
    | otherwise = '#'
 
-- 6.5
-- Lines are assumed to have same length
superimposeLine :: [Char] -> [Char] -> [Char]
superimposeLine l1 l2 = [superimposeChar c1 c2 | (c1,c2) <- (zip l1 l2)]

-- 6.6
-- Pictures are assumed to have the same dimensions
superimpose :: Picture -> Picture -> Picture
superimpose p1 p2 = [ superimposeLine l1 l2 | (l1,l2) <- (zip p1 p2)]

-- 6.7
printPicture :: Picture -> IO ()
printPicture pic = putStr $ concat [appendNewLine l | l <- pic]
    where
        appendNewLine line = line ++ "\n"
        

-- 6.10

scaleRow :: [Char] -> Int -> [Char]
scaleRow row fact = concat [ replicate fact c | c <- row ]

scale :: Picture -> Int -> Picture
scale pic fact
    | fact <= 0 = []
    | otherwise = concat [replicate fact (scaleRow r fact) | r <- pic]

-- 6.17

padH :: Picture -> Int -> Picture
padH pic w = [ r ++ (replicate w '.') | r <- pic]

padV :: Picture -> Int -> Picture
padV pic h = pic ++ (replicate h (replicate (width pic) '.'))

-- 6.18
makeRectangularPic :: Picture -> Picture
makeRectangularPic pic = [ r ++ (replicate (maxW - (length r)) '.') | r <- pic]
    where
        maxW = maximum [ length r | r <- pic ]

