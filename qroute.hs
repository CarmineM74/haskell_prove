-- Quickest route to London

data Section = Section { getA :: Int, getB :: Int, getC :: Int} deriving (Show)
type RoadSystem = [Section]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

heathrowToLondon :: RoadSystem 
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

-- [1,2,3,4,5,6,7] -> [[1,2,3],[4,5,6][7]]
groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) = 
    let priceA = sum $ map snd pathA
        priceB = sum $ map snd pathB
        fwdPriceToA = priceA + a
        fwdPriceToB = priceB + b
        xPriceToA = priceB + b + c
        xPriceToB = priceA + a + c
        newPathToA = if fwdPriceToA <= xPriceToA
                                then (A,a):pathA
                                else (C,c):(B,b):pathB
        newPathToB = if fwdPriceToB <= xPriceToB
                                then (B,b):pathB
                                else (C,c):(A,a):pathA
    in (newPathToA, newPathToB)
                                    

-- Optimal path 
-- [(B,10),(C,30),(A,5),(C,20),(B,2),(B,8)]
optimalPath :: RoadSystem -> Path
optimalPath roadsystem = 
    let (bestAPath, bestBPath) = foldl roadStep ([],[]) roadsystem
    in if sum (map snd bestAPath) <= sum (map snd bestBPath)
        then reverse bestAPath
        else reverse bestBPath
        
main = do
    contents <- getContents
    let triplets = groupsOf 3 $ map read $ lines contents
        roadsystem = map (\[a,b,c] -> Section a b c) triplets
        path = optimalPath roadsystem
        pathString = concat $ map (show . fst) path
        pathPrice = sum $ map snd path
    putStrLn $ "The best path to take is: " ++ pathString
    putStrLn $ "The price is: " ++ show pathPrice