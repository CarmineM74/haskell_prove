--Game Of Life
width = 50
height = 40

type Pos = (Int, Int)
type Board = [Pos]

cls :: IO ()
cls = putStr "\ESC[2J"

beep :: IO ()
beep = putStr "\BEL"

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = do
    goto p
    putStr xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a:as) = do
    a
    seqn as

glider :: Board
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]

flipper :: Board
flipper = [(1,1),(8,8),(3,2),(1,5),(10,6),(4,2),(2,2),(7,8),(6,8)]

showcells :: Board -> IO ()
showcells b = seqn [ writeat p "O" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1,y-1),(x,y-1),(x+1,y-1),(x-1,y),(x+1,y),(x-1,y+1),(x,y+1),(x+1,y+1)]

wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width)+1,((y-1) `mod` height)+1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b p = length [ n | n <- (neighbs p), isAlive b n]

survivors :: Board -> [Pos]
survivors b = [ p | p <- b, elem (liveneighbs b p) [2,3]]

births :: Board -> [Pos]
--births b = [ p | p <- (foldr (\x v -> if not (elem x v) then x:v else v) [] $ concat $ map neighbs b), isEmpty b p, (liveneighbs b p) == 3 ]
births b = [ p | p <- (removedups $ concat $ map neighbs b), isEmpty b p, (liveneighbs b p) == 3 ]

removedups :: [Pos] -> [Pos]
removedups [] = []
removedups (x:xs) = x : removedups (filter (/= x) xs)
    
nextgen :: Board -> Board
nextgen b = (survivors b) ++ (births b)

wait :: Integer -> IO ()
wait n = seqn [ return () | _ <- [1..n]]

diff :: Board -> Board -> Board
diff b_new [] = b_new
diff [] _ = []
diff b_new b = [ p | p <- b_new, not (elem p b)]

life :: Board -> Board -> Int -> IO ()
life b_new b gencount = do
    cls
    writeat (1,height+1) ("Generation: " ++ (show gencount))
    showcells (diff b_new b)
    wait 50000
    life (nextgen b_new) b_new (gencount+1)

main = life (flipper ++ glider) [] 0