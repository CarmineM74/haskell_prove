-- Game of NIM
import Control.Monad

initial_board :: [Int]
initial_board = [5,4,3,2,1]

show_board :: [Int] -> IO ()
-- http://www.haskell.org/haskellwiki/Avoiding_IO
--show_board b = seqn [putStrLn $ (show row) ++ ":" ++ (take num $ repeat '*') | (row,num) <- (zipWith (\a b -> (a,b)) [1..] b) ]
show_board b = putStrLn $ unlines [(show row) ++ ":" ++ (take num $ repeat '*') | (row,num) <- (zipWith (\a b -> (a,b)) [1..] b) ]

pick_from_board_at :: Int -> [Int] -> [Int]
pick_from_board_at pos b = left ++ [v] ++ right
                            where
                                left = take pos b
                                right = drop (pos+1) b
                                actual = (b !! pos)
                                v = if actual == 0 then 0 else actual - 1

validate_player_move :: Int -> [Int] -> [Int]
validate_player_move r b 
                | and [r>=0, r<(length b), (b !! r) > 0] = pick_from_board_at r b
                | otherwise = b

player_move :: Int -> [Int] -> IO ([Int])
player_move p b = do
    putStr ("Player " ++ (show p) ++ ", make your move: ")
    row <- getLine
    let row_num = read row :: Int
    let new_board = validate_player_move (row_num-1) b
    when (new_board == b) $ do
        putStrLn "Invalid move. Try again!"
-- http://www.haskell.org/haskellwiki/Common_Misunderstandings
-- if / then / else
--    if (new_board == b)
--        then
--            putStrLn "Invalid move. Try again!"
--        else
--            return ()
    return new_board

next_player :: Int -> Int
next_player 1 = 2
next_player 2 = 1

game_over :: [Int] -> Bool
game_over = all (== 0)

nim :: Int -> [Int] -> IO ()
nim p b = do
    show_board b
    new_board <- player_move p b
    if (new_board == b) 
        then nim p new_board
        else
            if (game_over new_board) 
                then putStrLn ("Player " ++ (show p) ++ " WINS!")
                else nim (next_player p) new_board    
