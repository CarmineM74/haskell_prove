module Chap8Ex where
import Data.Char

-- 8.11
palindromeTest :: String -> IO ()
palindromeTest value =
    case value == reverse value of
        True -> putStrLn "The input you gave is a palindrome"
        False -> putStrLn "The input you gave is not a plaindrome"

palindromeInput :: IO ()
palindromeInput = do
    putStrLn "Palindrome test"
    putStr "Input> "
    value <- getLine
    palindromeTest value
    
-- 8.12
integerSum :: IO ()
integerSum = do
    putStr "Please enter fist number: "
    s1 <- getLine
    let n1 = read s1 :: Integer
    putStr "Please enter second number: "
    s2 <- getLine
    let n2 = read s2 :: Integer
    putStr "The sum of the numbers you gave is: "
    putStrLn $ show (n1 + n2)
    
-- 8.14 

printStats :: (Int, Int, Int) -> IO ()
printStats (wc,lc,chars) = do
                putStrLn "There were:"
                putStrLn $ "- " ++ (show wc) ++ " words"
                putStrLn $ "- " ++ (show lc) ++ " lines"
                putStrLn $ "- " ++ (show chars) ++ " characters"
                putStrLn "in the input."

computeLineStats :: (Int, Int, Int) -> String -> (Int, Int, Int)
computeLineStats (wc,lc,chars) line = (word_count, line_count, chars_count)
    where
        line_count = lc + 1
        chars_count = chars + length line
        word_count = wc + length (words line)
        
computeStats :: (Int, Int, Int) -> IO ()
computeStats stats = do
    line <- getLine
    if line == "" 
        then printStats stats
        else do
                let new_stats = computeLineStats stats line
                computeStats new_stats
        
wc :: IO ()
wc = do
    computeStats (0,0,0)

-- 8.15

removePunctuations :: String -> String
removePunctuations "" = ""
removePunctuations (c:cs)
    | isPunctuation c = removePunctuations cs
    | otherwise = [c] ++ (removePunctuations cs)

isPalindrome :: String -> Bool
isPalindrome s = lowercase == (reverse lowercase)
    where
        concatenated = concat $ words s
        cleaned = removePunctuations concatenated
        lowercase = [ toLower c | c <- cleaned]