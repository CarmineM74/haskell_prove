import System.IO
import Data.Char

main = do
	contents <- getContents
--	mapM_ putStrLn $ shortLinesOnly contents
	putStr (shortLinesOnly contents)

shortLinesOnly :: String -> String
--shortLinesOnly = unlines . filter ((<10) . length) . lines
shortLinesOnly ls =
	let allLines = lines ls
	    shortOnly = filter ((<10) . length) allLines
	    result = unlines shortOnly
	in result 
