import System.Environment
import System.IO

dispatch :: [(String, [String] -> IO ())]
dispatch = [
		("add", add)
		,("view", view)
		,("remove", remove)
	   ]

main = do
	(command:args) <- getArgs
	let (Just action) = lookup command dispatch
	action args

add :: [String] -> IO ()
add [fileName, todoItem] = appenFile fileName (todoItem ++ "\n")

view :: String -> IO ()
view fileName = do
		contents <- readFile fileName
		let todoTasks = lines contents
			numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
		putStr $ lines numberedTasks

 
