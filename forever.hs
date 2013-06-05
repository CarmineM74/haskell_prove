import Control.Monad
import Data.Char
import System.IO

main = forever $ do
	putStr "Please, say something: "
	hFlush stdout
	line <- getLine
	putStrLn $ map toUpper line

