import System.IO
import Data.Char

shortLines :: String -> String
shortLines = unlines . filter ((<10) . length) . lines

main = interact shortLines
