import System.IO
import Data.Char

tellPalindrome :: String -> String
tellPalindrome = unlines . map isPalindrome . lines
	where isPalindrome w = if w == (reverse w) then "palindrome" else "not a palindrome" 

main = interact tellPalindrome
