import Data.List
import Data.List.Split

capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs 

capitalizeParagraph :: String -> String
capitalizeParagraph = (intercalate ". ") . map capitalizeWord  . (splitOn ". ")

