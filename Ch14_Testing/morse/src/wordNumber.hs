module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord x
    | x == 9 = "nine"
    | x == 8 = "eight"
    | x == 7 = "seven"
    | x == 6 = "six"
    | x == 5 = "five"
    | x == 4 = "four"
    | x == 3 = "three"
    | x == 2 = "two"
    | x == 1 = "one"
    | x == 0 = "zero"

digits :: Int -> [Int]
digits n = reverse $ go n
    where go num
            | num < 10 = [num]
            | otherwise = (num `mod` 10) : go (num `div` 10)

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits