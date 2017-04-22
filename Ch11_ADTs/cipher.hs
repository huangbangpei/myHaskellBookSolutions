module Cipher where

import Data.List
import Data.Char
import Data.Time
import Data.Int


-- bugs:
-- 1. only work on lowercase
-- 2. can't decode space

casearChar :: Int -> Char -> Char
casearChar 0 ch = ch
casearChar shift ch
    | ord ch + shift > ord 'z' = casearChar (shift - 26) ch
    | ord ch + shift < ord 'a' = casearChar (shift + 26) ch
    | otherwise               = chr $ ord ch + shift

casearStr shift = map (casearChar shift)
uncasearStr shift = map (casearChar (negate shift))

repeatStr :: String -> String -> String
repeatStr str salt = take (length str) $ cycle salt

distanceStr :: String -> [Int]
distanceStr = map (\a -> ord a - ord 'a') 

vignere :: String -> String -> String
vignere str salt = let dis = distanceStr $ repeatStr str salt in
    zipWith (\a b -> casearChar a b) dis str

unVignere :: String -> String -> String
unVignere str salt = let dis = distanceStr $ repeatStr str salt in
    zipWith (\a b -> casearChar (negate a) b) dis str