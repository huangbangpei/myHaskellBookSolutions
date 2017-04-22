module Cipher where

import Data.List
import Data.Char
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

casearStr :: Int -> String -> String
casearStr shift = map (casearChar shift)

uncasearStr :: Int -> String -> String
uncasearStr shift = map (casearChar (negate shift))

repeatStr :: String -> String -> String
repeatStr salt str = take (length str) $ cycle salt

distanceStr :: String -> [Int]
distanceStr = map (\a -> ord a - ord 'a') 

vignere :: String -> String -> String
vignere salt str = let dis = distanceStr $ repeatStr salt str in
    zipWith (\a b -> casearChar a b) dis str

unVignere :: String -> String -> String
unVignere salt str = let dis = distanceStr $ repeatStr salt str in
    zipWith (\a b -> casearChar (negate a) b) dis str