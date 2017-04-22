module Cipher where

import Data.Char


casearChar :: Int -> Char -> Char
casearChar shift ch
    | ord ch + shiftMod > ord 'z' = casearChar shiftMod (chr $ ord ch - 26)
    | ord ch + shiftMod < ord 'a' = casearChar shiftMod (chr $ ord ch + 26)
    | otherwise =  chr $ ord ch + shiftMod
    where shiftMod = shift `mod` 26

casear :: Int -> String -> String
casear shift str = map (casearChar shift) str

unCaesar :: Int -> String -> String
unCaesar shift str = map (casearChar $ negate shift) str