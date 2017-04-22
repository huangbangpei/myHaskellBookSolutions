-- P325




-- Data.Char

-- 1. 
isUpper :: Char -> Bool
toUpper :: Char -> Char

-- 2.
takeUpper :: String -> String
takeUpper = filter isUpper

-- 3.
capFirst :: String -> String
capFirst (x:xs) = (toUpper x) : xs

-- 4.
toUpperStr' :: String -> String
toUpperStr' "" = ""
toUpperStr' (x:xs) = toUpper x : toUpperStr' xs

-- 5 & 6.
capHead :: String -> Char
capHead = toUpper . head



-- Ciphers
-- see ciphers.hs


-- Writing your own standard functions
-- see myStd.hs

