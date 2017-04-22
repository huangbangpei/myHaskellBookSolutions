-- 1. bottom

-- 2. it will return [2]

-- 3. it won't run

-- 4. itIsMystery has the type of :: String -> [Bool]
--   the function takes a string as argument and returns a list of bool values
--   which indicate whether each character of the string is a vowel or not 

-- 5.a [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]

-- 5.b [1, 10, 20]

-- 5.c [15, 15, 15]

-- 6.
map (\x -> bool x (-x) (x == 3)) [1..10]
