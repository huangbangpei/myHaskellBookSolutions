-- P351

-- 1. b c

-- 2. foldl (flip (*)) 1 [1..3]
--(flip (*) (flip (*) (flip (*) 1 1) 2) 3)
--(flip (*) (flip (*) 1 2) 3)
--(flip (*) 2 3)
--6


-- 3. c

-- 4. a

-- 5. fixed:
foldr (++) "" ["woot", "WOOT", "woot"]
foldr max ' ' "fear is the little death"
foldr (&&) True [False, True]
foldr (||) True [False, True]
foldr ((++) . show) "" [1..5]
foldl const 'a' [1..5]
foldl const 0 "tacos"
foldl const 0 "burritos"
foldr (flip const) 'z' [1..5]