-- 1. d

-- 2. b

-- 3. d (I'm incorrect on this one. My answer is a.
--       I guess once you provide a numeric value which implements
--       Num and Ord as an argument to a function that accepts two
--       arguments, and the another argument must implement
--       the same typeclasses as the first one 
--      )

-- 4. b

-- 5. a



-- Let’s write code

-- 1. a)

tensDigit :: Integral a => a -> a
tensDigit x = d
    where (xLast, _) = x `divMod` 10
          (_, d) = xLast `divMod` 10 

-- 1. b) yes

-- 1. c)

hundredsDigit :: Integral a => a -> a
hundredsDigit x =
    where (xLast, _) = x `divMod` 100
          (_, d) = xLast `divMod` 10



-- 2.
-- case of version:
foldBool :: a -> a -> Bool -> a
foldBool x y cond = case cond of
    True -> x
    False -> y 

-- guard version:
foldBool :: a -> a -> Bool -> a
foldBool x y cond 
    | cond == True = x
    | otherwise = y            


-- 3.
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

-- For 4 5 6, see arith4.hs   