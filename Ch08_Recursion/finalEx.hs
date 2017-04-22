-- Review of types

-- 1. d
-- 2. b
-- 3. d
-- 4. b




-- Reviewing currying

flippy :: String -> String -> String
appedCatty :: String -> String
frappe :: String -> String

-- 1. "woops mrow woohoo!"
-- 2. "1 mrow haha"
-- 3. "woops mrow 2 mrow haha"
-- 4. "woops mrow blue mrow haha"
-- 5. "pink mrow haha mrow green mrow woops mrow blue"
-- 6. "are mrow Pugs mrow awesome"




-- Recursion

-- 1. dividedBy 15 2
--= go 15 2 0
--= go 13 2 1
--= go 11 2 2
--= go 9  2 3
--= go 7  2 4
--= go 5  2 5
--= go 3  2 6
--= go 1  2 7
--= (7, 1)

-- 2.
sumToN :: (Eq a, Num a) => a -> a
sumToN 0 = 0
sumToN n = n + sumToN (n - 1)
-- or
sumToN' :: (Eq a, Num a) => a -> a
sumToN' n = go n 0 0
	where go m sums i
			| i == (m + 1) = sums
			| otherwise = go m (sums + i) (i + 1)

-- 3.

-- only for positive number
mul :: Integral a => a -> a -> a
mul x n = go x 0 0
    where go x sum count
            | count == n = sum
            | otherwise = go x (sum + x) (count + 1)

-- maybe better
mul' :: Integral a => a -> a -> a
mul' x y 
    | x > 0 = y + mul' (x - 1) y
    | x < 0 = (negate y) + mul' (x + 1) y
    | otherwise = 0




-- Fixing dividedBy

dividedBy :: Integer -> Integer -> DividedResult
dividedBy _ 0 = DividedByZero
dividedBy num denom = go (abs num) (abs denom) 0
    where go n d count
              | n < d && num > 0 && denom > 0 = Result (count, n)
              | n < d && num < 0 && denom > 0 = Result (negate count, negate n)
              | n < d && num < 0 && denom < 0 = Result (count, negate n)
              | n < d && num > 0 && denom < 0 = Result (negate count, n)
              | otherwise = go (n - d) d (count + 1)




-- mc91 function

mc91 :: Integer -> Integer
mc91 x
    | x > 100 = x - 10
    | otherwise = mc91 $ mc91 $ x + 11




-- Numbers into words
-- see wordNumber.hs




