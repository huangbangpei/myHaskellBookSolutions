-- 1

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  -- | otherwise = 'F'
  | y >= 0.7  = 'C'
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.59 = 'D'
  | y <  0.59 = 'F'
  where y = x / 100

-- if uncomment the otherwise case above and it will always return 'F'



-- 2. No, when given 90, it matches the top-most guard which is the | y > 0.7 
-- which returns 'C'


-- 3. b)

-- 4. Eq a => [a]

-- 5. Eq a => [a] -> Bool

-- 6. c)

-- 7. Num a => a

-- 8. (Num a, Ord a, Num b) => a -> b

