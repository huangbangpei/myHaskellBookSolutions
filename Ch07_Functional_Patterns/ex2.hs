-- 1.
-- a) k :: (a, b) -> a
-- b) k2 :: [Char] No
-- c) k3


-- 2.
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f)) 

