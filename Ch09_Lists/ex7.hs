-- P324

-- 1.
myZip :: [a] -> [b] -> [(a, b)]
myZip _ [] = []
myZip [] _ = []
myZip (x:xs) (y:ys)  = (x, y) : (myZip xs ys)

-- 2.
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f _ [] = []
myZipWith f [] _ = []
myZipWith f (x:xs) (y:ys) = (f x y) : (myZipWith f xs ys)

-- 3.
myZip' = myZipWith (,)
