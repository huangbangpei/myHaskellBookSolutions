myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x == True then True else myOr xs

-- or

myOr' :: [Bool] -> Bool
myOr' [] = False
myOr' (x:xs) = x || myOr' xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) = f x || myAny f xs


myElem :: Eq a => a -> [a] -> Bool
myElem n [] = False
myElem n (x:xs) = if n == x then True else myElem n xs

myElem' n = myAny (==n)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f (x:xs) = f x ++ squishMap f xs  

squish' :: [[a]] -> [a]
squish' = squishMap (id)

myMaximumBy :: Ord a => (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:[]) = x
myMaximumBy f (x:xs) = case f x y of
    LT -> y
    EQ -> x
    GT -> x
    where y = myMaximumBy f xs

-- Alterlative
myMaximumBy' :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy' _ (x:[]) = x
myMaximumBy' f (x:xs) = if (f x (myMaximumBy' f xs)) == GT then x else (myMaximumBy' f xs)


myMinimumBy :: Eq a => (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:x1:xs)
    | f x x1 == LT && xs == [] = x
    | f x x1 == GT && xs == [] = x1
    | f x x1 == GT = myMinimumBy f (x1:xs)
    | otherwise = myMinimumBy f (x:xs) 

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare



