myOr :: [Bool] -> Bool
myOr = foldr (||) False

myOr' :: [Bool] -> Bool
myOr' = foldr (\el acc -> if el == True then True else acc) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> f a || b) False

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f = foldr ((||) . f) False

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr ((||) . (==x)) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = myAny' (==x)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\el acc -> if f el == True then el : acc else acc) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldl (\el acc -> if f el acc == GT then el else acc) x xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldl (\el acc -> if f el acc == LT then el else acc) x xs

myMaximumBy' :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy' f = foldr1 (\ el acc -> if f acc el == GT then acc else el) 

myMinimumBy' :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy' f = foldr1 (\ el acc -> if f acc el == LT then acc else el)