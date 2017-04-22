-- P775

import Data.Foldable
import Data.Monoid
import Data.Semigroup

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a = foldr ((||) . (==) a) False

-- elem :: (Foldable t, Eq a) => a -> t a -> Bool
-- elem x = getAny . foldMap (Any . (x ==))

-- alt solution: https://github.com/dmvianna/haskellbook/blob/master/src/Ch20-Foldable.hs
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = fmap getMax . getOption . foldMap (Option . Just . Max)

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = fmap getMin . getOption . foldMap (Option . Just . Min)

null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

length' :: (Foldable t) => t a -> Int
length' = getSum . foldMap (const (Sum 1))

-- length' = foldr (\_ a -> a + 1) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr ((<>) . f) mempty