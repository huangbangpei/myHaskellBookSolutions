-- P551
module Arith where

import Data.List (sort)
import Data.Char


-- 1.
half x = x / 2
-- this property should hold
halfIdentity = (*2) . half


-- 2.
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)


-- 3.
plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y = x + y == y + x  


-- 4.
multAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
multAssociative x y z = x * (y * z) == (x * y) * z

multCommutative :: (Eq a, Num a) => a -> a -> Bool
multCommutative x y = x * y == y * x     

-- 5.
quotAndRem :: (Eq a, Integral a) => a -> a -> Bool
quotAndRem x y = (quot x y)*y + (rem x y) == x

divAndMod :: (Eq a, Integral a) => a -> a -> Bool
divAndMod x y = (div x y)*y + (mod x y) == x   

-- 6.
powerAssociative :: (Eq a, Integral a) => a -> a -> a -> Bool
powerAssociative x y z = (x ^ y) ^ z == x ^ (y ^ z)

powerCommutative :: Integral b => b -> b -> Bool
powerCommutative x y = x ^ y == y ^ x


-- 7.
reverseIdentity :: Eq a => [a] -> Bool
reverseIdentity xs = (reverse . reverse) xs == id xs

-- 8.
-- todo


-- 9.


-- 10.

takeAndLength :: Int -> [a] -> Bool
takeAndLength n xs = length (take n xs) == n

-- 11.
readAndShow :: String -> Bool
readAndShow x = (read (show x)) == x


--------------
square :: Float -> Float
square x = x * x 

squareIdentity = square . sqrt


--------------
-- Idempotence

twice f = f . f
fourTimes = twice . twice

capWordIdem :: Char -> Bool
capWordIdem x = (toUpper x == twice toUpper x) && (toUpper x == fourTimes toUpper x)

sortIdem :: [Int] -> Bool
sortIdem xs = (sort xs == twice sort xs) && (sort xs == fourTimes sort xs)


--------------
data Fool = Fulse | Frue deriving (Eq, Show)