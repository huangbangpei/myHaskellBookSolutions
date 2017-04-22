-- Multiple choice
-- 1. c
-- 2. a
-- 3. b
-- 4. c




-- Determine the type

-- 1.
a = (* 9) 6
b = head [(0,"doge"),(1,"kitteh")]
c = head [(0 :: Integer ,"doge"),(1,"kitteh")]
d = if False then True else False
e = length [1, 2, 3, 4, 5]
f = (length [1, 2, 3, 4]) > (length "TACOCAT")

-- a :: Num a => a
-- b :: Num a => (a, [Char])
-- c :: (Integer, [Char])
-- d :: Bool
-- e :: Int
-- f :: Bool

-- 2.
-- Num a => a

-- 3.
-- Num a => a -> a

-- 4.
-- Fractional a => a

-- 5.
-- [Char]




-- Does It Compile
-- 1.
bigNum = (^) 5 $ 10
wahoo = bigNum * 10

-- 2.
x = print
y = print "woohoo!"
z = x "hello world"

-- 3.
c = a b 10
d = a c 200

-- 4.
a = 12 + b
b = 10000 * c
c = 2




-- Type variable or specific type constructor?
-- 1. is an example

-- 2.
-- f :: zed -> Zed -> Blah
--     [1]    [2]    [3]
-- 1 = fully polymorphic
-- 2 = concrete
-- 3 = concrete

-- 3.
-- f :: Enum b => a -> b -> C
--               [1]  [2]  [3]
-- 1 = fully polymorphic
-- 2 = constrained polymorphic
-- 3 = concrete

-- 4.
-- f :: f -> g -> C
--     [1]  [2]  [3]
-- 1 = fully polymorphic
-- 2 = fully polymorphic
-- 3 = concrete




-- Write a type signature

-- 1.
functionH :: [a] -> a
functionH (x:_) = x

-- 2.
functionC :: (Ord a) => a -> a -> Bool
functionC x y = if (x > y) then True else False

-- 3.
functionS :: (a, b) -> b
functionS (x, y) = y




-- Given a type, write the function
-- 1.
i :: a -> a
i x = x

-- 2.
c :: a -> b -> a
c x y = x

-- 3.
c'' :: b -> a -> b
c'' x y = x

-- 4.
c' :: a -> b -> b
c' x y = y

-- 5.
r :: [a] -> [a]
r x = tail x

-- 6.
co :: (b -> c) -> (a -> b) -> (a -> c)
co f g = f . g

-- 7.
a :: (a -> c) -> a -> a
a f x = x

-- 8.
a' :: (a -> b) -> a -> b
a' f x = f x




-- Fix It
-- 1. see sing.hs
-- 2. see sing2.hs
-- 3. see arith3Broken.hs




-- Type-Kwon-Do
-- see typeKwonDo.hs