-- Multiple choice
-- 1. c
-- 2. b
-- 3. a
-- 4. c
-- 5. a



-- Does it typecheck?

-- 1. It won't work because Person is not an instance of Show
-- fix:

data Person = Person Bool deriving Show 

-- 2. It won't work because Mood is not an instance of Eq
-- fix:

data Mood = Blah | Woot deriving (Show, Eq)

-- 3. 
-- 	 a) values of Mood type (Blah & Woot)
--   b) error. 9 is not the value of Mood type
--   c) error. Because Mood needs to be the instance of Ord

-- 4. Yes, it works
 


-- Given a datatype declaration, what can we do?

-- 1. It won't work, because Yeah is a custom data constructor and should match its value
-- fix:
Phew = Papu (Rock "chases") (Yeah True)

-- 2. It works

-- 3. It works

-- 4. It won't work because Papu is not an instance of Ord



-- Match the types
-- 1. No
-- 2. No
-- 3. Yes
-- 4. Yes
-- 5. Yes
-- 6. Yes
-- 7. No
-- 8. No
-- 9. Yes
-- 10. Yes
-- 11. No



-- Type-Kwon-Do
-- 1.
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = b == (f a) 

-- 2.
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f a x = fromInteger a + f x 
