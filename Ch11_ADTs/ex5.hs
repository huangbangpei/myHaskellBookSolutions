-- P396

-- 1.
------
newtype Pair = Pair (Int, String) deriving Show

instance TooMany Pair where
  tooMany (Pair (a, _)) = a > 42
------
--or
------
{-# LANGUAGE FlexibleInstances #-}
instance TooMany (Int, String) where
   tooMany (a, _) = b > 42


-- 2.
newtype Group = Group (Int, Int) deriving Show

instance TooMany Group where
  tooMany (Group (m, n)) = m + n > 42

