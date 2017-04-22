import Data.Monoid
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


-- data Sum a b = First a | Second b deriving (Eq, Show)
data Validation e a = Error e | Success a deriving (Eq, Show)

-- instance Functor (Sum a) where
--   fmap f (First a) = First a
--   fmap f (Second b) = Second (f b)

-- instance Applicative (Sum a) where
--   pure = Second
--   Second f <*> Second b = Second (f b)

-- same as Sum/Either
instance Functor (Validation e) where
  fmap _ (Error a) = Error a
  fmap f (Success b) = Success (f b)

-- This is different
instance Monoid e => Applicative (Validation e) where
  pure = Success
  Error a <*> Error b = Error (a <> b)
  Error a <*> _ = Error a
  Success f <*> Success a = Success (f a)
  _ <*> Error a = Error a

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    e <- arbitrary
    a <- arbitrary
    frequency [(1, return $ Error e), (1, return $ Success a)]

instance (Eq a, Eq e) => EqProp (Validation e a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ applicative $ (Success ("a", "b", "c") :: Validation String (String, String, String))