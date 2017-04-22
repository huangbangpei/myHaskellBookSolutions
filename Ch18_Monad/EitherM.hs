import Control.Monad
import Control.Applicative

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure = Second
  _ <*> First a = First a
  First a <*> _ = First a
  Second f <*> Second a = Second (f a)

instance Monad (Sum a) where
  return = pure
  First a >>= _ = First a
  Second a >>= f = f a