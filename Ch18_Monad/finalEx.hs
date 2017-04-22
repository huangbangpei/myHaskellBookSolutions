import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative
import Data.Monoid
import Data.Functor
import Control.Monad (join, (>=>), foldM, ap)


-- 1.

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap f NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
  return = pure
  NopeDotJpg >>= _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = 
    return NopeDotJpg

instance Eq a => EqProp (Nope a) where (=-=) = eq



-- 2.
data PhbtEither b a = Left' a | Right' b deriving (Eq, Show)

instance Functor (PhbtEither b) where
  fmap f (Left' a) = Left' (f a)
  fmap f (Right' a) = Right' a

instance Applicative (PhbtEither b) where
  pure = Left' 
  Left' f <*> Left' a = Left' (f a)
  Right' f <*> _      = Right' f
  Left' f <*> Right' a = Right' a

instance Monad (PhbtEither b) where
  return = pure
  Left' a >>= f = f a
  Right' a >>= f = Right' a

instance (Arbitrary b, Arbitrary a) => Arbitrary (PhbtEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ Left' a), (1, return $ Right' b)]

instance (Eq a, Eq b) => EqProp (PhbtEither b a) where (=-=) = eq


-- 3.
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity b = Identity (f b)  

instance Monad (Identity) where
  return = pure
  Identity a >>= f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Eq a => EqProp (Identity a) where
  (=-=) = eq


-- 4.
data List a = Nil | Cons a (List a) deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f s (Cons x xs) = f x (fold f s xs)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f = concat' . fmap f

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof [Cons <$> arbitrary <*> arbitrary, return Nil]

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Applicative List where
  pure = flip Cons Nil
  fs <*> xs = flatMap (<$> xs) fs

instance Monad List where
  xs >>= f = flatMap f xs

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ functor $ (undefined :: Nope (Int, String, Int))
  quickBatch $ applicative $ (undefined :: Nope (Int, String, Int))
  quickBatch $ monad $ (undefined :: Nope (Int, String, Int))
  putStrLn "--------------------------"

  quickBatch $ functor $ (undefined :: PhbtEither Double (Int, String, Int))
  quickBatch $ applicative $ (undefined :: PhbtEither Double (Int, String, Int))
  quickBatch $ monad $ (undefined :: PhbtEither Double (Int, String, Int))
  putStrLn "--------------------------"

  quickBatch $ functor $ (undefined :: Identity (Int, String, Int))
  quickBatch $ applicative $ (undefined :: Identity (Int, String, Int))
  quickBatch $ monad $ (undefined :: Identity (Int, String, Int))
  putStrLn "--------------------------"

  quickBatch $ functor $ (undefined :: List (Int, Int, Int))
  quickBatch $ applicative $ (undefined :: List (Int, Int, Int))
  quickBatch $ monad $ (undefined :: List (Int, Int, Int))



-------------------------------------------

-- <*> in Monad
ap' :: Monad m => m (a -> b) -> m a -> m b
ap' f m = m >>= (\x -> f >>= (\f -> (return . f) x))


j :: Monad m => m (m a) -> m a
j = (>>= id) -- join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f = (>>= return . f)

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f x y = l1 f x `ap` y 

a :: Monad m => m a -> m (a -> b) -> m b
a = flip ap

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh (x:xs) f = do
  y <- f x
  fmap ((:) y) (meh xs f)