import Data.Monoid
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data List a = Nil | Cons a (List a) deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f = concat' . fmap f

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof [Cons <$> arbitrary <*> arbitrary, return Nil]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a b) = Cons (f a) (fmap f b)

instance Applicative List where
  pure x = Cons x Nil
  fs <*> xs = flatMap (<$> xs) fs

main :: IO ()
main = do
  quickBatch $ functor $ Cons ('a', 'b', 'c') Nil
  quickBatch $ applicative $ Cons ('a', 'b', 'c') Nil

