import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

import Control.Applicative
import Data.Monoid

-- Identity

newtype Identity a = Identity a deriving (Eq, Show, Ord)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq


-- Constant

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ (Constant a) = mempty

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure (Constant a)

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = fmap Constant arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq


-- Maybe

data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return Nada), (1, return $ Yep a)]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq


-- List
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons a l) = (f a) <> (foldMap f l)

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons a l) = Cons <$> f a <*> traverse f l 

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof [liftA2 Cons arbitrary arbitrary, return Nil]

instance Eq a => EqProp (List a) where
  (=-=) = eq


-- 
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c 

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq


-- Three'
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Foldable (Three' a) where
  foldMap f (Three' a b c) = (f b) <> (f c)

instance Traversable (Three' a) where
  traverse f (Three' a b c) = Three' a <$> f b <*> f c

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = liftA3 Three' arbitrary arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq


---------
data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S a b) = S (fmap f a) (f b)

instance Foldable n => Foldable (S n) where
  foldMap f (S a b) = f b <> foldMap f a

instance Traversable n => Traversable (S n) where
  traverse f (S a b) = S <$> traverse f a <*> f b 

instance (Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = liftA2 S arbitrary arbitrary

instance (Eq (n a), Eq a) => EqProp (S n a) where
  (=-=) = eq


--- Binary Tree
data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node t a t') = Node (fmap f t) (f a) (fmap f t') 

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node t a t') = f a <> foldMap f t <> foldMap f t'

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node t a t') = liftA3 Node (traverse f t) (f a) (traverse f t')
  -- traverse f (Node t a t') = Node <$> traverse f t <*> f a <*> traverse f t'

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = oneof [return Empty, fmap Leaf arbitrary, liftA3 Node arbitrary arbitrary arbitrary]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ traversable $ (undefined :: Identity (Int, Int, [Int]))
  quickBatch $ traversable $ (undefined :: Constant Int (Int, Int, [Int]))
  quickBatch $ traversable $ (undefined :: Optional (Int, Int, [Int]))
  quickBatch $ traversable $ (undefined :: List (Int, Int, [Int]))
  quickBatch $ traversable $ (undefined :: Three Int Int (Int, Int, [Int]))
  quickBatch $ traversable $ (undefined :: Three' Int (Int, Int, [Int]))
  quickBatch $ traversable $ (undefined :: S [] (Int, Int, [Int]))
  quickBatch $ traversable $ (undefined :: Tree (Int, Int, [Int]))