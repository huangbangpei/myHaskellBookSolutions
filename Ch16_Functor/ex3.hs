-- P623
{-# LANGUAGE ViewPatterns #-}

import Test.QuickCheck
import Test.QuickCheck.Function


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) =>
      f a
      -> Fun a b
      -> Fun b c
      -> Bool

functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

-----------------
-- 1.

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return (Identity x)

type IdentityFC = Identity Int -> IntToInt -> IntToInt -> Bool


-----------------
-- 2.

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    return (Pair x x)

type PairFC = Pair Int -> IntToInt -> IntToInt -> Bool


-----------------
-- 3.

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoFC = Two Double Int -> IntToInt -> IntToInt -> Bool

-----------------
-- 4.

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

type ThreeFC = Three Double Char Int -> IntToInt -> IntToInt -> Bool


-----------------
-- 5.

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Three' a b b)

type Three'FC = Three' Double Int -> IntToInt -> IntToInt -> Bool


-----------------
-- 6.

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

type FourFC = Four Double Char Double Int -> IntToInt -> IntToInt -> Bool


-----------------
-- 7.

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Four' a a a b)

type Four'FC = Four' Char Int -> IntToInt -> IntToInt -> Bool


-----------------
-- P632
data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

instance Arbitrary a => Arbitrary (Possibly a) where
  arbitrary = do
    x <- arbitrary
    frequency [(1, return LolNope), (1, return $ Yeppers x)]

type IntToInt = Fun Int Int
type PossiblyFC = Possibly Int -> IntToInt -> IntToInt -> Bool


-----------------
-- P634
data Sum a b = First a | Second b deriving (Eq, Show)

type IntToInt = Fun Int Int

instance Functor (Sum a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ First a), (2, return $ Second b)]

type SumFC = Sum Double Int -> IntToInt -> IntToInt -> Bool


main :: IO ()
main = do
  putStrLn "Identity a: "
  quickCheck $ \x -> functorIdentity (x :: Identity Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Identity Int)  
  quickCheck (functorCompose' :: IdentityFC)

  putStrLn "\nPair a: "
  quickCheck $ \x -> functorIdentity (x :: Pair Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Pair Int)  
  quickCheck (functorCompose' :: PairFC)

  putStrLn "\nTwo a b: "
  quickCheck $ \x -> functorIdentity (x :: Two Char Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Two Char Int)  
  quickCheck (functorCompose' :: TwoFC)

  putStrLn "\nThree a b c: "
  quickCheck $ \x -> functorIdentity (x :: Three Double Char Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Three Double Char Int)  
  quickCheck (functorCompose' :: ThreeFC)

  putStrLn "\nThree' a b: "
  quickCheck $ \x -> functorIdentity (x :: Three' Double Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Three' Double Int)  
  quickCheck (functorCompose' :: Three'FC)

  putStrLn "\nFour a b c d: "
  quickCheck $ \x -> functorIdentity (x :: Four Double Char Double Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Four Double Char Double Int)  
  quickCheck (functorCompose' :: FourFC)

  putStrLn "\nFour' a b: "
  quickCheck $ \x -> functorIdentity (x :: Four' Double Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Four' Double Int)  
  quickCheck (functorCompose' :: Four'FC)

  putStrLn "Possibly a: "
  quickCheck $ \x -> functorIdentity (x :: Possibly Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Possibly Int)  
  quickCheck (functorCompose' :: PossiblyFC)

  putStrLn "Sum a b: "
  quickCheck $ \x -> functorIdentity (x :: Sum Double Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Sum Double Int)  
  quickCheck (functorCompose' :: SumFC)