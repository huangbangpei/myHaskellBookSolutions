import Data.Monoid
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 1. Type
-- []
-- Methods
pure :: a -> [a]
(<*>) ::  [(a -> b)] -> [a] -> [b]


-- 2. Type
-- IO
-- Methods
pure :: a -> IO a
(<*>) ::  IO (a -> b) -> IO a -> IO b


-- 3. Type
-- (,) a
-- Methods
pure :: Monoid a => b -> (a, b)
(<*>) :: Monoid a => (a, (b -> c)) -> (a, b) -> (a, c)


-- 4. Type
-- (->) a
-- Methods
pure :: b -> (b -> a)
(<*>) :: (c -> (b -> a)) -> (c -> b) -> (c -> a)




-- 1.
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity b = Identity (f b)  

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Eq a => EqProp (Identity a) where
  (=-=) = eq


-- 2.
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure x = Pair x x
  Pair f g <*> Pair a b = Pair (f a) (g b)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = liftA2 Pair arbitrary arbitrary

instance Eq a => EqProp (Pair a) where
  (=-=) = eq


-- 3.
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a)  where
  pure = Two mempty
  Two a f <*> Two a' x = Two (a <> a') (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = liftA2 Two arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq


-- 4.
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  Three a b f <*> Three a' b' x = Three (a <> a') (b <> b') (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq


-- 5.
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  Three' a f g <*> Three' a' x y = Three' (a <> a') (f x) (g y) 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = liftA3 Three' arbitrary arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq


-- 6.

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  Four a b c f <*> Four a' b' c' x = Four (a <> a') (b <> b') (c <> c') (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq


-- 7.
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty 
  Four' a b c f <*> Four' a' b' c' x = Four' (a <> a') (b <> b') (c <> c') (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

main :: IO ()
main = do
  putStrLn "Identity: "
  quickBatch (applicative (undefined :: Identity (Int, Int, Int)))
  putStrLn "\nPair: "
  quickBatch (applicative (undefined :: Pair (Int, Int, Int)))
  putStrLn "\nTwo: "
  quickBatch (applicative (undefined :: Two (String, String, String) (Int, Int, Int)))
  putStrLn "\nThree: "
  quickBatch (applicative (undefined :: Three String [Double] (Double, Double, Double)))
  putStrLn "\nThree': "
  quickBatch (applicative (undefined :: Three' String (Int, Int, Int)))
  putStrLn "\nFour: "
  quickBatch (applicative (undefined :: Four String [Int] (Maybe String) (Int, Int, Int)))
  putStrLn "\nFour': "
  quickBatch (applicative (undefined :: Four' (Maybe String) (Int, Int, Int)))


-----------------------------------
stops, vowels :: String
stops = "pbtdkg"
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
