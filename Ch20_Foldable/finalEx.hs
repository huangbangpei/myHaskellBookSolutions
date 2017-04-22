 import Data.Foldable
import Data.Monoid


data Constant a b = Constant a deriving (Eq, Show)

instance Foldable (Constant a) where
  foldMap f (Constant y) = mempty


data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
  foldMap f (Two x y) = f y


data Three a b c = Three a b c deriving (Eq, Show)

instance Foldable (Three a b) where
  foldMap f (Three x y z) = f z


data Three' a b = Three' a b b deriving (Eq, Show)

instance Foldable (Three' a) where
  foldMap f (Three' a b c) = f b <> f c


data Four' a b = Four' a b b b deriving (Eq, Show)

instance Foldable (Four' a) where
  foldMap f (Four' a b c d) = f b <> f c <> f d

filterF :: (Applicative f, Foldable f, Monoid (f a)) => (a -> Bool) -> f a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)

filterF' :: (Applicative f, Foldable f, Monoid (f a)) => (a -> Bool) -> f a -> f a
filterF' f = foldMap pure . filter f . toList