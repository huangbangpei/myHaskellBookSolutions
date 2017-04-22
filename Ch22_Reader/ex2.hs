-- P809
{-# LANGUAGE InstanceSigs #-}

import Control.Applicative

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

newtype Reader r a =
  Reader { getReader :: r -> a }

data Person = 
  Person {
    humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog =
  Dog {
    dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

myLiftA2 :: Applicative f =>
  (a -> b -> c)
  -> f a -> f b -> f c

myLiftA2 fn f1 f2 = fn <$> f1 <*> f2 

asks :: (r -> a) -> Reader r a
asks = Reader 

instance Functor (Reader r) where
  fmap f (Reader r) = Reader $ f . r

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader (\r -> a)

  (<*>) :: Reader r (a -> b)
    -> Reader r a
    -> Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ \r -> rab r (ra r)

getDogR' :: Reader Person Dog
getDogR' = Dog <$> Reader dogName <*> Reader address