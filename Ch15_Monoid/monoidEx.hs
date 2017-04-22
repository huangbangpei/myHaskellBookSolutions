import Data.Monoid hiding ((<>))
import Data.Semigroup hiding (mappend)
import Test.QuickCheck hiding (Success, Failure)
import Control.Monad


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Monoid m, Semigroup m, Eq m) => m -> Bool
monoidLeftIdentity x = mempty <> x == x

monoidRightIdentity :: (Monoid m, Semigroup m, Eq m) => m -> Bool
monoidRightIdentity x = x <> mempty == x

type S = String
type Id = Identity

---------------------
-- 1.

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _= Trivial 

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = 
    return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool


---------------------
-- 2.

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

instance (Monoid a, Semigroup a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>) 

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return (Identity x)  

type IdentityAssoc = Id S -> Id S -> Id S -> Bool 


---------------------
-- 3.

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Monoid a, Semigroup a, Monoid b, Semigroup b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoAssoc = Two S S -> Two S S -> Two S S -> Bool


---------------------
-- 4.

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  (BoolConj _) <> (BoolConj _)       = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary :: Gen Bool
    return (BoolConj a)  

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool


---------------------
-- 5.

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj True) <> (BoolDisj _)    = BoolDisj True
  (BoolDisj _) <> (BoolDisj True)    = BoolDisj True
  (BoolDisj _) <> (BoolDisj _)       = BoolDisj False
  
instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary :: Gen Bool
    return (BoolDisj a)  

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool


---------------------
-- 6.

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (f <> g)

instance (Monoid b, Semigroup b) => Monoid (Combine a b) where
  mempty = Combine mempty
  mappend = (<>)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return (Combine f)

-- need to improve 
instance Show (Combine a b) where
  show (Combine f) = "Combine Function"

combineAssocFunc :: (Eq m, Semigroup m) => Combine a m -> Combine a m -> Combine a m -> a -> Bool
combineAssocFunc f g h x = unCombine ((f <> g) <> h) x == unCombine (f <> (g <> h)) x

monoidLeftIdentityCombine :: (Eq b, Semigroup b, Monoid b) => Combine a b -> a -> Bool
monoidLeftIdentityCombine f c = unCombine (mempty <> f) c == unCombine f c

monoidRightIdentityCombine :: (Eq b, Semigroup b, Monoid b) => Combine a b -> a -> Bool
monoidRightIdentityCombine f c = unCombine (f <> mempty) c == unCombine f c

type CombineAssoc = Combine Int S -> Combine Int S -> Combine Int S -> Int -> Bool


---------------------
-- 7.
newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup a => Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f <> g)

instance (Semigroup a, Monoid a) => Monoid (Comp a) where
  mempty = Comp mempty
  mappend = (<>)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    f <- arbitrary
    return (Comp f)    

-- need to improve 
instance Show (Comp a) where
  show (Comp f) = "Comp Function"

compAssocFunc :: (Eq a, Semigroup a) => Comp a -> Comp a -> Comp a -> a -> Bool
compAssocFunc f g h x = unComp ((f <> g) <> h) x == unComp (f <> (g <> h)) x

monoidLeftIdentityComp :: (Eq a, Semigroup a, Monoid a) => Comp a -> a -> Bool
monoidLeftIdentityComp f c = unComp (mempty <> f) c == unComp f c

monoidRightIdentityComp :: (Eq a, Semigroup a, Monoid a) => Comp a -> a -> Bool
monoidRightIdentityComp f c = unComp (f <> mempty) c == unComp f c

type CompAssoc = Comp S -> Comp S -> Comp S -> S -> Bool


---------------------
-- 8.
-- based on https://github.com/Tclv/HaskellBook/blob/master/ch15/chapterExercises/Monoid.hs
newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance Monoid a => Monoid (Mem s a) where
  mappend (Mem f1) (Mem f2) = Mem $ \s ->
    let (a', s')   = f1 s
        (a'', s'') = f2 s'
    in (a' `mappend` a'', s'')
  mempty = Mem $ \s -> (mempty, s)

instance Show (Mem a b) where
  show (Mem f) = "Mem Func"

instance (CoArbitrary s, Arbitrary s, Arbitrary a) => Arbitrary (Mem s a) where
  arbitrary = do
    s <- arbitrary
    return $ Mem s

monoidMemAssoc :: (Eq a, Eq s, Monoid a) => Mem s a -> Mem s a -> Mem s a -> s -> Bool
monoidMemAssoc f g h c = runMem ((f `mappend` g) `mappend` h) c == runMem (f `mappend` (g `mappend` h)) c

monoidLeftIdentityMem :: (Eq a, Eq s, Monoid a) => Mem s a -> s -> Bool
monoidLeftIdentityMem f c = runMem (mempty `mappend` f) c == runMem f c

monoidRightIdentityMem :: (Eq a, Eq s, Monoid a) => Mem s a -> s -> Bool
monoidRightIdentityMem f c = runMem (f `mappend` mempty) c == runMem f c

f' :: Mem Int String
f' = Mem $ \s -> ("hi", s + 1)


main :: IO ()
main = do
  putStrLn "Trivial tests: "
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)

  putStrLn "\nIdentity Tests: "
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: Identity S -> Bool)
  quickCheck (monoidRightIdentity :: Identity S -> Bool)

  putStrLn "\nTwo Tests: "
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two S S -> Bool)
  quickCheck (monoidRightIdentity :: Two S S -> Bool)

  putStrLn "\nBoolConj Tests: "
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)

  putStrLn "\nBoolDisj Tests: "
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

  putStrLn "\nCombine Tests: "
  quickCheck (combineAssocFunc :: CombineAssoc)
  quickCheck (monoidLeftIdentityCombine :: Combine Int S -> Int -> Bool)
  quickCheck (monoidRightIdentityCombine :: Combine Int S -> Int -> Bool)

  putStrLn "\nComp Tests: "
  quickCheck (compAssocFunc :: CompAssoc)
  quickCheck (monoidLeftIdentityComp :: Comp S -> S -> Bool)
  quickCheck (monoidRightIdentityComp :: Comp S -> S -> Bool)

  putStrLn "\nMem Tests: "
  quickCheck (monoidMemAssoc :: Mem Int S -> Mem Int S -> Mem Int S -> Int -> Bool)
  quickCheck (monoidLeftIdentityMem :: Mem Int S -> Int -> Bool)
  quickCheck (monoidRightIdentityMem :: Mem Int S -> Int -> Bool)

  