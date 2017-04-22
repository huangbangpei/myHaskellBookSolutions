import Data.Semigroup
import Test.QuickCheck hiding (Success, Failure)
import Control.Monad

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type S = String
type Id = Identity

---------------------
-- 1.

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial


type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

instance Arbitrary Trivial where
  arbitrary = 
    return Trivial

---------------------
-- 2.

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)
   
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

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoAssoc = Two S S -> Two S S -> Two S S -> Bool        

---------------------
-- 4.

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

type ThreeAssoc = Three S S S -> Three S S S -> Three S S S -> Bool

---------------------
-- 5.

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a b c d) <> (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

type FourAssoc = Four S S S S -> Four S S S S -> Four S S S S -> Bool

---------------------
-- 6.

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  (BoolConj _) <> (BoolConj _)       = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary :: Gen Bool
    return (BoolConj a)  

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool


---------------------
-- 7.

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj True) <> (BoolDisj _)    = BoolDisj True
  (BoolDisj _) <> (BoolDisj True)    = BoolDisj True
  (BoolDisj _) <> (BoolDisj _)       = BoolDisj False
  

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary :: Gen Bool
    return (BoolDisj a)  

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool


---------------------
-- 8.

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
  Snd a <> _ = Snd a
  _ <> Snd a = Snd a
  Fst a <> Fst b = Fst a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ Fst a), (1, return $ Snd b)]  

type OrAssoc = Or S S -> Or S S -> Or S S -> Bool

---------------------
-- 9.

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (f <> g)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return (Combine f)

-- need to improve 
instance Show (Combine a b) where
  show (Combine f) = "Combine Function"

combineAssocFunc :: (Eq m, Semigroup m) => Combine a m -> Combine a m -> Combine a m -> a -> Bool
combineAssocFunc f g h x = unCombine ((f <> g) <> h) x == unCombine (f <> (g <> h)) x

type CombineAssoc = Combine Int S -> Combine Int S -> Combine Int S -> Int -> Bool


---------------------
-- 10.

newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup a => Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f <> g)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    f <- arbitrary
    return (Comp f)    

-- need to improve 
instance Show (Comp a) where
  show (Comp f) = "Comp Function"

compAssocFunc :: (Eq a, Semigroup a) => Comp a -> Comp a -> Comp a -> a -> Bool
compAssocFunc f g h x = unComp ((f <> g) <> h) x == unComp (f <> (g <> h)) x


type CompAssoc = Comp S -> Comp S -> Comp S -> S -> Bool


---------------------
-- 11.

data Validation a b = Failure a | Success b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Validation a b) where
  Failure x <> Failure y = Failure (x <> y)
  Failure x <> _ = Failure x
  _ <> Failure y = Failure y
  Success x <> Success y = Success (x <> y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ Failure a), (1, return $ Success b)]  

type ValidationAssoc = Validation S S -> Validation S S -> Validation S S -> Bool


---------------------
-- 12.

newtype AccumulateRight a b = AccumulateRight (Validation a b) deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  AccumulateRight _ <> AccumulateRight (Failure x) = AccumulateRight (Failure x)
  AccumulateRight (Success x) <> AccumulateRight (Success y) = AccumulateRight (Success y)
  AccumulateRight (Failure x) <> AccumulateRight (Success y) = AccumulateRight (Failure x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ AccumulateRight (Failure a)), (1, return $ AccumulateRight (Success b))]  

type AccumulateRightAssoc = AccumulateRight S S -> AccumulateRight S S -> AccumulateRight S S -> Bool


---------------------
-- 13.

newtype AccumulateBoth a b = AccumulateBoth (Validation a b) deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  AccumulateBoth (Success x) <> AccumulateBoth (Success y) = AccumulateBoth $ Success (x <> y)
  AccumulateBoth (Failure x) <> AccumulateBoth (Success y) = AccumulateBoth $ Failure x
  AccumulateBoth (Failure x) <> AccumulateBoth (Failure y) = AccumulateBoth $ Failure (x <> y)
  AccumulateBoth _ <> AccumulateBoth (Failure x) = AccumulateBoth (Failure x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ AccumulateBoth (Failure a)), (1, return $ AccumulateBoth (Success b))]  

type AccumulateBothAssoc = AccumulateBoth S S -> AccumulateBoth S S -> AccumulateBoth S S -> Bool



main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck (combineAssocFunc :: CombineAssoc)
  quickCheck (compAssocFunc :: CompAssoc)
  quickCheck (semigroupAssoc :: ValidationAssoc)
  quickCheck (semigroupAssoc :: AccumulateRightAssoc)
  quickCheck (semigroupAssoc :: AccumulateBothAssoc)