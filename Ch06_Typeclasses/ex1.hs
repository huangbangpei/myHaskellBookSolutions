-- 1

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn a) (TisAn b) = a == b

-- 2

data TwoInteger = Two Integer Integer

instance Eq TwoInteger where
    (==) (Two a b) (Two a1 b1) = a == a1 && b == b1

-- 3

data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt a) (TisAnInt b) = a == b
    (==) (TisAString a) (TisAString b) = a == b
    (==) _ _ = False   

-- 4

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair a a1) (Pair b b1) = a == b && a1 == b1

-- 5

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a a1) (Tuple b b1) = a == b && a1 == b1

-- 6

data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
    (==) (ThisOne a) (ThisOne b) = a == b  
    (==) (ThatOne a) (ThatOne b) = a == b
    (==) _ _ = False

-- 7

data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a) (Hello b) = a == b
    (==) (Goodbye a) (Goodbye b) = a == b
    (==) _ _ = False 