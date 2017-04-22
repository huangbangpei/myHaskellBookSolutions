-- P464

-- Determine the kinds
-- 1. *

-- 2. The kind of a is *, the kind of f is * -> * 


-- String processing

vowels = "aeiou"

notThe :: String -> Maybe String
notThe str
    | str == "the" = Nothing
    | otherwise = Just str

replaceThe :: String -> String
replaceThe = unwords . foldr (\el acc -> if el == "the" then "a" : acc else el : acc) [] . words

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = count . words 
    where 
        count (x:y:tail)
            | x == "the" && head y `elem` vowels = 1 + count (y:tail)
            | otherwise = 0 + count (y:tail)
        count _ = 0

countVowels :: String -> Integer
countVowels = fromIntegral . length . filter (`elem` vowels)  



-- Validate the word

consonants = "bcdfghjklmnpqrstvwxyz"

countConsonants :: String -> Integer
countConsonants = fromIntegral . length . filter (`elem` consonants)

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord str
    | countVowels str > countConsonants str = Nothing
    | otherwise = Just $ Word' str



-- It’s only Natural

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat n
    | n < 0 = Nothing
    | otherwise = Just $ down n
    where down n
            | n == 0 = Zero
            | otherwise = Succ (down (n-1))


-- Small library for Maybe

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee acc f Nothing = acc
mayybee acc f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just a) = a

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: Eq a => [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs)
    | x == Nothing = catMaybes xs
    | otherwise = catJust x : catMaybes xs
    where catJust (Just a) = a

flipMaybe :: Eq a => [Maybe a] -> Maybe [a]
flipMaybe s
    | takeWhile (==Nothing) s == [] = Nothing
    | otherwise = Just $ realFlip s 
    where
        realFlip [] = []
        realFlip (x:xs) = flipJust x : realFlip xs
            where
                flipJust (Just a) = a
-- or
flipMaybe' :: [Maybe a] -> Maybe [a]
flipMaybe' = foldr justOrNothing (Just [])
  where
    justOrNothing (Just x) (Just acc) = Just (x : acc)
    justOrNothing _ _ = Nothing




-- Small library for Either

lefts' :: [Either a b] -> [a]
lefts'  = foldr retLeft []
    where 
        retLeft (Left a) acc = a : acc
        retLeft _ acc = acc

rights' :: [Either a b] -> [b]
rights' = foldr retRight []
    where 
        retRight (Right a) acc = a : acc
        retRight _ acc = acc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr retEitherTuple ([], [])
    where 
        retEitherTuple (Left a) (l, r) = (a:l, r)
        retEitherTuple (Right a) (l, r) = (l, a:r)

-- or
partitionEithers'' :: [Either a b] -> ([a], [b])
partitionEithers'' x = (lefts' x, rights' x)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left x) = Nothing
eitherMaybe' f (Right y) = Just $ f y

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g x = case x of
    Left a -> f a
    Right a -> g a

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' g = either' (\x -> Nothing) (Just . g)



-- Write your own iterate and unfoldr

myIterate :: (a -> a) -> a -> [a]
myIterate f x = [x] ++ (myIterate f (f x)) 

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
    Nothing -> []
    Just (y, z) -> [y] ++ myUnfoldr f z

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\b -> Just (b, f b))



-- Finally something other than a list!
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x = case f x of
    Nothing -> Leaf
    Just (a, y, b) -> Node (unfold f a) y (unfold f b)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
    where f x = if x < n then Just (x+1, x, x+1) else Nothing