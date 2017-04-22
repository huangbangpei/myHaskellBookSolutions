module Jammin where

import Data.List	

data Fruit =
	Peach
  | Plum
  | Apple
  | Blackberry
  deriving (Eq, Show, Ord)

data JamJars = Jam { 
    fruit :: Fruit
  , jars :: Int 
} deriving (Eq, Show, Ord)

-- the cardinality of JamJars is  4 * 2^64

row1 = Jam Apple 3
row2 = Jam Plum 5
row3 = Jam Peach 7
row4 = Jam Blackberry 10
row5 = Jam Peach 3
row6 = Jam Plum 1
allJam = [row1, row2, row3, row4, row5, row6]

sumOfJams :: [JamJars] -> Int
sumOfJams = sum . map 

mostRow :: [JamJars] -> JamJars
mostRow = maximumBy (\a b -> compare (jars a) (jars b))

compareKind (Jam k _) (Jam k' _) = compare k k'

sortJams :: [JamJars] -> [JamJars]
sortJams = sortBy compareKind

groupJams :: [JamJars] -> [[JamJars]]
groupJams = groupBy (\a b -> compareKind a b == EQ) . sortJams