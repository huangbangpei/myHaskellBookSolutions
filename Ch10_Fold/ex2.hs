-- P356

import Data.Time


data DatabaseItem = DbString String
    | DbNumber Integer
    | DbDate UTCTime
    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime
    (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime
    (fromGregorian 1921 5 1)
    (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr f []
    where f el acc = case el of
            (DbDate time) -> time : acc
            _ -> acc

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber =  foldr f []
    where f el acc = case el of
            (DbNumber num) -> num : acc
            _ -> acc

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb xs = (fromIntegral $ sumDb xs) / (fromIntegral $ length $ filterDbNumber $ xs) 