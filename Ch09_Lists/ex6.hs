-- 1.
filter (\x -> x `mod` 3 == 0) [1..30]

-- 2.
length . filter (\x -> x `mod` 3 == 0) $ [1..30]

-- 3.
myFilter = filter (\x -> not (x `elem` ["the", "a", "an"])) . words