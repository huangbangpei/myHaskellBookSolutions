-- P362

-- Warm-up and review

-- 1.a)

[(x,y,z) | x <- stops, y <- vowels, z <- stops]

-- 1.b)
[ x:y:z:"" | x <- stops, y <- vowels, z <- stops, x == 'p']

-- 1.c)

nouns = ["Jeremy", "Julian", "cat", "dog"]
verbs = ["play", "call", "open", "love"]

[(x,y,z) | x <- nouns, y <- verbs, z <- nouns]

-- 2.
-- It calculates the average word length of an input string. Its type is:
-- String -> Int

-- 3.
seekritFunc :: String -> Double
seekritFunc x =
    (/) (fromIntegral $ (sum (map length (words x))))
        (fromIntegral $ (length (words x)))



-- Rewriting functions using folds
-- see myStd.hs


