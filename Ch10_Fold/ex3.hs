-- P362

-- 1.
fibs' = take 20 $ 1 : scanl (+) 1 fibs'

-- 2.

fibsLessThan100 = takeWhile (<100) $ 1 : scanl (+) 1 fibsLessThan100