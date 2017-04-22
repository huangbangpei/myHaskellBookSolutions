-- 1.
myWords :: String -> [String]
myWords ("") = []
myWords str = takeWhile (/=' ') str : (myWords $ dropWhile (==' ') . dropWhile (/=' ') $ str)


-- 2. see poemLines.hs


-- 3.
myPartition :: Char -> String -> [String]
myPartition ch "" = []
myPartition ch str = takeWhile (/=ch) str : (myPartition ch remainStr)
    where remainStr = dropWhile (==ch) . dropWhile (/=ch) $ str

myWords' :: String -> [String]
myWords' = myPartition ' '

myLines' :: String -> [String]
myLines' = myPartition '\n'