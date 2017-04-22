isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) str = x `elem` str && isSubsequenceOf xs str

capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs 

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\el -> (el, capitalizeWord el)) . words 