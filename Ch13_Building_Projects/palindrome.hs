import Control.Monad
import System.Exit (exitSuccess)
import Data.Char

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (onlyLowerLetter line1 == reverse (onlyLowerLetter line1) )of
    True -> putStrLn "It's a palindrome!"
    False -> do 
      putStrLn "Nope!"
      exitSuccess

onlyLowerLetter :: String -> String
onlyLowerLetter = filter (isLetter) . map (toLower)            