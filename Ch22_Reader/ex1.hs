import Data.Char
import Control.Applicative


cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

tupled :: [Char] -> ([Char], [Char])
tupled = liftA2 (,) rev cap

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
  a <- rev
  b <- cap
  return (a, b)

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = rev <$> cap >>= (,)

newtype Reader r a =
  Reader { getReader :: r -> a }

asksid :: Reader a a
asksid = Reader id