{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.QuickCheck
import Cipher

genLetter :: Gen Char
genLetter = elements ['a'..'z']

genLetters :: Gen String
genLetters = listOf genLetter

prop_casearIdentity :: Property
prop_casearIdentity = 
  forAll
  genLetters
  (\x -> x == (uncasearStr 5 $ casearStr 5 x))

prop_vignereIdentity :: Property
prop_vignereIdentity = 
  forAll
  genLetters
  (\x -> x == (unVignere "test" $ vignere "test" x))  


return []
runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
  _ <- runTests
  return ()  