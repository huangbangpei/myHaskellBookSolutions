{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.List (sort)
import Test.QuickCheck
import Arith

-- some generators

genInt :: Gen Int
genInt = arbitrary

genSafeChar :: Gen Char
genSafeChar = elements ['a'..'z']

genDouble :: Gen Double
genDouble = arbitrary

genString :: Gen [Char]
genString = listOf genSafeChar


---------------------
-- 1.
halfGen :: Gen Double
halfGen = arbitrary

prop_halfIdentity :: Property
prop_halfIdentity = forAll
  halfGen
  (\x -> halfIdentity x == x)


-- 2.

prop_listOrdered :: Property
prop_listOrdered = forAll
  genString
  (listOrdered . sort)  


-- 3.

prop_plusAssociative :: Property
prop_plusAssociative = forAll
  genInt
  plusAssociative

prop_plusCommutative :: Property
prop_plusCommutative = forAll
  genDouble
  plusCommutative


-- 4.

prop_multAssociative :: Property
prop_multAssociative = forAll
  genInt
  multAssociative

prop_multCommutative :: Property
prop_multCommutative = forAll
  genDouble
  multCommutative


-- 5.

-- http://stackoverflow.com/questions/18408292/haskell-quickcheck-generate-random-data-for-function-with-many-input-variables
genNonZero :: Gen Integer
genNonZero = arbitrary `suchThat` (/=0)

prop_quotAndRem :: Property
prop_quotAndRem = 
  forAll genNonZero $ \x -> 
  forAll genNonZero $ \y -> 
  (quot x y) * y + (rem x y) == x

prop_divAndMod :: Property
prop_divAndMod = 
  forAll genNonZero $ \x -> 
  forAll genNonZero $ \y -> 
  (div x y) * y + (mod x y) == x

-- http://stackoverflow.com/questions/14294802/evaluating-function-at-random-arguments-using-quickcheck
-- genNonZeroArgs :: Gen (Int, Int)
-- genNonZeroArgs = do
--   x <- arbitrary `suchThat` (/= 0)
--   y <- arbitrary `suchThat` (/= 0)
--   return (x, y)


-- prop_quotAndRem :: Property
-- prop_quotAndRem = 
--   forAll
--   genNonZeroArgs
--   (uncurry quotAndRem)
  

-- prop_divAndMod :: Property
-- prop_divAndMod = 
--   forAll
--   genNonZeroArgs
--   (uncurry divAndMod)


-- 6.
prop_powerCommutative :: Property
prop_powerCommutative = 
  forAll
  genInt
  powerCommutative

prop_powerAssociative :: Property
prop_powerAssociative = 
  forAll
  genInt
  powerAssociative 


-- 7.
prop_reverseIdentity :: Property
prop_reverseIdentity = 
  forAll
  (arbitrary :: Gen [Int])
  reverseIdentity


-- 8.
-- todo


-- 9.
prop_foldrCons :: Property
prop_foldrCons =
  forAll
  (arbitrary :: Gen [Int])
  (\x -> foldr (:) [] x == (++) [] x)

prop_foldrConcat :: Property
prop_foldrConcat =
  forAll
  (arbitrary :: Gen [[Int]])
  (\x -> foldr (++) [] x == concat x)


-- 10.
genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = arbitrary

-- maybe should using posistive numbers?
prop_takeAndLength :: Property
prop_takeAndLength =
  forAll
  (genTuple :: Gen (Int, [Int]))
  (uncurry takeAndLength)

-- 11.
prop_readAndShow :: Property
prop_readAndShow = 
  forAll
  genString
  readAndShow

--------------------

prop_squareId :: Property
prop_squareId = 
  forAll
  (arbitrary :: Gen Float)
  (\x -> squareIdentity x == x)


--------------------

prop_capWordIdem :: Property
prop_capWordIdem = 
  forAll
  genSafeChar
  capWordIdem

prop_sortIdem :: Property
prop_sortIdem = 
  forAll
  (arbitrary :: Gen [Int])
  sortIdem


--------------------

instance Arbitrary Fool where
  arbitrary = elements [Fulse, Frue]

genFool :: Gen Fool
genFool = arbitrary

-- 2/3 Fulse, 1/3 Frue
genFool' :: Gen Fool
genFool' = elements [Fulse, Fulse, Frue]

return []
runTests :: IO Bool
runTests = $quickCheckAll




main :: IO ()
main = do
  _ <- runTests
  return ()  