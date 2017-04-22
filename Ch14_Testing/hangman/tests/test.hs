module Main where

import Test.Hspec
import Hangman

newPuzzle = freshPuzzle "cat"
expectedPuzzle1 = Puzzle "cat" [Just 'c', Nothing, Nothing] ['c']
expectedPuzzle2 = Puzzle "cat" [Nothing, Nothing, Nothing] ['e']


main :: IO ()
main = hspec $ do 
  describe "fillInCharacter" $ do
    it "should fill in puzzle when given character is correct" $
      (fillInCharacter newPuzzle 'c') `shouldBe` expectedPuzzle1
    it "should not fill in puzzle when given character is not correct" $
      (fillInCharacter newPuzzle 'e') `shouldBe` expectedPuzzle2
    -- todo
