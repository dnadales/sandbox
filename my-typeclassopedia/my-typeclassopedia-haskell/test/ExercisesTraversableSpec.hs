-- |

module ExercisesTraversableSpec where

import           Data.Either
import           Data.Either.Validation
import           ExercisesTraversable
import           Test.Hspec

errorsList :: Validation [a] b -> [a]
errorsList (Success x) = []
errorsList (Failure xs) = xs


spec :: Spec
spec = do
  describe "traverse with Either as applicative" $ do
    it "should return a Right value, containing the list of parsed integers" $ do
      traverse parseInt ["0", "1", "2"] `shouldBe` Right [0, 1, 2]
    it "should return a Left value, containing the error" $ do
      isLeft (traverse parseInt ["0", "foo", "2"]) `shouldBe` True

  describe "traverse with Validation as applicative" $ do
    it "should return a Success value, containing the list of parsed integers" $ do
      traverse parseIntV ["0", "1", "2"] `shouldBe` Success [0, 1, 2]
    it "should return a Failure value, containing the errors" $ do
      let result = traverse parseIntV ["0", "foo", "bar"]
      (length . errorsList) result `shouldBe` 2

  describe "sequencing using traverse" $ do
    it "collects the values inside an option" $ do
      traverse id [Just 0, Just 1, Just 2] `shouldBe` Just [0, 1, 2]

    it "returns nothing if any of the values is nothing" $ do
      traverse id [Just 0, Nothing, Just 2] `shouldBe` Nothing

  describe "traversing for effect" $ do
    it "executes the effects discarding the results" $ do
      sequence_ [Just 0, Just 1, Just 2] `shouldBe` Just ()

    it "executes the effects discarding the results, and returns Nothing" $ do
      sequence_ [Just 0, Nothing, Just 2] `shouldBe` Nothing
