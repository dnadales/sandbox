-- |

module ExercisesFoldableSpec where

import           Data.Foldable
import           Data.Monoid
import           Test.Hspec

spec :: Spec
spec = do
  describe "foldl" $ do
    it "folds a list of numbers with +" $ do
      foldl (+) 0 [0, 1, 2, 3] `shouldBe` 6

    it "folds a list of strings with ++" $ do
      foldl (++) "" ["a", "b", "c"] `shouldBe` "abc"

  describe "fold" $ do
    it "folds a list of monoids (String is a Monoid)" $ do
      fold ["a", "b", "c"] `shouldBe` "abc"

    it "folds a list of monoids (Integer with + is a Monoid)" $ do
      fold [0, 1, 2, 3] `shouldBe` (6 :: Sum Int)

    it "folds a list of monoids (Integer with * is a Monoid)" $ do
      fold [0, 1, 2, 3] `shouldBe` (0 :: Product Int)

  describe "foldMap" $ do
    it "sums the string lengths" $ do
      foldMap (Sum . length) ["a", "b", "c"] `shouldBe` 3

    it "concatenates the string representation of numbers" $ do
      foldMap show [0, 1, 2, 3] `shouldBe` "0123"

  describe "foldK" $ do
    it "is not necessary in the case of lists" $ do
      fold [[0, 1, 2], [3, 4, 5]] `shouldBe` [0, 1, 2, 3, 4, 5]

    it ("requires to define a different instance of Maybe to " ++
      " have the same behavior as in Cats") $ do
      fold [Nothing, Just "one", Just "two"] `shouldBe` Just "onetwo"

  describe "find" $ do
    it "returns Just when an element that satisfies the predicate is found" $ do
      find (2 <) [0, 1, 2, 3] `shouldBe` Just 3

    it "returns Nothing when no element that satisfies the predicate is found" $ do
      find (5 <) [0, 1, 2, 3] `shouldBe` Nothing

  describe "toList" $ do
    it "behaves as the identity function for lists" $ do
      toList [0, 1, 2] `shouldBe` [0, 1, 2]

    it "extracts the value inside a Maybe value" $ do
      toList (Just (Sum 15)) `shouldBe` [15]

    it "returns an empty list when Nothing is passed" $ do
      toList Nothing `shouldBe` ([] :: [()])
