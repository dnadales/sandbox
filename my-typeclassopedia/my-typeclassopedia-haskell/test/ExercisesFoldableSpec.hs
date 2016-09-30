-- |

module ExercisesFoldableSpec where

import           Data.Foldable
import           Data.Functor.Compose
import           Data.Monoid
import           Data.Semigroup
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

    it "extracts the value of Maybe (Just)" $ do
      fold (Just 15) `shouldBe` (15 :: Sum Int)

    it "gives the zero element of the monoid when the maybe value is Nothing" $ do
      fold Nothing `shouldBe` (0 :: Sum Int)

    it ("gives the zero element of the monoid when the maybe value is Nothing"
        ++ " (using product as the monoid operation)") $ do
      fold Nothing `shouldBe` (1 :: Product Int)

  describe "foldMap" $ do
    it "sums the string lengths" $ do
      foldMap (Sum . length) ["a", "b", "c"] `shouldBe` 3

    it "concatenates the string representation of numbers" $ do
      foldMap show [0, 1, 2, 3] `shouldBe` "0123"

    it "gives the minimum and maximum values of a list (*new*)" $ do
      let minMax :: Int -> (Min Int, Max Int)
          minMax x = (Min x, Max x)
      foldMap minMax [0, 1, 2, 3] `shouldBe` (Min 0, Max 3)


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

  describe "composition of foldables" $ do
    it "composes lists,  maybes, and ints (with sum)" $ do
      let xs :: Compose [] Maybe (Sum Int)
          xs = Compose [Just 0, Just 1, Just 2]
      fold xs `shouldBe` 3
      -- In this case the fold is applied to the list, and to the maybe values.
      -- A fold on the maybe values extracts their values.
      -- Note how this is different from `fold [Just 0, Just 1, Just 2] == Just 3`

    it "is different from not using composition" $ do
      let xs = [Just 0, Just 1, Just 2]
      fold xs `shouldBe` Just (3 :: Sum Int)

    it "composes lists,  maybes, and strings" $ do
      let xs :: Compose [] Maybe String
          xs = Compose [Just "0", Nothing, Just "2"]
      fold xs `shouldBe` "02"
