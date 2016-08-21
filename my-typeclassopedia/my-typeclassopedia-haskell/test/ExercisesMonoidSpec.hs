-- |

module ExercisesMonoidSpec where

import qualified Data.Map    as Map
import           Data.Monoid
import           MMap
import           Test.Hspec

spec :: Spec
spec = do
  describe "String as an instance of monoid" $ do

    it "defines 'mempty' as the empty string" $ do
      mempty `shouldBe` ""

    it "defines 'mconcat' as the concatenation of all the strings in the list" $ do
      mconcat ["a", "b", "c"] `shouldBe` "abc"

    it "returns the empty string when 'mconcat' is appliead to the empty list" $ do
      mconcat [] `shouldBe` ""

  describe "MMap as an instance of monoid" $ do

    it "defines mconcat as the combination of the values with the (<>) operator" $ do
      let m0 = MkMMap $ Map.fromList [("a", 1), ("b", 2)] :: MMap String (Sum Int)
          m1 = MkMMap $ Map.fromList[("a", 3)]
      mconcat [m0, m1] `shouldBe` (MkMMap $ Map.fromList [("a", 4), ("b", 2)])

    it "returns an empty map when 'mconcat' is applied to the empty list" $ do
      mconcat [] `shouldBe` (MkMMap $ Map.empty :: MMap String (Sum Int))

  describe "'foldMap' on lists" $ do

    it "sums the numbers when the identity function is used" $ do
      foldMap id [0, 1, 2, 3, 4, 5] `shouldBe` (15 :: Sum Int)

    it "concatenates strings when the show function is used " $ do
      foldMap show [0, 1, 2, 3, 4, 5] `shouldBe` "012345"

    it "applies (<>) on tuples" $ do
      foldMap (\x -> (Sum x, show x)) [0, 1, 2, 3, 4, 5] `shouldBe` (15, "012345")
