-- |

module ExercisesSemigroupSpec where

import qualified Data.Map       as Map
import           Data.Semigroup
import           MMap
import           Test.Hspec

spec :: Spec
spec = do
  describe "Combine" $ do
    it "combines numbers, with sum" $ do
      (1 <> 2 :: Sum Int) `shouldBe` 3

    it "combines numbers, with product" $ do
      (1 <> 2 :: Product Int) `shouldBe` 2

    it "combines lists" $ do
      ([0, 1, 2] <> [3, 4, 5]) `shouldBe` [0, 1, 2, 3, 4, 5]

    it "combines Just and Just" $ do
      (Just 1 <> Just 2 :: Maybe (Sum Int)) `shouldBe` Just 3

    it "combines Just and Nothing" $ do
      (Just 1 <> Nothing :: Maybe (Sum Int)) `shouldBe` Just 1

    it "combines functions" $ do
      let f = ((\x -> x + 1) :: Sum Int -> Sum Int)
          g = ((\x -> x * 10) :: Sum Int -> Sum Int)
      (f <> g) 6 `shouldBe` 67

    it "combines maps" $ do
      let bar5 = MkMMap $ Map.fromList [("bar", 5)] :: MMap String (Sum Int)
          bar6 = MkMMap $ Map.fromList [("bar", 6)] :: MMap String (Sum Int)
          foo5 = MkMMap $ Map.fromList [("foo", bar5)]
          foo6 = MkMMap $ Map.fromList [("foo", bar6)]
          bar11 = MkMMap $ Map.fromList [("bar", 11)] :: MMap String (Sum Int)
      (foo5 <> foo6) ! "foo" `shouldBe` bar11
