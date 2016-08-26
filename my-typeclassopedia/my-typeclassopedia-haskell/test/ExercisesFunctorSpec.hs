-- |

module ExercisesFunctorSpec where

import           Data.Functor.Compose
import           Test.Hspec


spec :: Spec
spec = do
  describe "Maybe as an instance of functor" $ do
    it "applies function length to the string inside the Maybe value" $ do
      length <$> Just ("hello") `shouldBe` Just 5

    it "does not apply the length function when the value is None" $ do
      length <$> (Nothing :: Maybe String) `shouldBe` Nothing

  describe "(a ->) as an instance of functor" $ do
    it "makes `fmap` behave as function composition" $ do
      ((*2) <$> (10+) $ 2) `shouldBe` 24

  describe "Lift for Maybe" $ do
    it "lifts a -> b to Maybe a -> Maybe b" $ do
      -- No need to invent a `lift` operation here. Unlike in Cats.
      (length <$>) (Just "hello") `shouldBe` Just 5

  describe "composition of functors" $ do
    it "just works in Haskell!" $ do
      let xs = Compose [Just 0, Nothing, Just 2] :: Compose [] Maybe Int
      (+1) <$> xs `shouldBe` Compose [Just 1, Nothing, Just 3]
