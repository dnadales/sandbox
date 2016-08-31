-- |

module ExercisesApplicativeSpec where

import           Control.Applicative
import           Data.Functor.Compose
import           Test.Hspec

spec :: Spec
spec = do
  describe "Pure and compose" $ do
    it "should compose List and Maybe" $ do
      pure 1 `shouldBe` Compose [Just 1]
