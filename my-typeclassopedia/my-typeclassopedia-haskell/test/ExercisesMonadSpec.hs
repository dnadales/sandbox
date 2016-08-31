-- |

module ExercisesMonadSpec where

import           Control.Conditional
import           Control.Monad       (join)
import           Test.Hspec

spec :: Spec
spec = do
  describe "Maybe as an instance of Monad" $ do
    it "can be flattened" $ do
      res <- return $ join $ Just (Just 0)
      res `shouldBe` (Just 0)

  describe "ifM" $ do
    it "works on lists" $ do
      ifM [True, False, True] [0, 1] [2, 3] `shouldBe` [0, 1, 2, 3, 0, 1]

