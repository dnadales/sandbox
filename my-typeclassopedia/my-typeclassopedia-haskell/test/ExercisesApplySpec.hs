-- |

module ExercisesApplySpec where


import           Data.Functor.Compose
import           Test.Hspec


spec :: Spec
spec = do
  describe "Apply for Maybe" $ do
    it "converts option to String" $ do
      Just show <*> Just 1 `shouldBe` Just "1"

    it "Absorbs the Maybe value when there's no function to apply" $ do
      Nothing <*> Just 1 `shouldBe` (Nothing :: Maybe Int)

  describe "Apply for Compose" $ do
    it "just works in Haskell!" $ do
      let xs = Compose [Just 0, Nothing, Just 2] :: Compose [] Maybe Int
      Compose [Just(+1)] <*> xs `shouldBe` Compose [Just 1, Nothing, Just 3]

  describe "Apply for higher arity functions" $ do
    it "does not need ap2, ap3, etc!" $ do
      Just (+) <*> Just 1 <*> Just 2 `shouldBe` Just 3

    it "does not need ap3, ap4, etc!" $ do
      Just (\x y z -> x + y + z) <*> Just 1 <*> Just 2 <*> Just 3 `shouldBe` Just 6

    it "returns none if any of its wrapped arguments do" $ do
      Just (\x y z -> x + y + z) <*> Just 1 <*> Nothing <*> Just 3 `shouldBe` Nothing

  describe "Map on higher arity functions" $ do
    it "just works in Haskell!" $ do
      (+) <$> Just 1 <*> Just 2 `shouldBe` Just 3

  describe "Tuples as a function to `fmap`" $ do
    it "does not need tuple22!" $ do
      (,,) <$> Just 0 <*> Just 1 <*> Just 2 `shouldBe` Just (0, 1, 2)
