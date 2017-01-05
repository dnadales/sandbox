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
    it "works on maybe" $ do
      ifM (Just True) (Just "truthy") (Just "falsy") `shouldBe` Just "truthy"
    it "works on lists" $ do
      ifM [True, False, True] [0, 1] [2, 3] `shouldBe` [0, 1, 2, 3, 0, 1]
      -- | What is happening here?
      --
      -- The definition of @ifM@ is as follows:
      --
      -- > ifM :: Monad m => m Bool -> m a -> m a -> m a
      -- > ifM b t f = do b <- b; if b then t else f
      --
      -- So basically we can rewrite the code above to:
      --
      -- > [True, False, True] >>= \b -> if cond then [0, 1] else [2, 3]
      --
      -- The @>>=@ instance for lists is defined as:
      --
      -- > xs >>= f = [y | x <- xs, y <- f x]
      --
      -- Which means that function @f@ will be applied to each element of the
      -- list @xs@, and then all the elements the resulting lists (@f x@ above,
      -- with @x@ taken from @xs@) will be returned in a single list. This is
      -- operationally equivalent to concatenating the results of
      --
      -- > f x0, f x1, ..., f xn-1
      --
      -- where xs = [x0, x1, ..., xn-1].
      --
      -- Replacing @xs@ with [True, False, True] and @f@ with the if expression
      -- we get:
      --
      -- > [y | cond <- [True, False, True], y <- if cond then [0, 1] else [2, 3]]
      --
      -- Which explains the final result.







