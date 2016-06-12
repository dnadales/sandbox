-- |

module MyLibSpec where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           MyLib
import           Test.Hspec

boom = error "Handle me!"

pokemonHandler :: a -> SomeException -> IO a
pokemonHandler a e = return a

spec :: Spec
spec = do
  describe "casMVar" $ do
    it "puts back a value on the MVar under an exception" $ do
      m <- newMVar "foo"
      a <- async $
        casMVar m boom "bar" `catch` pokemonHandler True
      _ <- wait a
      val <- readMVar m
      val `shouldBe` "foo"
