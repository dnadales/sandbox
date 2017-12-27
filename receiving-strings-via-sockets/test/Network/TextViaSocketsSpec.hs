{-# LANGUAGE OverloadedStrings #-}
module Network.TextViaSocketsSpec where

import Control.Concurrent
import Control.Monad
import Data.Foldable
import Data.Text (Text)
import Network.Socket hiding (close)
import Control.Exception.Base hiding (assert)
import Control.Concurrent.Async
import qualified Data.Text as T

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.QuickCheck.Modifiers
import           Data.Text.Arbitrary ()

import Network.TextViaSockets

newtype ValidPort = ValidPort {port :: PortNumber} deriving (Show, Eq)

instance Arbitrary ValidPort where
    arbitrary = do
        nr <- choose (9000, 65535)
        return $ ValidPort (fromInteger nr)

allMessagesReceived :: ValidPort -> [PrintableString] -> Property
allMessagesReceived validPort strs = monadicIO $ do
    a <- run $ async $ do
        conn <- connectTo "localhost" (show (port validPort))
        lines <- replicateM (length msgs) (readLineFrom conn)
        close conn
        return lines
    run $ forkIO $ writer msgs a
    res <- run $ waitCatch a
    case res of
        Left _ -> assert $ True
        Right lines ->
            if lines == msgs
            then assert True
            else do
                run $ print lines
                run $ print $ msgs
                assert False
    where
      msgs = map (T.pack . getPrintableString) strs
      writer xs a = tryToWrite `catch` handler
          where 
            tryToWrite = do
                conn <- acceptOn (port validPort)
                traverse_ (putLineTo conn) xs
                return ()
            handler :: IOException -> IO ()
            handler ex = cancel a -- An IOException will likely happen when we
                                  -- the address is already in use.
              

spec :: Spec
spec =
    describe "Good weather messages reception." $ 
        it "receives all the messages" $ property $
             \port msgs -> allMessagesReceived port msgs
