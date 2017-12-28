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

readerProcess :: IO Connection -> Int -> IO [Text]
readerProcess mConn howMany = do
    conn <- mConn
    lines <- replicateM howMany (readLineFrom conn)
    close conn
    return lines

writerProcess :: IO Connection -> [Text] -> Async a -> IO ()
writerProcess mConn msgs a = tryToWrite `catch` handler
  where
    tryToWrite = do
        conn <- mConn
        traverse_ (putLineTo conn) msgs
        close conn
        return ()
    handler :: IOException -> IO ()
    handler ex = cancel a -- An IOException will likely happen when we the
                          -- address is already in use.

checkMessages :: Either a [Text] -> [Text] -> PropertyM IO ()
checkMessages (Left _) _ = monitor $ collect "Address in use"
checkMessages (Right lines) expected =
    if lines == expected
    then assert True
    else do
        run $ print lines
        run $ print $ expected
        assert False

allMessagesReceived :: IO Connection -- ^ How to get a reader connection.
                    -> IO Connection -- ^ How to get a writer connection.
                    -> ValidPort
                    -> [PrintableString]
                    -> Property
allMessagesReceived readerConnM writerConnM validPort strs = monadicIO $ do
    a <- run $ async $ readerProcess readerConnM (length msgs)
    run $ forkIO $ writerProcess writerConnM msgs a
    res <- run $ waitCatch a
    checkMessages res msgs
    where
      msgs = map (T.pack . getPrintableString) strs

clientReceivesAll :: ValidPort -> [PrintableString] -> Property
clientReceivesAll validPort strs =
    allMessagesReceived connectToServer serveOnPort validPort strs
    where
      connectToServer = connectTo "localhost" (show (port validPort))
      serveOnPort = acceptOn (port validPort)

serverReveivesAll :: ValidPort -> [PrintableString] -> Property
serverReveivesAll validPort strs = 
    allMessagesReceived serveOnPort connectToServer validPort strs
    where
      connectToServer = connectTo "localhost" (show (port validPort))
      serveOnPort = acceptOn (port validPort)

spec :: Spec
spec =
    describe "Good weather messages reception:" $ do
        it "The client receives all the messages" $ 
            quickCheck clientReceivesAll
        it "The server receives all the messages" $
            quickCheck serverReveivesAll
        it "The server and client receive all the messages" $
            pending
            
