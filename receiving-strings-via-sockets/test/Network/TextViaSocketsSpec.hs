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
import           Data.Text.Arbitrary ()

import Network.TextViaSockets

newtype ValidPort = ValidPort {port :: PortNumber} deriving (Show, Eq)

instance Arbitrary ValidPort where
    arbitrary = do
        nr <- choose (49152, 65535) -- See https://en.wikipedia.org/wiki/Ephemeral_port
        return $ ValidPort (fromInteger nr)

-- | Timeout token.
data Timeout = Timeout

-- | A process that sends and receives data to and from another process, @p@.
sndRcvProc :: Connection     -- ^ Connection to @p@.
           -> Int            -- ^ Number of messages to be received from @p@.
           -> [Text]         -- ^ Messages to send to @p@.
           -> MVar (Async a) -- ^ Async handle to @p@. If an exception arises
                             -- at this process, @p@ has to be canceled.
           -> IO (Either Timeout [Text])
sndRcvProc conn howMany svrMsgs aSvrTV =
    timeout `race` (sndRcvProc' `catch` handler)
    where
      sndRcvProc' = do
          a <- async $ sendMsgs conn svrMsgs
          res <- receiveMsgs conn howMany
          wait a
          close conn
          return res
    -- An IOException will likely happen when we the
    -- address is already in use.        
      handler :: IOException -> IO [Text]
      handler ex = do
          aSvr <- takeMVar aSvrTV
          cancel aSvr
          throwIO ex  

sendMsgs :: Connection -> [Text] -> IO ()
sendMsgs conn = traverse_ (putLineTo conn)

receiveMsgs :: Connection -> Int -> IO [Text]
receiveMsgs conn howMany = replicateM howMany (readLineFrom conn)

checkMessages :: Either a (Either Timeout [Text]) -> [Text] -> PropertyM IO ()
checkMessages (Left _) _ = monitor $ collect ("Address in use." :: String)
checkMessages (Right (Left Timeout)) _ = monitor $ collect ("Timeout." :: String)
checkMessages (Right (Right msgs)) expected =
    if msgs == expected
    then do
        monitor $ collect ("Successful connection." :: String)
        assert True
    else do
        run $ print msgs
        run $ print expected
        assert False

allMessagesReceived :: [PrintableString] -- ^ Messages to be sent to the client.
                    -> [PrintableString] -- ^ Messages to be sent to the server.
                    -> Property
allMessagesReceived strsCli strsSvr = monadicIO $ do
    aCliTV <- run newEmptyMVar
    aSvrTV <- run newEmptyMVar
    sock <- run getFreeSocket
    a <- run $ async $ acceptOnSocket sock
    pnum <- run $ socketPort sock
    cliConn <- run $ connectToWithRetry "localhost" (show pnum)
    svrConn <- run $ wait a
    aCli <- run $ async $ sndRcvProc cliConn (length msgsCli) msgsSvr aSvrTV
    aSvr <- run $ async $ sndRcvProc svrConn (length msgsSvr) msgsCli aCliTV
    run $ putMVar aCliTV aCli
    run $ putMVar aSvrTV aSvr    
    resCli <- run $ waitCatch aCli
    resSvr <- run $ waitCatch aSvr
    checkMessages resCli msgsCli
    checkMessages resSvr msgsSvr
    where
      msgsCli = map (T.pack . getPrintableString) strsCli
      msgsSvr = map (T.pack . getPrintableString) strsSvr

clientReceivesAll :: [PrintableString] -> Property
clientReceivesAll strs =
    allMessagesReceived strs []

timeout :: IO Timeout
timeout = do
    threadDelay ((10:: Int) ^ (6 :: Int))
    return Timeout

serverReveivesAll :: [PrintableString] -> Property
serverReveivesAll =
    allMessagesReceived []

spec :: Spec
spec =
    describe "Good weather messages reception:" $ do
        it "The client receives all the messages" $ 
            property clientReceivesAll
        it "The server receives all the messages" $
            property serverReveivesAll
        it "The server and client receive all the messages" $
            property allMessagesReceived
            
