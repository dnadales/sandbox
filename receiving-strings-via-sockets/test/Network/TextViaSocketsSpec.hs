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
import Control.Arrow
import Control.Concurrent.MVar

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.QuickCheck.Modifiers
import           Data.Text.Arbitrary ()

import Network.TextViaSockets

newtype ValidPort = ValidPort {port :: PortNumber} deriving (Show, Eq)

instance Arbitrary ValidPort where
    arbitrary = do
        nr <- choose (49152, 65535) -- See https://en.wikipedia.org/wiki/Ephemeral_port
        return $ ValidPort (fromInteger nr)

-- | Timeout token.
data Timeout = Timeout

-- | A process that sends and received data.
sndRcvProc :: IO Connection  -- ^ How to get a connection.
           -> Int            -- ^ Number of messages to be received.
           -> [Text]         -- ^ Messages to send.
           -> MVar (Async a) -- ^ Async handle to the receiving process. If an
                             -- exception arises at this process, the receiving
                             -- process has to be canceled.
           -> IO (Either Timeout [Text])
sndRcvProc mConn howMany svrMsgs aSvrTV =
    timeout `race` (sndRcvProc' `catch` handler)
    where
      sndRcvProc' = do
          conn <- mConn
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

cliProc :: ValidPort -- ^ Port to connect to. 
        -> Int       -- ^ Number of messages to be received.
        -> [Text]    -- ^ Messages to send to the server.
        -> MVar (Async a) -- ^ Async handle to the server process. If an
                          -- exception arises at the client, the server process
                          -- has to be canceled.
        -> IO (Either Timeout [Text])
cliProc vPort howMany svrMsgs aSvrTV =
    sndRcvProc mConn howMany svrMsgs aSvrTV
    where
      mConn = connectTo "localhost" (show (port vPort))

sendMsgs :: Connection -> [Text] -> IO ()
sendMsgs conn msgs = traverse_ (putLineTo conn) msgs

receiveMsgs :: Connection -> Int -> IO [Text]
receiveMsgs conn howMany = replicateM howMany (readLineFrom conn)

svrProc :: ValidPort -- ^ Port to serve on.
        -> Int       -- ^ Number of messages to be received.
        -> [Text]    -- ^ Messages to send to the client.
        -> MVar (Async a) -- ^ Async handle to the client process. If an exception
                          -- arises at the server, this process has to be canceled.
        -> IO (Either Timeout [Text])
svrProc vPort howMany msgs aCliTV = do
    sndRcvProc mConn howMany msgs aCliTV 
  where
    mConn = acceptOn (port vPort)

checkMessages :: Either a (Either Timeout [Text]) -> [Text] -> PropertyM IO ()
checkMessages (Left _) _ = monitor $ collect "Address in use."
checkMessages (Right (Left Timeout)) expected = monitor $ collect "Timeout."
checkMessages (Right (Right lines)) expected =
    if lines == expected
    then do
        monitor $ collect "Successful connection."
        assert True
    else do
        run $ print lines
        run $ print $ expected
        assert False

allMessagesReceived :: ValidPort         -- ^ A port where 
                    -> [PrintableString] -- ^ Messages to be sent to the client.
                    -> [PrintableString] -- ^ Messages to be sent to the server.
                    -> Property
allMessagesReceived port strsCli strsSvr = monadicIO $ do
    aCliTV <- run $ newEmptyMVar
    aSvrTV <- run $ newEmptyMVar    
    aCli <- run $ async $ cliProc port (length msgsCli) msgsSvr aSvrTV
    aSvr <- run $ async $ svrProc port (length msgsSvr) msgsCli aCliTV
    run $ putMVar aCliTV aCli
    run $ putMVar aSvrTV aSvr    
    resCli <- run $ waitCatch aCli
    resSvr <- run $ waitCatch aSvr
    checkMessages resCli msgsCli
    checkMessages resSvr msgsSvr
    where
      msgsCli = map (T.pack . getPrintableString) strsCli
      msgsSvr = map (T.pack . getPrintableString) strsSvr

clientReceivesAll :: ValidPort -> [PrintableString] -> Property
clientReceivesAll vPort strs =
    allMessagesReceived vPort strs []

timeout :: IO Timeout
timeout = do
    threadDelay (10^6)
    putStrLn "timing out"
    return Timeout

serverReveivesAll :: ValidPort -> [PrintableString] -> Property
serverReveivesAll vPort strs =
    allMessagesReceived vPort [] strs

serverAndClientReceiveAll :: ValidPort
                          -> [PrintableString] -- ^ Messages for the server.
                          -> [PrintableString] -- ^ Messages for the client.
                          -> Property
serverAndClientReceiveAll serStrs cliStrs = undefined
      
    
spec :: Spec
spec =
    describe "Good weather messages reception:" $ do
        it "The client receives all the messages" $ 
            quickCheck clientReceivesAll
        it "The server receives all the messages" $
            quickCheck serverReveivesAll
        it "The server and client receive all the messages" $
            pending
            
