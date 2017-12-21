{-# LANGUAGE NamedFieldPuns #-}
module Lib
    ( someFunc
    ) where

import Network.Socket hiding (recv, close)
import qualified Network.Socket as Socket
import Network.Socket.ByteString
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM
import Data.Maybe

import qualified Data.ByteString as B

data Connection = Connection
    { connSock :: Socket
    , linesTQ :: TQueue Text
    , socketReaderTid :: ThreadId
    }

connectTo :: HostName -> ServiceName -> IO Connection
connectTo h sn = withSocketsDo $ do
    -- Open the socket.
    addrinfos <- getAddrInfo Nothing (Just h) (Just sn)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    connect sock (addrAddress serveraddr)
    -- Create an empty queue of lines.
    lTQ <- newTQueueIO
    -- Spawn the reader process.
    rTid <- forkIO $ reader sock lTQ []
    return $ Connection sock lTQ rTid
    where
      reader sock lTQ acc = do
          msg <- recv sock 8 -- 1024
          unless (B.null msg) $
              case decodeUtf8' msg of
                  Left decErr -> return () -- TODO: we might need to put the error somewhere
                  Right text -> do
                      -- putStrLn $ "Working on " ++ show text
                      rest <- putLines lTQ text acc
                      reader sock lTQ rest

      putLines lTQ text acc = do
          -- putStrLn $ "text = " ++ show text
          -- putStrLn $ "acc = " ++ show acc
          if isNothing $ T.find (=='\n') text
              then return (text:acc)
              else do
              let (suffix, remainder) = T.break (== '\n') text
                  line = T.concat (reverse (suffix:acc))
              atomically $ writeTQueue lTQ line
              putLines lTQ (T.tail remainder) []
              

readFrom :: Connection -> IO Text
readFrom Connection {linesTQ} =
    atomically $ readTQueue linesTQ

putTo :: Connection -> Text -> IO ()
putTo = undefined

close :: Connection -> IO ()
close Connection{connSock, socketReaderTid} = do
    Socket.close connSock
    killThread socketReaderTid
    

someFunc :: IO ()
someFunc = withSocketsDo $ do
    conn <- connectTo "localhost" "9090"
    line <- readFrom conn
    print line
    line <- readFrom conn
    print line
    line <- readFrom conn
    print line
    line <- readFrom conn
    print line    

    
