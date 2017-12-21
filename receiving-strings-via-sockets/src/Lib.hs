module Lib
    ( someFunc
    ) where

import Network.Socket hiding (recv)
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
          msg <- recv sock 1024
          unless (B.null msg) $
              case decodeUtf8' msg of
                  Left decErr -> return () -- TODO: we might need to put the error somewhere
                  Right text -> do
                      rest <- putLines lTQ text acc
                      reader sock lTQ (text:acc)

      putLines lTQ text acc = do
          if isNothing $ T.find (=='\n') text
              then return (text:acc)
              else do
              let (suffix, remainder) = T.break (== '\n') text
                  line = T.concat (reverse (suffix:acc))
              atomically $ writeTQueue lTQ line
              return [remainder]
              

getLine :: Connection -> IO Text
getLine = undefined

putLine :: Connection -> Text -> IO ()
putLine = undefined

close :: Connection -> IO ()
close = undefined

someFunc :: IO ()
someFunc = withSocketsDo $ do
  addrinfos <- getAddrInfo Nothing (Just "localhost") (Just "9090")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  connect sock (addrAddress serveraddr)
  sockReader sock

  where
    sockReader sock = do
        msg <- recv sock 8 -- I don't like the fact that the size is quite arbitrary.

        -- If the @msg@ is a null string, it means the connection on the Sender
        -- side has been closed.
        unless (B.null msg) $
            case decodeUtf8' msg of
                Left err -> print err
                Right text -> print text >> sockReader sock
