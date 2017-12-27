{-# LANGUAGE NamedFieldPuns #-}
module Lib
    ( someFunc
    , Connection ()
    , connectTo
    , readFrom
    , putTo
    , close
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
import Data.Text.Encoding.Error
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

data Connection = Connection
    { connSock :: Socket
    , linesTQ :: TQueue Text
    , socketReaderTid :: ThreadId
    }

-- | Connect to the given host and service name (usually a port number).
--
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
    rTid <- forkIO $ reader sock lTQ [] (streamDecodeUtf8With lenientDecode)
    return $ Connection sock lTQ rTid
    where
      -- | Reads byte-strings from the given socket, decodes the byte-string
      -- into a @Text@ value, and as soon as a new line is found in the text,
      -- the line is placed in the given @TQueue@.
      reader :: Socket      -- ^ Socket on which the byte-strings will be received.
             -> TQueue Text -- ^ Transactional queue where to put the text
                            -- lines that are received.
             -> [Text]      -- ^ Text fragments that were received so far,
                            -- where no new lines are found
             -> (ByteString -> Decoding) -- ^ Decoding function. See @Data.Text.Encoding@
             -> IO ()
      reader sock lTQ acc f = do
          msg <- recv sock 8 -- 1024
          -- Receiving a null byte-string probably means that the sending side
          -- has closed the connection.
          unless (B.null msg) $ do 
              let Some text _ g = f msg
              rest <- putLines lTQ text acc
              reader sock lTQ rest g

      -- | If a new-line is found in @text@, put @text@ together with the
      -- remainder date in @acc@ as one line in @lTQ@. The buffer @acc@ stores
      -- the line fragments as a stack, so it is necessary to reverse this list
      -- before concatenating all the fragments together.
      putLines lTQ text acc =
          if isNothing $ T.find (=='\n') text
              then return (text:acc) -- The text does not contain a new line,
                                     -- so we add it to the front to the
                                     -- fragments list. This means that the
                                     -- text fragments will appear in the
                                     -- reverse order, so it is necessary to
                                     -- reverse the elements when forming the
                                     -- whole line with these fragments.
              else do
              let (suffix, remainder) = T.break (== '\n') text
                  line = T.concat (reverse (suffix:acc)) -- Note that we're
                                                         -- reversing the
                                                         -- buffer here.
              atomically $ writeTQueue lTQ line
              putLines lTQ (T.tail remainder) [] -- We take the tail of the
                                                 -- remaining fragment to
                                                 -- discard the new line
                                                 -- character.

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

    
