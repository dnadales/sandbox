{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric   #-}
module EchoServer where

import Servant ( Proxy (Proxy), Handler, Tagged, Application
               , Raw, JSON, (:<|>) ((:<|>)), (:>), serve
               , Tagged (Tagged)
               , Post, hoistServer
               , ServerT
               , PostCreated
               , Capture
               , NoContent
               )
import           Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import           Data.Text (Text)
import           Data.Aeson                  (FromJSON, ToJSON)
import           GHC.Generics                (Generic)
import           Control.Concurrent.MVar (MVar)
import qualified Data.Map as Map
import           Data.Map (Map)
import           Control.Concurrent.Chan (Chan, dupChan)
import           Network.Wai.EventSource (eventSourceAppChan, ServerEvent)
import           Control.Monad.IO.Class (liftIO)
import           GHC.Conc (TVar, atomically, readTVar)
import           Network.Wai (responseLBS)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as T
import           Network.HTTP.Types.Status (status404)
import           Data.Semigroup ((<>))

-- * The API

type API = NewSessionEP :<|> SendEP :<|> EventsEP

type NewSessionEP = "session"
                 :> "new"
                 :> PostCreated '[JSON] SessionId

type SendEP = "session"
           :> Capture "sid" SessionId
           :> "echo"
           :> Post '[JSON] NoContent
              
type EventsEP = "events"
             :> Capture "sid" SessionId
             :> Raw

-- * The data

newtype Message = Message { msg :: Text }
    deriving (Eq, Show, Generic)

instance ToJSON Message

type SessionId = Int

data Env = Env
    { nextId :: MVar Int
    , sessionChans :: TVar (Map SessionId (Chan ServerEvent))
    }

-- * The handlers

type EchoHandler = ReaderT Env Handler

-- * The application

app :: Env -> Application
app env = serve api $ hoistServer api (nt env) server
    where
      nt :: Env -> EchoHandler a -> Handler a
      nt env handler = runReaderT handler env
      api :: Proxy API
      api = Proxy
      server :: ServerT API EchoHandler
      server = newSessionH :<|> sendH :<|> eventsH env
          where
            newSessionH :: EchoHandler SessionId
            newSessionH = undefined
            sendH :: SessionId -> EchoHandler NoContent
            sendH = undefined
            eventsH :: Env -> SessionId -> Tagged EchoHandler Application
            eventsH env sid = Tagged $ \req respond -> do
                mCh <- atomically $ do
                    sc <- readTVar (sessionChans env)
                    return (Map.lookup sid sc)
                case mCh of
                    Nothing -> do
                        let msg = "Could not find session with id: "
                               <> encodeUtf8 (T.pack (show sid))
                        respond $ responseLBS status404 [] msg
                    Just ch -> do
                        ch' <- dupChan ch
                        eventSourceAppChan ch req respond

