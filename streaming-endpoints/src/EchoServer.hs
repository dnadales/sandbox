{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module EchoServer where

import           Control.Concurrent.Chan     (Chan, dupChan, newChan, writeChan)
import           Control.Concurrent.MVar     (MVar)
import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar, newTVarIO,
                                              readTVar)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Binary.Builder         (Builder, fromByteString)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Semigroup              ((<>))
import           Data.Text.Encoding          as TE
import           Data.Text.Lazy              (Text)
import qualified Data.Text.Lazy              as T
import           Data.Text.Lazy.Encoding     as TLE
import           GHC.Generics                (Generic)
import           Network.HTTP.Types.Status   (status404)
import           Network.Wai                 (responseLBS)
import           Network.Wai.EventSource     (ServerEvent,
                                              ServerEvent (ServerEvent),
                                              eventData, eventId, eventName,
                                              eventSourceAppChan)
import           Network.Wai.Middleware.Cors (simpleCors)
import           Servant                     ((:<|>) ((:<|>)), (:>),
                                              Application, Capture, Handler,
                                              JSON, NoContent (NoContent), Post,
                                              PostCreated, Proxy (Proxy), Raw,
                                              ReqBody, Server, Tagged,
                                              Tagged (Tagged), err404,
                                              hoistServer, serve, throwError)

-- * The API

type API = NewSessionEP :<|> SendEP :<|> EventsEP

type NewSessionEP = "session"
                 :> "new"
                 :> PostCreated '[JSON] SessionId

type SendEP = "session"
           :> Capture "sid" SessionId
           :> "echo"
           :> ReqBody '[JSON] Message
           :> Post '[JSON] NoContent

type EventsEP = "session"
             :> Capture "sid" SessionId
             :> "events"
             :> Raw

-- * The data

newtype Message = Message { msgText :: Text }
    deriving (Eq, Show, Generic)

instance ToJSON Message
instance FromJSON Message

type SessionId = Int

data Env = Env
    { nextId       :: TVar SessionId
    , sessionChans :: TVar (Map SessionId (Chan ServerEvent))
    }

newEnv :: IO Env
newEnv = Env <$> newTVarIO 0 <*> newTVarIO emptySessions
    where
      emptySessions = Map.empty

newSession :: Env -> IO SessionId
newSession env = do
    nc <- newChan
    atomically $ do
        nSid <- readTVar (nextId env)
        modifyTVar (nextId env) (+1)
        modifyTVar (sessionChans env) (Map.insert nSid nc)
        return nSid

lookupChannel :: Env -> SessionId -> IO (Maybe (Chan ServerEvent))
lookupChannel env sid = atomically $ do
    sc <- readTVar (sessionChans env)
    return (Map.lookup sid sc)

asServerEvent :: Message -> ServerEvent
asServerEvent msg = ServerEvent
    { eventName = Just eName
    , eventId = Nothing
    , eventData = [msg']
    }
    where
      eName :: Builder
      eName = fromByteString "Message arrived"
      msg'  :: Builder
      msg'  = fromByteString $ TE.encodeUtf8 $ T.toStrict $ msgText msg

-- * The application

app :: Env -> Application
app env = simpleCors $ serve api server
    where
      api :: Proxy API
      api = Proxy
      server :: Server API
      server = newSessionH :<|> sendH :<|> eventsH
          where
            newSessionH :: Handler SessionId
            newSessionH =
                liftIO $ newSession env
            sendH :: SessionId -> Message -> Handler NoContent
            sendH sid msg = do
                mCh <- liftIO $ lookupChannel env sid
                case mCh of
                    Nothing -> do
                        throwError err404
                    Just ch -> do
                        liftIO $ writeChan ch (asServerEvent msg)
                        return NoContent
            eventsH :: SessionId -> Tagged Handler Application
            eventsH sid = Tagged $ \req respond -> do
                mCh <- lookupChannel env sid
                case mCh of
                    Nothing -> do
                        let msg = "Could not find session with id: "
                               <> TLE.encodeUtf8 (T.pack (show sid))
                        respond $ responseLBS status404 [] msg
                    Just ch -> do
                        ch' <- dupChan ch
                        eventSourceAppChan ch req respond

