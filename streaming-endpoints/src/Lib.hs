{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MonomorphismRestriction #-}
module Lib
    ( app
    ) where

import Prelude hiding (mapM_, mapM, map)
import Servant ( Proxy (Proxy), Handler, Tagged, Application
               , Raw, JSON, (:<|>) ((:<|>)), (:>), serve
               , Tagged (Tagged)
               )
import Servant.API.Stream
import Data.Aeson.Types
import GHC.Generics
import Control.Concurrent (threadDelay)
import Conduit  ( Source, (.|)
                , runConduit, yield
                , getZipSource
                , ConduitM
                , ZipSource (..)
                , repeatC
                , yieldMany
                )
import Data.Void (Void)
import           Data.Conduit.Combinators ( mapM
                                          , mapM_
                                          , map)

import           Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.EventSource (eventSourceAppChan, ServerEvent)
import Control.Concurrent.Chan (Chan)
import Control.Monad.IO.Class (liftIO)
    
-- | The data types
data User = User
  { name :: String
  , age :: Int
  } deriving (Eq, Show, Generic, ToJSON)

isaac :: User
isaac = User "Isaac" 99

albert :: User
albert = User "Albert" 123

simon :: User
simon = User "Simon" 60

-- | API definition.
type API = UsersEP :<|> NumbersEP :<|> SseEP
type UsersEP = "users" :> StreamGet NewlineFraming JSON (StreamGenerator User)
type NumbersEP = "numbers" :> StreamGet NewlineFraming JSON (StreamGenerator User)
type SseEP = "sse" :> Raw

streamAPI :: Proxy API
streamAPI = Proxy

streamUsers :: StreamGenerator User
streamUsers = StreamGenerator $ \sendFirst sendRest -> do
                       sendFirst isaac
                       threadDelay (10^6)
                       sendRest albert
                       threadDelay (10^6)                       
                       sendRest simon

numbersConduit :: Source IO Int
numbersConduit = yieldMany [1..10]

streamNumbers :: StreamGenerator User
streamNumbers = StreamGenerator $ \sendFirst sendRest ->
    runConduit $ getZipSource ((,) <$> isFirstSource <*> usersSource)
              .| mapM_ (sendData sendFirst sendRest)
    where
      isFirstSource :: ZipSource IO Bool
      isFirstSource = ZipSource $ yield True >> repeatC  False
      usersSource :: ZipSource IO User
      usersSource = ZipSource $ numbersConduit
               .| map (User "John")
               .| mapM delayAndPass
      delayAndPass a =
          threadDelay (10^6) >> return a
      sendData :: (a -> IO ()) -> (a -> IO ()) -> (Bool, a) -> IO ()
      sendData f g (True, a)= 
          f a
      sendData f g (False, a)= 
          g a          

sseHandler :: Chan User -> Tagged Handler Application
-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
sseHandler uCh = Tagged $ \req respond -> do
    liftIO $ print "Hello"
    eventSourceAppChan (chanT uCh) req respond

myChan :: Chan User
myChan = undefined

chanT :: Chan User -> Chan ServerEvent
chanT = undefined

app :: Application
app = simpleCors $ serve streamAPI (return streamUsers
                                    :<|> return streamNumbers
                                    :<|> sseHandler myChan
                                   )

