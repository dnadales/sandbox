{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Lib
    ( app
    ) where

import Prelude hiding (mapM_, mapM, map)
import Servant
import Servant.API.Stream
import Data.Aeson.Types
import GHC.Generics
import Control.Concurrent (threadDelay)
import Conduit  (Source, (.|), runConduit)
import           Data.Conduit.Combinators

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
type API = UsersEP :<|> NumbersEP
type UsersEP = "users" :> StreamGet NewlineFraming JSON (StreamGenerator User)
type NumbersEP = "numbers" :> StreamGet NewlineFraming JSON (StreamGenerator User)

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
streamNumbers = StreamGenerator $ \sendFirst sendRest -> do
    runConduit $ numbersConduit
              .| map (User "John")
              .| mapM delayAndPass
              .| mapM_ sendFirst
              .| mapM_ sendRest
    where
      delayAndPass a = threadDelay (10^6) >> return a

app :: Application
app = serve streamAPI (return streamUsers :<|> return streamNumbers)

