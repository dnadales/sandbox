{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
module NumberStreamServer where

import           Prelude hiding (mapM_, mapM, map)
import           Servant ( JSON, (:>), StreamGenerator, StreamGet
                         , NewlineFraming, Application, Proxy (Proxy), Server
                         , serve
                         , StreamGenerator (StreamGenerator))
import           Data.Aeson                  (ToJSON)
import           GHC.Generics                (Generic)
import           Conduit  ( Source, (.|)
                          , runConduit, yield
                          , getZipSource
                          , ConduitM
                          , ZipSource (..)
                          , repeatC
                          , yieldMany
                          )
import           Control.Concurrent (threadDelay)
import           Data.Conduit.Combinators ( mapM
                                          , mapM_
                                          , map)
import           Conduit  ( Source, (.|)
                          , runConduit, yield
                          , getZipSource
                          , ConduitM
                          , ZipSource (..)
                          , repeatC
                          , yieldMany
                          )                 
-- * The data

newtype Number = Number { intVal :: Int }
    deriving (Eq, Show, Generic)

instance ToJSON Number

-- * The API

type API = NumbersEP

type NumbersEP = "numbers"
              :> StreamGet NewlineFraming JSON (StreamGenerator Number)

-- * The numbers stream
streamNumbers :: StreamGenerator Number
streamNumbers = StreamGenerator $ \sendFirst sendRest ->
    runConduit $ getZipSource ((,) <$> isFirstSource <*> usersSource)
              .| mapM_ (sendData sendFirst sendRest)
    where
      isFirstSource :: ZipSource IO Bool
      isFirstSource = ZipSource $ yield True >> repeatC  False
      usersSource :: ZipSource IO Number
      usersSource = ZipSource $ numbersConduit
               .| map Number
               .| mapM delayAndPass
      delayAndPass a =
          threadDelay (10^6) >> return a
      sendData :: (a -> IO ()) -> (a -> IO ()) -> (Bool, a) -> IO ()
      sendData f g (True, a)=
          f a
      sendData f g (False, a)=
          g a
      numbersConduit :: Source IO Int
      numbersConduit = yieldMany [1..10]

-- * The application

app :: Application
app = serve api server
    where
      api :: Proxy API
      api = Proxy
      server :: Server API
      server = return streamNumbers
