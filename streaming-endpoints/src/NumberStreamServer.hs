{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module NumberStreamServer where

import           Conduit                     (ConduitM, ConduitT,
                                              ZipSource (..), getZipSource,
                                              repeatC, runConduit, yield,
                                              yieldMany, (.|))
import           Conduit                     (ConduitM, Source, ZipSource (..),
                                              getZipSource, repeatC, runConduit,
                                              yield, yieldMany, (.|))
import           Control.Concurrent          (threadDelay)
import           Data.Aeson                  (ToJSON)
import           Data.Conduit.Combinators    (map, mapM, mapM_)
import           GHC.Generics                (Generic)
import           Network.Wai.Middleware.Cors (simpleCors)
import           Prelude                     hiding (map, mapM, mapM_)
import           Servant                     ((:>), Application, JSON,
                                              NewlineFraming, Proxy (Proxy),
                                              Server, StreamGenerator,
                                              StreamGenerator (StreamGenerator),
                                              StreamGet, serve)
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
      sendData _ g (False, a)=
          g a
      numbersConduit :: ConduitT () Int IO ()
      numbersConduit = yieldMany [1..10]

-- * The application

app :: Application
app = simpleCors $ serve api server
    where
      api :: Proxy API
      api = Proxy
      server :: Server API
      server = return streamNumbers
