{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module WeatherReporterFunctionsAndMonads where

import           Control.Monad.IO.Class

-- * Some aliases to keep things simple and the discussion focused.
type WeatherData = String

-- Note that we are also using the 'IO' monad as our base monad to simplify
-- this discussion. In practice a richer monad that accounts for error or
-- logging might be more useful.

class Monad m => WeatherService ws m where
    fetch :: ws -> m WeatherData

-- And we provide a mock implementation.
data DummyService = DummyService

instance Monad m => WeatherService DummyService m where
    fetch _ = return "winter is coming, and I wish I was in the South Hemisphere!"

class Monad m => Storage s m where
    store :: s  -> WeatherData -> m ()

-- A mock implementation for storage.
data DummyStorage = DummyStorage

instance MonadIO m => Storage DummyStorage m where
    store _ w = liftIO $ putStrLn $ "I'm just going to throw away this: " ++ w

class Monad m => Reporter r m where
    report :: r -> WeatherData -> m ()

-- A mock implementation for reporter.

data DummyReporter = DummyReporter

instance MonadIO m => Reporter DummyReporter m where
    report _ w = liftIO $ putStrLn $ "Here in The Netherlands " ++ w

reportWeather :: (MonadIO m, WeatherService ws m, Storage s m, Reporter r m)
              => ws -> s -> r -> m ()
reportWeather ws s r = do
      w <- fetch ws
      store s w
      report r w

-- And then we have to choose the implementation in the concrete function:
dummyWeatherReport :: IO ()
dummyWeatherReport = reportWeather DummyService DummyStorage DummyReporter
