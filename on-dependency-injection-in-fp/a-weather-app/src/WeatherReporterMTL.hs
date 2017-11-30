{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WeatherReporterMTL where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class

type WeatherData = String

class Monad m => WeatherService m where
    fetch :: m WeatherData

class Monad m => Storage m where
    store :: WeatherData -> m ()

class Monad m => Reporter m where
    report :: WeatherData -> m ()


-- | A dummy implementation of the @WeatherService@
newtype DummyService m a = DummyService { runDummyService :: m a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadIO m => WeatherService (DummyService m) where
    fetch = return "won't get any warmer in December."

-- | A dummy implementation of the @Storage@
newtype DummyStorage m a = DummyStorage { runDummyStorage :: m a }
    deriving (Functor, Applicative, Monad, MonadIO, WeatherService)

-- It seems wrong that the storage has to be an instance the weather service
-- (@WeatherService@) ...

instance MonadIO m => Storage (DummyStorage m) where
    store d = liftIO $ putStrLn $ "No room left for this report: " ++ d

-- | A dummy implementation of the @Reporter@
newtype DummyReporter m a = DummyReporter { runDummyReporter :: m a }
    deriving (Functor, Applicative, Monad, MonadIO, WeatherService, Storage)

-- Ok, now this seems even worse. Now it seems we're putting information about
-- how we're gonna stack our monads :/

instance MonadIO m => Reporter (DummyReporter m) where
    report d = liftIO $ putStrLn $ "Here at the MTL side " ++ d

reportWeather :: (WeatherService m, Storage m, Reporter m) => m ()
reportWeather = do
    w <- fetch
    store w
    report w

dummyWeatherReport :: IO ()
dummyWeatherReport = runDummyService $ runDummyStorage $ runDummyReporter reportWeather

