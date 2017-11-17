module WeatherReporterFunctions where

-- * Some aliases to keep things simple and the discussion focused.
type WeatherData = String

-- Note that we are also using the 'IO' monad as our base monad to simplify
-- this discussion. In practice a richer monad that accounts for error or
-- logging might be more useful.

class WeatherService ws where
    fetchWeather :: ws -> IO WeatherData

-- And we provide a mock implementation.
data DummyService = DummyService

instance WeatherService DummyService where
    fetchWeather _ = return "winter is coming, and I wish I was in the South Hemisphere!"

class Storage s where
    store :: s  -> WeatherData -> IO ()

-- A mock implementation for storage.
data DummyStorage = DummyStorage

instance Storage DummyStorage where
    store _ w = putStrLn $ "I'm just going to throw away this: " ++ w

class Reporter r where
    report :: r -> WeatherData -> IO ()

-- A mock implementation for reporter.

data DummyReporter = DummyReporter

instance Reporter DummyReporter where
    report _ w = putStrLn $ "Here in The Netherlands " ++ w

reportWeather :: (WeatherService ws, Storage s, Reporter r)
              => ws -> s -> r -> IO ()
reportWeather ws s r = do
      w <- fetchWeather ws
      store s w
      report r w

-- And then we have to choose the implementation in the concrete function:
dummyWeatherReport :: IO ()
dummyWeatherReport = reportWeather DummyService DummyStorage DummyReporter
