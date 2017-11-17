{-# LANGUAGE DeriveFunctor #-}
module WeatherReporterFree where

import           Control.Monad.Free

-- KISS.
type WeatherData = String

data WeatherF a = Fetch (WeatherData -> a)
                | Store WeatherData a
                | Report WeatherData a
                deriving (Functor)

fetch :: Free WeatherF WeatherData
fetch = liftF $ Fetch id

store :: WeatherData -> Free WeatherF ()
store w = liftF $ Store w ()

report :: WeatherData -> Free WeatherF ()
report w = liftF $ Report w ()

reportWeather :: Free WeatherF ()
reportWeather = do
    w <- fetch
    store w
    report w

-- Now it is easy to write an interpreter for this.

interp :: WeatherF a -> IO a
interp (Fetch f) = return $ f "it's gonna freeze!"
interp (Store w a) = do
    putStrLn $ "I'm also throwing this away: " ++ w
    return a
interp (Report w a) = do
    putStrLn $ "The blackie weather forecast says " ++ w
    return a

dummyWeatherReport :: IO ()
dummyWeatherReport = foldFree interp reportWeather


-- The problem with the interpreter above is that we don't have separation of
-- concerns.
