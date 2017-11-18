{-# LANGUAGE DeriveFunctor #-}
module WeatherReporterFreeSeparated where

import           Control.Monad.Free
import           Data.Functor.Sum

-- Let's try to separate concerns.

-- The trick seems to be define two functors, two interpreters, and then merge
-- them by doing some type-level magic.

type WeatherData = String

data WeatherServiceF a = Fetch (WeatherData -> a) deriving (Functor)

fetch :: Free WeatherServiceF WeatherData
fetch = undefined -- We need to make this work for sum types as well!

data StorageF a = Store WeatherData a deriving (Functor)

data ReporterF a = Report WeatherData a deriving (Functor)

type WeatherF  = Sum WeatherServiceF (Sum StorageF ReporterF)

reportWeather :: Free WeatherF ()
reportWeather = do
    w <- fetch
    store w
    report w

dummyServiceInterp :: WeatherServiceF a -> IO a
dummyServiceInterp = undefined

dummyStorageInterp :: StorageF a -> IO a
dummyStorageInterp = undefined

dummyReporterInterp :: ReporterF a -> IO a
dummyReporterInterp = undefined

dummyWeatherInterp :: WeatherF a -> IO a
-- I'm missing a `coproduct` operator in Haskell
dummyWeatherInterp (InL service)        = dummyServiceInterp service
dummyWeatherInterp (InR (InL storage))  = dummyStorageInterp storage
dummyWeatherInterp (InR (InR reporter)) = dummyReporterInterp reporter

-- Then we can write out program in the same way as the `WeatherReporterFree`
-- module.

dummyWeatherReport :: IO ()
dummyWeatherReport = foldFree dummyWeatherInterp reportWeather
