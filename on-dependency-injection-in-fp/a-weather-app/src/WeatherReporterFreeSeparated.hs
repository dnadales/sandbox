{-# LANGUAGE DeriveFunctor #-}
module WeatherReporterFreeSeparated where

import           Control.Monad.Free
import           Data.Functor.Sum

-- Let's try to separate concerns.

-- The trick seems to be define two functors, two interpreters, and then merge
-- them by doing some type-level magic.

type WeatherData = String

data WeatherServiceF a = Fetch (WeatherData -> a) deriving (Functor)

data StorageF a = Store WeatherData a deriving (Functor)

data ReporterF a = Report WeatherData a deriving (Functor)

type WeatherF  = Sum WeatherServiceF (Sum StorageF ReporterF)


dummyServiceInterp :: WeatherF a -> IO a
dummyServiceInterp = undefined

dummyStorageInterp :: StorageF a -> IO a
dummyStorageInterp = undefined

dummyReporterInterp :: StorageF a -> IO a
dummyReporterInterp = undefined

dummyWeatherF :: WeatherF a -> IO a
-- I'm missing this `or` operator in Haskell
dummyWeatherF = dummyServiceInterp `or` dummyStorageInterp `or` dummyReporterInterp
