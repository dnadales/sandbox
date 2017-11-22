{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
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

-- Now the problem is that we would like to define a program that has type
-- @Free WeatherF ()@:
--
-- > reportWeather :: Free WeatherF ()
-- > reportWeather = do
-- >   w <- fetch
-- >   store w
-- >   report w
--
-- Then, we cannot define fetch having type @Free WeatherServiceF WeatherData@,
-- as we did before!
--
-- > fetch :: Free WeatherServiceF WeatherData
-- > fetch = liftF $ Fetch id
--

-- | Class that represents the relationship between a functor 'sup' containing
-- a functor 'sub'.
class (Functor sub, Functor sup) => sub :-<: sup where
    inj :: sub a -> sup a

-- | A functor contains itself.
instance Functor f => f :-<: f where
    inj = id

-- | A functor is contained in the sum of that functor with another.
instance (Functor f, Functor g) => f :-<: (Sum f g) where
    inj = InL

-- | If a functor 'f' is contained in a functor 'g', then f is contained in the
-- sum of a third functor, say 'h', with 'g'.
instance {-# OVERLAPS #-} (Functor f, Functor g, Functor h, f :-<: g) => f :-<: (Sum h g) where
    inj = InR . inj

-- | Then we can define:
fetch :: (WeatherServiceF :-<: g) => Free g WeatherData
fetch = liftF $ inj $ Fetch id

store :: (StorageF :-<: g) => WeatherData -> Free g ()
store d = liftF $ inj $ Store d ()

report :: (ReporterF :-<: g) => WeatherData -> Free g ()
report d = liftF $ inj $ Report d ()

-- Then we can write out program in the same way as the `WeatherReporterFree`
-- module.
reportWeather :: Free WeatherF ()
reportWeather = do
    w <- fetch
    store w
    report w

dummyServiceInterp :: WeatherServiceF a -> IO a
dummyServiceInterp (Fetch f) = return $ f "it seems sunnier at the MTL side."

dummyStorageInterp :: StorageF a -> IO a
dummyStorageInterp (Store w a) = do
    putStrLn $ "Guess what, this will go to the garbage (collector): " ++ w
    return a

dummyReporterInterp :: ReporterF a -> IO a
dummyReporterInterp (Report w a) = do
    putStrLn $ "And the orthogonal free-monad weather report says " ++ w
    return a

dummyWeatherInterp :: WeatherF a -> IO a
-- I'm missing a `coproduct` operator in Haskell
dummyWeatherInterp (InL service)        = dummyServiceInterp service
dummyWeatherInterp (InR (InL storage))  = dummyStorageInterp storage
dummyWeatherInterp (InR (InR reporter)) = dummyReporterInterp reporter

dummyWeatherReport :: IO ()
dummyWeatherReport = foldFree dummyWeatherInterp reportWeather

-- * Let's try Benjamin's code, to see if we don't get any overlapping instances there

data Interact a = Ask (String -> a) | Tell String a deriving (Functor)

data DataOp a = AddCat String a | GetAllCats ([String] -> a) deriving (Functor)

type CatsApp = Sum Interact DataOp

ask :: (Interact :-<: f) => Free f String
ask = liftF $ inj $ Ask id

tell :: (Interact :-<: f) => String -> Free f ()
tell str = liftF $ inj $ Tell str ()

addCat :: (DataOp :-<: f) => String -> Free f ()
addCat cat = liftF $ inj $ AddCat cat ()

getCats :: (DataOp :-<: f) => Free f [String]
getCats = liftF $ inj $ GetAllCats id

myProgram :: (Interact :-<: f, DataOp :-<: f) => Free f ()
myProgram = ask >>= addCat
