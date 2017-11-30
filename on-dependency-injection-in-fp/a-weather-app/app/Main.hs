module Main where

import qualified WeatherReporterFree
import qualified WeatherReporterFreeSeparated
import qualified WeatherReporterFunctions
import qualified WeatherReporterFunctionsAndMonads
import qualified WeatherReporterMTL

main :: IO ()
main = do
    putStrLn "Weather report with functions:"
    WeatherReporterFunctions.dummyWeatherReport
    putStrLn ""
    putStrLn "Weather report with functions and monads:"
    WeatherReporterFunctionsAndMonads.dummyWeatherReport
    putStrLn ""
    putStrLn "Weather report with free-monads:"
    WeatherReporterFree.dummyWeatherReport
    putStrLn ""
    putStrLn "Weather report with free-monads, using orthogonal interpreters:"
    WeatherReporterFreeSeparated.dummyWeatherReport
    putStrLn ""
    putStrLn "Weather report with MTL:"
    WeatherReporterMTL.dummyWeatherReport
